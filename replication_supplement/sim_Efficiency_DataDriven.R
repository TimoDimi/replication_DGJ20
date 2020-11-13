library(here)
library(doParallel)
library(reliabilitydiag)
library(dplyr)

source(here("replication_supplement/rel_diag_classic.R"))

dist.par.tbl <- readRDS(here("replication_supplement/data/DGPs_DataDriven.rds"))

# Multiple hours of runtime.
# Only run if necessary to confirm reproducibility.

M <- 1000
n.set <- 2^seq(6,13)
k.set <- c(10, 50, Inf)
CEP.true.set <- c("calibrated", "estimated")

bins.list <- list(5,10,50,"Q-n(1/6)","Q-n(2/6)","Q-n(3/6)")

core.max <- 70
cl <- makeCluster(min(parallel::detectCores()-1, M, core.max) )
registerDoParallel(cl)
start.time <- Sys.time()
res.df.MC <- foreach(i_MC = 1:M, .combine=rbind, .packages=c("reliabilitydiag", "dplyr", "tibble", "ggplot2"), .export=c("rel.diag.classic"))%dopar%{
  set.seed(i_MC) # set seed for reproducibility
  res.df <- tibble()

  for (n in n.set){
    for (k in k.set){
      for (dist.x in dist.par.tbl$FC.type){
        dist.tbl.select <- dist.par.tbl %>% filter(FC.type==dist.x)
        if (k==Inf){
          x <- rbeta(n, dist.tbl.select$FC.shape1, dist.tbl.select$FC.shape2)
        } else {
          pdf <- dbeta( ((1:k)-0.5)/k, dist.tbl.select$FC.shape1, dist.tbl.select$FC.shape2)
          prob <- pdf/sum(pdf)
          x <- (sample(1:k, n, replace=TRUE, prob=prob) - 0.5)/k
        }

        for (CEP.true in CEP.true.set){
          if (CEP.true == "calibrated"){
            p <- x
          } else {
            p <- pbeta(x, dist.tbl.select$CEP.shape1, dist.tbl.select$CEP.shape2)
          }

          y <- rbinom(n, 1, p)

          # PAV Reliability Diagram
          rel <- reliabilitydiag::reliabilitydiag(x=x, y=y,  region.method=NA)
          CEP.df <- rel$x$cases
          if (CEP.true == "calibrated"){
            CEP.df <- CEP.df %>% mutate(CEPtrue = x)
          } else {
            CEP.df <- CEP.df %>% mutate(CEPtrue = pbeta(x, dist.tbl.select$CEP.shape1, dist.tbl.select$CEP.shape2))
          }

          MSE <- with(CEP.df, mean((CEP_pav-CEPtrue)^2))

          if (!is.null(MSE)){
            res.df <- rbind(res.df, tibble(Measure="MSE", RelDiag.Type="PAV", MC.rep=i_MC, n=n, m.bins="PAV", CEP.true=CEP.true, dist.x=dist.x, k=k, Value=MSE))
          }

          # "Classic" Reliability Diagrams
          for (bins.index in 1:length(bins.list)){
            if (class(bins.list[[bins.index]]) == "numeric"){
              m.bins <- bins.list[[bins.index]]
            } else if (bins.list[[bins.index]] == "n(1/6)") {
              m.bins <- floor(n^(1/6))
            } else if (bins.list[[bins.index]] == "n(2/6)") {
              m.bins <- floor(n^(2/6))
            } else if (bins.list[[bins.index]] == "n(3/6)") {
              m.bins <- floor(n^(3/6))
            } else if (bins.list[[bins.index]] == "n(4/6)") {
              m.bins <- floor(n^(4/6))
            } else if (bins.list[[bins.index]] == "Q-n(1/6)") {
              bins.amount <- max(3,floor(n^(1/6)))
              m.bins <- unique(c(0,quantile(x, (1:(bins.amount-1))/bins.amount),1))
            } else if (bins.list[[bins.index]] == "Q-n(2/6)") {
              bins.amount <- max(3,floor(n^(2/6)))
              m.bins <- unique(c(0,quantile(x, (1:(bins.amount-1))/bins.amount),1))
            } else if (bins.list[[bins.index]] == "Q-n(3/6)") {
              bins.amount <- max(3,floor(n^(3/6)))
              m.bins <- unique(c(0,quantile(x, (1:(bins.amount-1))/bins.amount),1))
            }

            RelDiag.Bins <- tryCatch(rel.diag.classic(y, x, bins=m.bins), error=function(e) NULL)

            if (!is.null(RelDiag.Bins)){
              df <- with(RelDiag.Bins, merge(df, df.bins, by="bin.index")) %>% as.tibble() %>% arrange(FC)

              if (CEP.true == "calibrated"){
                CEP.df.classic <- df %>% summarize(x = FC, CEPtrue = x, CEP.est = bin.freq)
              } else {
                CEP.df.classic <- df %>% summarize(x = FC, CEPtrue = pbeta(x, dist.tbl.select$CEP.shape1, dist.tbl.select$CEP.shape2), CEP.est = bin.freq)
              }

              MSE.classic <- with(CEP.df.classic, mean((CEP.est-CEPtrue)^2))

              if (class(bins.list[[bins.index]])=="numeric"){
                res.df <- rbind(res.df, data.frame(Measure="MSE", RelDiag.Type="Bins.fixed", MC.rep=i_MC, n=n, m.bins=as.character(bins.list[[bins.index]]), CEP.true=CEP.true, dist.x=dist.x, k=k, Value=MSE.classic))
              } else if (class(bins.list[[bins.index]])=="character" & substring(bins.list[[bins.index]],1,1)=="Q"){
                res.df <- rbind(res.df, data.frame(Measure="MSE", RelDiag.Type="Bins.Q.n", MC.rep=i_MC, n=n, m.bins=substring(bins.list[[bins.index]],3), CEP.true=CEP.true, dist.x=dist.x, k=k, Value=MSE.classic))
              } else {
                res.df <- rbind(res.df, data.frame(Measure="MSE", RelDiag.Type="Bins.n", MC.rep=i_MC, n=n, m.bins=as.character(bins.list[[bins.index]]), CEP.true=CEP.true, dist.x=dist.x, k=k, Value=MSE.classic))
              }
            }
          }
        }
      }
    }
  }

  res.df
}
stopCluster(cl)
end.time <- Sys.time()
(run.time <- end.time-start.time)

saveRDS(res.df.MC, file = here("replication_supplement/data/sim_Rel_Efficiency_20201015.rds"))
