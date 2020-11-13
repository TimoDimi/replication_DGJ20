library(here)
library(doParallel)
library(reliabilitydiag)
library(dplyr)

dist.par.tbl <- readRDS(here("replication_supplement/data/DGPs_DataDriven.rds"))

# Multiple hours of runtime.
# Only run if necessary to confirm reproducibility.

M.MC <- 1000
n.set <- 2^seq(6,13)
k.set <- c(10, 20 ,50, Inf)
CEP.true.set <- c("calibrated", "estimated")
bounds.plot.set <- c("diagonal","estimate")

core.max <- 70
cl <- makeCluster(min(parallel::detectCores()-1, M.MC, core.max) )
registerDoParallel(cl)

start.time <- Sys.time()
df.MC <- foreach(i_MC = 1:M.MC, .combine=rbind, .packages=c("reliabilitydiag", "dplyr", "tibble", "ggplot2"))%dopar%{
  set.seed(i_MC) # set seed for reproducibility
  RealDiag.CIfreq.df <- tibble()

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

          for (bounds.plot in bounds.plot.set){
            rel <- tryCatch(reliabilitydiag(p=p, y=y, region.level = 0.9, region.position = bounds.plot, n.boot=100), error=function(e) NULL)

            if (!is.null(rel)){
              df.bounds <- tibble(lower = approx(x=c(0,rel$p$regions$x,1), y=c(0,rel$p$regions$lower,1), xout=rel$p$cases$x)$y,
                                  upper = approx(x=c(0,rel$p$regions$x,1), y=c(0,rel$p$regions$upper,1), xout=rel$p$cases$x)$y,
                                  x=rel$p$cases$x,
                                  bin_id=rel$p$cases$bin_id) %>%
                select(x, lower, upper, bin_id) %>%
                merge(rel$p$bins[,c("bin_id","CEP_pav")], by="bin_id") %>% as.tibble()

              if (bounds.plot == "diagonal"){
                df.bounds <- df.bounds %>% mutate(RelDiag_in_Bounds = (CEP_pav >= lower & CEP_pav <= upper))
              } else {
                df.bounds <- df.bounds %>% mutate(RelDiag_in_Bounds = (x >= lower & x <= upper))
              }

              if (k == Inf){
                df.bounds <- df.bounds %>% mutate(x.region=findInterval(x, seq(0,1,by=0.05))) %>%
                  group_by(x.region) %>% summarize(RelDiag_in_Bounds=mean(RelDiag_in_Bounds), .groups="drop") %>%
                  mutate(x.region=(x.region-0.5)/20)
                df.append <- df.bounds %>% add_column(n=n, k=k, CEP.true=CEP.true, dist.x=dist.x, bounds.plot=bounds.plot, FC.dist.plot=rel$p$xinfo$type, Bounds.Method=rel$p$regions$method[[1]], i_MC=i_MC)
              } else {
                df.append <- df.bounds %>% group_by(x) %>% summarize(RelDiag_in_Bounds=mean(RelDiag_in_Bounds), .groups="drop") %>% rename(x.region=x) %>%
                  add_column(n=n, k=k, CEP.true=CEP.true, dist.x=dist.x, bounds.plot=bounds.plot, FC.dist.plot=rel$p$xinfo$type, Bounds.Method=rel$p$regions$method[[1]], i_MC=i_MC)
              }

              # Add multiple (identical) rows for multiple values of the FCs
              RealDiag.CIfreq.df <- bind_rows(RealDiag.CIfreq.df, df.append)

            }
          }
        }
      }
    }
  }
  RealDiag.CIfreq.df
}
stopCluster(cl)
end.time <- Sys.time()
(run.time <- end.time-start.time)
head(df.MC)

# Save in two separate files due to the 100MB Github limit!
df.MC1 <- df.MC %>% dplyr::filter(i_MC <= 500)
df.MC2 <- df.MC %>% dplyr::filter(i_MC > 500)

saveRDS(df.MC1, file = here("replication_supplement/data/sim_Bounds_Coverage_20201015_Part1.rds"))
saveRDS(df.MC2, file = here("replication_supplement/data/sim_Bounds_Coverage_20201015_Part2.rds"))
# df.MC <- readRDS(file = "./replication_supplement/data/sim_Bounds_Coverage_20201015.rds")
