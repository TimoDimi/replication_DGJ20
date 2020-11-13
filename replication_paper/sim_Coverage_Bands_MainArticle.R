library(here)
library(reliabilitydiag)
library(doParallel)
library(dplyr)

# Multiple hours of runtime.
# Only run if necessary to confirm reproducibility.

M.MC <- 1000
n.set <- 2^seq(6,13)
k.set <- c(10, 20 ,50, Inf)
alpha.set <- c(1)

dist.x.set <- c("unif", "rectangular", "beta-unif")
bounds.plot.set <- c("diagonal","estimate")

ratio.beta.unif <- 4
beta.param.2 <- 10

core.max <- 70
cl <- makeCluster(min(parallel::detectCores()-1, M.MC, core.max) )
registerDoParallel(cl)

start.time <- Sys.time()
df.MC <- foreach(
  i_MC = 1:M.MC,
  .combine=rbind,
  .packages=c("RelDiag", "dplyr", "tibble")
)%dopar%{
  set.seed(i_MC) # set seed for reproducibility
  RealDiag.CIfreq.df <- tibble()

  for (n in n.set){
    for (k in k.set){
      for (dist.x in dist.x.set){
        if (k==Inf){
          if (dist.x=="rectangular"){
            x <- 1/3*(sqrt(15*runif(n)+1) - 1)
          } else if (dist.x=="beta-unif") {
            x <- c(
              rbeta(floor(n*(ratio.beta.unif-1)/ratio.beta.unif),1,beta.param.2),
              runif(ceiling(n/ratio.beta.unif)))
          } else {
            x <- runif(n)
          }
        } else {
          if (dist.x=="rectangular"){
            prob.unnormalized <- seq(1,4,length.out=k)
            prob <- prob.unnormalized/sum(prob.unnormalized)
            x <-(sample(1:k, n, replace=TRUE, prob=prob) - 0.5)/k
          } else if (dist.x=="beta-unif") {
            pdf <- (ratio.beta.unif-1)/ratio.beta.unif *
              dbeta( ((1:k)-0.5)/k ,1,beta.param.2) +
              1/ratio.beta.unif * dunif(((1:k)-0.5)/k)
            prob <- pdf/sum(pdf)
            x <- (sample(1:k, n, replace=TRUE, prob=prob) - 0.5)/k
          } else {
            x <- (sample(1:k, n, replace=TRUE) - 0.5)/k
          }
        }

        for (alpha in alpha.set){
          p <- x^alpha
          y <- rbinom(n, 1, p)

          for (bounds.plot in bounds.plot.set){

            rel <- tryCatch(
              reliabilitydiag(
                X=p, y=y,
                region.level = 0.9, region.position = bounds.plot, n.boot=100),
              error=function(e) NULL
            )

            if (!is.null(rel)){
              df.bounds <- tibble(
                lower = approx(
                  x=c(0,rel$X$regions$x,1),
                  y=c(0,rel$X$regions$lower,1),
                  xout=rel$X$cases$x)$y,
                upper = approx(
                  x=c(0,rel$X$regions$x,1),
                  y=c(0,rel$X$regions$upper,1),
                  xout=rel$X$cases$x)$y,
                x=rel$X$cases$x,
                bin_id=rel$X$cases$bin_id
              ) %>%
                select(x, lower, upper, bin_id) %>%
                merge(rel$X$bins[,c("bin_id","CEP_pav")], by="bin_id") %>%
                as.tibble()

              if (bounds.plot == "diagonal"){
                df.bounds <- df.bounds %>%
                  mutate(RelDiag_in_Bounds = (CEP_pav >= lower & CEP_pav <= upper))
              } else {
                df.bounds <- df.bounds %>%
                  mutate(RelDiag_in_Bounds = (x >= lower & x <= upper))
              }

              if (k == Inf){
                df.bounds <- df.bounds %>%
                  mutate(x.region=findInterval(x, seq(0,1,by=0.05))) %>%
                  group_by(x.region) %>%
                  summarize(
                    RelDiag_in_Bounds=mean(RelDiag_in_Bounds),
                    .groups="drop") %>%
                  mutate(x.region=(x.region-0.5)/20)
                df.append <- df.bounds %>%
                  add_column(
                    n=n,
                    k=k,
                    alpha=alpha,
                    dist.x=dist.x,
                    bounds.plot=bounds.plot,
                    FC.dist.plot=rel$X$xinfo$type,
                    Bounds.Method=rel$X$regions$method[[1]],
                    i_MC=i_MC)
              } else {
                df.append <- df.bounds %>%
                  group_by(x) %>%
                  summarize(
                    RelDiag_in_Bounds=mean(RelDiag_in_Bounds),
                    .groups="drop") %>%
                  rename(x.region=x) %>%
                  add_column(
                    n=n,
                    k=k,
                    alpha=alpha,
                    dist.x=dist.x,
                    bounds.plot=bounds.plot,
                    FC.dist.plot=rel$X$xinfo$type,
                    Bounds.Method=rel$X$regions$method[[1]],
                    i_MC=i_MC)
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

saveRDS(
  df.MC,
  file = here(
    "replication_paper/data/sim_Bounds_Coverage_MainArticle_20201015.rds"
  )
)
