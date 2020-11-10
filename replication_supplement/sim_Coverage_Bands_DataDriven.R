library(doParallel)
library(dplyr)
library(tibble)
library(ggplot2)
library(reliabilitydiag)
library(here)

dist.par.tbl <- readRDS(here("replication_supplement/data/DGPs_DataDriven.rds"))


runSimulations <- FALSE
if (isTRUE(runSimulations)) {

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

}


###########################################################################################
###########################################################################################
### Coverage Plot for the paper

plot.df <-
  # Reading in df.MC
  dplyr::bind_rows(
    readRDS(file = here("replication_supplement/data/sim_Bounds_Coverage_20201015_Part1.rds")),
    readRDS(file = here("replication_supplement/data/sim_Bounds_Coverage_20201015_Part2.rds"))
  ) %>%
  # Take values with highest counter only
  dplyr::filter(!(x.region %in% c(0,1)))  %>%
  group_by(n, k, CEP.true, dist.x, bounds.plot, Bounds.Method) %>%
  summarize(Value=mean(RelDiag_in_Bounds), counter=n()) %>%
  dplyr::filter(counter >= 100) %>% ungroup() %>%
  group_by(n, k, CEP.true, dist.x, bounds.plot) %>%
  filter(counter == max(counter))



### PRECIP

# Filter Precip only
plot.df.precip <- plot.df %>% filter(dist.x %in% c("precip.EMOS", "precip.EPC", "precip.ENS", "precip.Logistic"))
plot.df.precip$dist.x <- factor(plot.df.precip$dist.x, levels=c("precip.ENS", "precip.EMOS", "precip.EPC", "precip.Logistic"))

bounds.plot.labs <-  c("Consistency Bands", "Confidence Bands")
names(bounds.plot.labs) <- c("diagonal", "estimate")
dist.x.labs <-c("EMOS", "ENS", "EPC", "Logistic")
names(dist.x.labs) <- c("precip.EMOS", "precip.ENS", "precip.EPC", "precip.Logistic")
CEP.true.labs <- c("Estimated CEP", "Calibrated CEP")
names(CEP.true.labs) <- c("estimated", "calibrated")

p.individual <- ggplot(plot.df.precip, aes(x=n, y=Value)) + theme_bw() +
  facet_grid(CEP.true+bounds.plot~dist.x,
             labeller = labeller(bounds.plot=bounds.plot.labs, dist.x=dist.x.labs, CEP.true=CEP.true.labs)) +
  geom_hline(yintercept=0.9) +
  geom_line(aes(colour=factor(k)), size=1) +
  geom_point(aes(colour=factor(k), shape=Bounds.Method), size=4) +
  theme(legend.position="bottom", legend.text=element_text(size=rel(1))) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  scale_shape_discrete(name = "Uncertainty Quantification via", labels = c("continuous asymptotic theory", "discrete asymptotic theory", "resampling")) +
  scale_color_discrete(name = "Number k of Distinct Forecast Values") +
  scale_x_continuous(trans='log2') +
  ylab("Empirical Coverage") +
  xlab("Sample Size")
print(p.individual)
ggsave(here("replication_supplement/plots/sim_CoverageDefault_PrecipData.pdf"), p.individual, height=15, width=15,units="in")




### SOLAR FLARES

# Filter Solar Flares only
plot.df.flares <- plot.df %>% filter(dist.x %in% c("C1.NOAA", "C1.DAFFS", "M1.NOAA", "M1.DAFFS"))
plot.df.flares$dist.x <- factor(plot.df.flares$dist.x, levels=c("C1.NOAA", "C1.DAFFS", "M1.NOAA", "M1.DAFFS"))

bounds.plot.labs <-  c("Consistency Bands", "Confidence Bands")
names(bounds.plot.labs) <- c("diagonal", "estimate")
dist.x.labs <- c("C1 Flares & NOAA Forecasts", "C1 Flares & DAFFS Forecasts", "M1 Flares & NOAA Forecasts", "M1 Flares & DAFFS Forecasts")
names(dist.x.labs) <- c("C1.NOAA", "C1.DAFFS", "M1.NOAA", "M1.DAFFS")
CEP.true.labs <- c("Estimated CEP", "Calibrated CEP")
names(CEP.true.labs) <- c("estimated", "calibrated")

p.individual <- ggplot(plot.df.flares, aes(x=n, y=Value)) + theme_bw() +
  facet_grid(CEP.true+bounds.plot~dist.x,
             labeller = labeller(bounds.plot=bounds.plot.labs, dist.x=dist.x.labs, CEP.true=CEP.true.labs)) +
  geom_hline(yintercept=0.9) +
  geom_line(aes(colour=factor(k)), size=1) +
  geom_point(aes(colour=factor(k), shape=Bounds.Method), size=4) +
  theme(legend.position="bottom", legend.text=element_text(size=rel(1))) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  scale_shape_discrete(name = "Uncertainty Quantification via", labels = c("continuous asymptotic theory", "discrete asymptotic theory", "resampling")) +
  scale_color_discrete(name = "Number k of Distinct Forecast Values") +
  scale_x_continuous(trans='log2') +
  ylab("Empirical Coverage") +
  xlab("Sample Size")
print(p.individual)
ggsave(here("replication_supplement/plots/sim_CoverageDefault_SolarFlaresData.pdf"), p.individual, height=15, width=15,units="in")



### RECIDIVISM

# Filter Recidivism only
plot.df.recid <- plot.df %>% filter(dist.x %in% c("Recid.Logit", "Recid.GBM", "Recid.MTurk", "Recid.COMPAS"))
plot.df.recid$dist.x <- factor(plot.df.recid$dist.x, levels=c("Recid.COMPAS", "Recid.MTurk", "Recid.Logit", "Recid.GBM"))

bounds.plot.labs <-  c("Consistency Bands", "Confidence Bands")
names(bounds.plot.labs) <- c("diagonal", "estimate")
dist.x.labs <- c("Logistic", "GBM", "MTurk", "COMPAS")
names(dist.x.labs) <- c("Recid.Logit", "Recid.GBM", "Recid.MTurk", "Recid.COMPAS")
CEP.true.labs <- c("Estimated CEP", "Calibrated CEP")
names(CEP.true.labs) <- c("estimated", "calibrated")

p.individual <- ggplot(plot.df.recid, aes(x=n, y=Value)) + theme_bw() +
  facet_grid(CEP.true+bounds.plot~dist.x,
             labeller = labeller(bounds.plot=bounds.plot.labs, dist.x=dist.x.labs, CEP.true=CEP.true.labs)) +
  geom_hline(yintercept=0.9) +
  geom_line(aes(colour=factor(k)), size=1) +
  geom_point(aes(colour=factor(k), shape=Bounds.Method), size=4) +
  theme(legend.position="bottom", legend.text=element_text(size=rel(1))) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  scale_shape_discrete(name = "Uncertainty Quantification via", labels = c("continuous asymptotic theory", "discrete asymptotic theory", "resampling")) +
  scale_color_discrete(name = "Number k of Distinct Forecast Values") +
  scale_x_continuous(trans='log2') +
  ylab("Empirical Coverage") +
  xlab("Sample Size")
print(p.individual)
ggsave(here("replication_supplement/plots/sim_CoverageDefault_Recidivism.pdf"), p.individual, height=15, width=15,units="in")


### SPF

# Filter SPF only
plot.df.SPF <- plot.df %>% filter(dist.x %in% c("SPF.84.0Quarter", "SPF.84.1Quarter", "SPF.84.4Quarter", "SPF.Avg.0Quarter", "SPF.Avg.1Quarter", "SPF.Avg.4Quarter"))

bounds.plot.labs <-  c("Consistency Bands", "Confidence Bands")
names(bounds.plot.labs) <- c("diagonal", "estimate")
dist.x.labs <-c("SPF #84: Nowcast", "SPF #84: 1Q-Ahead", "SPF #84: 4Q-Ahead", "SPF Avg: Nowcast", "SPF Avg: 1Q-Ahead", "SPF Avg: 4Q-Ahead")
names(dist.x.labs) <- c("SPF.84.0Quarter", "SPF.84.1Quarter", "SPF.84.4Quarter", "SPF.Avg.0Quarter", "SPF.Avg.1Quarter", "SPF.Avg.4Quarter")
CEP.true.labs <- c("Estimated CEP", "Calibrated CEP")
names(CEP.true.labs) <- c("estimated", "calibrated")

p.individual <- ggplot(plot.df.SPF, aes(x=n, y=Value)) + theme_bw() +
  facet_grid(CEP.true+bounds.plot~dist.x,
             labeller = labeller(bounds.plot=bounds.plot.labs, dist.x=dist.x.labs, CEP.true=CEP.true.labs)) +
  geom_hline(yintercept=0.9) +
  geom_line(aes(colour=factor(k)), size=1) +
  geom_point(aes(colour=factor(k), shape=Bounds.Method), size=4) +
  theme(legend.position="bottom", legend.text=element_text(size=rel(1))) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  scale_shape_discrete(name = "Uncertainty Quantification via", labels = c("continuous asymptotic theory", "discrete asymptotic theory", "resampling")) +
  scale_color_discrete(name = "Number k of Distinct Forecast Values") +
  scale_x_continuous(trans='log2') +
  ylab("Empirical Coverage") +
  xlab("Sample Size")
print(p.individual)
ggsave(here("replication_supplement/plots/sim_CoverageDefault_SPF.pdf"), p.individual, height=15, width=15,units="in")

