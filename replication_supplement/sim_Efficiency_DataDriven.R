
library(rlist)
library(tibble)
library(devtools)
library(EnvStats)
library(ggplot2)
library(dplyr)
library(doParallel)
library(RColorBrewer)
library(reshape2)
library(reliabilitydiag)

source("replication_supplement/rel_diag_classic.R")

dist.par.tbl <- readRDS("./replication_supplement/data/DGPs_DataDriven.rds")

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

saveRDS(res.df.MC, file = "./replication_supplement/data/sim_Rel_Efficiency_20201015.rds")



#######################################################################################################################
###  Efficiency plot for the paper

res.df.MC <- readRDS(file = "./replication_supplement/data/sim_Rel_Efficiency_20201015.rds")
res.df <- res.df.MC %>% group_by(Measure, RelDiag.Type, n, m.bins, CEP.true, dist.x, k) %>% summarize(Value = mean(Value), count=n())

df.plot <- res.df %>%
  filter(RelDiag.Type %in% c("PAV","Bins.fixed","Bins.Q.n")  & Measure=="MSE" & k%in%c(10,50,Inf) & m.bins%in%c("PAV",5,10,50,"n(1/6)","n(2/6)","n(3/6)")) %>%
  arrange(Measure, CEP.true, dist.x, k, n, RelDiag.Type,
          match(RelDiag.Type, c("PAV", "Bins.fixed", "Bins.Q.n")),
          match(m.bins, c("PAV", "5", "10", "50", "n(1/6)", "n(2/6)", "n(3/6)")))
df.plot$m.bins.arrange <- factor(df.plot$m.bins, levels=c("PAV", "5", "10", "50", "n(1/6)", "n(2/6)", "n(3/6)"))

dist.x.labs <- c(c("EMOS", "EPC", "ENS", "Logistic"),
                 c("C1 Flares & NOAA Forecasts", "C1 Flares & DAFFS Forecasts", "M1 Flares & NOAA Forecasts", "M1 Flares & DAFFS Forecasts"),
                 c("Logit", "GBM", "MTurk", "COMPAS"),
                 c("SPF #84: Nowcast", "SPF #84: 1Q-Ahead", "SPF #84: 4Q-Ahead", "SPF Avg: Nowcast", "SPF Avg: 1Q-Ahead", "SPF Avg: 4Q-Ahead") )

names(dist.x.labs) <- c("precip.EMOS", "precip.EPC", "precip.ENS", "precip.Logistic",
                        "C1.NOAA", "C1.DAFFS", "M1.NOAA", "M1.DAFFS",
                        "Recid.Logit", "Recid.GBM", "Recid.MTurk", "Recid.COMPAS",
                        "SPF.84.0Quarter", "SPF.84.1Quarter", "SPF.84.4Quarter", "SPF.Avg.0Quarter","SPF.Avg.1Quarter", "SPF.Avg.4Quarter")

k.labs <-  c("Discrete: k = 10", "Discrete: k = 50", "Continuous")
names(k.labs) <- c("10","50","Inf")

CEP.true.labs <- c("Estimated CEP", "Calibrated CEP")
names(CEP.true.labs) <- c("estimated", "calibrated")

options(scipen=100)



###  PRECIP
plot.df.precip <- df.plot %>% filter(dist.x %in% c("precip.ENS", "precip.EMOS", "precip.EPC", "precip.Logistic"))
plot.df.precip$dist.x <- factor(plot.df.precip$dist.x, levels=c("precip.ENS", "precip.EMOS", "precip.EPC", "precip.Logistic"))

p.eff <- ggplot(plot.df.precip, aes(x=n, y=Value)) +
  theme_bw() +
  geom_line(aes(linetype=RelDiag.Type, color=m.bins.arrange), size=1) +
  facet_grid(CEP.true+k~dist.x, scales="free",
             labeller = labeller(k=k.labs, dist.x=dist.x.labs, CEP.true=CEP.true.labs)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  scale_x_continuous(trans='log2') +
  scale_y_log10() +
  scale_color_manual(values=c( "green3", brewer.pal(n = 4, name = 'Reds')[2:4], brewer.pal(n = 5, name = 'Blues')[3:5]),
                     name = " ",
                     labels=c("CORP","5","10","50", expression(n^{1/6}), expression(n^{1/3}), expression(n^{1/2}))) +
  scale_linetype_manual(values=c("PAV"="solid", "Bins.fixed"="longdash","Bins.Q.n"="dotted"), name = "Binning Method", labels = c("CORP","fixed","n-dependent")) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1), linetype=FALSE) +
  theme(legend.position="bottom") +
  ylab("MSE") +
  xlab("Sample Size n")
p.eff
ggsave("./replication_supplement/plots/Efficiency_PrecipData.pdf", p.eff, height=15, width=12, units="in")



### SOLAR FLARES
plot.df.flares <- df.plot %>% filter(dist.x %in% c("C1.NOAA", "C1.DAFFS", "M1.NOAA", "M1.DAFFS"))
plot.df.flares$dist.x <- factor(plot.df.flares$dist.x, levels=c("C1.NOAA", "C1.DAFFS", "M1.NOAA", "M1.DAFFS"))

p.eff <- ggplot(plot.df.flares, aes(x=n, y=Value)) +
  theme_bw() +
  geom_line(aes(linetype=RelDiag.Type, color=m.bins.arrange), size=1) +
  facet_grid(CEP.true+k~dist.x, scales="free",
             labeller = labeller(k=k.labs, dist.x=dist.x.labs, CEP.true=CEP.true.labs)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  scale_x_continuous(trans='log2') +
  scale_y_log10() +
  scale_color_manual(values=c( "green3", brewer.pal(n = 4, name = 'Reds')[2:4], brewer.pal(n = 5, name = 'Blues')[3:5]),
                     name = " ",
                     labels=c("CORP","5","10","50", expression(n^{1/6}), expression(n^{1/3}), expression(n^{1/2}))) +
  scale_linetype_manual(values=c("PAV"="solid", "Bins.fixed"="longdash","Bins.Q.n"="dotted"), name = "Binning Method", labels = c("CORP","fixed","n-dependent")) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1), linetype=FALSE) +
  theme(legend.position="bottom") +
  ylab("MSE") +
  xlab("Sample Size n")
p.eff
ggsave("./replication_supplement/plots/Efficiency_SolarFlaresData.pdf", p.eff, height=15, width=12, units="in")



### RECIDIVISM
plot.df.recid <- df.plot %>% filter(dist.x %in% c("Recid.Logit", "Recid.GBM", "Recid.MTurk", "Recid.COMPAS"))
plot.df.recid$dist.x <- factor(plot.df.recid$dist.x, levels=c("Recid.COMPAS", "Recid.MTurk", "Recid.Logit", "Recid.GBM"))

p.eff <- ggplot(plot.df.recid, aes(x=n, y=Value)) +
  theme_bw() +
  geom_line(aes(linetype=RelDiag.Type, color=m.bins.arrange), size=1) +
  facet_grid(CEP.true+k~dist.x, scales="free",
             labeller = labeller(k=k.labs, dist.x=dist.x.labs, CEP.true=CEP.true.labs)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  scale_x_continuous(trans='log2') +
  scale_y_log10() +
  scale_color_manual(values=c( "green3", brewer.pal(n = 4, name = 'Reds')[2:4], brewer.pal(n = 5, name = 'Blues')[3:5]),
                     name = " ",
                     labels=c("CORP","5","10","50", expression(n^{1/6}), expression(n^{1/3}), expression(n^{1/2}))) +
  scale_linetype_manual(values=c("PAV"="solid", "Bins.fixed"="longdash","Bins.Q.n"="dotted"), name = "Binning Method", labels = c("CORP","fixed","n-dependent")) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1), linetype=FALSE) +
  theme(legend.position="bottom") +
  ylab("MSE") +
  xlab("Sample Size n")
p.eff
ggsave("./replication_supplement/plots/Efficiency_RecidivismData.pdf", p.eff, height=15, width=12, units="in")



### SPF
plot.df.SPF <- df.plot %>% filter(dist.x %in% c("SPF.84.0Quarter", "SPF.84.1Quarter", "SPF.84.4Quarter", "SPF.Avg.0Quarter", "SPF.Avg.1Quarter", "SPF.Avg.4Quarter"))
plot.df.SPF$dist.x <- factor(plot.df.SPF$dist.x, levels=c("SPF.84.0Quarter", "SPF.84.1Quarter", "SPF.84.4Quarter", "SPF.Avg.0Quarter", "SPF.Avg.1Quarter", "SPF.Avg.4Quarter"))

p.eff <- ggplot(plot.df.SPF, aes(x=n, y=Value)) +
  theme_bw() +
  geom_line(aes(linetype=RelDiag.Type, color=m.bins.arrange), size=1) +
  facet_grid(CEP.true+k~dist.x, scales="free",
             labeller = labeller(k=k.labs, dist.x=dist.x.labs, CEP.true=CEP.true.labs)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  scale_x_continuous(trans='log2') +
  scale_y_log10() +
  scale_color_manual(values=c( "green3", brewer.pal(n = 4, name = 'Reds')[2:4], brewer.pal(n = 5, name = 'Blues')[3:5]),
                     name = " ",
                     labels=c("CORP","5","10","50", expression(n^{1/6}), expression(n^{1/3}), expression(n^{1/2}))) +
  scale_linetype_manual(values=c("PAV"="solid", "Bins.fixed"="longdash","Bins.Q.n"="dotted"), name = "Binning Method", labels = c("CORP","fixed","n-dependent")) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1), linetype=FALSE) +
  theme(legend.position="bottom") +
  ylab("MSE") +
  xlab("Sample Size n")
p.eff
ggsave("./replication_supplement/plots/Efficiency_SPFData.pdf", p.eff, height=15, width=15, units="in")


