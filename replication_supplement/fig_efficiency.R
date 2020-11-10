library(here)
library(reliabilitydiag)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

res.df <-
  readRDS(file = here(
    "replication_supplement/data/sim_Rel_Efficiency_20201015.rds"
  )) %>%
  group_by(Measure, RelDiag.Type, n, m.bins, CEP.true, dist.x, k) %>% summarize(Value = mean(Value), count=n())

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
print(p.eff)
ggsave(here("replication_supplement/plots/Efficiency_PrecipData.pdf"), p.eff, height=15, width=15, units="in")



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
print(p.eff)
ggsave(here("replication_supplement/plots/Efficiency_SolarFlaresData.pdf"), p.eff, height=15, width=15, units="in")



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
print(p.eff)
ggsave(here("replication_supplement/plots/Efficiency_RecidivismData.pdf"), p.eff, height=15, width=15, units="in")



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
print(p.eff)
ggsave(here("replication_supplement/plots/Efficiency_SPFData.pdf"), p.eff, height=15, width=15, units="in")
