library(here)
library(dplyr)
library(ggplot2)

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

