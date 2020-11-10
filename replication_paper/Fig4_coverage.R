library(here)
library(dplyr)
library(ggplot2)

df.MC <- readRDS(here(
  "replication_paper/data/sim_Bounds_Coverage_MainArticle_20201015.rds"
))

plot.df <- df.MC %>%
  dplyr::filter(alpha==1 & !(x.region %in% c(0,1)))  %>%
  group_by(n, k, alpha, dist.x, bounds.plot, FC.dist.plot, Bounds.Method) %>%
  summarize(Value=mean(RelDiag_in_Bounds), counter=n()) %>%
  dplyr::filter(counter >= 100)

bounds.plot.labs <-  c("Consistency Bands", "Confidence Bands")
names(bounds.plot.labs) <- c("diagonal", "estimate")
dist.x.labs <-  c("Uniform", "Linear", "Beta Mixture")
names(dist.x.labs) <- c("unif", "rectangular", "beta-unif")
plot.df$dist.x.arrange <- factor(
  plot.df$dist.x,
  levels=c("unif", "rectangular", "beta-unif"))

p.individual <- ggplot(plot.df, aes(x=n, y=Value)) +
  theme_bw() +
  facet_grid(
    bounds.plot~dist.x.arrange,
    labeller = labeller(
      bounds.plot=bounds.plot.labs,
      dist.x.arrange=dist.x.labs)) +
  geom_hline(yintercept=0.9) +
  geom_line(aes(colour=factor(k)), size=1) +
  geom_point(aes(colour=factor(k), shape=Bounds.Method), size=4) +
  theme(legend.position="bottom", legend.text=element_text(size=rel(1))) +
  theme(
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 12)) +
  scale_shape_discrete(
    name = "Uncertainty Quantification via",
    labels = c(
      "continuous asymptotic theory",
      "discrete asymptotic theory",
      "resampling")) +
  scale_color_discrete(name = "Number k of Distinct Forecast Values") +
  scale_x_continuous(trans='log2') +
  ylim(0.8,1) +
  ylab("Empirical Coverage") +
  xlab("Sample Size")

print(p.individual)

ggsave(
  here("replication_paper/plots/Fig4_Coverage.pdf"),
  p.individual,
  height = 8,
  width = 14,
  units = "in"
)
