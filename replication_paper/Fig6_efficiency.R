library(here)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


res.df.MC <- readRDS(file = here(
  "replication_paper/data/sim_Rel_Efficiency_MainArticle_20201015.rds"
))

res.df <- res.df.MC %>%
  group_by(Measure, RelDiag.Type, n, m.bins, alpha, dist.x, k) %>%
  summarize(Value = mean(Value), count=n())

df.plot <- res.df %>%
  filter(
    RelDiag.Type %in% c("PAV","Bins.fixed","Bins.Q.n") &
      alpha==1  &
      Measure=="MSE" &
      k%in%c(10,50,Inf) &
      m.bins%in%c("PAV",5,10,50,"n(1/6)","n(2/6)","n(3/6)")) %>%
  arrange(
    Measure,
    alpha,
    dist.x,
    k,
    n,
    RelDiag.Type,
    match(RelDiag.Type, c("PAV", "Bins.fixed", "Bins.Q.n")),
    match(m.bins, c("PAV", "5", "10", "50", "n(1/6)", "n(2/6)", "n(3/6)")))

df.plot$m.bins.arrange <- factor(
  df.plot$m.bins,
  levels=c("PAV", "5", "10", "50", "n(1/6)", "n(2/6)", "n(3/6)"))

df.plot$dist.x.arrange <- factor(
  df.plot$dist.x,
  levels=c("unif", "rectangular", "beta-unif"))
dist.x.labs <-  c("Uniform", "Linear", "Beta Mixture")
names(dist.x.labs) <- c("unif", "rectangular", "beta-unif")

k.labs <-  c("Discrete: k = 10", "Discrete: k = 50", "Continuous")
names(k.labs) <- c("10","50","Inf")

options(scipen=100)

p.eff <- ggplot(df.plot, aes(x=n, y=Value)) +
  theme_bw() +
  geom_line(aes(linetype=RelDiag.Type, color=m.bins.arrange), size=1) +
  facet_grid(
    k~dist.x.arrange,
    labeller = labeller(dist.x.arrange=dist.x.labs, k=k.labs), scales="free") +
  theme(
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 12)) +
  scale_x_continuous(trans='log2') +
  scale_y_log10() +
  scale_color_manual(
    values=c(
      "green3",
      brewer.pal(n = 4, name = 'Reds')[2:4],
      brewer.pal(n = 5, name = 'Blues')[3:5]),
    name = " ",
    labels=c(
      "CORP",
      "5","10","50",
      expression(n^{1/6}), expression(n^{1/3}), expression(n^{1/2}))) +
  scale_linetype_manual(
    values=c("PAV"="solid", "Bins.fixed"="longdash","Bins.Q.n"="dotted"),
    name = "Binning Method",
    labels = c("CORP","fixed","n-dependent")) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1), linetype=FALSE) +
  theme(legend.position="bottom") +
  ylab("MSE") +
  xlab("Sample Size n")

print(p.eff)

ggsave(
  here("replication_paper/plots/Fig6_Efficiency.pdf"),
  p.eff,
  height = 8,
  width = 14,
  units = "in"
)

