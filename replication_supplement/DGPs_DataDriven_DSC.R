library(here)
library(reliabilitydiag)
library(dplyr)
library(EnvStats)
library(ggplot2)
library(RColorBrewer)
library(ggExtra)
library(gridExtra)

# Load classic binning and counting reliability diagram function
source(here("replication_supplement/rel_diag_classic.R"))

FC.tbl <- readRDS(file = here("replication_supplement/data/DGPs_FCtbl.rds"))

beta_func <- function(par, df){ sum( (pbeta(df$x, par[1], par[2]) - df$CEP_pav)^2 ) }

# Compute the Beta pdf and cdf fits
out.list <- lapply(1:max(FC.tbl$FC.id), function(FC.id.index){
  X <- FC.tbl %>% dplyr::filter(FC.id==FC.id.index) %>% dplyr::pull(x)
  Y <- FC.tbl %>% dplyr::filter(FC.id==FC.id.index) %>% dplyr::pull(y)

  rel.fit <- reliabilitydiag(x=X,y=Y, region.level = NA)
  CEP.est <- rel.fit$x$cases

  # Beta PDF Fit
  out.pdf <- EnvStats::ebeta(CEP.est$x, method = "mle")
  # Beta CDF Fit
  out.cdf <- optim(c(1,1), beta_func, lower=c(.01,.01), upper=c(200,200), method="L-BFGS-B", df=CEP.est)

  p.rel <- autoplot(rel.fit, type = "discrimination", params_ggMarginal = NA) +
    ggtitle(paste0((FC.tbl%>%dplyr::filter(FC.id==FC.id.index)%>%dplyr::pull(FC.name.print))[1])) +
    annotate(
      "text",
      x = .125,
      y = .94,
      label = sprintf("MCB = .%03d",
                      round(summary(rel.fit)$miscalibration * 1000))
    ) +
    annotate(
      "text",
      x = .125,
      y = .88,
      label = sprintf("DSC = .%03d",
                      round(summary(rel.fit)$discrimination * 1000)),
      color = "red"
    ) +
    annotate(
      "text",
      x = .125,
      y = .82,
      label = sprintf("UNC = .%03d",
                      round(summary(rel.fit)$uncertainty * 1000))
    )

  p.rel <- ggMarginal(
    p.rel,
    type = "hist",
    xparams = list(bins = 100,
                   fill = "grey"),
    yparams = list(bins = 100,
                   fill = "red")
  )

  seq.x <- seq(0.001,0.999,length.out=500)
  p <- autoplot(rel.fit) +
    geom_line(aes(x=seq.x, y=pbeta(seq.x, out.cdf$par[1], out.cdf$par[2])), color="blue") +
    geom_line(aes(x=seq.x, y=0.2*dbeta(seq.x, out.pdf$par[1], out.pdf$par[2])/max(dbeta(seq.x, out.pdf$par[1], out.pdf$par[2]))), color="purple") +
    ggtitle(paste((FC.tbl %>% dplyr::filter(FC.id==FC.id.index) %>% dplyr::pull(FC.type))[1]))

  list(p, out.pdf$parameters, out.cdf$par, FC.id.index, (FC.tbl %>% dplyr::filter(FC.id==FC.id.index) %>% dplyr::pull(FC.type))[1], p.rel)
}
)

p.list <- lapply(1:length(out.list), function(i) out.list[[i]][[1]])
p.rel.list <- lapply(1:length(out.list), function(i) out.list[[i]][[6]])
FC.dist.par <- sapply(1:length(out.list), function(i) out.list[[i]][[2]]) %>% t()
CEP.dist.par <- sapply(1:length(out.list), function(i) out.list[[i]][[3]]) %>% t()
FC.id <- sapply(1:length(out.list), function(i) out.list[[i]][[4]])
FC.type <- sapply(1:length(out.list), function(i) out.list[[i]][[5]])

# Generate distribution tibble
dist.par.hlp <- cbind(FC.dist.par, CEP.dist.par, FC.id)
colnames(dist.par.hlp) <- c("FC.shape1", "FC.shape2", "CEP.shape1", "CEP.shape2", "FC.id")
dist.par.tbl <- as_tibble(dist.par.hlp)
dist.par.tbl$FC.type <- FC.type

## Save data-set wise rel diags in
names(p.rel.list) <- c("EPC", "EMOS", "Logistic", "ENS",
                       "C1 Flares: NOAA", "C1 Flares: DAFFS", "M1 Flares: NOAA", "M1 Flares: DAFFS",
                       "Logit", "GBM", "MTurk", "COMPAS",
                       "SPF #84: Nowcast", "SPF #84: 1Q Ahead", "SPF #84: 4Q Ahead",
                       "SPF Avg: Nowcast", "SPF Avg: 1Q Ahead", "SPF Avg: 4Q Ahead")

# Precip
p.precip <- gridExtra::grid.arrange(grobs=p.rel.list[c("ENS", "EMOS", "EPC", "Logistic")], nrow=1)
ggsave(here("replication_supplement/plots/Precip_DSC.pdf"), p.precip, height=5, width=20, units="in")

# Solar Flares
p.flares <- gridExtra::grid.arrange(grobs=p.rel.list[c("C1 Flares: NOAA", "C1 Flares: DAFFS", "M1 Flares: NOAA", "M1 Flares: DAFFS")], nrow=1)
ggsave(here("replication_supplement/plots/SolarFlares_DSC.pdf"), p.flares, height=5, width=20, units="in")

# Recidivism
p.recid <- gridExtra::grid.arrange(grobs=p.rel.list[c("COMPAS", "MTurk", "Logit", "GBM")], nrow=1)
ggsave(here("replication_supplement/plots/Recidivism_DSC.pdf"), p.recid, height=5, width=20, units="in")

# SPF
p.SPF <- gridExtra::grid.arrange(grobs=p.rel.list[c("SPF #84: Nowcast", "SPF #84: 1Q Ahead", "SPF #84: 4Q Ahead",
                                                    "SPF Avg: Nowcast", "SPF Avg: 1Q Ahead", "SPF Avg: 4Q Ahead")], nrow = 2)
ggsave(here("replication_supplement/plots/SPF_DSC.pdf"), p.SPF, height=10, width=15, units="in")
