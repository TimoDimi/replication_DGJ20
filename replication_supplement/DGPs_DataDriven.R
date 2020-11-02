
library(rlist)
library(tibble)
library(EnvStats)
library(ggplot2)
library(dplyr)
library(doParallel)
library(RColorBrewer)
library(reshape2)


# devtools::install_github("https://github.com/aijordan/reliabilitydiag")
# devtools::install_local("reliabilitydiag_0.1.0.tar.gz")
library(reliabilitydiag)

# Load classic binning and counting reliability diagram function
source("replication_supplement/rel_diag_classic.R")

# Load data sets
load("data/spf.gdp.long.rda")
load("data/SF.FC.C1.rda")
load("data/SF.FC.M1.rda")
load("data/recid.rda")


# Precip Forecasts
FC.tbl <- tibble(x=precip_Niamey_2016$EPC, y=precip_Niamey_2016$obs, FC.type="precip.EPC", FC.id=1)
FC.tbl <- bind_rows(FC.tbl, tibble(x=precip_Niamey_2016$EMOS, y=precip_Niamey_2016$obs, FC.type="precip.EMOS", FC.id=2))
FC.tbl <- bind_rows(FC.tbl, tibble(x=precip_Niamey_2016$Logistic, y=precip_Niamey_2016$obs, FC.type="precip.Logistic", FC.id=3))
FC.tbl <- bind_rows(FC.tbl, tibble(x=precip_Niamey_2016$ENS, y=precip_Niamey_2016$obs, FC.type="precip.ENS", FC.id=4))
# M1 Solar Flares
FC.tbl <- bind_rows(FC.tbl, tibble(x=SF.FC.M1$NOAA, y=SF.FC.M1$rlz.M1, FC.type="M1.NOAA", FC.id=5))
FC.tbl <- bind_rows(FC.tbl, tibble(x=SF.FC.M1$DAFFS, y=SF.FC.M1$rlz.M1, FC.type="M1.DAFFS", FC.id=6))
# C1 Solar Flares
FC.tbl <- bind_rows(FC.tbl, tibble(x=SF.FC.C1$NOAA, y=SF.FC.C1$rlz.C1, FC.type="C1.NOAA", FC.id=7))
FC.tbl <- bind_rows(FC.tbl, tibble(x=SF.FC.C1$DAFFS, y=SF.FC.C1$rlz.C1, FC.type="C1.DAFFS", FC.id=8))
# Recid
FC.tbl <- bind_rows(FC.tbl, tibble(x=recid$logitpredprobs, y=recid$two_year_recid, FC.type="Recid.Logit", FC.id=9))
FC.tbl <- bind_rows(FC.tbl, tibble(x=recid$gbmpredprobs, y=recid$two_year_recid, FC.type="Recid.GBM", FC.id=10))
FC.tbl <- bind_rows(FC.tbl, tibble(x=recid$mturkpredprobs, y=recid$two_year_recid, FC.type="Recid.MTurk", FC.id=11))
FC.tbl <- bind_rows(FC.tbl, tibble(x=recid$compaspredprobs.linear, y=recid$two_year_recid, FC.type="Recid.COMPAS", FC.id=12))
# SPF
FC.tbl <- bind_rows(FC.tbl, tibble(x=(filter(spf.gdp.long, ID=="84" & FC.Horizon==0))$Prob.Forecast, y=(filter(spf.gdp.long, ID=="84" & FC.Horizon==0))$gdp.first.recess, FC.type="SPF.84.0Quarter", FC.id=13))
FC.tbl <- bind_rows(FC.tbl, tibble(x=(filter(spf.gdp.long, ID=="84" & FC.Horizon==1))$Prob.Forecast, y=(filter(spf.gdp.long, ID=="84" & FC.Horizon==1))$gdp.first.recess, FC.type="SPF.84.1Quarter", FC.id=14))
FC.tbl <- bind_rows(FC.tbl, tibble(x=(filter(spf.gdp.long, ID=="84" & FC.Horizon==4))$Prob.Forecast, y=(filter(spf.gdp.long, ID=="84" & FC.Horizon==4))$gdp.first.recess, FC.type="SPF.84.4Quarter", FC.id=15))
FC.tbl <- bind_rows(FC.tbl, tibble(x=(filter(spf.gdp.long, ID=="0" & FC.Horizon==0))$Prob.Forecast, y=(filter(spf.gdp.long, ID=="0" & FC.Horizon==0))$gdp.first.recess, FC.type="SPF.Avg.0Quarter", FC.id=16))
FC.tbl <- bind_rows(FC.tbl, tibble(x=(filter(spf.gdp.long, ID=="0" & FC.Horizon==1))$Prob.Forecast, y=(filter(spf.gdp.long, ID=="0" & FC.Horizon==1))$gdp.first.recess, FC.type="SPF.Avg.1Quarter", FC.id=17))
FC.tbl <- bind_rows(FC.tbl, tibble(x=(filter(spf.gdp.long, ID=="0" & FC.Horizon==4))$Prob.Forecast, y=(filter(spf.gdp.long, ID=="0" & FC.Horizon==4))$gdp.first.recess, FC.type="SPF.Avg.4Quarter", FC.id=18))


# Assign printable FC names
FC.tbl$FC.name.print <- factor(FC.tbl$FC.type, levels=unique(FC.tbl$FC.type),
                               labels=c("EPC", "EMOS", "Logistic", "ENS",
                                                        "C1 Flares: NOAA", "C1 Flares: DAFFS", "M1 Flares: NOAA", "M1 Flares: DAFFS",
                                                        "Logit", "GBM", "MTurk", "COMPAS",
                                                        "SPF #84: Nowcast", "SPF #84: 1Q Ahead", "SPF #84: 4Q Ahead",
                                                        "SPF Avg: Nowcast", "SPF Avg: 1Q Ahead", "SPF Avg: 4Q Ahead"))


beta_func <- function(par, df){ sum( (pbeta(df$x, par[1], par[2]) - df$CEP_pav)^2 ) }

# Compute the Beta pdf and cdf fits
out.list <- lapply(1:max(FC.tbl$FC.id), function(FC.id.index){
  X <- FC.tbl %>% dplyr::filter(FC.id==FC.id.index) %>% dplyr::pull(x)
  Y <- FC.tbl %>% dplyr::filter(FC.id==FC.id.index) %>% dplyr::pull(y)

  rel.fit <- reliabilitydiag(x=X,y=Y)
  CEP.est <- rel.fit$x$cases

  # Beta PDF Fit
  out.pdf <- EnvStats::ebeta(CEP.est$x, method = "mle")
  # Beta CDF Fit
  out.cdf <- optim(c(1,1), beta_func, lower=c(.01,.01), upper=c(200,200), method="L-BFGS-B", df=CEP.est)

  p.rel <- plot(rel.fit) + ggtitle(paste0((FC.tbl%>%dplyr::filter(FC.id==FC.id.index)%>%dplyr::pull(FC.name.print))[1])) +
    annotate("text", x = 0.125, y = 0.94, label = paste("MCB = ", formatC(as.numeric(summary(rel.fit)[3]), digits=3, format="f"))) +
    annotate("text", x = 0.125, y = 0.88, label = paste("DSC = ", formatC(as.numeric(summary(rel.fit)[4]), digits=3, format="f"))) +
    annotate("text", x = 0.125, y = 0.82, label = paste("UNC = ", formatC(as.numeric(summary(rel.fit)[5]), digits=3, format="f")))

  seq.x <- seq(0.001,0.999,length.out=500)
  p <- plot(rel.fit) +
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

saveRDS(FC.tbl, file = "./replication_supplement/data/DGPs_FCtbl.rds")
saveRDS(dist.par.tbl, file = "./replication_supplement/data/DGPs_DataDriven.rds")


## Save data-set wise rel diags in
names(p.rel.list) <- c("EPC", "EMOS", "Logistic", "ENS",
                       "C1 Flares: NOAA", "C1 Flares: DAFFS", "M1 Flares: NOAA", "M1 Flares: DAFFS",
                       "Logit", "GBM", "MTurk", "COMPAS",
                       "SPF #84: Nowcast", "SPF #84: 1Q Ahead", "SPF #84: 4Q Ahead",
                       "SPF Avg: Nowcast", "SPF Avg: 1Q Ahead", "SPF Avg: 4Q Ahead")

# Precip
p.precip <- gridExtra::grid.arrange(grobs=p.rel.list[c("ENS", "EMOS", "EPC", "Logistic")], nrow=1)
ggsave("./replication_supplement/plots/Precip.pdf", p.precip, height=4, width=16, units="in")

# Solar Flares
p.flares <- gridExtra::grid.arrange(grobs=p.rel.list[c("C1 Flares: NOAA", "C1 Flares: DAFFS", "M1 Flares: NOAA", "M1 Flares: DAFFS")], nrow=1)
ggsave("./replication_supplement/plots/SolarFlares.pdf", p.flares, height=4, width=16, units="in")

# Recidivism
p.recid <- gridExtra::grid.arrange(grobs=p.rel.list[c("COMPAS", "MTurk", "Logit", "GBM")], nrow=1)
ggsave("./replication_supplement/plots/Recidivism.pdf", p.recid, height=4, width=16, units="in")

# SPF
p.SPF <- gridExtra::grid.arrange(grobs=p.rel.list[c("SPF #84: Nowcast", "SPF #84: 1Q Ahead", "SPF #84: 4Q Ahead",
                                                    "SPF Avg: Nowcast", "SPF Avg: 1Q Ahead", "SPF Avg: 4Q Ahead")], nrow=2)
ggsave("./replication_supplement/plots/SPF.pdf", p.SPF, height=8, width=12, units="in")
