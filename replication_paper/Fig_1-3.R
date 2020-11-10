library(dplyr)
library(tidyr)
library(reliabilitydiag)
library(RColorBrewer)
library(ggplot2)
source("replication_supplement/rel_diag_classic.R")



precip <- precip_Niamey_2016


### Figure 1
Fig1 <- tibble(
  enum = letters[1:6],
  forecast = c("ENS", "EPC", "Logistic") %>% rep(each = 2),
  reldiag_type = c("BaC", "CORP") %>% rep(3)
) %>%
  mutate(.,
         plot = purrr::pmap(., function(enum, forecast, reldiag_type) {
           if (reldiag_type == "BaC") {
             rel.diag.classic(rlz=precip$obs, FC=precip[[forecast]], bins=10)$p +
               ggtitle(sprintf("(%s) %s / Binning and Counting", enum, forecast)) +
               xlab("Forecast value") +
               ylab("CEP") +
               theme_bw()
           } else if (reldiag_type == "CORP") {
             set.seed(42)
             r <- reliabilitydiag(precip[[forecast]], y = precip$obs, n.boot = 100)
             autoplot(r) +
               ggtitle(sprintf("(%s) %s / CORP", enum, forecast)) +
               annotate("text", x = .125, y = .94, label = sprintf("MCB = %.3f", summary(r)$miscalibration), color="red") +
               annotate("text", x = .125, y = .88, label = sprintf("DSC = %.3f", summary(r)$discrimination)) +
               annotate("text", x = .125, y = .82, label = sprintf("UNC = %.3f", summary(r)$uncertainty))
           }})
  )

purrr::pmap(Fig1, function(enum, forecast, reldiag_type, plot) {
  ggsave(
    sprintf("replication_paper/plots/Fig1%s_%s_%s.pdf", enum, forecast, reldiag_type),
    plot,
    width=5, height=5, units="in"
  )
})

Fig1$plot




### Figure 2

Fig2 <- tibble(
  enum = letters[1:4],
  bins = c(9, 10, 11, NA),
  reldiag_type = c("BaC", "CORP") %>% rep.int(c(3, 1))
) %>%
  mutate(.,
         plot = purrr::pmap(., function(enum, bins, reldiag_type) {
           if (reldiag_type == "BaC") {
             rel.diag.classic(rlz=precip$obs, FC=precip[["EMOS"]], bins=bins)$p +
               ggtitle(sprintf("(%s) EMOS / %i Equidistant Bins", enum, bins)) +
               xlab("Forecast value") +
               ylab("CEP") +
               theme_bw()
           } else if (reldiag_type == "CORP") {
             set.seed(42)
             r <- reliabilitydiag(precip[["EMOS"]], y = precip$obs, n.boot = 100)
             autoplot(r) +
               ggtitle(sprintf("(%s) EMOS / CORP", enum)) +
               annotate("text", x = .125, y = .94, label = sprintf("MCB = %.3f", summary(r)$miscalibration), color="red") +
               annotate("text", x = .125, y = .88, label = sprintf("DSC = %.3f", summary(r)$discrimination)) +
               annotate("text", x = .125, y = .82, label = sprintf("UNC = %.3f", summary(r)$uncertainty))
           }})
  )


purrr::pmap(Fig2, function(enum, bins, reldiag_type, plot) {
  ggsave(
    sprintf("replication_paper/plots/Fig2%s_%s_%s.pdf", enum, bins, reldiag_type),
    plot,
    width=5, height=5, units="in"
  )
})

Fig2$plot



### Figure 3

Fig3 <- tibble(
  enum = letters[1:4],
  xtype = c("discrete", "continuous") %>% rep(each = 2),
  region.position = c("diagonal", "estimate") %>% rep(2)
) %>%
  mutate(.,
         plot = purrr::pmap(., function(enum, xtype, region.position) {
           set.seed(42)
           n <- 1024
           if (xtype == "discrete") {
             k <- 10
             x <- sample((1:k - 0.5) / k, n, replace=TRUE)
           } else if (xtype == "continuous") {
             x <- runif(n)
           }
           y <- rbinom(n, 1, x^0.5)
           r <- reliabilitydiag(x, y = y, region.position = region.position)
           autoplot(r) +
             ggtitle(sprintf("(%s)", enum)) +
             annotate("text", x = .125, y = .94, label = sprintf("MCB = %.3f", summary(r)$miscalibration), color="red") +
             annotate("text", x = .125, y = .88, label = sprintf("DSC = %.3f", summary(r)$discrimination)) +
             annotate("text", x = .125, y = .82, label = sprintf("UNC = %.3f", summary(r)$uncertainty))
         })

  )


purrr::pmap(Fig3, function(enum, xtype, region.position, plot) {
  ggsave(
    sprintf("replication_paper/plots/Fig3%s_%s_%s.pdf", enum, xtype, region.position),
    plot,
    width=5, height=5, units="in"
  )
})


Fig3$plot
