library(here)
library(reliabilitydiag)
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)

source(here("replication_supplement/rel_diag_classic.R"))

precip <- precip_Niamey_2016

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
               theme_bw() +
               theme(aspect.ratio = 1)
           } else if (reldiag_type == "CORP") {
             set.seed(42)
             r <- reliabilitydiag(precip[["EMOS"]], y = precip$obs, n.boot = 100)
             autoplot(r) +
               ggtitle(sprintf("(%s) EMOS / CORP", enum)) +
               annotate(
                 "text",
                 x = .125,
                 y = .94,
                 label = sprintf("MCB = .%03d",
                                 round(summary(r)$miscalibration * 1000)),
                 color = "red"
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .88,
                 label = sprintf("DSC = .%03d",
                                 round(summary(r)$discrimination * 1000))
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .82,
                 label = sprintf("UNC = .%03d",
                                 round(summary(r)$uncertainty * 1000))
               )
           }})
  )


purrr::pmap(Fig2, function(enum, bins, reldiag_type, plot) {
  ggsave(
    here(sprintf("replication_paper/plots/Fig2%s_%s_%s.pdf", enum, bins, reldiag_type)),
    plot,
    width=5, height=5, units="in"
  )
})

grid.arrange(grobs = Fig2$plot)
