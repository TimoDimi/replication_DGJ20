library(reliabilitydiag)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(here)

data("precip_Niamey_2016")

precip <- precip_Niamey_2016

df <- tibble(enum = letters[1:4],
             forecast = c("ENS", "EMOS", "EPC", "Logistic")) %>%
  mutate(p = purrr::pmap(., function(enum, forecast) {
    r <- reliabilitydiag(precip[[forecast]], y = precip$obs, region.level = NA)
    p <- autoplot(r, type = "discrimination", params_ggMarginal = NA) +
      ggtitle(sprintf("(%s) %s", enum, forecast)) +
      annotate(
        "text",
        x = .125,
        y = .855,
        label = sprintf("UNC = %.3f", summary(r)$uncertainty)
      ) +
      annotate(
        "text",
        x = .125,
        y = .905,
        label = sprintf("DSC = %.3f", summary(r)$discrimination),
        col = "red"
      ) +
      annotate(
        "text",
        x = .125,
        y = .955,
        label = sprintf("MCB = %.3f", summary(r)$miscalibration)
      )

    ggMarginal(
      p,
      type = "hist",
      xparams = list(bins = 100,
                     fill = "grey"),
      yparams = list(bins = 100,
                     fill = "red")
    )
  }))

gridExtra::grid.arrange(grobs = df$p)

purrr::pmap(df, function(enum, forecast, p) {
  ggsave(
    here(sprintf("replication_supplement/plots/marginals_%s.pdf", forecast)),
    p,
    width = 6,
    height = 6,
    units = "in"
  )
})


