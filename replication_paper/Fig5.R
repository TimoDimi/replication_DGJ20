library(here)
library(reliabilitydiag)
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(ggExtra)

data("precip_Niamey_2016")

precip <- precip_Niamey_2016

df <- tibble(enum = letters[1:2],
             forecast = c("EMOS", "Logistic")) %>%
  mutate(p = purrr::pmap(., function(enum, forecast) {
    r <- reliabilitydiag(precip[[forecast]], y = precip$obs, region.level = NA)
    p <- autoplot(r, type = "discrimination", params_ggMarginal = NA) +
      ggtitle(sprintf("(%s) %s", enum, forecast)) +
      annotate(
        "text",
        x = .125,
        y = .94,
        label = sprintf("MCB = .%03d",
                        round(summary(r)$miscalibration * 1000))
      ) +
      annotate(
        "text",
        x = .125,
        y = .88,
        label = sprintf("DSC = .%03d",
                        round(summary(r)$discrimination * 1000)),
        col = "red"
      ) +
      annotate(
        "text",
        x = .125,
        y = .82,
        label = sprintf("UNC = .%03d",
                        round(summary(r)$uncertainty * 1000))
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

gridExtra::grid.arrange(grobs = df$p, nrow = 1)

purrr::pmap(df, function(enum, forecast, p) {
  ggsave(
    here(sprintf("replication_paper/plots/Fig5%s_%s.pdf", enum, forecast)),
    p,
    width = 5,
    height = 5,
    units = "in"
  )
})


