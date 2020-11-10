library(here)
library(reliabilitydiag)
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)

source(here("replication_supplement/rel_diag_classic.R"))

precip <- precip_Niamey_2016

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
         })

  )


purrr::pmap(Fig3, function(enum, xtype, region.position, plot) {
  ggsave(
    here(sprintf("replication_paper/plots/Fig3%s_%s_%s.pdf", enum, xtype, region.position)),
    plot,
    width=5, height=5, units="in"
  )
})

grid.arrange(grobs = Fig3$plot)
