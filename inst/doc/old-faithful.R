## ----------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  dpi = 200
)
options(width = 85)
set.seed(1967)

## ----setup-------------------------------------------------------------------------
library(weird)

## ----oldfaithful-------------------------------------------------------------------
oldfaithful

## ----of-density--------------------------------------------------------------------
dist_kde(oldfaithful$duration) |>
  gg_density(show_points = TRUE, jitter = TRUE) +
  labs(x = "Duration (seconds)")

## ----of-density2-------------------------------------------------------------------
oldfaithful |>
  select(duration, waiting) |>
  dist_kde() |>
  gg_density(show_points = TRUE, alpha = 0.15) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")

## ----of-test-----------------------------------------------------------------------
oldfaithful |> filter(peirce_anomalies(duration))
oldfaithful |> filter(chauvenet_anomalies(duration))
oldfaithful |> filter(grubbs_anomalies(duration))
oldfaithful |> filter(dixon_anomalies(duration))

## ----of-boxplot--------------------------------------------------------------------
oldfaithful |>
  ggplot(aes(x = duration)) +
  geom_boxplot() +
  scale_y_discrete() +
  labs(y = "", x = "Duration (seconds)")
oldfaithful |> gg_hdrboxplot(duration) + labs(x = "Duration (seconds)")
oldfaithful |>
  gg_hdrboxplot(duration, show_points = TRUE) +
  labs(x = "Duration (seconds)")

## ----of-boxplot2-------------------------------------------------------------------
oldfaithful |>
  gg_bagplot(duration, waiting) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")
oldfaithful |>
  gg_bagplot(duration, waiting, show_points = TRUE) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")

## ----of-boxplot3-------------------------------------------------------------------
oldfaithful |>
  gg_hdrboxplot(duration, waiting) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")
oldfaithful |>
  gg_hdrboxplot(duration, waiting, show_points = TRUE) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")

## ----of-scores---------------------------------------------------------------------
oldfaithful |>
  mutate(
    surprisal = surprisals(cbind(duration, waiting)),
    surprisal_prob = surprisals_prob(cbind(duration, waiting), approximation = "gpd"),
    strayscore = stray_scores(cbind(duration, waiting)),
    lofscore = lof_scores(cbind(duration, waiting), k = 150),
    gloshscore = glosh_scores(cbind(duration, waiting))
  ) |>
  filter(
    surprisal_prob < 0.002 |
      strayscore > quantile(strayscore, prob = 0.998) |
      lofscore > quantile(lofscore, prob = 0.998) |
      gloshscore > quantile(gloshscore, prob = 0.998)
  ) |>
  arrange(surprisal_prob)

## ----of-mvscale--------------------------------------------------------------------
mvscale(oldfaithful)
mvscale(oldfaithful, cov = NULL)

