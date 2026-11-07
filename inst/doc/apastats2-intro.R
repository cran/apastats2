## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(apastats2)

set.seed(1)
t_res <- t.test(rnorm(20, mean = 10, sd = 2))
t_str <- apa(t_res)
t_str

## -----------------------------------------------------------------------------
data("sleep")
mt_str <- apa_mean_and_t(sleep$extra, sleep$group, which.mean = 3, paired = TRUE)
mt_str

## -----------------------------------------------------------------------------
set.seed(2)
x <- rnorm(40)
y <- 0.5 * x + rnorm(40, sd = 0.8)
cor_str <- apa(cor.test(x, y))
cor_str

## -----------------------------------------------------------------------------
tbl <- matrix(c(30, 20, 15, 35), nrow = 2, byrow = TRUE)
chi_str <- apa(chisq.test(tbl))
chi_str

## -----------------------------------------------------------------------------
glm_res <- glm(Freq ~ (Age + Sex) * Survived, family = poisson, data = data.frame(Titanic))
glm_str <- apa(glm_res, term = "SexFemale:SurvivedYes", dtype = 3)
glm_str

## -----------------------------------------------------------------------------
data(memory_noise)

# Between-subject intervals: standard vs high-variability sample
target_higher <- memory_noise[
  memory_noise$relative_noise == "target more noisy",
]
between_ci <- get_adjusted_ci(
  target_higher,
  value_var = "bias_percent",
  between = "experiment",
  wid = "participant"
)
between_ci[, c("experiment", "descr")]

# Within-subject Cousineau-Morey intervals
within_ci <- get_adjusted_ci(
  memory_noise[memory_noise$experiment == "Exp. 1A", ],
  value_var = "bias_percent",
  within = "relative_noise",
  wid = "participant"
)
within_ci[, c("relative_noise", "descr")]

# Mixed design
mixed_ci <- get_adjusted_ci(
  memory_noise,
  value_var = "bias_percent",
  within = "relative_noise",
  between = "experiment",
  wid = "participant"
)
mixed_ci[, c("experiment", "relative_noise", "descr")]

## ----fig.width=5, fig.height=3------------------------------------------------
plot_pointrange(
  memory_noise,
  aes(x = relative_noise, color = experiment, y = bias_percent),
  wid = "participant",
  within_subj = TRUE,
  withinvars = "relative_noise",
  betweenvars = "experiment",
  connecting_line = TRUE
) +
  labs(
    x = "Relative noise",
    y = "Bias toward non-target (%)",
    color = "Experiment"
  ) +
  scale_x_discrete(labels = c(
    "target less noisy" = "Target less noisy",
    "target more noisy" = "Target more noisy"
  ))

## -----------------------------------------------------------------------------
round_p(c(0.025, 0.0001, 0.568))
mean_ci_str <- apa_mean_conf(c(1.1, 1.5, 0.9, 1.8, 1.2), bootCI = FALSE)
mean_ci_str

