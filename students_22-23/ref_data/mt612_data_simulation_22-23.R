################################################################################
#                        Data Simulation Students                              #
################################################################################

# options ----------------------------------------------------------------------
set.seed(42)
# libraries --------------------------------------------------------------------
library(tidyverse)
library(here)
library(faux)

# functions --------------------------------------------------------------------
rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd))
  qnorm(U, mean, sd) 
}

# examples ---------------------------------------------------------------------
# sim_data <- 
#   rnorm_multi(
#     n = 100, 
#     mu = c(0, 20, 20),
#     sd = c(1, 5, 5),
#     r = c(0.5, 0.5, 0.25), 
#     varnames = c(
#       "var_1", 
#       "var_2", 
#       "var_3"
#     ),
#     empirical = FALSE
#   )

# Siobhan ######################################################################
siobhan_data <- 
  tibble(
    resource_condition = sample(c("No social resources", "No financial resources", "Social and financial resources"), 100, replace = TRUE),
    participant_gender = sample(c("male","female"), 100, replace = TRUE),
    perceived_precariousness = rtruncnorm(100, 5.33, 1.1, 1, 7),
    position_evaluation = rtruncnorm(100, 3, 2, 1, 7),
    anticipated_acceptance = rtruncnorm(100, 4, 1, 1, 7),
    anticipated_influence = rtruncnorm(100, 3.5, 2, 1, 7)
  ) |> 
  write_csv(here("students_22-23/ref_data/siobhan/siobhan_data.csv"))

# Richard ######################################################################
richard_data <- 
  tibble(
    highest_education = sample(c("Lower secondary school", "Intermediate secondary school", "Upper secondary school", "College or University"), 100, replace = TRUE),
    participant_gender = sample(c("male","female"), 100, replace = TRUE),
    directive_leadership = rtruncnorm(100, 5.33, 1.1, 1, 7),
    empowering_leadership = rtruncnorm(100, 3, 2, 1, 7),
    emotional_engagement = rtruncnorm(100, 4, 1, 1, 7),
    emotional_fatigue = rtruncnorm(100, 3.5, 2, 1, 7)
  ) |> 
  write_csv(here("students_22-23/ref_data/richard/richard_data.csv"))

# Ian ##########################################################################
ian_data <- 
  tibble(
    participant_group = sample(c("Untrained controls", "Natural bodybuilders", "Endurance athletes", "Sprinters"), 100, replace = TRUE),
    measure_time = sample(c("baseline","post-exercise"), 100, replace = TRUE),
    hexose_level = rtruncnorm(100, 6000, 600, 2000, 12000),
    taurine_level = rtruncnorm(100, 150, 15, 50, 200),
    tetradecenoylcarnitine_level = rtruncnorm(100, 0.1, 0.01, 0.05, 0.25),
    alanine_level = rtruncnorm(100, 500, 50, 200, 800)
  ) |> 
  write_csv(here("students_22-23/ref_data/ian/ian_data.csv"))
