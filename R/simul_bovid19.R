library(tidyverse)
## the purpose of the function defined in this file
## is to simulate cases of covid-19, a disease that affects cattle
## as well as 2 different treatments for the disease
## and the outcome of these treatments in terms of survival/death

## disease severity is a function of age
## treatment is allocated as a function of severity
## treatment A is allocated to less severe cases, and is associated in the lowest reduction in mortality
## treatment B is allocated to more severe cases, and is associated in the highest reduction in mortality

## the objective here is to be able to generate a Simpson's paradox
## whereby, overall treatment A seems to result in a lower death rate
## but when stratifying by age, treatment A works better

################################################################################
### inverse logit function
################################################################################
invlogit <- function(x){ exp(x) / (1 + exp(x)) }


################################################################################
### bovid-19
################################################################################
## function arguments
# N: number of cows
## The age distribution of the diseased animals is simulated using a Beta distribution
## scaled between 0 and age_max
# age_shape_a
# age_shape_b
# age_max
## Severity of the disease that determines the choice of the treatment
# treat_coF <- c(6, 5)
# curve(invlogit(-4 + treat_coF[1] * x), 
#       from = 0, to = 1,
#       ylim = c(0, 1))
# curve(invlogit(-4 + treat_coF[2] * x), add = TRUE, col = "red")

bovid19 <- function(N = 1000, age_shape_a = 3, age_shape_b = 1, age_max = 10, 
                      treat_coF = c(6, 5)){
  
  ## Simulated dataset
  bovid <- dplyr::tibble(
    anim_id = 1:N,
    age = round(rbeta(N, age_shape_a, age_shape_b) * age_max, 1),
    age_std = age / age_max,
    sever = invlogit(-3 + 5 * age_std))
  
  ## treatment depends on disease severity
  bovid$treat <- rbinom(nrow(bovid), 1, bovid$sever)
  
  ## Probability of death depending on treatment
  bovid$death_prob_if_A <- invlogit(-4 + treat_coF[1] * bovid$sever)
  bovid$death_prob_if_B <- invlogit(-4 + treat_coF[2] * bovid$sever)
  ## Observed probability of death
  bovid$death_prob_obs <- invlogit(-4 + treat_coF[bovid$treat + 1] * bovid$sever)
  ## Outcome depending on treatment
  bovid$death_if_A <- apply(bovid, 1, function(x) rbinom(n = 1, size = 1, prob = x["death_prob_if_A"]))
  bovid$death_if_B <- apply(bovid, 1, function(x) rbinom(n = 1, size = 1, prob = x["death_prob_if_B"]))
  ## Observed outcome
  bovid$death_obs <- with(bovid, (1 - treat) * death_if_A + treat * death_if_B )
  
  ## treatment label replaced with A & B
  bovid$treat <- c("A", "B")[bovid$treat + 1]
  
  return(bovid)

  }

# test <- bovid19()

# ## Number of individuals
# N <- 10000
# ## Parameters for the age distribution of the diseased animals
# age_shape_a <- 3
# age_shape_b <- 1
# ## Maximum of age
# age_max <- 20
# ## Severity of the disease that determines the choice of the treatment
# 
# ## Inverse logit function
# 
# ## Simulated dataset
# bovid <- tibble(
#   age = round(rbeta(N, age_shape_a, age_shape_b) * age_max, 1),
#   age_std = age / age_max,
#   sever = invlogit(-3 + 5 * age_std))
# 
# ## severity depends on age, treatment depends on severity
# # bovid$treat <- apply(bovid, 1, function(x) rbinom(n = 1, size = 1, prob = x["sever"]))
# bovid$treat <- ifelse(bovid$sever > .4, 1, 0)
# bovid$treat <- rbinom(nrow(bovid), 1, bovid$sever)
# 
# ## deaths depends on severity and treatment
# treat_coF <- c(6, 4)
# treat_coF <- c(6, 5)
# curve(invlogit(-4 + treat_coF[1] * x), 
#       from = 0, to = 1,
#       ylim = c(0, 1))
# curve(invlogit(-4 + treat_coF[2] * x), add = TRUE, col = "red")
# ## Probability of death depending on treatment
# bovid$death_prob_if_a <- invlogit(-4 + treat_coF[1] * bovid$sever)
# bovid$death_prob_if_b <- invlogit(-4 + treat_coF[2] * bovid$sever)
# ## Observed probability of death
# bovid$death_prob_obs <- invlogit(-4 + treat_coF[bovid$treat + 1] * bovid$sever)
# ## Outcome depending on treatment
# bovid$death_if_a <- apply(bovid, 1, function(x) rbinom(n = 1, size = 1, prob = x["death_prob_if_a"]))
# bovid$death_if_b <- apply(bovid, 1, function(x) rbinom(n = 1, size = 1, prob = x["death_prob_if_b"]))
# ## Observed outcome
# bovid$death_obs <- with(bovid, (1 - treat) * death_if_a + treat * death_if_b )

# bovid |> 
#   group_by(treat) |> 
#   summarise(
#     prob_death = length(death_obs[death_obs == 1]) / length(death_obs)
#   )
# 
# 
# sum(bovid$death_if_b - bovid$death_if_a) / nrow(bovid)
# 
# bovid <- bovid |> 
#   mutate(age_kat = ifelse(age > 10, 1, 0))
# 
# bovid |> 
#   group_by(treat, age_kat) |> 
#   summarise(
#     prob_death = length(death_obs[death_obs == 1]) / length(death_obs)
#   )
# 
# 
