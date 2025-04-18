## ======================================================================================================================= ##
## Script:    FUNCTION: SIMULATE DATASET FOR PARAMETER RECOVERY
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Mon Jul  1 17:37:49 2024
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

dd_task_simulation <- function(discounting_rates, inverse_temperatures, n_participants) {

  ## Initialise Parameters and Objects ======================================================================================
  k <- discounting_rates # initialise vector of discounting rates (k)
  beta <- inverse_temperatures # initialise vector of inverse temperatures (beta)
  n_par <- n_participants # indicate number of participants

  sim_params <- data.frame(1:n_participants, k, beta) %>%
    setNames(c("subjID", "k", "beta")) %>%
    dplyr::slice(1:(2 * n_participants))

  # initialise empty tibble for storing results
  simu_results <- tibble(
    subjID        = numeric(),
    trial         = numeric(),
    choice        = numeric(),
    su            = numeric(),
    amount_sooner = numeric(),
    delay_sooner  = numeric(),
    amount_later  = numeric(),
    delay_later   = numeric(),
    minBot        = numeric(),
    maxBot        = numeric(),
    minTop        = numeric(),
    maxTop        = numeric(),
    k             = numeric(),
    beta          = numeric()
  )

  ## Global Parameters ------------------------------------------------------------------------------------------------------
  increments_env <- 0.5 # increment size in adjustment algorithm
  standard_amount_env <- 10 # standard (later) reward
  delays_env <- c(2, 30, 180, 365) # potential delays of later reward
  maxTop_env <- standard_amount_env # upper bound of top reward bound
  minTop_env <- standard_amount_env # lower bound of top reward bound
  maxBot_env <- 0 # lower bound of bottom reward bound
  minBot_env <- 0 # upper bound of bottom reward bound
  converged_env <- FALSE # convergence defaults to false
  trial_max_env <- 166 # maximum number of trials for participant

  ## Participant Loop =======================================================================================================
  for (pp in 1:n_par) {
    # initialise agent tibble for storing trial data
    agent_dat <- tibble(
      subjID        = numeric(), # subject identifier
      trial         = numeric(), # trial number
      choice        = numeric(), # behavioural choice (0 = variable/sooner, 1 = standard/later)
      su            = numeric(), # subjective utility (later over sooner option)
      amount_sooner = numeric(), # reward at variable/sooner choice
      delay_sooner  = numeric(), # delay at variable/sooner choice (fixed = 00)
      amount_later  = numeric(), # reward at standard/later choice (fixed = 10)
      delay_later   = numeric(), # delay at standard/later choice
      minBot        = numeric(), # starts at 0
      maxBot        = numeric(), # starts at 0
      minTop        = numeric(), # starts at 10
      maxTop        = numeric(), # starts at 10
      k             = numeric(),
      beta          = numeric()
    )

    # initialise trial tibble to store parameters for algorithm
    trial_matrix <- tibble(
      delay     = delays_env, # delays takes one of 2, 30, 180, 365 days
      minBot    = minBot_env, # upper lower bound of amount_sooner
      maxBot    = maxBot_env, # lower lower bound of amount_sooner
      minTop    = minTop_env, # upper upper bound of amount_sooner
      maxTop    = maxTop_env, # lower upper bound of amount_sooner
      converged = converged_env # converged defaults to false
    )

    converged_full <- FALSE # convergence of all delay types defaults to false
    t_idx <- 0 # trial index, starting at 0

    ## Trial Loop ===========================================================================================================
    while (!converged_full) {
      t_idx <- t_idx + 1

      ## Select Offer -------------------------------------------------------------------------------------------------------
      f <- 1 / increments_env
      v <- base::floor(f * (trial_matrix$maxTop - trial_matrix$maxBot - 2 * increments_env) * runif(1) + .5) / f
      amount_sooner_trial <- trial_matrix$maxBot + 1 * increments_env + v

      ## Select Question ----------------------------------------------------------------------------------------------------
      if (sum(trial_matrix$converged) < 3) { # 2+ options to sample from

        trial <- trial_matrix %>%
          filter(converged == FALSE) %>% # filter non-converged trial types/delays
          filter(delay == sample(.$delay, 1)) # random selection of filtered delays
      } else { # 1 option remaining, removing sample function

        trial <- trial_matrix %>%
          filter(converged == FALSE) # select remaining non-converged trial type/delay
      }

      # get index of selected question delay
      matrix_idx <- case_when(
        trial$delay == 2 ~ 1,
        trial$delay == 30 ~ 2,
        trial$delay == 180 ~ 3,
        trial$delay == 365 ~ 4,
      )

      # add trial information to agent data
      agent_dat %<>%
        add_row(
          subjID        = pp,
          trial         = t_idx,
          amount_sooner = amount_sooner_trial[matrix_idx],
          amount_later  = 10,
          delay_sooner  = 0,
          delay_later   = trial_matrix$delay[matrix_idx],
          minBot        = trial["minBot"],
          maxBot        = trial["maxBot"],
          minTop        = trial["minTop"],
          maxTop        = trial["maxTop"],
          k             = k[pp],
          beta          = beta[pp]
        )

      # populate agent_trial tibble with current trial's data
      agent_trial <- agent_dat %>% filter(trial == t_idx)

      ## Choice Utility ---------------------------------------------------=-------------------------------------------------
      su_later <- agent_trial$amount_later / (1 + k[pp] * agent_trial$delay_later)
      su_sooner <- agent_trial$amount_sooner / (1 + k[pp] * agent_trial$delay_sooner)
      su_trial <- su_later - su_sooner

      ## Choice Probability -------------------------------------------------------------------------------------------------
      prob_later <- 1 / (1 + exp(-beta[pp] * (su_trial)))

      ## Simulate Behaviour -------------------------------------------------------------------------------------------------
      agent_trial %<>%
        mutate(
          su = su_trial,
          # sample choice based on computed choice probabilities
          choice = sample(c(1, 0), 1, prob = c(prob_later, 1 - prob_later)),
          minBot = trial_matrix$minBot[matrix_idx],
          maxBot = trial_matrix$maxBot[matrix_idx],
          minTop = trial_matrix$minTop[matrix_idx],
          maxTop = trial_matrix$maxTop[matrix_idx]
        )

      ## Update algorithm parameters ----------------------------------------------------------------------------------------
      if (agent_trial$choice == 0) { # agent chose variable/sooner option

        if (agent_trial$amount_sooner > agent_trial$minTop) { # amount > lower upper bound
          trial_matrix[matrix_idx, ]$maxTop <- maxTop_env
          trial_matrix[matrix_idx, ]$minTop <- agent_trial$amount_sooner
        } else if (agent_trial$amount_sooner > agent_trial$minBot) { # amount > upper lower bound
          trial_matrix[matrix_idx, ]$maxBot <- agent_trial$minBot
          trial_matrix[matrix_idx, ]$minBot <- agent_trial$amount_sooner
        } else if (agent_trial$amount_sooner <= agent_trial$minBot) { # amount <= upper lower bound
          trial_matrix[matrix_idx, ]$maxBot <- agent_trial$amount_sooner
        }
      } else { # agent chose standard/delayed option

        if (agent_trial$amount_sooner < agent_trial$minBot) { # amount < upper lower bound
          trial_matrix[matrix_idx, ]$maxBot <- maxBot_env
          trial_matrix[matrix_idx, ]$minBot <- agent_trial$amount_sooner
        } else if (agent_trial$amount_sooner < agent_trial$minTop) { # amount < lower upper bound
          trial_matrix[matrix_idx, ]$maxTop <- agent_trial$minTop
          trial_matrix[matrix_idx, ]$minTop <- agent_trial$amount_sooner
        } else if (agent_trial$amount_sooner >= agent_trial$minTop) { # amount >= lower upper bound
          trial_matrix[matrix_idx, ]$maxTop <- agent_trial$amount_sooner
        }
      }

      # update agent tibble with above computation results
      agent_dat %<>%
        mutate(
          choice = ifelse(trial == t_idx, agent_trial$choice, choice), # choice (1 = delayed, 0 = sooner)
          su     = ifelse(trial == t_idx, su_trial, su), # subjective utility
          minBot = ifelse(trial == t_idx, agent_trial$minBot, minBot), # upper lower bound
          maxBot = ifelse(trial == t_idx, agent_trial$maxBot, maxBot), # lower lower bound
          minTop = ifelse(trial == t_idx, agent_trial$minTop, minTop), # lower upper bound
          maxTop = ifelse(trial == t_idx, agent_trial$maxTop, maxTop) # upper upper bound
        )

      ## Converge trial when bound differences is smaller/equal to algorithm increments -------------------------------------
      if (abs(agent_trial$maxTop - agent_trial$maxBot) <= increments_env) {
        trial_matrix[matrix_idx, ]$converged <- TRUE
      }

      ## Converge all if maximum trial number is reached --------------------------------------------------------------------
      if (t_idx == trial_max_env) {
        trial_matrix[1, ]$converged <- TRUE
        trial_matrix[2, ]$converged <- TRUE
        trial_matrix[3, ]$converged <- TRUE
        trial_matrix[4, ]$converged <- TRUE
      }

      # set converged_full to true if all trial types have converged
      converged_full <- trial_matrix %>% summarise(convergence_full = all(converged))

      ## Save agent's results -----------------------------------------------------------------------------------------------
      if (converged_full %>% pull()) {
        simu_results %<>%
          add_row(agent_dat)
      }
    }
  }

  sim_results_list <- list(simu_results, sim_params)
  return(sim_results_list)
}
