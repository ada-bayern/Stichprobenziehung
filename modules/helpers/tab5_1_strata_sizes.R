#' Stratification and Sampling Module
#'
#' This module contains functions to define constraints and estimate stratum
#' sizes, using both naive and integer programming approaches to ensure
#' appropriate stratification in sampling processes.
#'
#' Functionalities:
#' - Define and structure constraints for integer programming solutions.
#' - Estimate strata sizes and proportions for given sample and population.
#' - Utilize linear programming (`lp` method) to optimize sample distribution
#'   across different strata subject to constraints.
#'
#' Important Variables/Functions:
#' - `define_constraints`: Constructs constraint matrices and vectors.
#' - `naive_guess`: Provides a rough estimate of strata sizes based on
#'   proportions.
#' - `lp_guess`, `lp_md_guess`: Optimizes strata sizes using integer programming
#'   with constraints on minimum distance.
#' - `get_strata_counts`: Calculates counts for each stratum in the data.
#' - `strata_sizes`: Main function calculating and comparing strata sizes using
#'   different methods, returning a summary dataframe with stratification data.

# Load necessary library
library(lpSolve)

# Source utility scripts
source("modules/helpers/utils.R")


#' Define constraints for lp() and return them as list
#'
#' @param ratios List of ratios for different categories.
#' @param cat_counts Vector of categorical counts.
#' @param strata_counts Vector of counts for each stratum.
#' @param strata_combos Matrix of all category combinations (strata).
#' @param strat_min Minimum number of data points per stratum.
#' @param sample_size Desired sample size.
#' @return A list with matrices and vectors defining constraints for use in
#'         lp().
define_constraints <- function(ratios, cat_counts, strata_counts, strata_combos,
                               strat_min, sample_size) {
  num_strata <- length(strata_counts)
  num_cats <- sum(lengths(ratios))
  constr <- list()

  # Maximum constraints: Upper-bound each stratum by its occurrences in data
  constr$strat_max_matrix <- diag(num_strata)
  constr$strat_max_vec <- as.vector(strata_counts)
  constr$strat_max_vec[is.na(constr$strat_max_vec)] <- 0
  constr$strat_max_dir_vec <- rep("<=", num_strata)

  # Minimum constraints: Lower-bound each stratum by provided min or max
  constr$strat_min_vec <- pmin(rep(strat_min, num_strata), constr$strat_max_vec)
  constr$strat_min_dir <- rep(">=", num_strata)

  # Category constraints: Map strata and categories
  constr$cat_min_matrix <- matrix(0, nrow = num_cats, ncol = num_strata)
  row_idx <- 1
  for (col_name in names(ratios)) {
    col_categories <- names(ratios[[col_name]])
    for (cat in col_categories) {
      strata_idx <- apply(strata_combos, 1, function(x) cat %in% x[col_name])
      constr$cat_min_matrix[row_idx, strata_idx] <- 1
      row_idx <- row_idx + 1
    }
  }

  # Constraint: Minimum number of category occurrences based on ratios
  constr$cat_min_vec <- unlist(ratios) * sample_size
  constr$cat_min_vec <- as.vector(pmin(constr$cat_min_vec, unlist(cat_counts)))
  constr$cat_min_vec[is.na(constr$cat_min_vec)] <- 0
  constr$cat_min_dir <- rep(">=", num_cats)

  constr
}

#' Naive approach to Strata Size Problem
#'
#' @param ratios List of category ratios.
#' @param strata_combos Dataframe of possible stratum combinations.
#' @param strata_counts Vector of counts per stratum.
#' @param sample_size Number of samples desired.
#' @return Vector of estimated sizes for each stratum.
naive_guess <- function(ratios, strata_combos, strata_counts, sample_size) {
  r_grid <- expand.grid(ratios)
  props <- apply(r_grid, 1, function(row) prod(unlist(row)))
  strata_sizes <- round(props * sample_size)
  pmin(strata_sizes, strata_counts)
}

#' Use integer programming to guess strata sizes
#'
#' @param num_strata Number of strata.
#' @param constr List of constraints.
#' @return Solution vector for optimal strata sizes.
lp_guess <- function(num_strata, constr) {
  mat_constr <- rbind(constr$strat_max_matrix,
                      constr$strat_max_matrix,
                      constr$cat_min_matrix)
  constr_vec <- c(constr$strat_max_vec,
                  constr$strat_min_vec,
                  constr$cat_min_vec)
  constr_dir <- c(constr$strat_max_dir_vec,
                  constr$strat_min_dir,
                  constr$cat_min_dir)

  lp_problem <- lp(direction = "min",
                   objective.in = rep(1, num_strata),
                   const.mat = mat_constr,
                   const.dir = constr_dir,
                   const.rhs = constr_vec,
                   all.int = TRUE)
  lp_problem$solution
}

#' Multidimensional Distance (MD) approach to the Strata Size Problem
#'
#' @param num_cats Number of categories.
#' @param num_strata Number of strata.
#' @param strat_min Minimum number of points per stratum.
#' @param sample_size Target number of samples.
#' @param constr List of constraints.
#' @return Vector of optimized strata sizes.
lp_md_guess <- function(num_cats, num_strata, strat_min, sample_size, constr) {
  obj_fn_min_dist <- c(rep(0, num_strata), rep(1, num_cats))
  empty_strat_cat_mat <- matrix(0, num_strata, num_cats)

  # Auxiliary constraints
  constr_aux_lhs_part1 <- cbind(constr$cat_min_matrix, -1 * diag(num_cats))
  constr_aux_dir_part1 <- rep("<=", num_cats)
  constr_aux_lhs_part2 <- cbind(constr$cat_min_matrix, diag(num_cats))
  constr_aux_dir_part2 <- rep(">=", num_cats)
  constr_aux_rhs <- constr$cat_min_vec

  # Maximum strata count constraint
  constr_strat_max_lhs <- cbind(diag(num_strata), empty_strat_cat_mat)
  constr_strat_max_dir <- rep("<=", num_strata)
  constr_strat_max_rhs <- constr$strat_max_vec

  # Minimum strata count constraint
  constr_strat_min_lhs <- cbind(diag(num_strata), empty_strat_cat_mat)
  constr_strat_min_dir <- rep(">=", num_strata)
  constr_strat_min_rhs <- pmin(constr$strat_max_vec, rep(strat_min, num_strata))

  # Sample size constraint
  constr_sample_size_lhs <- matrix(c(rep(1, num_strata), rep(0, num_cats)), nrow = 1) # nolint
  constr_sample_size_dir <- c("=")
  constr_sample_size_rhs <- c(sample_size)

  constr_min_dist_lhs <- rbind(constr_aux_lhs_part1, constr_aux_lhs_part2,
                               constr_strat_max_lhs, constr_strat_min_lhs,
                               constr_sample_size_lhs)
  constr_min_dist_dir <- c(constr_aux_dir_part1, constr_aux_dir_part2,
                           constr_strat_max_dir, constr_strat_min_dir,
                           constr_sample_size_dir)
  constr_min_dist_rhs <- c(constr_aux_rhs, constr_aux_rhs,
                           constr_strat_max_rhs, constr_strat_min_rhs,
                           constr_sample_size_rhs)
  print(constr_min_dist_lhs)
  print(constr_min_dist_dir)
  print(constr_min_dist_rhs)
  lp_problem_min_dist <- lp(direction = "min",
                            objective.in = obj_fn_min_dist,
                            const.mat = constr_min_dist_lhs,
                            const.dir = constr_min_dist_dir,
                            const.rhs = constr_min_dist_rhs,
                            all.int = TRUE)
  lp_problem_min_dist$solution[1:num_strata]
}

#' Count and retrieve counts of different strata
#'
#' @param data Dataframe containing observations with columns representing
#' strata.
#' @param strata_combos Combinations of strata to be evaluated.
#' @return Named vector with counts of each stratum.
get_strata_counts <- function(data, strata_combos) {
  strata_names <- apply(strata_combos, 1, paste, collapse = "_")
  strata_counts <- rep(0, length(strata_names))
  data$stratum <- apply(data, 1, paste, collapse = "_")
  counts_data <- table(data$stratum)
  names(strata_counts) <- strata_names
  strata_counts[strata_names] <- counts_data[strata_names]
  strata_counts[is.na(strata_counts)] <- 0
  data$stratum <- NULL
  strata_counts
}

#' Determine sizes for each stratum considering population and sample
#'
#' @param data Dataframe with individual data points.
#' @param ratios List of desired ratios for categories.
#' @param cat_counts Vector of counts per category in entire dataset.
#' @param strat_min Minimum data points required per stratum.
#' @param sample_size Desired sample size for analysis.
#' @return Dataframe with stratum sizes and sampling proportions.
strata_sizes <- function(data, ratios, cat_counts, strat_min, sample_size) {
  # Generate combinations of category names
  strata_combos <- expand.grid(lapply(cat_counts, names))
  strata_counts <- get_strata_counts(data, strata_combos)

  # Get constraints for LP algorithm
  num_strata <- length(strata_counts)
  num_cats <- sum(lengths(ratios))
  constraints <- define_constraints(
    ratios,
    cat_counts,
    strata_counts,
    strata_combos,
    strat_min,
    sample_size
  )

  # Apply different solving algorithms
  strata_sizes_naive <- naive_guess(
    ratios,
    strata_combos,
    strata_counts,
    sample_size
  )

  # TODO: Find problem in LP guess method
  # strata_sizes_lp <- lp_guess(
  #   num_strata,
  #   constraints
  # )

  strata_sizes_md <- lp_md_guess(
    num_cats,
    num_strata,
    strat_min,
    sample_size,
    constraints
  )

  stratum_col <- apply(strata_combos, 1, function(x) {
    paste(names(x), x, sep = " = ", collapse = ", ")
  })

  print(stratum_col)
  print(strata_counts)
  print(strata_sizes_naive)
  print(strata_sizes_md)

  out <- data.frame(
    Stratum = stratum_col,
    Size.Population = strata_counts,
    Size.Naive = as.integer(strata_sizes_naive),
    # Size.LP = as.integer(strata_sizes_lp), TODO: find problem
    Size.MD = as.integer(strata_sizes_md)
  )

  # Making sure that ratios and probabilities are rendered as percentages
  out$Ratio.Population <- out$Size.Population / sum(out$Size.Population)
  out$Ratio.Population <- round(out$Ratio.Population, 2)
  # out$Selection.Probability.LP <- out$Size.LP / out$Size.Population
  # out$Selection.Probability.LP <- round(out$Selection.Probability.LP, 2)
  out$Selection.Probability.MD <- out$Size.MD / out$Size.Population
  out$Selection.Probability.MD <- round(out$Selection.Probability.MD, 2)

  out
}