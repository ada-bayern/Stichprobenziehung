library(lpSolve)

source("modules/utils.R")

#' Define constraints for lp() and return them as list
define_constraints <- function(ratios, cat_counts, strata_counts, strata_combos,
                               strat_min, sample_size) {
  num_strata <- length(strata_counts)
  num_cats <- sum(lengths(ratios))
  constr <- list()

  # Maximum constraints:
  # Each stratum is upper-bounded by the number of occurences in the data
  constr$strat_max_matrix <- diag(num_strata)
  constr$strat_max_vec <- as.vector(strata_counts)
  constr$strat_max_vec[is.na(constr$strat_max_vec)] <- 0
  constr$strat_max_dir_vec <- rep("<=", num_strata)

  # Also, each stratum should have at least a certain amount of data points

  # Minimum constraints:
  # Each stratum is lower-bounded by a given parameter "strat_min"
  constr$strat_min_vec <- pmin(rep(strat_min, num_strata), constr$strat_max_vec)
  constr$strat_min_dir <- rep(">=", num_strata)

  # Category constraints:
  # * mapping between each stratum and all categories it comprises
  # Create an empty matrix with rows for each category and columns for each
  # stratum
  constr$cat_min_matrix <- matrix(0, nrow = num_cats, ncol = num_strata)

  # Fill the matrix with 1s for the strata that include each category
  row_idx <- 1
  for (col_name in names(ratios)) {
    col_categories <- names(ratios[[col_name]])
    for (cat in col_categories) {
      strata_idx <- apply(strata_combos, 1, function(x) cat %in% x[col_name])
      constr$cat_min_matrix[row_idx, strata_idx] <- 1
      row_idx <- row_idx + 1
    }
  }

  # Constraint:
  # * minimum number of category occurences in sample
  #   * according to ratios or total number if ratio to high
  # * >= that the solution should be higher
  constr$cat_min_vec <- unlist(ratios) * sample_size
  constr$cat_min_vec <- as.vector(pmin(constr$cat_min_vec, unlist(cat_counts)))
  constr$cat_min_vec[is.na(constr$cat_min_vec)] <- constr$cat_min_vec
  constr$cat_min_dir <- rep(">=", num_cats)

  constr
}

#' ### Naive approach to Strata Size Problem
#' Estimates sizes given the assumption that all category combinations are
#' sufficiently represented in the data
#' Can result in lower sample size
naive_guess <- function(ratios, strata_combos, strata_counts, sample_size) {
  r_grid <- expand.grid(ratios)
  props <- apply(r_grid, 1, function(row) prod(unlist(row)))

  # Calculate the strata sizes for sample
  strata_sizes <- round(props * sample_size)

  pmin(strata_sizes, strata_counts)
}

lp_guess <- function(num_strata, constr) {
  # Combining constraints
  mat_constr <- rbind(constr$strat_max_matrix,
                      constr$strat_max_matrix,
                      constr$cat_min_matrix)
  constr_vec <- c(constr$strat_max_vec,
                  constr$strat_min_vec,
                  constr$cat_min_vec)
  constr_dir <- c(constr$strat_max_dir_vec,
                  constr$strat_min_dir,
                  constr$cat_min_dir)

  print(constr_vec)

  # Constructing and solving integer programming problem
  lp_problem <- lp(direction = "min",
                   objective.in = rep(1, num_strata),
                   const.mat = mat_constr,
                   const.dir = constr_dir,
                   const.rhs = constr_vec,
                   all.int = TRUE)

  lp_problem$solution
}

lp_md_guess <- function(
  num_cats,
  num_strata,
  strat_min,
  sample_size,
  constr
) {
  # objective function for minimizing absolute distances from category counts
  obj_fn_min_dist <- c(rep(0, num_strata), rep(1, num_cats))

  cat_diag <- diag(num_cats)

  # constraints of type x_1 <= y_1
  constr_aux_lhs_part1 <- cbind(constr$cat_min_matrix, -1 * cat_diag)
  constr_aux_dir_part1 <- rep("<=", num_cats)
  constr_aux_rhs <- constr$cat_min_vec

  # constraints of type -x_1 <= y_1
  constr_aux_lhs_part2 <- cbind(constr$cat_min_matrix, cat_diag)
  constr_aux_dir_part2 <- rep(">=", num_cats)

  # constraining stratum size in sample to size in data
  constr_strat_max_lhs <- cbind(diag(num_strata),
                                matrix(0, num_strata, num_cats))
  constr_strat_max_dir <- rep("<=", num_strata)
  constr_strat_max_rhs <- constr$strat_max_vec

  # constraining to at least max_size
  constr_strat_min_lhs <- cbind(diag(num_strata),
                                matrix(0, num_strata, num_cats))
  constr_strat_min_dir <- rep(">=", num_strata)
  constr_strat_min_rhs <- pmin(constr$strat_max_vec, rep(strat_min, num_strata))


  # constraining sample size
  constr_sample_size_lhs <- matrix(c(rep(1, num_strata),
                                     rep(0, num_cats)), nrow = 1)
  constr_sample_size_dir <- c("=")
  constr_sample_size_rhs <- c(sample_size)

  # creating constraints matrix and vectors
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

  # creating minimization problem
  lp_problem_min_dist <- lp(direction = "min",
                            objective.in = obj_fn_min_dist,
                            const.mat = constr_min_dist_lhs,
                            const.dir = constr_min_dist_dir,
                            const.rhs = constr_min_dist_rhs,
                            all.int = TRUE)

  lp_problem_min_dist$solution[1:num_strata]
}

#' count different strata
#' -> add a column to the dataframe representing the stratum
#' -> remove column
get_strata_counts <- function(data, strata_names) {
  data$stratum <- apply(data, 1, paste, collapse = "_")
  counts_data <- table(data$stratum)
  strata_counts <- rep(0, length(strata_names))
  names(strata_counts) <- strata_names
  strata_counts[strata_names] <- counts_data[strata_names]
  strata_counts[is.na(strata_counts)] <- 0
  data$stratum <- NULL

  strata_counts
}

strata_sizes <- function(data, ratios, cat_counts, strat_min, sample_size) {
  # Generate combinations of category names
  strata_combos <- expand.grid(lapply(ratios, names))
  strata_names <- apply(strata_combos, 1, paste, collapse = "_")
  strata_counts <- get_strata_counts(data, strata_names)

  # Get constraints for lp
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

  # apply different solving algorithms
  strata_sizes_naive <- naive_guess(
    ratios,
    strata_combos,
    strata_counts,
    sample_size
  )

  # TODO: find problem
  # strata_sizes_lp <- lp_guess(
  #   strat_min,
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

  cat(stratum_col)
  cat(strata_counts)
  cat(strata_sizes_md)
  
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
  # out$Selection.Probability.LP <- format_fraction(out$Selection.Probability.LP,
  #                                                 out$Size.Population)
  out$Selection.Probability.MD <- out$Size.MD / out$Size.Population
  out$Selection.Probability.MD <- round(out$Selection.Probability.MD, 2)
  out
}