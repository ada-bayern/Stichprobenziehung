source("modules/utils.R")

# TODO: standardize

#' ### Naive approach to Strata Size Problem
#' Estimates sizes given the assumption that all category combinations are
#' sufficiently represented in the data
#' Can result in lower sample size
naive_guess <- function(ratios, strata_combos, strata_counts, sample_size) {
  props <- apply(expand.grid(ratios), 1, prod)

  # Calculate the strata sizes for sample
  strata_sizes <- round(props * sample_size)

  pmin(strata_sizes, strata_counts)
}

lp_guess <- function(data, ratios, strata_combos, strata_counts, strat_min,
                     sample_size) {
  num_strata <- length(strata_counts)
  num_cats <- sum(lengths(ratios))

  # Defining objective function
  obj_fn_min <- rep(1, num_strata)

  # Maximum constraints:
  # Each stratum is upper-bounded by the number of occurences in the data
  constr_strat_max_matrix <- diag(num_strata)
  constr_strat_max_vec <- as.vector(strata_counts)
  constr_strat_max_vec[is.na(constr_strat_max_vec)] <- 0
  constr_strat_max_dir_vec <- rep("<=", num_strata)

  # Also, each stratum should have at least a certain amount of data points

  # Minimum constraints:
  # Each stratum is lower-bounded by a given parameter "strat_min"
  constr_strat_min_vec <- pmin(rep(strat_min, num_strata), constr_strat_max_vec)
  constr_strat_min_dir <- rep(">=", num_strata)

  # Category constraints:
  # * mapping between each stratum and all categories it comprises
  # Create an empty matrix with rows for each category and columns for each
  # stratum
  constr_cat_min_matrix <- matrix(0, nrow = num_cats, ncol = num_strata)

  # Fill the matrix with 1s for the strata that include each category
  row_idx <- 1
  for (col_name in names(ratios)) {
    col_categories <- names(ratios[[col_name]])
    for (cat in col_categories) {
      # TODO
      strata_idx <- apply(strata_combos, 1, function(x) cat %in% x[col_name])
      constr_cat_min_matrix[row_idx, strata_idx] <- 1
      row_idx <- row_idx + 1
    }
  }

  constr_cat_min_vec <- (unlist(ratios)) * sample_size
  contr_cat_min_dir <- rep(">=", num_cats)

  # counting occurences of categories in data so target per category isn't
  # higher than what is actually in data, which could result in linear model
  # with category minimums not having any feasible solutions
  data_col_cat_unique <- lapply(names(ratios), function(col) {
    paste(col, data[[col]], sep = ".")
  })
  data_flattened <- unlist(data_col_cat_unique)
  cat_counts <- table(data_flattened)[names(constr_cat_min_vec)]
  constr_cat_min_vec <- pmin(constr_cat_min_vec, cat_counts)

  # Combining constraints
  mat_constr <- rbind(constr_strat_max_matrix,
                      constr_strat_max_matrix,
                      constr_cat_min_matrix)
  constr_vec <- c(constr_strat_max_vec,
                  constr_strat_min_vec,
                  constr_cat_min_vec)
  constr_dir <- c(constr_strat_max_dir_vec,
                  constr_strat_min_dir,
                  contr_cat_min_dir)

  # Constructing and solving integer programming problem
  lp_problem <- lp(
    direction = "min",
    objective.in = obj_fn_min,
    const.mat = mat_constr, const.dir = constr_dir,
    const.rhs = constr_vec, all.int = TRUE
  )

  lp_problem$solution
}

strata_sizes <- function(strat_layers, strat_min, sample_size) {
  names <- features(strat_layers, "name")
  data <- data.frame(features(strat_layers, "col"))
  colnames(data) <- names
  ratios <- features(strat_layers, "ratios")
  names(ratios) <- names

  # Generate combinations of category names
  strata_combos <- expand.grid(names)

  # count different strata
  # -> add a column to the dataframe representing the stratum
  # -> remove column
  data$stratum <- apply(data, 1, function(row) paste(row, collapse = "_"))
  strata_counts <- table(data$stratum)
  strata_counts[is.na(strata_counts)] <- 0
  data$stratum <- NULL

  strata_sizes_lp <- lp_guess(
    data,
    ratios,
    strata_combos,
    strata_counts,
    strat_min,
    sample_size
  )

  out <- data.frame(
    Stratum = apply(strata_combos, 1,
                    function(x) {
                      paste(names(x), x, sep = " = ", collapse = ", ")
                    }),
    Size.Population = strata_counts,
    Size.Naive = NULL, # strata_sizes_naive_guess,
    Size.LP = as.integer(strata_sizes_lp),
    Size.MD = NULL # strata_sizes_min_dist
  )
  out
}