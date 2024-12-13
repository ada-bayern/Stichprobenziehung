library("lpSolve")

#' Berechnung des Umfangs für der einzelnen Strata
#'
#' @description
#' `strata_sizes()` berechnet ein Data.Frame mit Informationen über die Strata
#' in Grundgesamtheit und Stichprobe. Die strata ergeben sich als Kombinationen der
#' jeweils angegbenen Kategorien der Spalten. Für jedes Stratum wird berechnet:
#' Der Umfang in der Grundgesamtheit und der sich daraus ergebende Anteil, so wie
#' die Größe des Stratums in der Stichprobe und die sich daraus ergebende
#' Auswahlwahrscheinlichkeit. Dazu werden verschiedene Berechnungsmethoden genutzt.
#'
#' * Naive steht hier für eine Berechnung aus Basis der Anteile an der Grundgesamtheit
#' der einzelnen Kategorien, unter Annahme, dass diese unabhängig verteilt sind.
#' * LP steht für ´linear programming´, hier werden die Größen der Strata mit
#' Hilfe von Linearer Programmierung so berechnet, dass für jede Kategorie
#' mindestens ein angegebener Umfang eingehalten wird, wenn genug Datenpunkte
#' vorhanden sind. Hierbei kann es sein, dass der Stichprobenumfang den gewünschten
#' Umfang überschreitet.
#' * MD steht für ´minimum distance´. Auch hier wird lineare Programmierung
#' eingesetzt. Allerdings wird die Stichprobengröße garantiert, wobei die Lösung
#' mit der geringsten Distanz zwischen dem gewünschten und tatsächlichen Umfang
#' der Kategorien gefunden wird.
#'
#' @param x Der Datensatz mit der Grundgesamtheit als Data.Frame
#' @param sample_size Integer mit der gewünschten Stichprobengröße. Default von 100.
#' @param strat_min Integer mit der Mindestgröße der Strata. Default von 1.
#' @param strat_names Vector mit Namen der Spalten, auf deren Basis
#' stratifiziert werden soll
#' @param ratio_types Vector, der bestimmt, wie für eine Spalte Angaben in Umfänge
#' für die Kategorie umgerechnet werden. Reihenfolge muss sich mit der in
#' ´strat_names´ decken. Mögliche Angaben:
#' * ´"proportional"´ für Kategorienumfänge proportional zur Grundgesamtheit,
#' * ´"sample"´ wenn der Anteil an der gesamten Stichprobe angegeben werden soll,
#' * ´"population"´ um den Umfang in Relation zur Grundgesamtheit anzugeben. Vor
#' allem kann mit ´1´ so jeder Datenpunkt aus dieser Kategorie ausgewählt werden
#' @param ... Named vector mit Angaben für die Anteile der Kategorien. Werte
#' zwischen ´0´ und ´1´. Wenn für die Spalte proportionale Anteile genutzt werden,
#' kann ´NA´ als Wert genutzt werden.
#'
#' @returns Data.Frame mit Strata-Größen für die verschiedenen Berechnungsmethoden,
#' Auswahlwahrscheinlichkeiten, und Umfang und Anteil der Strata in Grundgesamtheit.
#'
#' @examples
#' strata <- strata_sizes(akten, 400, 3, c("Anbietung", "Sachgebiet.Kat",
#' "Verfahrensdauer.Kat"),
#' c("population", "sample", "proportional"),
#' Anbietung = c(`Ja` = 1, `Nein` = NA),
#' Sachgebiet.Kat = c(`Verkehrsunfallsachen` = 0.2,
#'                   `Körper und Person` = 0.2,
#'                   `Leben und Wohnen` = 0.2,
#'                   `Wirtschaftsrecht` = 0.2,
#'                   `Sonstiges` = 0.2),
#' Verfahrensdauer.Kat = c(`kurzes Verfahren` = NA,
#'                        `mittleres Verfahren` = NA,
#'                        `langes Verfahren` = NA))
#'
#' @export
#'

strata_sizes <- function(x, sample_size = 100, strat_min = 1, strat_names, ratio_types, ...){

  # Extract the stratification variables from the data
  x <- x[, strat_names, drop = FALSE]

  # Check if the length of ratio_types matches the number of stratification variables
  if (length(ratio_types) != length(strat_names)) {
    stop("The number of ratio types must match the number of stratification variables.")
  }

  # Check if ratio_types is a vector of character strings or factors
  if (!is.vector(ratio_types) || !all(sapply(ratio_types, function(x) is.character(x) || is.factor(x)))) {
    stop("Input 'ratio_types' must be a vector of character strings or factors.")
  }

  # Create a list of category names for each stratification variable
  # If passed category names are not in data, execution stops with error
  # If category names in data are not used as argument, it is assumed that this
  #   category should be ignored, but a warning is thrown
  # If no category names are passed for a column, the categories are inferred from data
  category_names <- list(...)
  category_names <- lapply(category_names, names)

  for (name in names(category_names)){
    vals <- unique(x[[name]])
    if (!is.null(category_names[[name]])){
      data_not_in_args <- !vals %in% category_names[[name]]
      args_not_in_data <- !category_names[[name]] %in% vals
      if (any(data_not_in_args)){
        warning(paste("The following values were in the data for", name, "but
                      not passed in the arguments:",
                      paste(vals[data_not_in_args]),collapse = ","))
      }
      if (any(args_not_in_data)){
        stop(paste("The following categories were passed in arguments for", name, "but
                      don't exist in data:",
                   paste(vals[args_not_in_data]),collapse = ","))
      }
    } else {
      warning(paste("No categories passed in arguments for", name, ". Categories
                      inferred from data."))
      category_names[[name]] <- vals
    }
  }

  # remove data which does not have one of the given categories for each column
  for (name in names(category_names)){
    cats <- category_names[[name]]
    x <- x[x[[name]] %in% cats, , drop = FALSE]
  }

  # Setting proportions for each category
  # If a category name found in category_names has no defined value, it is set to NA
  proportions <- list(...)

  for (name in names(proportions)){
    names_defined_proportions <- names(proportions[[name]])
    names_undefined_proportions <- category_names[[name]][!category_names[[name]] %in% names_defined_proportions]
    for (new_cat in names_undefined_proportions){
      proportions[[name]][[new_cat]] <- NA
    }
    # reordering by order of category in category_names
    proportions[[name]] <- proportions[[name]][category_names[[name]]]
  }


  # Adjust proportions based on ratio types
  for (i in seq_along(strat_names)) {
    var_name <- strat_names[i]
    ratio_type <- ratio_types[[i]]


    if (ratio_type == "proportional") {
      # Calculate proportions as proportion to category counts in data
      category_counts <- table(x[[var_name]])
      total_count <- sum(category_counts)
      proportions[[var_name]] <-
        category_counts[category_names[[var_name]]] / total_count
    } else if (ratio_type == "population") {
      # Convert ratios relative to size in sample to ratios relative to population size
      category_counts <- table(x[[var_name]])
      total_count <- sum(category_counts)
      total_per_group <- (category_counts[category_names[[var_name]]] * proportions[[var_name]])
      proportions[[var_name]] <- total_per_group / sample_size
      prop_total <- sum(proportions[[var_name]], na.rm = TRUE)
      if (prop_total > 1) {
        warning("Ratios given are too high. Sampling this many data points relative to total category
                size would result in sample larger than sample size.")
        proportions[[var_name]] <- proportions[[var_name]] / prop_total
      }
    } else if (ratio_type == "sample") {
      # do nothing
    } else {
      stop(paste("Invalid ratio type:", ratio_type, "for variable", var_name))
    }

    # Filling imputing values and normalizing so ratios always add up to 1.
    provided_ratios <- proportions[[var_name]]
    provided_ratios <- provided_ratios[!is.na(provided_ratios)]
    sum_provided_ratios <- sum(provided_ratios)
    missing_ratios <- sum(is.na(proportions[[var_name]]))

    if(missing_ratios > 0){
      if(sum_provided_ratios < 1){
        missing_ratio <- (1 - sum_provided_ratios) / missing_ratios
        proportions[[var_name]][is.na(proportions[[var_name]])] <- missing_ratio
      } else {
        proportions[[var_name]][is.na(proportions[[var_name]])] <- 0
      }
    }

    sum_ratios <- sum(proportions[[var_name]])
    if(sum_ratios != 1){
      warning(paste("Ratios for", var_name, "don't sum up to 1 and have been standardized."))
      proportions[[var_name]] <- proportions[[var_name]] / sum_ratios
    }

  }


  # Generate combinations of category names
  strata_combos <- expand.grid(category_names)
  props <- apply(expand.grid(proportions), 1, prod)

  # Calculate the strata sizes for sample
  strata_sizes <- round(props * sample_size)

  # Count occurrences of each stratum combination in the data
  strata_names <- apply(strata_combos, 1, paste, collapse = ", ")
  x$stratum <- apply(x[, strat_names, drop = FALSE], 1, paste, collapse = ", ")
  counts_data <- table(x$stratum)
  strata_counts <- rep(0, nrow(strata_combos))
  names(strata_counts) <- strata_names
  strata_counts[strata_names] <- counts_data[strata_names]
  strata_counts[is.na(strata_counts)] <- 0
  x$stratum <- NULL

  # Calculating number of columns and strata, which make up number of constraints
  num_strata <- nrow(strata_combos)
  num_columns <- length(strat_names)
  num_categories <- sum(lengths(category_names))

  # Defining objective function
  obj_fn_min <- rep(1, num_strata)

  # Vector of decision variable types
  var_types <- rep("I", num_strata)

  # Defining constraints. Each stratum can only be filled with count of data points
  # belonging to this stratum in data
  constr_strat_max_matrix <- diag(num_strata)
  constr_strat_max_vec <- as.vector(strata_counts)
  constr_strat_max_vec[is.na(constr_strat_max_vec)] <- 0
  constr_strat_max_dir_vec <- rep("<=", num_strata)

  # Also, each stratum should have at least a certain amount of data points

  # Further constraints. Each category should have at least the number of data
  # points as calculates to fulfill ratios
  constr_strat_min_vec <- pmin(rep(strat_min, num_strata), constr_strat_max_vec)
  constr_strat_min_dir <- rep(">=", num_strata)

  # Create an empty matrix with rows for each category and columns for each stratum
  constr_cat_min_matrix <- matrix(0, nrow = num_categories, ncol = num_strata)

  # Fill the matrix with 1s for the strata that include each category
  row_idx <- 1
  for (col_name in names(category_names)) {
    col_categories <- category_names[[col_name]]
    for (cat in col_categories) {
      strata_idx <- apply(strata_combos, 1, function(x) cat %in% x[col_name])
      constr_cat_min_matrix[row_idx, strata_idx] <- 1
      row_idx <- row_idx + 1
    }
  }

  constr_cat_min_vec <- (unlist(proportions)) * sample_size
  contr_cat_min_dir <- rep(">=", num_categories)

  # counting occurences of categories in data so target per category isn't higher
  # than what is actually in data, which could result in linear model with category
  # minimums not having any feasible solutions
  data_col_cat_unique <- lapply(names(x), function(col) {
    paste(col, x[[col]], sep = ".")
  })
  data_flattened <- unlist(data_col_cat_unique)
  cat_counts <- table(data_flattened)[names(constr_cat_min_vec)]
  constr_cat_min_vec <- pmin(constr_cat_min_vec, cat_counts)

  # Combining constraints
  mat_constr <- rbind(constr_strat_max_matrix, constr_strat_max_matrix, constr_cat_min_matrix)
  constr_vec <- c(constr_strat_max_vec, constr_strat_min_vec, constr_cat_min_vec)
  constr_dir <- c(constr_strat_max_dir_vec, constr_strat_min_dir, contr_cat_min_dir)

  # Constructing and solving integer programming problem
  lp_problem <- lp(direction = "min", objective.in = obj_fn_min,
                   const.mat = mat_constr, const.dir = constr_dir,
                   const.rhs = constr_vec, all.int = TRUE)

  strata_sizes_lp <- lp_problem$solution

  # objective function for minimizing absolute distances from category counts
  obj_fn_min_dist <- c(rep(0, num_strata), rep(1, num_categories))

  cat_diag <- diag(num_categories)

  # constraints of type x_1 <= y_1
  constr_aux_lhs_part1 <- cbind(constr_cat_min_matrix, -1 * cat_diag)
  constr_aux_dir_part1 <- rep("<=", num_categories)
  constr_aux_rhs <- constr_cat_min_vec

  # constraints of type -x_1 <= y_1
  constr_aux_lhs_part2 <- cbind(constr_cat_min_matrix, cat_diag)
  constr_aux_dir_part2 <- rep(">=", num_categories)

  # constraining stratum size in sample to size in data
  constr_strat_max_lhs <- cbind(diag(num_strata), matrix(0, num_strata, num_categories))
  constr_strat_max_dir <- rep("<=", num_strata)
  constr_strat_max_rhs <- constr_strat_max_vec

  # constraining to at least max_size
  constr_strat_min_lhs <- cbind(diag(num_strata), matrix(0, num_strata, num_categories))
  constr_strat_min_dir <- rep(">=", num_strata)
  constr_strat_min_rhs <- pmin(constr_strat_max_vec, rep(strat_min, num_strata))

  # constraining sample size
  constr_sample_size_lhs <- matrix(c(rep(1, num_strata), rep(0, num_categories)), nrow=1)
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
                               constr_strat_max_rhs, constr_strat_min_vec, constr_sample_size_rhs)

  # creating minimization problem
  lp_problem_min_dist <- lp(direction = "min",
                            objective.in = obj_fn_min_dist,
                            const.mat = constr_min_dist_lhs,
                            const.dir = constr_min_dist_dir,
                            const.rhs = constr_min_dist_rhs,
                            all.int = TRUE)

  strata_sizes_min_dist <- lp_problem_min_dist$solution[1:num_strata]


  strata_sizes_naive_guess <- pmin(strata_sizes, constr_strat_max_vec)

  # Create a data frame with strata information and sizes
  strata_info <- data.frame(
    Stratum = apply(strata_combos, 1, function(x) paste(names(x), x, sep = " = ", collapse = ", ")),
    Size.Population = strata_counts,
    Size.Naive = strata_sizes_naive_guess,
    Size.LP = strata_sizes_lp,
    Size.MD = strata_sizes_min_dist
  )

  format_fraction <- function(value, denom) {
    ifelse(denom == 0, "Undefiniert",
           ifelse(value < 0.0001, "<0.01%",
                  paste0(format(round(value * 100, 2), nsmall = 2), "%")
           )
    )
  }

  # Making sure that ratios and probabilities are rendered as percentages
  strata_info$Ratio.Population <- strata_info$Size.Population / sum(strata_info$Size.Population)
  strata_info$Ratio.Population <- format_fraction(strata_info$Ratio.Population, strata_info$Size.Population)
  strata_info$Selection.Probability.LP <- strata_info$Size.LP / strata_info$Size.Population
  strata_info$Selection.Probability.LP <- format_fraction(strata_info$Selection.Probability.LP,
                                                          strata_info$Size.Population)
  strata_info$Selection.Probability.MD <- strata_info$Size.MD / strata_info$Size.Population
  strata_info$Selection.Probability.MD <- format_fraction(strata_info$Selection.Probability.MD,
                                                          strata_info$Size.Population)


  # count data as ints
  strata_info$Size.Naive = as.integer(strata_info$Size.Naive)
  strata_info$Size.LP = as.integer(strata_info$Size.LP)
  strata_info$Size.MD = as.integer(strata_info$Size.MD)

  return(strata_info)
}