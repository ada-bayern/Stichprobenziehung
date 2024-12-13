#' Stratified Sampling Function
#'
#' This function performs stratified sampling from a dataset based on specified
#' strata sizes and category names. It ensures each stratum is sampled
#' according to its determined size, allowing for effective and representative
#' sampling across strata.
#'
#' @param data Dataframe containing the data to be sampled, with columns
#'        representing strata categories.
#' @param strata_sizes Vector of sizes indicating how many samples to draw
#'        from each stratum.
#' @param cat_names List of category names, each representing a stratum.
#' @return A dataframe containing the sampled data, stratified according to
#'         provided sizes.

strat_sample <- function(data, strata_sizes, cat_names) {

  # Create a vector of stratum labels from the combination of category names
  strata_labels <- apply(expand.grid(cat_names), 1, paste, collapse = "_")

  # Validate that the number of strata sizes matches the number of strata
  if (length(strata_sizes) != length(strata_labels)) {
    stop("The number of strata sizes does not match the number of strata
          combinations.")
  }

  # Add a stratum identifier column to the data
  vars <- names(cat_names)
  data$stratum <- apply(data[, vars, drop = FALSE], 1, paste, collapse = "_")

  # Initialize a dataframe to store the resulting sample
  sample_data <- data.frame()

  # Perform stratified sampling for each stratum
  for (i in seq_along(strata_sizes)) {
    stratum <- strata_labels[i]
    stratum_size <- strata_sizes[i]

    # Filter data for the current stratum
    stratum_data <- data[data$stratum == stratum, ]

    # Adjust stratum size if there's not enough data, by sampling whatever is
    # available
    stratum_size <- min(stratum_size, nrow(stratum_data))

    # Sample from the current stratum
    stratum_sample <- stratum_data[sample(nrow(stratum_data), stratum_size), ]

    # Append the sampled data to the result
    sample_data <- rbind(sample_data, stratum_sample)
  }

  # Remove the stratum identifier column from the sampled data
  sample_data$stratum <- NULL

  # Return the stratified sample data
  return(sample_data)
}