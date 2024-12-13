#' Stratifizierte Stichprobe basierend auf vorgegebenen Parametern.
#'
#' Zieht eine stratifizierte Stichprobe auf Basis von vorgegebenen Strata-
#' Größen.
#' Diese werden am besten mit der Funktion `strat_sizes()` berechnet.
#'
#' @name strat_sample
#' @param data Die Grundgesamtheit, aus der die Stichprobe gezogen werden soll.
#' @param strata_sizes Vektor mit Größen der einzelnen Strata. Berechnet mit
#' strata_sizes().
#' @param ... Vektoren mit Namen der relevanten Kategorien.
#' @returns Die Stichprobe.
#' @examples
#' strat_sample(akten, strata$Size.LP, Anbietung = c("Ja", "Nein"),
#' Sachgebiet.Kat = c("Verkehrsunfallsachen", "Körper und Person",
#'                   "Leben und Wohnen", "Wirtschaftsrecht",
#'                   "Sonstiges"))
#'
#' @export
strat_sample <- function(data, strata_sizes, cat_names) {
  # Create a vector of strata labels
  strata_labels <- apply(expand.grid(cat_names), 1, paste, collapse = "_")

  # Check if the length of strata_sizes matches the number of strata
  if (length(strata_sizes) != length(strata_labels)) {
    stop("The number of strata sizes does not match the number of strata
          combinations.")
  }

  # Create a stratum identifier column in the data
  vars <- names(cat_names)

  data$stratum <- apply(data[, vars, drop = FALSE], 1, paste, collapse = "_")
  # Initialize an empty data frame to store the sample
  sample_data <- data.frame()

  print(unique(data$stratum))
  print(strata_labels)

  # Perform stratified sampling
  for (i in seq_along(strata_sizes)) {
    stratum <- strata_labels[i]
    stratum_size <- strata_sizes[i]
    stratum_data <- data[data$stratum == stratum, ]

    # If there's not enough data to fit given stratum size, sample as much as
    # possible for now
    stratum_size <- min(stratum_size, nrow(stratum_data))

    # Sample from the stratum
    stratum_sample <- stratum_data[sample(nrow(stratum_data), stratum_size), ]
    sample_data <- rbind(sample_data, stratum_sample)
  }

  # Remove the stratum identifier column
  sample_data$stratum <- NULL

  return(sample_data)
}