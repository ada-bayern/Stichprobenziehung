
#' Fasst Werte eines Vektors in Kategorien zusammen
#'
#' @name select_groups
#' @param vector Vektor mit Werten, die kategorisiert werden sollen.
#' @param categories Liste von Listen mit den neuen Kategorien und den Werten,
#' aus denen diese erstellt werden sollen.
#' @param other_cat Kategorie für Werte, die nicht in ´categories´ definiert
#' werden. Default ist ´NA´.
#' @returns Vektor der gleichen Länge mit den neuen Kategorien.
#'
#' @examples
#' vektor <- c("Apfel", "Gurke", "Orange", "Paprika", "Snickers")
#' kategorien <- c(Gemüse = c("Gurke", "Paprika"), Obst = c("Apfel", "Orange"))
#' vektor_kategorisiert <- select_groups(vektor, kategorien, "weder noch")
#'
#' @export

select_groups <- function(vector, categories, other_cat = NA) {
  result <- rep(other_cat, length(vector))

  for (i in seq_along(vector)) {
    value <- vector[i]

    # Check if the value belongs to any of the categories
    for (category in names(categories)) {
      if (value %in% categories[[category]]) {
        result[i] <- category
        break  # Exit the inner loop once a category is found
      }
    }
  }

  return(result)
}