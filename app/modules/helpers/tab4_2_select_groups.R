#' Categorizes categorical `vector` according to mapping `categories`
#'
#' @param vector Vector of values that should be categorized
#' @param categories List of lists representing a mapping between values and
#'        summarized categories of the `vector``
#' @param other_cat Category for values that are not defined in `categories`.
#'        Default is `NA`.
#' @returns Categorized vector.
#' @examples
#' vector <- c("apple", "cucumber", "orange", "pepper", "snickers")
#' categories <- c(vegetable = c("cucumber", "pepper"),
#'                 fruit = c("apple", "orange"))
#' select_groups(vector, categories, "neither")
#'
#' [1] c("fruit", "vegetable", "fruit", "vegetable", "neither")
#' @export
select_groups <- function(vector, categories, other_cat = NA) {
  result <- rep(other_cat, length(vector))
  for (i in seq_along(vector)) {
    value <- vector[i]
    # Check if the value belongs to any of the categories
    for (category in names(categories)) {
      if (value %in% categories[[category]]) {
        result[i] <- category
        break # Exit the inner loop once a category is found
      }
    }
  }
  return(result)
}