
library(DT)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(stringr)

# TODO: documentation

# global constants
MAX_VAL <- 100
ERROR_MESSAGE <- paste("Das Merkmal enthält über", MAX_VAL, "verschiedene
                        (teils) kategorische Werte. Die Darstellung solcher
                        Merkmale kann einige Augenblicke dauern und
                        unübersichtlich werden. Trotzdem fortfahren?")

#' Filter data by values in a single column
filter_data <- function(data, group_col, filter_vals, numeric = FALSE) {
  if (is.null(filter_vals)) {
    return(data)
  }

  # filter data by selected grouping values
  if (numeric) {
    filtered_data <- data[
      data[[group_col]] >= filter_vals$min &
        data[[group_col]] <= filter_vals$max,
    ]
  } else {
    filtered_data <- data[data[[group_col]] %in% filter_vals, ]
  }
  filtered_data
}

#' Returns a summary of a data column of arbitrary type as a DT (datatable)
summarise_col <- function(data, var) {
  column_data <- data[[var]]
  if (is.numeric(column_data)) {
    # Summary for numeric data
    summary_df <- as.data.frame(t(summary(column_data))) %>%
      select("Var2", "Freq")
    colnames(summary_df) <- c("Statistik", "Wert")

  } else if (is.factor(column_data) || is.character(column_data)) {
    # Frequency table for categorical data
    summary_df <- as.data.frame(table(column_data))
    colnames(summary_df) <- c("Klasse", "Anzahl")

  } else {
    # Error message
    summary_df <- data.frame(Message = "Error: Der Datentyp des ausgewählten
																				Merkmals ist unbekannt.")
  }
  datatable(summary_df, options = list(dom = "t", paging = FALSE))
}

#' Returns a plotly object displaying a distribution of a selected data column
#' of arbitrary type
plot_univariate <- function(data, var) {
  # Get the selected column
  col <- data[[var]]

  # Check data type and render appropriate plot
  if (is.numeric(col)) {
    # Histogram for numeric data
    plot <- plot_ly(data, x = ~get(var), type = "histogram") %>%
      layout(yaxis = list(title = "Häufung"))

  } else if (is.factor(col) || is.character(col)) {
    # Bar plot for categorical data
    plot <- plot_ly(x = ~col, type = "histogram") %>%
      layout(yaxis = list(title = "Anzahl"))
  }
  plot %>%
    layout(title = paste("Verteilung von", var),
           xaxis = list(title = var))
}

#' Returns a plotly object displaying a bivariate distribution of two selected
#' data columns of arbitrary type with custom tooltips
plot_bivariate <- function(data, var1, var2, max_vals = 100) {
  # Get the selected columns
  col1 <- data[[var1]]
  col2 <- data[[var2]]

  # Prepare data for plotting
  plot_data <- data %>%
    mutate(
      x_label = if (is.factor(col1) || is.character(col1))
        as.character(col1) else col1,
      y_label = if (is.factor(col2) || is.character(col2))
        as.character(col2) else col2,
      x = if (is.factor(col1) || is.character(col1))
        as.numeric(factor(col1)) else col1,
      y = if (is.factor(col2) || is.character(col2))
        as.numeric(factor(col2)) else col2
    ) %>%
    # Count occurrences of each unique (x, y) pair and retain labels
    count(x, y, x_label, y_label, name = "count")

  # Create custom tooltip text
  plot_data <- plot_data %>%
    mutate(text = paste("X:", x_label, "<br>Y:", y_label, "<br>Count:", count))

  # Create the plot with density-based point sizes and custom tooltips
  plot <- plot_ly(
    plot_data,
    x = ~x,
    y = ~y,
    size = ~count,  # Point size based on the count of (x, y) pairs
    text = ~text,   # Custom tooltip text
    type = "scatter",
    mode = "markers",
    marker = list(sizemode = "diameter", sizeref = 1, opacity = 0.6)
  ) %>%
    layout(
      title = paste("Bivariate Verteilung von", var1, "und", var2),
      xaxis = list(
        title = var1,
        showticklabels = FALSE
        # tickvals = ~x,
        # ticktext = ~x_label
      ),
      yaxis = list(
        title = var2,
        showticklabels = FALSE
        # tickvals = ~y,
        # ticktext = ~y_label
      ),
      hovermode = "closest"
    )

  plot
}

#' Small function to get the names of all layers in a list of layers
#' ### Args:
#' * **collection**: list or vector of objects
#' * **feature**: feature that all objects in the collection must have
#' ### Returns
#' list containing the feature for each object
#' @export
features <- function(collection, feature) {
  unname(lapply(collection, function(obj) obj[[feature]]))
#   r <- c()
#   for (obj in collection) {
#     r <- c(r, obj[[feature]])
#   }
#   r
}

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