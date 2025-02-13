#' Utility Functions for Data Filtering and Visualization
#'
#' This module provides several utility functions for data filtering and
#' visualization, using libraries like dplyr for data manipulation, DT for
#' table rendering, and plotly for interactive plotting capabilities. It aims
#' to offer a set of tools for processing and visualizing datasets with various
#' data types for multiple modules.
#'
#' Libraries:
#' - Includes DT for rendering interactive data tables, dplyr for data
#'   manipulation, plotly for dynamic plotting, shinyWidgets for UI
#'   enhancements, and stringr for string operations.
#'
#' Global Constants:
#' - `MAX_VAL`: A threshold constant that limits certain operations to avoid
#'              excessive processing time. Configurable based on performance
#'              needs.
#' - `ERROR_MESSAGE`: A pre-defined error message that warns users about the
#'                    limitations of rendering complex features.
#'
#' Functions:
#' - `get_filter_index`: Applies a given condition to a vector producing a
#'                       filter index.
#'   - Takes into account numeric ranges or sets of categorical values.
#' - `summarise_col`: Generates a summary of a data column that can be either
#'                    numeric or categorical, presented in a DT datatable
#'                    format.
#' - `plot_univariate`: Creates a univariate distribution plot using plotly,
#'                      supporting both numeric (histogram) and categorical
#'                      (bar plot) types.
#' - `plot_bivariate`: Constructs a bivariate distribution plot with custom
#'                     tooltips to illustrate the relationship between two
#'                     variables, accommodating mixed data types.

# Import required libraries
library(DT)
library(shinyWidgets)
library(plotly)
library(dplyr)

# Global constants
MAX_VAL <- 100
ERROR_MESSAGE <- paste(
  "Das Merkmal enthält über", MAX_VAL, "verschiedene",
  "(teils) kategorische Werte. Die Darstellung solcher",
  "Merkmale kann einige Augenblicke dauern und",
  "unübersichtlich werden. Trotzdem fortfahren?"
)
TIMEOUT_MESSAGE <- "Die Operation wurde abgebrochen, da sie zu lange dauerte."


#' Define a conitional filter index for a vector
#'
#' @param x A vector.
#' @param filter_vals Filter criteria for the specified column.
#' @param numeric Flag indicating if filtering is numeric-based.
#' @return Logical vector indicating which rows to keep.
get_filter_index <- function(x, filter_vals, numeric = FALSE) {
  if (is.null(filter_vals)) {
    return(rep(TRUE, length(x)))
  }
  # Filter data by selected grouping values
  if (numeric) {
    x >= filter_vals$min & x <= filter_vals$max
  } else {
    x %in% filter_vals
  }
}

#' Returns a summary of a data column of arbitrary type as a DT (datatable)
#'
#' @param data Data frame containing the data column.
#' @param var Column name to summarize.
#' @return Summary DT datatable.
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
    summary_df <- data.frame(
      Message = "Error: Der Datentyp des ausgewählten Merkmals ist unbekannt."
    )
  }
  datatable(summary_df, options = list(dom = "t", paging = FALSE))
}

#' Returns a plotly object displaying a distribution of a selected data column
#' of arbitrary type
#'
#' @param data Data frame containing the data.
#' @param var Column name to plot.
#' @return Plotly object showing the distribution.
plot_univariate <- function(data, var, timeout = 2) {
  # Get the selected column
  col <- data[[var]]

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
#'
#' @param data Data frame containing the data.
#' @param var1 First column name for analysis.
#' @param var2 Second column name for analysis.
#' @param max_vals Maximum unique values allowed for plot.
#' @return Plotly object showing the bivariate distribution.
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
    size = ~count, # Point size based on the count of (x, y) pairs
    text = ~text, # Custom tooltip text
    type = "scatter",
    mode = "markers",
    marker = list(sizemode = "diameter", sizeref = 1, opacity = 0.6)
  ) %>%
    layout(
      title = paste("Bivariate Verteilung von", var1, "und", var2),
      xaxis = list(
        title = var1,
        showticklabels = FALSE
      ),
      yaxis = list(
        title = var2,
        showticklabels = FALSE
      ),
      hovermode = "closest"
    )
  plot
}

#' Execute an expression with a specified timeout limit
#' When this function works, it will be sooo nice
#'
#' @param expr The expression to evaluate.
#' @param timeout The timeout limit in seconds.
#' @return The result of the expression or a timeout message.
with_timeout <- function(expr, timeout = 5) {
  result <- tryCatch({
    setTimeLimit(elapsed = timeout)
    expr
  }, error = function(e) {
    if (grepl("reached elapsed time limit", e$message)) {
      TIMEOUT_MESSAGE
    } else {
      message("Error: ", e$message)
    }
  })
  setTimeLimit(elapsed = Inf) # Reset the time limit
  result
}