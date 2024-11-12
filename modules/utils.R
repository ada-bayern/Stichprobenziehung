
library(DT)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(stringr)


prepare_data <- function(input, data) {
  main_col <- input$column_selector
  group_col <- input$column_selector2
  filter_vals <- input$filter_selector

  if (is.null(main_col) || main_col == "") return(NULL)

  # filter data by selected grouping values
  if (is.numeric(data[[group_col]])) {
    # Extract bin range from filter_choices (binned labels like "0-10", etc.)

    # Split the bin labels into start and end ranges
    # (assumes label format "start-end")
    # filter_ranges <- sapply(filter_vals, function(v) {
    #   out <- strsplit(v, "-")
    #   out <- sapply(out, function(num) as.numeric(trimws((num))))
    #   out
    # })

    # # Filter the data based on numeric bins
    # filtered_data <- data %>%
    #   filter(
    #     apply(data, 1, function(row) {
    #       value <- row[[group_col]]
    #       any(sapply(filter_ranges, function(range) {
    #         value >= range[1] & value < range[2]
    #       }))
    #     })
    #   )
    filtered_data <- data

  } else {
    filtered_data  <- data %>%
      filter(data[[group_col]] %in% filter_vals)
  }

  return(list(data = filtered_data, main_col = main_col, group_col = group_col))
}

# Returns a summary of a data column of arbitrary type as a DT (datatable)
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
    summary_df <- data.frame(Message = "Error: Der Datentyp des ausgewählten Merkmals ist unbekannt.") # nolint
  }

  datatable(summary_df, options = list(dom = "t", paging = FALSE))
}

# Returns a plotly object displaying a distribution of a selected data column
# of arbitrary type
plot_univariate <- function(data, var) {
  # Get the selected column
  col <- data[[var]]

  # Check whether any column has too many factor levels
  if (is.factor(col) || is.character(col)) {
    # Check unique values count
    if (length(unique(col)) > 100) {
      return(plot_ly() %>%
               layout(title = "Error: Das ausgewählte Merkmal hat zu viele
                               Faktorstufen um geplottet zu werden.",
                      titlefont = list(color = "red")))
    }
  }

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

# Returns a plotly object displaying a bivariate distribution of two selected
# data columns of arbitrary type with custom tooltips
plot_bivariate <- function(data, var1, var2) {
  # Get the selected columns
  col1 <- data[[var1]]
  col2 <- data[[var2]]

  # Check whether any column has too many factor levels
  for (col in c(col1, col2)) {
    if (is.factor(col) || is.character(col)) {
      # Check unique values count
      if (length(unique(col)) > 100) {
        return(plot_ly() %>%
                 layout(title = "Error: Eines der ausgewählten Merkmale hat zu
                                 viele Faktorstufen um geplottet zu werden.",
                        titlefont = list(color = "red")))
      }
    }
  }

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

  # Extract unique x and y values with labels for axis ticks
  x_ticks <- plot_data %>%
    distinct(x, x_label) %>%
    arrange(x)
  y_ticks <- plot_data %>%
    distinct(y, y_label) %>%
    arrange(y)

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
        tickvals = x_ticks$x,
        ticktext = x_ticks$x_label
      ),
      yaxis = list(
        title = var2,
        tickvals = y_ticks$y,
        ticktext = y_ticks$y_label
      ),
      hovermode = "closest"
    )

  plot
}