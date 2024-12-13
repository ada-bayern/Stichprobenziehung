# ADA Bayern Stichprobenziehung

A web application for stratified sampling, providing tools for data exploration, categorization, and sample generation.

---

## Requirements

- **R**: Version 4.1 or newer.

---

## Set Up

### Prerequisites

For Linux users: Ensure your system has the following libraries installed:  
```{bash}
sudo apt install libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libgdal-dev
```

### Install Required Packages

Open an R console (in RStudio or your terminal) and execute the following command to install all necessary R packages:

```{R}
install.packages(c(
  'shiny', 'ggplot2', 'shinythemes', 'shinydashboard', 
  'shinyWidgets', 'tidyverse', 'sf', 'tinytex', 
  'sortable', 'DT', 'lpSolve', 'plotly', 'viridis', 'rmarkdown'
))
```

> ⚠️ This installation process may take several minutes.

To enable the generation of PDF reports, install TinyTeX by running:

```{R}
tinytex::install_tinytex()
```

---

## Run the Application

### Option 1: R Console

Open your R console in the same directory as `app.R` and run the app by typing the following into your console:

```{R}
shiny::runApp()
```

### Option 2: Command Line

Alternatively, launch the app from your terminal or CMD-line with:

```{bash}
R -e "shiny::runApp('app.R', launch.browser = TRUE)"
```

---

## For Developers

### Project Structure

The project is organized as follows:

```
├── README.md                        # This documentation
├── app.R                            # Main application file
├── report.Rmd                       # Template for generating PDF reports
└── modules                          # Application modules
    ├── tab0_manual.R                # User manual (UI and server)
    ├── tab1_start.R                 # Upload data and select columns
    ├── tab2_dashboard.R             # Data exploration and correlation analysis
    ├── tab3_filter.R                # Filter data to create a population
    ├── tab4_categories.R            # Summarize values into categories for stratification
    ├── tab5_sample.R                # Define sampling parameters and strata sizes
    ├── tab6_overview.R              # View the final sample and download results
    └── helpers                      # Supporting scripts
        ├── tab4_1_define_layer.R    # Sub-UI and server logic for tab4_categories
        ├── tab4_2_select_groups.R   # Categorization algorithm
        ├── tab5_1_strata_sizes.R    # Algorithm for optimal strata size calculation
        ├── tab6_1_strat_sample.R    # Sampling algorithm using strata sizes
        └── utils.R                  # Shared utility functions
```

---

### Contribution Notes

- Ensure that any new features or modules follow the modular structure demonstrated in the `modules` folder.
- Test your contributions locally by running the application (`shiny::runApp()`).
- Update this ReadMe and the relevant module documentation for any new features.

---

### Support

For questions or issues, please contact the project maintainer or consult the in-app manual (`tab0_manual.R`).

