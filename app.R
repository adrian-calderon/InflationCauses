library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(tidyr)
library(purrr) # For functional programming tools like reduce

# Global datasets definition
data_path_pct <- "C:/Users/Adrián Calderón/Desktop/SDSU/Junior Year/Semester 1/STAT442/Final Project/PctChangeData/"
data_path_abs <- "C:/Users/Adrián Calderón/Desktop/SDSU/Junior Year/Semester 1/STAT442/Final Project/Data/"

datasets_pct <- list(
  M1 = "M1SL.csv",
  M2 = "M2SL.csv",
  CPI = "CPIAUCSL.csv",
  GovExp = "FGEXPND.csv",
  GovDefSurp = "MTSDS133FMS.csv",
  IndPro = "INDPRO.csv",
  ScarceRes = "PPIACO.csv",
  Debt = "GFDEBTN.csv",
  Currcir = "CURRCIR.csv"
)

# Define UI for application
ui <- fluidPage(
  titlePanel("Dynamic Time Series Visualization"),
  tabsetPanel(
    tabPanel("Percent Change",
             fluidRow(
               column(12,
                      selectInput("variable_pct", "Choose a variable to display (Percent Change):",
                                  choices = names(datasets_pct),
                                  multiple = TRUE,
                                  width = '100%')
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("timeSeriesPlot_pct", height = "700px")
               )
             )
    ),
    tabPanel("Absolute Quantities",
             fluidRow(
               column(12,
                      selectInput("variable_abs", "Choose a variable to display (Absolute Quantities):",
                                  choices = names(datasets_pct), # Assuming same datasets names for simplicity
                                  multiple = TRUE,
                                  width = '100%')
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("timeSeriesPlot_abs", height = "700px")
               )
             )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Load and clean data functions
  loadData <- function(data_path, datasets) {
    lapply(names(datasets), function(dataset) {
      data <- read_csv(paste0(data_path, datasets[dataset]))
      colnames(data) <- c("DATE", dataset)
      data$DATE <- as.Date(data$DATE)
      data[[dataset]] <- as.numeric(as.character(data[[dataset]]))  # Convert to numeric
      data
    }) %>%
      reduce(full_join, by = "DATE") %>%
      arrange(DATE)
  }
  
  df_pct <- loadData(data_path_pct, datasets_pct)
  df_abs <- loadData(data_path_abs, datasets_pct)  # Assuming same filenames for simplicity
  
  output$timeSeriesPlot_pct <- renderPlotly({
  # Ensure there are variables selected
  vars_to_plot_pct <- input$variable_pct
  if (length(vars_to_plot_pct) == 0) return(NULL)

  # Prepare the data
  plot_data_pct <- df_pct %>%
    dplyr::select(DATE, all_of(vars_to_plot_pct)) %>%
    tidyr::pivot_longer(cols = -DATE, names_to = "Series", values_to = "Value")

  # Basic line plot
  p_pct <- ggplot(plot_data_pct, aes(x = DATE, y = Value, color = Series)) +
    geom_line() +
    labs(title = "Time Series of Selected Economic Indicators (Percent Change)",
         x = "Date", y = "Value", color = "Indicator") +
    theme_minimal() +
    guides(color = guide_legend(title = "Indicator"))

  # Convert to Plotly
  ggplotly(p_pct)
})

output$timeSeriesPlot_abs <- renderPlotly({
  vars_to_plot_abs <- input$variable_abs
  if (length(vars_to_plot_abs) == 0) return(NULL)

  plot_data_abs <- df_abs %>%
    dplyr::select(DATE, all_of(vars_to_plot_abs)) %>%
    tidyr::pivot_longer(cols = -DATE, names_to = "Series", values_to = "Value")

  p_abs <- ggplot(plot_data_abs, aes(x = DATE, y = Value, color = Series)) +
    geom_line() +
    labs(title = "Time Series of Selected Economic Indicators (Absolute Quantities)",
         x = "Date", y = "Value", color = "Indicator") +
    theme_minimal() +
    guides(color = guide_legend(title = "Indicator"))

  ggplotly(p_abs)
})

}

# Run the application
shinyApp(ui = ui, server = server)