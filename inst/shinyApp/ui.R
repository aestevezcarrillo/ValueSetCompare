library(shiny)
library(gridExtra)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("flatly"),
  # Title
  titlePanel("EQ-5D-5L vs EQ-5D-3L Value Sets Comparison"),
  # Radio Buttons
  # Tabset panel
  tabsetPanel(id = "tabs",
    # First tab
    tabPanel("Main Analysis", value = "tab1",
      sidebarLayout(
        sidebarPanel(
          radioButtons("selectionType", "Choose Selection Type:",
                       choices = c("By country" = "byCountry", "Manual selection" = "manualSelection"),
                       selected = "byCountry"),
          # By country conditional panel
          conditionalPanel(
            condition = "input.selectionType == 'byCountry'",
            selectInput("country", "Select a country:", choices = unique_value_sets, multiple = FALSE)
          ),
          # Conditional panel to display dropdowns based on selection type
          conditionalPanel(
            condition = "input.selectionType == 'manualSelection'",
            selectInput("eq5d3l", "EQ-5D-3L:", choices = available_value_sets_3L$shinyName, multiple = TRUE),
            selectInput("eq5d5l", "EQ-5D-5L:", choices = available_value_sets_5L$shinyName, multiple = TRUE),
            selectInput("eqxw", "EQ-XW:", choices = available_value_sets_3L$shinyName, multiple = TRUE),
            selectInput("eqxwr", "EQ-XWR:", choices = available_value_sets_5L$shinyName, multiple = TRUE)
          )
        ),
        mainPanel(
          # This will be filled based on the selection
          h4("Theoretical value set characteristics"),
          tableOutput("statsTable"),
          h4("Mean single level transitions by utility of starting health state"),
          uiOutput("dynamicPlotGrid"),
          # plotOutput("plotGrid"),
          h4("Kernel density plots"),
          plotOutput("densityPlot")
        )
      )
    ),
    # Second tab
    # New tab
    tabPanel("Additional Analysis", value = "tab2",
      sidebarLayout(
        sidebarPanel(
          selectInput("eq5d3lg", "EQ-5D-3L:", choices = available_value_sets_3L$shinyName, multiple = TRUE),
          selectInput("eq5d5lg", "EQ-5D-5L:", choices = available_value_sets_5L$shinyName, multiple = TRUE),
          selectInput("eqxwg", "EQ-XW:", choices = available_value_sets_3L$shinyName, multiple = TRUE),
          selectInput("eqxwrg", "EQ-XWR:", choices = available_value_sets_5L$shinyName, multiple = TRUE),
          checkboxInput("advanced", "Show advanced options", FALSE),
          conditionalPanel(
            condition = "input.advanced",
            selectInput("severity", "Severity variable:", choices = c("VAS"), multiple = FALSE),
            textInput("severityRange", "Severity variable range:", value = "0, 100"),
            textInput("severityBreaks", "Severity variable Breaks:", value = "10, 20, 30, 40, 50, 60, 70, 80, 90, 100"),
            selectInput("weightFunction", "Weight function:", choices = c("Mixed", "Triangular", "Gradient"), multiple = FALSE),
            numericInput("sampleSize", "Sample size:", 1000, min = 1, max = 2000),
            numericInput("numberOfSamples", "Number of samples:", 100, min = 1, max = 2000)
          ),
          actionButton("run_button", "Run this"),
        ),
        mainPanel(
          h4("Graphical comparison"),
          plotOutput("ribbonPlot"),
          h4("F statistics"),
          plotOutput("Fstatistics")
        )
      )
    )
  )
)
