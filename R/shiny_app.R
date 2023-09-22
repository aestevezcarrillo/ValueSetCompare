library(shiny)
library(gridExtra)
library(shinythemes)

################################################################################
# MAIN
################################################################################

shiny_plot <- function(df = cdta, 
                       dim_names_3L = c("mobility", "selfcare", "activity", "pain", "anxiety"),
                       dim_names_5L = c("mobility5l", "selfcare5l", "activity5l", "pain5l", "anxiety5l"),
                       value_sets_3L = NULL,
                       value_sets_5L = NULL,
                       value_sets_XW = NULL,
                       value_sets_XWR = NULL,
                       weight_column = "VAS",
                       weight_range = c(0:100),
                       breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                       weight_function = .makeWeightsMixed,
                       sample_size = 1000,
                       number_of_samples = 1000,
                       y_min_value = 0.025, 
                       y_max_value = 0.975){
  
  # Create utility columns
  colnames_utilities <- NULL
  if (length(value_sets_5L) > 0){
    colnames_5L <- paste0("EQ5D5L (", value_sets_5L, ")")
    colnames_utilities <- c(colnames_utilities, colnames_5L)
    df <- .add_EQ5D_utilities(df = df, value_sets = value_sets_5L, version = "5L", colnames = colnames_5L, dim.names = dim_names_5L)
  }
  if (length(value_sets_3L) > 0) {
    colnames_3L <- paste0("EQ5D3L (", value_sets_3L, ")")
    colnames_utilities <- c(colnames_utilities, colnames_3L)
    df <- .add_EQ5D_utilities(df = df, value_sets = value_sets_3L, version = "3L", colnames = colnames_3L, dim.names = dim_names_3L)
  }
  if (length(value_sets_XW) > 0){
    colnames_XW <- paste0("EQXW (", value_sets_XW, ")")
    colnames_utilities <- c(colnames_utilities, colnames_XW)
    df <- .add_EQ5D_utilities(df = df, value_sets = value_sets_XW, version = "XW", colnames = colnames_XW, dim.names = dim_names_5L)
  }
  if (length(value_sets_XWR) > 0){
    colnames_XWR <- paste0("EQXWR (", value_sets_XWR, ")")
    colnames_utilities <- c(colnames_utilities, colnames_XWR)
    df <- .add_EQ5D_utilities(df = df, value_sets = value_sets_XWR, version = "XWR", colnames = colnames_XWR, dim.names = dim_names_3L)
  }
  
  # Generate ribbon plot
  probability_levels <- c(y_min_value,y_max_value)
  names(probability_levels) <- paste0(probability_levels * 100, "%")
  probability_levels <-  c("2.5%" = 0.025, "97.5%" = 0.975)
  ribbon_plot <- severity_ribbon_plot(df, 
                                      utility_columns = colnames_utilities, 
                                      weight_column = weight_column, 
                                      weight_range = weight_range,
                                      weight_values = breaks,
                                      weight_function = weight_function,
                                      sample_size = sample_size, 
                                      number_of_samples = number_of_samples, 
                                      probability_levels = probability_levels)
  
  # Compute F statistics
  if (length(colnames_utilities) > 1){
    F_statistics_df <- compute_F_statistics(df = df, 
                                                  utility_columns = colnames_utilities,
                                                  weight_column = weight_column,
                                                  weight_range = weight_range, 
                                                  sample_size = sample_size, 
                                                  number_of_samples = number_of_samples, 
                                                  variant_fun = .cut_variable,
                                                  breaks = breaks)
  } else {
    F_statistics_df <- NULL
  }
  return(list(ribbon_plot = ribbon_plot, F_statistics = F_statistics_df))
  
}

exist_value_set <- function(version, shinyName) {
  return (nrow(available_value_sets[available_value_sets$version == version & available_value_sets$shinyName == shinyName, ]) > 0)
}

get_value_set_code <- function(version, shinyName) {
  if (length(shinyName) > 0){
    return(available_value_sets[available_value_sets$shinyName %in% shinyName & available_value_sets$version == version, "ISO3166Alpha2"])
  } else {
    NULL
  }
}

update_select_inputs <- function(session, exists, main_id, country) {
  if (exists) {
    updateSelectInput(session, main_id, selected = country)
  } else {
    updateSelectInput(session, main_id, selected = character(0))
  }
}

# Get available value sets
pkgenv <- getOption("eq.env")
available_value_sets_3L <-pkgenv$country_codes[["3L"]]
available_value_sets_3L$shinyName <- paste0(available_value_sets_3L[["Name_short"]]," (",available_value_sets_3L[["ISO3166Alpha2"]], ")")
available_value_sets_5L <- pkgenv$country_codes[["5L"]]
available_value_sets_5L$shinyName <- paste0(available_value_sets_5L[["Name_short"]]," (",available_value_sets_5L[["ISO3166Alpha2"]], ")")

available_value_sets_3L$version <- "EQ5D3L"
available_value_sets_5L$version <- "EQ5D5L"
available_value_sets <- rbind(available_value_sets_3L,available_value_sets_5L)
unique_value_sets <- unique(available_value_sets$shinyName)


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


server <- function(input, output, session) {
  observe({
    if (input$tabs == "tab1") {
      # Update inputs
      if(input$selectionType == "byCountry") {
        # Check and update for EQ5D3L and EQXW
        exists_3L <- exist_value_set("EQ5D3L", input$country)
        update_select_inputs(session, exists_3L, "eq5d3l", input$country)
        update_select_inputs(session, exists_3L, "eqxw", input$country)
        # Check and update for EQ5D5L and EQXWR
        exists_5L <- exist_value_set("EQ5D5L", input$country)
        update_select_inputs(session, exists_5L, "eq5d5l", input$country)
        update_select_inputs(session, exists_5L, "eqxwr", input$country)
      } 
      # Run analysis
      if(length(c(input$eq5d3l, input$eq5d5l, input$eqxw, input$eqxwr)) > 0){
        # Get value sets codes
        value_sets_3L <- get_value_set_code(version= "EQ5D3L", shinyName = input$eq5d3l)
        value_sets_5L <- get_value_set_code(version= "EQ5D5L", shinyName = input$eq5d5l)
        value_sets_XW <- get_value_set_code(version= "EQ5D3L", shinyName = input$eqxw)
        value_sets_XWR <- get_value_set_code(version= "EQ5D5L", shinyName = input$eqxwr)
        # Utility statistics
        utility_stats <- compute_utility_stats(value_sets_3L = value_sets_3L, 
                                               value_sets_5L = value_sets_5L, 
                                               value_sets_XW = value_sets_XW, 
                                               value_sets_XWR = value_sets_XWR, 
                                               format_results = TRUE) 
        output$statsTable <- renderTable({utility_stats}, rownames = TRUE)
        # Mean Single Transition plot
        list_transition_plots <- plot_transitions (value_sets_3L = value_sets_3L, 
                                                   value_sets_5L = value_sets_5L, 
                                                   value_sets_XW = value_sets_XW, 
                                                   value_sets_XWR = value_sets_XWR)
        calculated_width <- paste0(400 * ((length(list_transition_plots) %/% 2) + (length(list_transition_plots) %% 2)), "px")
        output$plotGrid <- renderPlot({
            grid.arrange(grobs = list_transition_plots, ncol = 2)
        })
        output$dynamicPlotGrid <- renderUI({
          plotOutput("plotGrid", height = calculated_width)
        })
        # Density plots
        density <- plot_density(value_sets_3L = value_sets_3L, 
                                value_sets_5L = value_sets_5L, 
                                value_sets_XW = value_sets_XW, 
                                value_sets_XWR = value_sets_XWR)
        output$densityPlot <- renderPlot({density}) 
      }
    }
    if (input$tabs == "tab2") {
      observeEvent(input$run_button, {
        # Update second tab selection
        range_vals <- as.numeric(unlist(strsplit(input$severityRange, ",")))
        severityRange <- range_vals[1]:range_vals[2]
        severityBreaks <- as.numeric(unlist(strsplit(input$severityBreaks, ", ")))
        if (input$weightFunction == "Triangular"){
          weight_function <- .makeWeightsTriangular
        } else if (input$weightFunction == "Gradient"){
          weight_function <- .makeWeightsGradient
        } else {
          weight_function <- .makeWeightsMixed
        }
        
        result_graphical <- shiny_plot(
          df = cdta,
          value_sets_3L = get_value_set_code(version= "EQ5D3L", input$eq5d3lg),
          value_sets_5L = get_value_set_code(version= "EQ5D5L", input$eq5d5lg),
          value_sets_XW = get_value_set_code(version= "EQ5D3L", input$eqxwg),
          value_sets_XWR = get_value_set_code(version= "EQ5D5L", input$eqxwrg),
          weight_column = input$severity,
          weight_range = severityRange,
          breaks = severityBreaks,
          weight_function = weight_function,
          sample_size = input$sampleSize,
          number_of_samples = input$numberOfSamples
        )
        #Graphical comparison
        output$ribbonPlot <- renderPlot({result_graphical$ribbon_plot$plot})
        output$Fstatistics <- renderPlot({result_graphical$F_statistics$plot})
      })
    }
  })
} 

shinyApp(ui = ui, server = server)