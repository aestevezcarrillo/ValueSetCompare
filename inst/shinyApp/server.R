library(shiny)
library(gridExtra)
library(shinythemes)

source("helper_functions.R")

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