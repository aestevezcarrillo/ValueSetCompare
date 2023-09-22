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

#' @title exist_value_set
#' @description This function checks if a value set with a specified version and Shiny name exists in the `available_value_sets` data frame.
#' @param version The version of the value set to check for (EQ5D3L or EQ5D5L).
#' @param shinyName The Shiny name of the value set to check for.
#' @return A logical value indicating whether the value set exists (`TRUE`) or not (`FALSE`).

exist_value_set <- function(version, shinyName) {
  return (nrow(available_value_sets[available_value_sets$version == version & available_value_sets$shinyName == shinyName, ]) > 0)
}

#' @title get_value_set_code
#' @description This function retrieves the ISO 3166 Alpha-2 code for a value set with a specified version and Shiny name from the `available_value_sets` data frame.
#' @param version The version of the value set to retrieve the code for (EQ5D3L or EQ5D5L).
#' @param shinyName The Shiny name of the value set to retrieve the code for.
#' @return The ISO 3166 Alpha-2 code for the specified value set.

get_value_set_code <- function(version, shinyName) {
  if (length(shinyName) > 0){
    return(available_value_sets[available_value_sets$shinyName %in% shinyName & available_value_sets$version == version, "ISO3166Alpha2"])
  } else {
    NULL
  }
}

#' @title update_select_inputs
#' @description This function updates a select input in a Shiny session based on whether a specific condition exists.
#' @param session The Shiny session object.
#' @param exists A logical value indicating whether the condition for updating exists (`TRUE`) or not (`FALSE`).
#' @param main_id The ID of the select input to update.
#' @param country The country value to set as selected if the condition exists.
#' @return None. The function updates the select input in the Shiny session.

update_select_inputs <- function(session, exists, main_id, country) {
  if (exists) {
    updateSelectInput(session, main_id, selected = country)
  } else {
    updateSelectInput(session, main_id, selected = character(0))
  }
}

#' @title Generate Plots and Statistics for Shiny App
#' @description This function add utility columns to df, generates ribbon plots and F statistics.
#' @param df Data frame containing the data to be plotted and analyzed. Default is `cdta`.
#' @param dim_names_3L Vector of column names for EQ5D-3L dimensions.
#' @param dim_names_5L Vector of column names for EQ5D-5L dimensions.
#' @param value_sets_3L Vector of value sets for EQ5D-3L.
#' @param value_sets_5L Vector of value sets for EQ5D-5L.
#' @param value_sets_XW Vector of value sets for EQXW.
#' @param value_sets_XWR Vector of value sets for EQXWR.
#' @param weight_column Column name for the weight variable. Default is "VAS".
#' @param weight_range Numeric vector specifying the range of weights. Default is `c(0:100)`.
#' @param breaks Numeric vector specifying the breaks for weight values. Default is `c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)`.
#' @param weight_function Function to generate weights. Default is `.makeWeightsMixed`.
#' @param sample_size Integer specifying the sample size for bootstrapping. Default is 1000.
#' @param number_of_samples Integer specifying the number of bootstrap samples. Default is 100.
#' @return A list containing the ribbon plot and F statistics data frame.

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
                       number_of_samples = 100){
  
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


