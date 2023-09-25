#' @title .makeWeightsTriangular
#' @description A function to calculate weights based on a triangular approach.
#' @param x A numeric vector of input values.
#' @param pointval A numeric point value within the range of x.
#' @param rng A numeric vector indicating the range of x.
#' @return A numeric vector of calculated weights.
#' @keyword internal

.makeWeightsTriangular <- function(x, pointval, rng) {
  # Calculate ranges
  toprange <- pointval - max(rng)
  botrange <- pointval - min(rng)
  max_range <- max(rng)
  min_range <- min(rng)
  # Logical indices
  idx_pointval <- x == pointval
  idx_top <- x > pointval & x <= max_range
  idx_bot <- x < pointval & x >= min_range
  # Calculate weights based on triangular approach
  weights <- numeric(length(x))
  weights[idx_pointval] <- 1
  weights[idx_top] <- (x[idx_top] - max_range) / toprange
  weights[idx_bot] <- (x[idx_bot] - min_range) / botrange
  return(weights)
}

#' @title .makeWeightsGradient
#' @description A function to calculate weights based on a gradient approach.
#' @param x A numeric vector of input values.
#' @param pointval A numeric point value within the range of x.
#' @param rng A numeric vector indicating the range of x.
#' @return A numeric vector of calculated weights.
#' @keyword internal

.makeWeightsGradient <- function(x, pointval, rng) {
  # Pre-compute common values
  max_range <- max(rng)
  min_range <- min(rng)
  range_diff <- max_range - min_range
  nmaxval <- (pointval - min_range) / range_diff
  nminval <- 1 - nmaxval
  # Calculate weights based on gradient approach
  weights <- ((nmaxval - nminval) * (x - min_range) / range_diff) + nminval
  return(weights)
}

#' @title .makeWeightsMixed
#' @description A function to calculate weights based on a mixed approach.
#' @param x A numeric vector of input values.
#' @param pointval A numeric point value within the range of x.
#' @param rng A numeric vector indicating the range of x.
#' @return A numeric vector of calculated weights.
#' @keyword internal

.makeWeightsMixed <- function(x, pointval, rng) {
  # Calculate weights based on mixed approach
  weights <- (.makeWeightsTriangular(x, pointval, rng) + .makeWeightsGradient(x, pointval, rng)) / 2
  return(weights)
}

#' @title .makeWeights
#' @description A function to calculate weights with specified variant function.
#' @param x A numeric vector of input values.
#' @param pointval A numeric point value within the range of x, default is 100.
#' @param rng A numeric vector indicating the range of x, default is 0 to 100.
#' @param variant_fun A function to calculate weights. It can be makeWeightsTriangular, makeWeightsGradient, makeWeightsMixed, or a custom function name. Default is makeWeightsTriangular.
#' @return A numeric vector of calculated and scaled weights.
#' @keyword internal

.makeWeights <- function(x, pointval = 100, rng = c(0:100), variant_fun = .makeWeightsMixed) {
  # Check if range is within the range of x
  if (any(rng < min(x)) || any(rng > max(x))) {
    warning('Some values in the specified range are outside the range of observed values. These will have a weight of 0.')
  }
  # Check if variant_fun is a function
  if (!is.function(variant_fun)) {
    stop("variant_fun must be a function. Please use default options (makeWeightsTriangular, makeWeightsGradient, makeWeightsMixed) or a custom function.")
  }
  # Call the variant function to calculate weights
  weights <- variant_fun(x, pointval, rng)
  # Scale weights
  weights <- (weights + min(rng)) / (1 + min(rng))
  return(weights)
}

#' @title .gen_samples
#' @description This function generates stratified samples from a specified dataframe.
#' @param df A data frame from which the samples are to be generated. 
#' @param weight_column A string specifying the name of the column to use for weighting.Defaults to "VAS".
#' @param weight_range A numeric vector indicating the range of weight_column, default is 0 to 100.
#' @param weight_function A function used to compute weights. Defaults to "makeWeightsTriangular".
#' @param weight_values A numeric vector specifying the weight values of interest. Defaults to c(0, 25, 50, 75, 100).
#' @param sample_size An integer specifying the size of each sample. Defaults to 1000.
#' @param number_of_samples An integer specifying the number of samples to generate. Defaults to 1000.
#' @return An array containing the indices of the sampled rows in the original data frame. 
#'  The dimensions of the array are determined by the number of weight values and the number of samples.
#'  @keyword internal

.gen_samples <- function(df, 
                         weight_column = "VAS", 
                         weight_range = c(0:100), 
                         weight_values = NULL,
                         weight_function = .makeWeightsMixed,
                         sample_size = 1000, 
                         number_of_samples = 1000) {
  # Add names
  if (is.null(weight_values)){
    weight_values <- weight_range
  }
  names(weight_values) <- weight_values
  
  # Pre-compute weights
  weights_list <- lapply(weight_values, function(w) {
    .makeWeights(df[[weight_column]], pointval = w, rng = weight_range, variant_fun = weight_function)
  })
  names(weights_list) <- weight_values
  # Generate samples
  samples_list <- lapply(1:number_of_samples, function(i) {
    sapply(weight_values, function(w) {
      sample(x = 1:nrow(df), size = sample_size, replace = TRUE, prob = weights_list[[as.character(w)]])
    }, USE.NAMES = TRUE)
  })
  # Convert list to array
  samples_array <- array(data = unlist(samples_list, recursive = FALSE), 
                         dim = c(dim(samples_list[[1]]), number_of_samples),
                         dimnames = c(dimnames(samples_list[[1]]), list(paste0("sample_", 1:number_of_samples))))
  return(samples_array[,,1:number_of_samples])
}

#' @title .extract_columns
#' @description This function extracts the specified columns from a data frame for the given samples. 
#' @param df A data frame from which to extract columns.
#' @param column_names A character vector of the names of the columns to be extracted from df.
#' @param sample_indices An array of sampled indices, each element of the array represents the index of a row in df.
#' @return A list of arrays, with each array  containing the values of the respective column for the sampled indices. 
#'   Each array has the same dimensions as sampledIndices. The names of the list elements are the names of column_names.
#' @keyword internal

.extract_columns <- function(df, column_names = c("VAS", "utility_3L", "utility_5L", "utility_xw"), sample_indices) {
  result_list <- lapply(X = column_names, FUN = function(column_name) {
    outv <- array(data = df[sample_indices, column_name], dim = dim(sample_indices), dimnames = dimnames(sample_indices))
    attr(outv, "gr") <- attr(sample_indices, "gr")
    return(outv)
  })
  names(result_list) <- column_names
  return(result_list)
}

#' @title .calculate_quantiles
#' @description This function calculates the mean, standard deviation, and specified quantiles for each column in a provided data array.
#' @param data_array A numeric array where calculations will be performed on each column. The array can be 2D or 3D.
#' @param data_margin An integer that indicates the margin on which to apply the function. Default is 2.
#' @param quantile_levels A named numeric vector of probabilities for which quantiles are required.
#' @return A data frame with columns for each calculated statistic, including mean, standard deviation, and user-specified quantiles.
#' @keyword internal

.calculate_quantiles <- function(data_array, data_margin = 2, quantile_levels = c("min" = 0, "2.5%" = 0.025, "25%" = 0.25, "median" = 0.5, "75%" = 0.75, "97.5%" = 0.975, "max" = 1)) {
  quantile_results <- as.data.frame(t(apply(X = data_array, MARGIN = data_margin, FUN = function(x) {
    c(MEAN = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), quantile(x, probs = quantile_levels, na.rm = TRUE))
  })))
  return(quantile_results)
}

#' @title .calculate_weighted_statistics
#' @description This function calculate the mean, standard deviation, and quantiles for each column in a provided data array.
#' @param data_array A data array where calculations will be performed on each column.
#' @param quantile_levels A named vector of probabilities for which quantiles are required.
#' @return A data frame with columns for each calculated statistic, including mean, standard deviation, upper and lower bounds, and user-specified quantiles.
#' @keyword internal

.calculate_weighted_statistics <- function(data_array, data_margin = 2, quantile_levels = c("min" = 0, "2.5%" = 0.025, "25%" = 0.25, "median" = 0.5, "75%" = 0.75, "97.5%" = 0.975, "max" = 1)) {
  # Check if quantile_levels is not empty and contains values in [0, 1]
  if (length(quantile_levels) == 0 || any(as.numeric(quantile_levels) < 0) || any(as.numeric(quantile_levels) > 1)) {
    stop("quantile_levels should be a non-empty list containing values in the range [0, 1]")
  }
  # Calculate bootstrapped means
  boot_means <- lapply(data_array, function(y) (apply(y, MARGIN  = 2, FUN = colMeans)))
  # Calculate weighted quantiles
  weighted_quants <- mapply(function(x, xn) {
    data_array <- .calculate_quantiles(x, quantile_levels, data_margin = 2)
    cbind(type = xn, weight_top = rownames(data_array), data_array)
  }, x = boot_means, xn = names(boot_means), SIMPLIFY = FALSE)
  # Convert weight_top to integer and calculate upper and lower bounds
  weighted_quants <- do.call(rbind, weighted_quants)
  weighted_quants$topval <- as.integer(weighted_quants$weight_top)
  weighted_quants$ub <- weighted_quants$MEAN + weighted_quants$SD
  weighted_quants$lb <- weighted_quants$MEAN - weighted_quants$SD
  return(weighted_quants)
}

#' @title .create_confidence_interval_plot
#' @description This function creates a ggplot2 plot with confidence intervals and ribbons for the given data frame.
#' @param df A data frame containing the data to be plotted. The data frame should have columns for 'MEAN', 'lb', 'ub', 'topval', and 'type'.
#' @param graph_title A string specifying the title of the graph. Default is an empty string.
#' @param x_axis_title A string specifying the title for the x-axis. Default is an empty string.
#' @param y_axis_title A string specifying the title for the y-axis. Default is an empty string.
#' @param legend_name A string specifying the name of the legend. Default is "Type".
#' @param legend_labels A named vector specifying custom labels for the legend. Default is NULL.
#' @param y_axis_limits A numeric vector specifying the limits for the y-axis. Default is c(0.15, 0.95).
#' @param y_min_value A string specifying the column name for the lower bound of the ribbon. Default is "2.5".
#' @param y_max_value A string specifying the column name for the upper bound of the ribbon. Default is "97.5".
#' @param color_palette A character vector specifying the color palette to use for the plot. Default is a set of 10 colors.
#' @return A ggplot2 object representing the plot.
#' @keyword internal

.create_confidence_interval_plot <- function(df, graph_title = "", x_axis_title = "", y_axis_title = "", legend_name = "Type", legend_labels = NULL,
                                            y_axis_limits = c(0.15, 0.95), y_min_value = "2.5%", y_max_value = "97.5%",
                                            color_palette = c("#bebada", "#fb8072","#8dd3c7","#80b1d3", "#ffff67", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")) {
  # Create initial plot
  ribbon_graph <- ggplot(data = df, mapping = aes(x = topval, y = MEAN, ymin = lb, ymax = ub, color = type, fill = type, group = type)) +
    theme_bw() +  
    geom_line(size = 1.2) +
    geom_ribbon(alpha = 0.5, linetype = 0) +
    geom_ribbon(aes(ymin = df[[y_min_value]], ymax = df[[y_max_value]]), alpha = 0.2, linetype = 0)
  # Add labels, title, and color scales
  ribbon_graph <- ribbon_graph +
    labs(x = x_axis_title, y = y_axis_title, title = graph_title, color = legend_name, fill = legend_name) +
    ylim(y_axis_limits[1], y_axis_limits[2]) +
    theme(legend.position = "bottom",  
          text = element_text(size = 12),  
          axis.title = element_text(size = 14, face = "bold"), 
          plot.title = element_text(size = 16, face = "bold"),
          panel.grid.major = element_line(color = "grey90"),  
          panel.grid.minor = element_line(color = "grey95"))
  # Modify legend labels if specified
  if (!is.null(legend_labels)) {
    # Add color scales with custom labels
    ribbon_graph <- ribbon_graph +
      scale_color_manual(values = color_palette, labels = legend_labels) + 
      scale_fill_manual(values = color_palette, labels = legend_labels)
  } else {
    # Add color scales without custom labels
    ribbon_graph <- ribbon_graph +
      scale_color_manual(values = color_palette) + 
      scale_fill_manual(values = color_palette)
  }
  return(ribbon_graph)
}

#' @title .gen_samples_proportional
#' @description The function takes in a dataframe and a column (factor_column) that dictates the groupings. It then 
#'   generates bootstrap samples ensuring that each sample is proportionally representative of the original dataset based on the given groupings.
#' @param df A dataframe containing the dataset.
#' @param factor_column A string specifying the column name in the dataframe that contains the groupings or factors.
#' @param sample_size An integer indicating the size of each bootstrap sample. Default is 1000.
#' @param number_of_samples An integer indicating the number of bootstrap samples to generate. Default is 1.
#' @return A matrix containing bootstrap samples with rows corresponding to individual samples and columns corresponding to observations in each sample.
#'    The matrix has an attribute "gr" that contains the calculated size for each group to ensure proportional representation.
#' @keyword internal

.gen_samples_proportional<- function(df, factor_column = "vasdecile", sample_size = 1000, number_of_samples = 1000) {
  # Convert column to factor
  grf <- as.factor(df[, factor_column])
  # Calculate group frequencies
  grprops <- table(grf)
  # Calculate number of samples for each group to ensure proportional representation
  integer_grpsize <- function(grprops) {
    grpsize <- sample_size * grprops / NROW(df)
    grpsize_integer <- floor(grpsize)
    # Handling rounding issues
    if(sum(grpsize_integer) < sample_size) {
      grpsize_remain <- grpsize - grpsize_integer
      grpsize_dev <- sum(grpsize - grpsize_integer)
      grpsize_dev <- sample(x = 1:length(grpsize), size = round(grpsize_dev), replace = FALSE, prob = grpsize_remain)
      grpsize_integer[grpsize_dev] <- grpsize_integer[grpsize_dev] + 1  
    }
    return(grpsize_integer)
  }
  grpsizes <- integer_grpsize(grprops)
  # Create a list where each element contains indices of rows corresponding to a particular group
  grp_list <- split(1:NROW(df), grf)
  # Generate bootstrap samples with weights
  generate_samples <- function() {
    samples <- unlist(lapply(names(grpsizes), function(grn) {
      sample(x = grp_list[[grn]], size = grpsizes[grn], replace = TRUE, prob = NULL)
    }))
    return(samples)
  }
  # Generate multiple bootstrap samples
  all_samples <- replicate(number_of_samples, generate_samples())
  all_samples <- t(all_samples)
  attr(all_samples, "gr") <- grpsizes
  return(all_samples)
}

#' @title .cut_variable
#' @description This function cuts a numeric variable into intervals based on the provided breaks.
#' @param variable A numeric vector that you want to cut into intervals.
#' @param breaks A numeric vector specifying the breakpoints for cutting the variable.
#' @return A factor vector representing the intervals into which the variable has been cut.
#' @keyword internal

.cut_variable <- function(variable, breaks) {
  if (is.null(breaks)) {
    stop("Please provide breaks for the 'cut' method.")
  }
  return(cut(variable, breaks = breaks, include.lowest = TRUE, right = FALSE))
}

#' @title .factorize_variable
#' @description This function factorizes a numeric variable in a data frame based on a specified variant function and optional breaks.
#' @param df A data frame containing the variable to be factorized.
#' @param weight_column A string specifying the name of the column in the data frame that contains the variable to be factorized.
#' @param variant_fun A function that will be applied to the variable for factorization.
#' @param breaks An optional numeric vector specifying the breakpoints for cutting the variable, if applicable.
#' @return A factor or numeric vector representing the factorized variable.
#' @keyword internal

.factorize_variable <- function(df, weight_column, variant_fun, breaks = NULL){
  # Chech variable exists
  if (!(weight_column %in% names(df))) {
    stop(paste0("The variable '", weight_column, "' is not found in the data frame."))
  }
  # Ensure the variable is numeric
  variable <- df[[weight_column]]
  if (!is.numeric(variable)) {
    stop("The variable must be numeric or factor.")
  }
  # Check if variant_fun is a function
  if (!is.function(variant_fun)) {
    stop("variant_fun must be a function.")
  }
  # Extract the variable from the data frame
  result <- variant_fun(variable, breaks)
  return(result)
}

#' @title .flatten_group_to_df
#' @description This function takes a matrix with an attribute "gr" representing group sizes and flattens it into a data frame, adding a 'gr' column to indicate the group each row belongs to.
#' @param x A matrix containing the data to be flattened. The matrix should have an attribute "gr" that contains the size for each group.
#' @return A data frame containing the flattened data with an additional 'gr' column indicating the group each row belongs to.
#' @keyword internal

.flatten_group_to_df <- function(x) {
  gr <- attr(x, "gr")
  x <- as.data.frame(t(x))
  grns <- names(gr)
  gr <- rep(grns, gr)
  return(cbind(gr = gr, x))
}

#' @title .f_stat_from_df
#' @description The function is designed to compute F-statistic(s) using one-way ANOVA for each numeric column in the provided data frame against a reference column named 'gr'. 
#' @param df A data frame containing one or more numeric columns and a reference column named 'gr'.
#' @param include_p A logical value indicating whether to include the p-values in  the result.
#' @return If include_p is FALSE, a named numeric vector of F-statistics for each column against the 'gr' column. 
#'   If include_p is TRUE, a matrix with  two rows containing F-statistics and p-values, respectively.
#' @keyword internal

.f_stat_from_df <- function(df, include_p = FALSE) {
  df <- as.data.frame(df)
  # Function to compute F-statistic for a given column
  compute_stat <- function(column_name) {
    # Handle special characters in column names by wrapping them in backticks
    safe_column_name <- if (grepl("[[:space:]()]", column_name)) {
      paste0("`", column_name, "`")
    } else {
      column_name
    }
    formula_str <- paste(safe_column_name, "~ gr")
    anova_result <- aov(as.formula(formula_str), data = df)
    summary_matrix <- as.matrix(summary(anova_result)[[1]])
    if (include_p) {
      return(summary_matrix[1, c(4, 5)])  # Return F-statistic and p-value
    } else {
      return(summary_matrix[1, 4])        # Return only F-statistic
    }
  }
  # Apply the function to each column (excluding the 'gr' column)
  columns_to_analyze <- setdiff(colnames(df), "gr")
  result <- sapply(columns_to_analyze, compute_stat)
  return(result)
}

#' @title compute_ratios
#' @description The function takes in a data frame and a set of utility variables. For each pair of utility variables, 
#'     it computes their ratio and adds a new column to the data frame. The new columns are named in the format "F_ratio_Var1_Var2".
#' @param df A data frame containing the utility variables for which ratios are to be computed.
#' @param utility_columns A character vector specifying the names of the utility variables in the data frame.
#' @return A data frame with additional columns corresponding to the computed ratios.
#' @keyword internal

.compute_ratios <- function(df, utility_columns) {
  colnames(df)<-c("VAS",utility_columns)
  # Generate combinations of utility variables taken 2 at a time
  combinations <- combn(x = utility_columns , 2)
  new_cols <- lapply(1:ncol(combinations), function(i) {
    pair <- combinations[,i]
    var1 <- as.character(pair[[1]])
    var2 <- as.character(pair[[2]])
    ratio_name <- paste0("F_ratio\n (", var1, " / \n", var2, ")")
    new_col <- df[[var1]] / df[[var2]]
    # Return as named list
    return(setNames(list(new_col), ratio_name))
  })
  # Flatten the list of new columns and bind them to the original data frame
  new_cols_df <- do.call(cbind.data.frame, new_cols)
  df <- cbind(df, new_cols_df)
  return(df)
}

#' @title Plot F-Statistics
#' @description This function creates a bar plot for F-statistics along with error bars for specified utility columns.
#' @param df A data frame containing the F-statistics.
#' @param utility_columns A character vector specifying the names of the utility columns in the data frame.
#' @param graph_title A string specifying the title of the graph. Default is an empty string.
#' @param x_axis_title A string specifying the title for the x-axis. Default is an empty string.
#' @param y_axis_title A string specifying the title for the y-axis. Default is an empty string.
#' @param y_min_value A numeric value specifying the minimum limit for the y-axis. Default is NULL.
#' @param y_max_value A numeric value specifying the maximum limit for the y-axis. Default is NULL.
#' @return A ggplot object representing the bar plot with error bars.
#' @keyword internal

.plot_F_statististics <- function(df, utility_columns, graph_title = "", x_axis_title = "", y_axis_title = "", y_min_value = NULL, y_max_value = NULL) {
  # Get data frame
  F_ratio_df <- as.data.frame(tail(df, ncol(combn(x = utility_columns , 2))))
  F_ratio_df$type <- rownames(F_ratio_df)
  # Create ggplot
  plot <- ggplot(F_ratio_df) +
    theme_bw() + 
    geom_bar(aes(x=type, y=MEAN),  stat = "identity", position = "dodge", fill = "#d9d9d9") +
    geom_errorbar(aes(x= type, ymin = `2.5%`, ymax = `97.5%`),
                  width = 0.4,
                  colour = "orange",
                  alpha = 0.9,
                  size = 1.3) + 
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
    geom_text(aes(x = type, y = MEAN, label = sprintf("%.2f", MEAN)), vjust = -0.5) +
    ggtitle(graph_title) + 
    xlab(x_axis_title) + 
    ylab(y_axis_title) 
  # Add y-axis limits if specified
  if (!is.null(y_min_value) && !is.null(y_max_value)) {
    plot <- plot + ylim(y_min_value, y_max_value)
  }
  # Display the plot
  return(plot)
}

#' @title severity_ribbon_plot
#' @description This function generates a ribbon plot for given utility columns, based on weighted statistics.
#' @param df A data frame containing the utility and weight columns.
#' @param utility_columns A character vector specifying the utility columns for which the ribbon plot will be generated.
#' @param weight_column A string specifying the column that contains the weights. Default is "VAS".
#' @param weight_range A numeric vector specifying the range of weights. Default is c(0:100).
#' @param weight_values A numeric vector specifying the weight values to be used. Default is NULL, in which case the weight_range will be used.
#' @param weight_function A function to generate weights. Default is .makeWeightsMixed.
#' @param sample_size An integer specifying the sample size for bootstrapping. Default is 1000.
#' @param number_of_samples An integer specifying the number of bootstrap samples. Default is 1000.
#' @param probability_levels A named vector specifying the probability levels for quantiles. 
#' @param graph_title A string specifying the title of the graph. Default is an empty string.
#' @param x_axis_title A string specifying the title for the x-axis. Default is an empty string.
#' @param y_axis_title A string specifying the title for the y-axis. Default is an empty string.
#' @param legend_name A string specifying the name for the legend. Default is "Type".
#' @param legend_labels A character vector specifying the labels for the legend. Default is NULL.
#' @param y_axis_limits A numeric vector specifying the limits for the y-axis. Default is c(0.15, 0.95).
#' @param y_min_value A string specifying the minimum value for the y-axis. 
#' @param y_max_value A string specifying the maximum value for the y-axis. 
#' @param color_palette A character vector specifying the color palette for the plot. Default is c("#8dd3c7", "#bebada", "#80b1d3", "#fb8072", "#ffff67", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd").
#' @return A list containing two elements: 'df' which is a data frame of weighted statistics, and 'plot' which is the ggplot object representing the ribbon plot.
#' @examples
#' result <- severity_ribbon_plot(df = cdta, utility_columns = c("EQ5D3L", "EQ5D5L", "EQXW")
#' @export
severity_ribbon_plot <- function(df, 
                                utility_columns, 
                                weight_column = "VAS", 
                                weight_range = c(0:100),
                                weight_values = NULL,
                                weight_function = .makeWeightsMixed,
                                sample_size = 1000, 
                                number_of_samples = 1000, 
                                probability_levels = c("min" = 0, "2.5%" = 0.025, "25%" = 0.25, "median" = 0.5, "75%" = 0.75, "97.5%" = 0.975, "max" = 1), 
                                graph_title = "", 
                                x_axis_title = "", 
                                y_axis_title = "", 
                                legend_name = "Type", 
                                legend_labels = NULL, 
                                y_axis_limits = c(0.15, 0.95),  
                                y_min_value = "2.5%", 
                                y_max_value = "97.5%",
                                color_palette = NULL){
  
  # Check df
  if (!is.data.frame(df)) {
    stop("Input df must be a data frame.")
  } else {
    if (nrow(df) == 0 || ncol(df) == 0) {
      stop("Input data frame should not be empty.")
    }
  }
  # Check df columns
  if (!is.character(utility_columns) || !is.character(weight_column)) {
    stop("utility_columns and must be of character type.")
  }
  if (length(unique(utility_columns)) != length(utility_columns)) {
    stop("Utility columns should not have duplicates.")
  }
  if (length(utility_columns) > 10){
    stop("Number of utility columns should not exceed 10.")
  }
  unavailable_vars <- setdiff(c(utility_columns, weight_column), names(df))
  if (length(unavailable_vars) > 0) {
    stop(paste0("The variable(s) '", paste(unavailable_vars, collapse = ", "), "' are not found in the data frame."))
  }
  # Check weight_values
  if(is.null(weight_values)){
    weight_values <- weight_range
  } else{
    if (!is.numeric(weight_values)) {
      stop("weight_values must be of numeric type.")
    }
    if (!all(weight_values %in% weight_range)) {
      stop("Some weight values are not included in the weight range.")
    }  
  }
  # Check if weight_function is a function
  if (!is.function(weight_function)) {
    stop("weight_function must be a function.")
  }
  if (is.null(color_palette)){
    color_palette <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599FF", "#80796BFF", "#fccde5", "#ffff67", "#80b1d3")
  }
  # Check if integers are actually integers
  # if (!is.integer(sample_size) || (!is.integer(number_of_samples))) {
  #   stop("sample_size and number_of_samples must be of integer type.")
  # }
  
  # Generate simulated data
  sample_indices <- .gen_samples(df, weight_column = weight_column, weight_range = weight_range, weight_values = weight_values, weight_function = weight_function, sample_size = sample_size, number_of_samples = number_of_samples) 
  boot_data <- .extract_columns(df, column_names = c(utility_columns, weight_column), sample_indices)
  # Analyze and plot
  weighted_statistics <- .calculate_weighted_statistics(boot_data, quantile_levels = probability_levels)
  ribbon_plot <- .create_confidence_interval_plot(weighted_statistics[weighted_statistics$type != weight_column, ],
                                                 graph_title, x_axis_title, y_axis_title, legend_name, legend_labels,
                                                 y_axis_limits,  y_min_value, y_max_value, color_palette)
  
  return(list(df = weighted_statistics, plot = ribbon_plot))
  
}

#' @title compute_F_statistics
#' @description This function computes F-statistics for specified utility columns in a data frame. 
#' @param df A data frame containing the utility and weight columns.
#' @param utility_columns A character vector specifying the names of utility columns.
#' @param weight_column A character string specifying the name of the weight column. Default is "VAS".
#' @param weight_range A numeric vector specifying the range of weights. Default is c(0:100).
#' @param sample_size An integer specifying the sample size for bootstrapping. Default is 1000.
#' @param number_of_samples An integer specifying the number of bootstrap samples. Default is 1000.
#' @param variant_fun A function to be applied for factorizing the weight column. Default is .cut_variable.
#' @param breaks A numeric vector specifying the breaks for the 'cut' method. Default is c(0,10,20,30,40,50,60,70,80,90,100).
#' @param graph_title A character string specifying the title of the plot. Default is an empty string.
#' @param x_axis_title A character string specifying the title for the x-axis. Default is an empty string.
#' @param y_axis_title A character string specifying the title for the y-axis. Default is an empty string.
#' @param y_min_value A numeric value specifying the minimum value for the y-axis. Default is NULL.
#' @param y_max_value A numeric value specifying the maximum value for the y-axis. Default is NULL.
#' @return A list containing two elements: 'df' which is a data frame of weighted statistics, and 'plot' which is the ggplot object representing the ribbon plot.
#' @examples
#' result <- compute_F_statistics(df = cdta, utility_columns = c("EQ5D3L", "EQ5D5L", "EQXW")
#' @export
 
compute_F_statistics <- function(df, 
                                 utility_columns, 
                                 weight_column = "VAS", 
                                 weight_range = c(0:100), 
                                 sample_size = 1000, 
                                 number_of_samples = 1000, 
                                 variant_fun = .cut_variable,
                                 breaks = c(0,10,20,30,40,50,60,70,80,90,100), 
                                 graph_title = "", 
                                 x_axis_title = "", 
                                 y_axis_title = "", 
                                 y_min_value = NULL, 
                                 y_max_value = NULL) {
  
  if (length(utility_columns) < 2 || length(utility_columns) > 10){
    stop("Number of utility columns should be between 2 amd 10.")
  }
  unavailable_vars <- setdiff(c(utility_columns, weight_column), names(df))
  if (length(unavailable_vars) > 0) {
    stop(paste0("The variable(s) '", paste(unavailable_vars, collapse = ", "), "' are not found in the data frame."))
  }
  
  # Compute factor variable
  df$gr <- .factorize_variable(df, weight_column, variant_fun, breaks = breaks)
  column_names <- c(weight_column, utility_columns)
  
  # All sample
  F_stats_all <- as.data.frame(as.list(.f_stat_from_df(df[, c("gr", column_names)])))
  F_stats_all <- .compute_ratios(F_stats_all, column_names[-1])
  rownames(F_stats_all) <- "Full sample"
  
  # By groups
  sample_indices_group <- .gen_samples_proportional(df, factor_column = "gr", sample_size = sample_size, number_of_samples = number_of_samples)
  boot_data_group <- .extract_columns(df, column_names = column_names, sample_indices = sample_indices_group)
  boot_flat_group <- lapply(boot_data_group, FUN = .flatten_group_to_df) 
  F_stats_groups <- as.data.frame(lapply(X = boot_flat_group, FUN = .f_stat_from_df))
  F_stats_groups <- .compute_ratios(F_stats_groups, column_names[-1])
  
  # F statistics table
  result <- t(rbind(F_stats_all, t(.calculate_quantiles(t(F_stats_groups), data_margin = 1))))
  
  # Plot F statistics
  plot <- .plot_F_statististics(df = result, 
                                utility_columns = utility_columns, 
                                graph_title = graph_title, 
                                x_axis_title = x_axis_title, 
                                y_axis_title = y_axis_title, 
                                y_min_value = y_min_value, 
                                y_max_value = y_max_value) 
  
  return(list(df = result, plot = plot))
}



