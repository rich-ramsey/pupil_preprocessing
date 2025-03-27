# 1. Function to create parameter labels
create_param_label <- function(params) {
  paste(names(params), params, sep = "=", collapse = ", ")
}

# 2. Function to apply a parameter combination and create plot
apply_param_and_plot <- function(input_data, param_set, param_name, processing_function) {
  tryCatch({
    # Apply the processing function with these parameters
    result <- do.call(processing_function, c(list(input_data), param_set))
    
    # Create plot with appropriate y column
    plot <- if (processing_function == "pupil_baselinecorrect") {
      create_pupil_plot(
        result, 
        paste0(processing_function, ": ", param_name),
        y_col = Pupil_Diameter_bc.mm
      )
    } else {
      create_pupil_plot(
        result, 
        paste0(processing_function, ": ", param_name)
      )
    }
    
    # Return result and plot
    list(data = result, plot = plot, params = param_set)
  }, error = function(e) {
    message(paste("Error with params", param_name, ":", e$message))
    NULL
  })
}

# 3. Function to save parameter test plot
save_param_test_plot <- function(plots, subj, tr, subj_dir, processing_function) {
  combined_plot <- wrap_plots(
    plots, 
    ncol = ceiling(sqrt(length(plots))),
    guides = "collect", 
    axes = "collect"
  )
  
  filename <- paste0("S", subj, "_T", tr, "_", processing_function, "_params.png")
  ggsave(file.path(subj_dir, filename), combined_plot, width = 12, height = 10)
  return(invisible(combined_plot))
}

# 4. Function to process one subject/trial
test_trial_parameters <- function(trial_data, trial_key, named_combinations, 
                                  processing_function, input_step, 
                                  plot_dir, output_dir, save_param_data) {
  # Extract subject and trial
  subj <- trial_data$subject
  tr <- trial_data$trial
  
  message(paste("Testing parameters for Subject:", subj, "Trial:", tr))
  
  # Get the data from the specified input step
  input_data <- trial_data[[input_step]]
  if (is.null(input_data)) {
    message(paste("  No data found for step:", input_step))
    return(NULL)
  }
  
  # Create directory for this subject
  subj_dir <- file.path(plot_dir, paste0("S", subj))
  dir.create(subj_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Apply each parameter combination
  param_results <- imap(named_combinations, function(params, param_name) {
    apply_param_and_plot(input_data, params, param_name, processing_function)
  }) %>%
    compact()  # Remove NULL results
  
  # Create combined plot of all parameter combinations
  if (length(param_results) > 0) {
    plots <- map(param_results, ~ .x$plot)
    save_param_test_plot(plots, subj, tr, subj_dir, processing_function)
    
    # Save individual parameter results if requested
    if (save_param_data) {
      # Create a folder for parameter data
      param_data_dir <- file.path(output_dir, "param_data", processing_function)
      dir.create(param_data_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Save each parameter result
      iwalk(param_results, function(result, param_name) {
        filename <- paste0("S", subj, "_T", tr, "_", 
                           gsub("[^a-zA-Z0-9]", "_", param_name), ".csv")
        write_csv(result$data, file.path(param_data_dir, filename))
      })
    }
  }
  
  # Enhance return structure to include subject, trial and parameter info
  enhanced_results <- map(param_results, function(result) {
    result$subject <- subj
    result$trial <- tr
    result$input_step <- input_step
    return(result)
  })
  
  # Return results for this subject/trial
  list(
    subject = subj,
    trial = tr,
    param_results = enhanced_results
  )
}

# 5. Main parameter testing function
test_parameter_combinations <- function(step_data,
                                        param_combinations,
                                        processing_function,
                                        input_step,
                                        plot_dir = here("plots", "parameter_tests"),
                                        output_dir = here("data", "parameter_tests"),
                                        save_param_data = TRUE) {
  
  # Create directories
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create a named list of parameter combinations
  named_combinations <- set_names(
    param_combinations,
    map_chr(param_combinations, create_param_label)
  )
  
  # Process each subject/trial
  results <- imap(step_data, function(trial_data, trial_key) {
    test_trial_parameters(
      trial_data, 
      trial_key, 
      named_combinations, 
      processing_function, 
      input_step, 
      plot_dir,
      output_dir,
      save_param_data
    )
  }) %>%
    compact()  # Remove NULL results
  
  # Save combined parameter results if requested
  if (save_param_data && length(results) > 0) {
    # Extract all parameter results
    all_param_results <- map(results, ~ .x$param_results) %>% 
      flatten()
    
    # Group by parameter combination
    param_names <- unique(map_chr(all_param_results, function(result) {
      create_param_label(result$params)
    }))
    
    # Create a combined data file for each parameter combination
    iwalk(param_names, function(param_name, idx) {
      # Filter results for this parameter combination
      param_data <- all_param_results %>%
        keep(~ create_param_label(.x$params) == param_name) %>%
        map_df(function(result) {
          # Add subject and trial columns to the data
          result$data %>%
            mutate(
              Subject = result$subject,
              Trial = result$trial
            )
        })
      
      # Save combined file
      safe_param_name <- gsub("[^a-zA-Z0-9]", "_", param_name)
      filename <- paste0(processing_function, "_", safe_param_name, "_all_subjects.csv")
      write_csv(param_data, file.path(output_dir, filename))
    })
    
    # Create a summary file with key metrics for each parameter combination
    summary_data <- map_df(param_names, function(param_name) {
      # Filter results for this parameter combination
      param_results <- all_param_results %>%
        keep(~ create_param_label(.x$params) == param_name)
      
      # Get parameter values
      param_values <- param_results[[1]]$params
      
      # Extract key metrics from results
      metrics <- map_df(param_results, function(result) {
        data <- result$data
        
        # Check which column to use for metrics
        y_col <- if ("Pupil_Diameter_bc.mm" %in% names(data) && 
                     processing_function == "pupil_baselinecorrect") {
          "Pupil_Diameter_bc.mm"
        } else {
          "Pupil_Diameter.mm"
        }
        
        tibble(
          Subject = result$subject,
          Trial = result$trial,
          Mean = mean(data[[y_col]], na.rm = TRUE),
          SD = sd(data[[y_col]], na.rm = TRUE),
          Min = min(data[[y_col]], na.rm = TRUE),
          Max = max(data[[y_col]], na.rm = TRUE),
          Missing = sum(is.na(data[[y_col]])) / length(data[[y_col]]) * 100
        )
      })
      
      # Add parameter information
      cbind(
        tibble(Parameter_Set = param_name),
        as_tibble(param_values),
        tibble(
          Mean_across_trials = mean(metrics$Mean, na.rm = TRUE),
          SD_across_trials = sd(metrics$Mean, na.rm = TRUE),
          Mean_missing = mean(metrics$Missing, na.rm = TRUE)
        )
      )
    })
    
    # Save summary file
    write_csv(summary_data, file.path(output_dir, 
                                      paste0(processing_function, "_parameter_summary.csv")))
  }
  
  return(results)
}