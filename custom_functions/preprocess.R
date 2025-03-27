# 1. Updated plotting function
create_pupil_plot <- function(data, title, units = "mm") {
  # Determine column name and label based on units
  if (units == "mm") {
    y_col_name <- "Pupil_Diameter.mm"
    y_col_bc_name <- "Pupil_Diameter_bc.mm"
    y_label <- "Pupil Size (mm)"
    y_label_bc <- "Baseline-Corrected Pupil Size (mm)"
  } else {
    y_col_name <- "Pupil_Diameter.px"
    y_col_bc_name <- "Pupil_Diameter_bc.px"
    y_label <- "Pupil Size (a.u.)"
    y_label_bc <- "Baseline-Corrected Pupil Size (a.u.)"
  }
  
  # Check if the data contains baseline-corrected column
  if (y_col_bc_name %in% names(data)) {
    # Use baseline-corrected column
    plot <- ggplot(data, aes_string(x = "Time", y = y_col_bc_name)) +
      geom_point(colour = "darkgrey") +
      geom_line() +
      labs(title = title, x = "Time (ms)", y = y_label_bc)
  } else if (y_col_name %in% names(data)) {
    # Use regular column
    plot <- ggplot(data, aes_string(x = "Time", y = y_col_name)) +
      geom_point(colour = "darkgrey") +
      geom_line() +
      labs(title = title, x = "Time (ms)", y = y_label)
  } else {
    # Fallback to first numeric column
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    if (length(numeric_cols) > 0) {
      y_col <- numeric_cols[1]
      plot <- ggplot(data, aes_string(x = "Time", y = y_col)) +
        geom_point(colour = "darkgrey") +
        geom_line() +
        labs(title = title, x = "Time (ms)", y = "Value")
      warning("Could not find expected column for units '", units, 
              "'. Using column '", y_col, "' instead.")
    } else {
      stop("No suitable numeric column found for plotting")
    }
  }
  
  return(plot)
}

# 2. Function to save a plot
save_plot <- function(plot, subj_dir, subj, tr, step_name, width = 10, height = 6) {
  filename <- paste0("S", subj, "_T", tr, "_", step_name, ".png")
  ggsave(file.path(subj_dir, filename), plot, width = width, height = height)
  return(invisible(plot))
}

# 3. Function to save intermediate data
save_step_data <- function(data, output_dir, subj, tr, step_name) {
  filename <- paste0(step_name, "_S", subj, "_T", tr, ".csv")
  write_csv(data, file.path(output_dir, "steps", filename))
  return(invisible(data))
}

# 4. Function to process a single step
process_step <- function(data, step_name, plot_title, process_fn, params, 
                         subj, tr, subj_dir, output_dir,
                         save_intermediate_data, save_individual_plots,
                         units = "mm") {
  # Apply processing function
  result <- do.call(process_fn, c(list(data), params))
  
  # Create plot with appropriate units
  plot <- create_pupil_plot(result, plot_title, units = units)
  
  # Save data if requested
  if (save_intermediate_data) {
    save_step_data(result, output_dir, subj, tr, step_name)
  }
  
  # Save individual plot if requested
  if (save_individual_plots) {
    save_plot(plot, subj_dir, subj, tr, step_name)
  }
  
  # Return result and plot
  list(data = result, plot = plot)
}

# 5. Function to process a single trial
# 5. Function to process a single trial
process_trial <- function(trial_data, subj, tr, subj_dir, output_dir, params, 
                          save_intermediate_data, save_individual_plots, return_all_steps,
                          bin_length = 20, smooth_then_interp = TRUE, units = "mm") {
  
  # Skip if not enough data
  if (nrow(trial_data) < 10) {
    message("    Skipping: Not enough data points")
    return(NULL)
  }
  
  # Process with error handling
  tryCatch({
    # Raw data
    raw_data <- trial_data
    raw_plot <- create_pupil_plot(raw_data, "Raw Pupil Data", units = units)
    
    if (save_intermediate_data) {
      save_step_data(raw_data, output_dir, subj, tr, "1_raw")
    }
    
    if (save_individual_plots) {
      save_plot(raw_plot, subj_dir, subj, tr, "1_raw")
    }
    
    # Process each step
    deblink <- process_step(raw_data, "2_deblink", "After Deblinking", 
                            "pupil_deblink", params$deblink, 
                            subj, tr, subj_dir, output_dir,
                            save_intermediate_data, save_individual_plots,
                            units = units)
    
    artifact1 <- process_step(deblink$data, "3_artifact1", "After First Artifact Removal", 
                              "pupil_artifact", params$artifact,
                              subj, tr, subj_dir, output_dir,
                              save_intermediate_data, save_individual_plots,
                              units = units)
    
    artifact2 <- process_step(artifact1$data, "4_artifact2", "After Second Artifact Removal", 
                              "pupil_artifact", params$artifact,
                              subj, tr, subj_dir, output_dir,
                              save_intermediate_data, save_individual_plots,
                              units = units)
    
    missing <- process_step(artifact2$data, "5_missing", "After Missing Data Check", 
                            "pupil_missing", params$missing,
                            subj, tr, subj_dir, output_dir,
                            save_intermediate_data, save_individual_plots,
                            units = units)
    
    upsample <- process_step(missing$data, "6_upsample", "After Upsampling", 
                             "pupil_upsample", params$upsample,
                             subj, tr, subj_dir, output_dir,
                             save_intermediate_data, save_individual_plots,
                             units = units)
    
    if (smooth_then_interp == TRUE) {
      # First smooth, then interpolate
      smooth <- process_step(upsample$data, "7_smooth", "After Smoothing", 
                             "pupil_smooth", params$smooth,
                             subj, tr, subj_dir, output_dir,
                             save_intermediate_data, save_individual_plots,
                             units = units)
      
      interpolate <- process_step(smooth$data, "8_interpolate", "After Interpolation", 
                                  "pupil_interpolate", params$interpolate,
                                  subj, tr, subj_dir, output_dir,
                                  save_intermediate_data, save_individual_plots,
                                  units = units)
    } else {
      # First interpolate, then smooth
      interpolate <- process_step(upsample$data, "7_interpolate", "After Interpolation", 
                                  "pupil_interpolate", params$interpolate,
                                  subj, tr, subj_dir, output_dir,
                                  save_intermediate_data, save_individual_plots,
                                  units = units)
      
      smooth <- process_step(interpolate$data, "8_smooth", "After Smoothing", 
                             "pupil_smooth", params$smooth,
                             subj, tr, subj_dir, output_dir,
                             save_intermediate_data, save_individual_plots,
                             units = units)
    }
    
    final <- process_step(interpolate$data, "9_final", "Final Processed Data", 
                          "pupil_missing", list(),
                          subj, tr, subj_dir, output_dir,
                          save_intermediate_data, save_individual_plots,
                          units = units)
    
    baseline <- process_step(final$data, "10_baselined", "After Baseline Correction", 
                             "pupil_baselinecorrect", params$baseline,
                             subj, tr, subj_dir, output_dir,
                             save_intermediate_data, save_individual_plots,
                             units = units)
    
    # Create binned data
    binned_data <- pupil_bin(baseline$data, bin_length = bin_length)
    
    # Always save the combined plot
    # And set the order of panels to match the order of smoothing vs interpolation
    if (smooth_then_interp == TRUE) {
      all_plots <- wrap_plots(
        list(raw_plot, deblink$plot, artifact1$plot, artifact2$plot, 
             missing$plot, upsample$plot, smooth$plot, interpolate$plot, 
             final$plot, baseline$plot),
        ncol = 4, guides = "collect", axes = "collect"
      )
    } else {
      all_plots <- wrap_plots(
        list(raw_plot, deblink$plot, artifact1$plot, artifact2$plot, 
             missing$plot, upsample$plot, interpolate$plot, smooth$plot, 
             final$plot, baseline$plot),
        ncol = 4, guides = "collect", axes = "collect"
      )
    }
    
    save_plot(all_plots, subj_dir, subj, tr, "all_steps", width = 18, height = 12)
    
    # Return result structure based on whether all steps are requested
    if (return_all_steps) {
      return(list(
        subject = subj,
        trial = tr,
        raw_data = raw_data,
        deblink_data = deblink$data,
        artifact1_data = artifact1$data,
        artifact2_data = artifact2$data,
        missing_data = missing$data,
        upsample_data = upsample$data,
        smooth_data = smooth$data,
        interpolate_data = interpolate$data,
        final_data = final$data,
        baseline_data = baseline$data,
        binned_data = binned_data
      ))
    } else {
      return(list(
        subject = subj,
        trial = tr,
        final_data = baseline$data,
        binned_data = binned_data
      ))
    }
  }, error = function(e) {
    message("    Error processing Subject ", subj, " Trial ", tr, ": ", e$message)
    return(NULL)
  })
}

# 6. Main function - now much cleaner
preprocess_and_visualize <- function(pupil_data, 
                                     plot_dir = here("plots", "preprocessing"),
                                     output_dir = here("data", "preprocessing"),
                                     params = list(
                                       deblink = list(extend = 75),
                                       artifact = list(n = 8),
                                       missing = list(missing_allowed = .90),
                                       upsample = list(),
                                       smooth = list(type = "hann", n = 50),
                                       interpolate = list(type = "cubic-spline", 
                                                          maxgap = 500, hz = 1000),
                                       baseline = list(bc_onset_message = "target",
                                                       baseline_duration = 150, 
                                                       type = "subtractive")
                                     ),
                                     save_intermediate_data = TRUE,
                                     save_individual_plots = FALSE,
                                     return_all_steps = TRUE,
                                     bin_length = 20,
                                     smooth_then_interp = TRUE,
                                     units = "mm") {
  
  # Create directories
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (save_intermediate_data) {
    dir.create(file.path(output_dir, "steps"), recursive = TRUE, 
               showWarnings = FALSE)
  }
  
  # Test if we have the necessary columns
  pupil_col <- if (units == "mm") "Pupil_Diameter.mm" else "Pupil_Diameter.px"
  necessary_cols <- c("Subject", "Trial", "Time", pupil_col)
  if (!all(necessary_cols %in% colnames(pupil_data))) {
    missing_cols <- necessary_cols[!necessary_cols %in% colnames(pupil_data)]
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Add required eye event columns for proper processing
  pupil_data <- pupil_data %>%
    mutate(
      Eye_Event = "",
    )
  
  # Process each subject
  results_list <- list()
  
  # Get unique subjects
  unique_subjects <- unique(pupil_data$Subject)
  
  for (subj in unique_subjects) {
    message("Processing Subject: ", subj)
    
    # Create subject directory for plots
    subj_dir <- file.path(plot_dir, paste0("S", subj))
    dir.create(subj_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Get subject data
    subj_data <- pupil_data %>% 
      filter(Subject == subj)
    
    # Get unique trials
    unique_trials <- unique(subj_data$Trial)
    
    # Process each trial
    trial_results <- map(unique_trials, function(tr) {
      message("  Trial: ", tr)
      
      # Get trial data
      trial_data <- subj_data %>% 
        filter(Trial == tr)
      
      # Process trial with units parameter
      process_trial(
        trial_data, subj, tr, subj_dir, output_dir, params,
        save_intermediate_data, save_individual_plots, return_all_steps,
        bin_length = bin_length, smooth_then_interp = smooth_then_interp,
        units = units
      )
    }) %>%
      compact()  # Remove NULL results
    
    # Save subject-level data
    if (length(trial_results) > 0) {
      # Combine all trials for this subject
      combined_final <- map(trial_results, ~ .x$final_data) %>% bind_rows()
      combined_binned <- map(trial_results, ~ .x$binned_data) %>% bind_rows()
      
      # Save combined data
      write_csv(combined_final, file.path(output_dir, paste0("processed_S", subj, ".csv")))
      write_csv(combined_binned, file.path(output_dir, paste0("binned_S", subj, ".csv")))
      
      # Add to results list
      results_list[[paste0("S", subj)]] <- trial_results
    }
  }
  
  # Extract data for the return value
  if (length(results_list) > 0) {
    # Flatten the nested list structure
    all_trials <- unlist(results_list, recursive = FALSE)
    
    if (!return_all_steps) {
      # Extract final and binned data for the standard return format
      all_final_data <- map(all_trials, ~ .x$final_data) %>% bind_rows()
      all_binned_data <- map(all_trials, ~ .x$binned_data) %>% bind_rows()
      
      # Save combined data
      write_csv(all_final_data, file.path(output_dir, "all_subjects_processed.csv"))
      write_csv(all_binned_data, file.path(output_dir, "all_subjects_binned.csv"))
      
      # Return the standard format
      return(list(
        processed_data = all_final_data,
        binned_data = all_binned_data
      ))
    } else {
      # For all_steps, return the full structure with all intermediate data
      # We'll still save the combined final and binned data
      all_final_data <- map(all_trials, ~ .x$final_data) %>% bind_rows()
      all_binned_data <- map(all_trials, ~ .x$binned_data) %>% bind_rows()
      
      write_csv(all_final_data, file.path(output_dir, "all_subjects_processed.csv"))
      write_csv(all_binned_data, file.path(output_dir, "all_subjects_binned.csv"))
      
      # Return all trial data with intermediate steps
      return(all_trials)
    }
  } else {
    message("No results were successfully processed.")
    return(list())
  }
}