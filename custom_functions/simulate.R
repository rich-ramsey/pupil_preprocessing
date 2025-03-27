## 1. Function to generate pupil size and position data for one trial with artifacts
generate_trial_data <- function(n_samples, participant_id, trial_id, eye = "both", hz = 60, 
                                blink_prob = 0.05, artifact_prob = 0.02,
                                x_center = 0.5, y_center = 0.5, position_var = 0.02) {
  time_ms <- seq(0, (n_samples - 1) * (1000 / hz), by = 1000 / hz)  # Generate time points at 60Hz
  
  # Generate base pupil size pattern (sine wave + noise)
  pupil_size_base <- 3 + sin(seq(0, pi * 2, length.out = n_samples)) + rnorm(n_samples, 0, 0.1)
  
  # Generate highly correlated pupil sizes for both eyes
  if(eye == "both") {
    # Basic pupil dilation/constriction pattern is identical for both eyes
    # This reflects the fact that pupil light reflex and cognitive influences affect both eyes equally
    
    # Apply a constant small offset between eyes (one eye is typically slightly larger)
    eye_size_difference <- rnorm(1, 0, 0.1)  # Random difference in baseline size
    
    # Left eye uses the base pattern
    pupil_size_left <- pupil_size_base
    
    # Right eye follows almost identical pattern, with just a constant offset and tiny noise
    # This creates ~0.99 correlation but with realistic individual differences
    pupil_size_right <- pupil_size_base + eye_size_difference + rnorm(n_samples, 0, 0.02)
  } else if(eye == "left") {
    pupil_size_left <- pupil_size_base
    pupil_size_right <- NULL
  } else if(eye == "right") {
    pupil_size_right <- pupil_size_base
    pupil_size_left <- NULL
  }
  
  # Generate PRIMARY eye movement paths that BOTH eyes will follow (with slight offsets)
  # Base eye movements follow a smooth random walk
  generate_movement_path <- function(n_samples, center, variance) {
    # Start with Brownian motion
    movement <- cumsum(rnorm(n_samples, 0, sqrt(variance)/5))
    
    # Add return-to-center tendency
    for(i in 2:n_samples) {
      # Pull toward center based on distance
      movement[i] <- movement[i] * 0.99 + (center - movement[i-1]) * 0.01
    }
    
    # Scale appropriately and center
    movement <- movement * sqrt(variance)/2
    movement <- movement + center - mean(movement)
    
    return(movement)
  }
  
  # Generate PRIMARY movement paths that both eyes will follow
  x_movement <- generate_movement_path(n_samples, x_center, position_var)
  y_movement <- generate_movement_path(n_samples, y_center, position_var)
  
  # Create positions for each eye, based on the PRIMARY paths
  # Basic idea: both eyes follow the SAME paths, just with constant offsets in x
  interpupillary_distance <- 0.05  # Horizontal separation between eyes
  
  if(eye == "both" || eye == "left") {
    # Left eye: shifted left from primary path
    x_pos_left <- x_movement - interpupillary_distance/2
    # Add tiny independent noise (much smaller than primary movement)
    x_pos_left <- x_pos_left + rnorm(n_samples, 0, position_var/50)
    
    # Y position follows primary path with minimal independent variation
    y_pos_left <- y_movement + rnorm(n_samples, 0, position_var/100)
  }
  
  if(eye == "both" || eye == "right") {
    # Right eye: shifted right from primary path
    x_pos_right <- x_movement + interpupillary_distance/2
    # Add tiny independent noise (much smaller than primary movement)
    x_pos_right <- x_pos_right + rnorm(n_samples, 0, position_var/50)
    
    # Y position follows primary path with minimal independent variation
    y_pos_right <- y_movement + rnorm(n_samples, 0, position_var/100)
  }
  
  # Introduce blinks (setting values to NA)
  if(eye == "both") {
    # For binocular recording, almost all blinks affect both eyes simultaneously
    # Physiologically, it's nearly impossible to blink only one eye during natural behavior
    
    # Generate blink mask
    shared_blink_mask <- runif(n_samples) < (blink_prob * 0.98)  # 98% of blinks are shared
    left_only_blink <- runif(n_samples) < (blink_prob * 0.01)    # 1% left eye only (very rare)
    right_only_blink <- runif(n_samples) < (blink_prob * 0.01)   # 1% right eye only (very rare)
    
    # Create masks for each eye
    left_blink_mask <- shared_blink_mask | left_only_blink
    right_blink_mask <- shared_blink_mask | right_only_blink
    
    # Apply blinks to pupil size and position data
    pupil_size_left[left_blink_mask] <- NA
    x_pos_left[left_blink_mask] <- NA
    y_pos_left[left_blink_mask] <- NA
    
    pupil_size_right[right_blink_mask] <- NA
    x_pos_right[right_blink_mask] <- NA
    y_pos_right[right_blink_mask] <- NA
    
    # For shared blinks - create realistic "onset asynchrony" 
    # In real data, one eye often starts blinking slightly before the other
    if(sum(shared_blink_mask) > 0) {
      # Get the starting indices of blink sequences
      blink_starts <- which(diff(c(0, shared_blink_mask)) == 1)
      
      for(start_idx in blink_starts) {
        # Randomly determine which eye blinks first (usually within 1-2 samples)
        first_eye <- sample(c("left", "right"), 1)
        lag_samples <- sample(1:2, 1)  # 1-2 samples lag (at 60Hz = 16-33ms)
        
        # Ensure we don't go beyond array bounds
        if(start_idx + lag_samples <= n_samples) {
          # Apply the lag to make one eye blink slightly after the other
          if(first_eye == "left") {
            # Right eye blinks lag_samples later than left
            right_blink_mask[start_idx:(start_idx + lag_samples - 1)] <- FALSE
          } else {
            # Left eye blinks lag_samples later than right
            left_blink_mask[start_idx:(start_idx + lag_samples - 1)] <- FALSE
          }
        }
      }
      
      # Re-apply the adjusted blink masks
      pupil_size_left[left_blink_mask] <- NA
      x_pos_left[left_blink_mask] <- NA
      y_pos_left[left_blink_mask] <- NA
      
      pupil_size_right[right_blink_mask] <- NA
      x_pos_right[right_blink_mask] <- NA
      y_pos_right[right_blink_mask] <- NA
    }
    
  } else if(eye == "left") {
    blink_mask <- runif(n_samples) < blink_prob
    pupil_size_left[blink_mask] <- NA
    x_pos_left[blink_mask] <- NA
    y_pos_left[blink_mask] <- NA
  } else if(eye == "right") {
    blink_mask <- runif(n_samples) < blink_prob
    pupil_size_right[blink_mask] <- NA
    x_pos_right[blink_mask] <- NA
    y_pos_right[blink_mask] <- NA
  }
  
  # Introduce artifacts: sudden rapid dilation/constriction and position jumps
  apply_artifacts <- function(pupil_size, x_pos, y_pos, n_samples, artifact_prob) {
    artifact_mask <- runif(n_samples) < artifact_prob  # Probability of artifact
    artifact_indices <- which(artifact_mask)  # Identify time points for artifacts
    
    for (idx in artifact_indices) {
      # Define random dilation/constriction magnitude
      size_change <- sample(c(-1, 1), 1) * runif(1, 0.5, 1.5)
      
      # Define random position change
      x_change <- sample(c(-1, 1), 1) * runif(1, 0.02, 0.05)
      y_change <- sample(c(-1, 1), 1) * runif(1, 0.02, 0.05)
      
      # Apply artifact over a short duration (200-500ms)
      duration <- sample(12:30, 1)
      end_idx <- min(idx + duration, n_samples)
      
      # Apply changes gradually if indices are valid
      if(idx <= length(pupil_size) && end_idx <= length(pupil_size)) {
        valid_indices <- idx:end_idx
        pupil_size[valid_indices] <- pupil_size[valid_indices] + size_change
        x_pos[valid_indices] <- x_pos[valid_indices] + x_change
        y_pos[valid_indices] <- y_pos[valid_indices] + y_change
      }
    }
    
    return(list(pupil_size = pupil_size, x_pos = x_pos, y_pos = y_pos))
  }
  
  # Apply artifacts - most artifacts affect both eyes simultaneously with similar patterns
  if(eye == "both") {
    # For binocular recording, create shared artifact indices
    shared_artifact_mask <- runif(n_samples) < (artifact_prob * 0.9)  # 90% of artifacts affect both eyes
    left_only_artifact <- runif(n_samples) < (artifact_prob * 0.1)    # 10% left eye only  
    right_only_artifact <- runif(n_samples) < (artifact_prob * 0.1)   # 10% right eye only
    
    # Get artifact indices for each eye
    shared_indices <- which(shared_artifact_mask)
    left_only_indices <- which(left_only_artifact)
    right_only_indices <- which(right_only_artifact)
    
    # Apply the shared artifacts to both eyes with very similar patterns
    for(idx in shared_indices) {
      # Create correlated artifact changes for both eyes
      size_change_base <- sample(c(-1, 1), 1) * runif(1, 0.5, 1.5)
      x_change_base <- sample(c(-1, 1), 1) * runif(1, 0.02, 0.05)
      y_change_base <- sample(c(-1, 1), 1) * runif(1, 0.02, 0.05)
      
      # Small variations in artifact magnitude between eyes
      size_change_left <- size_change_base * runif(1, 0.9, 1.1)
      size_change_right <- size_change_base * runif(1, 0.9, 1.1)
      
      x_change_left <- x_change_base * runif(1, 0.95, 1.05)
      x_change_right <- x_change_base * runif(1, 0.95, 1.05)
      
      y_change_left <- y_change_base * runif(1, 0.95, 1.05)
      y_change_right <- y_change_base * runif(1, 0.95, 1.05)
      
      # Apply artifact over same duration for both eyes
      duration <- sample(12:30, 1)
      end_idx <- min(idx + duration, n_samples)
      valid_indices <- idx:end_idx
      
      # Apply changes to both eyes
      if(length(valid_indices) > 0) {
        # Left eye
        pupil_size_left[valid_indices] <- pupil_size_left[valid_indices] + size_change_left
        x_pos_left[valid_indices] <- x_pos_left[valid_indices] + x_change_left
        y_pos_left[valid_indices] <- y_pos_left[valid_indices] + y_change_left
        
        # Right eye
        pupil_size_right[valid_indices] <- pupil_size_right[valid_indices] + size_change_right
        x_pos_right[valid_indices] <- x_pos_right[valid_indices] + x_change_right
        y_pos_right[valid_indices] <- y_pos_right[valid_indices] + y_change_right
      }
    }
    
    # Apply individual eye artifacts (much less common)
    apply_individual_artifact <- function(indices, pupil_size, x_pos, y_pos) {
      for(idx in indices) {
        size_change <- sample(c(-1, 1), 1) * runif(1, 0.3, 1.0)
        x_change <- sample(c(-1, 1), 1) * runif(1, 0.01, 0.03)
        y_change <- sample(c(-1, 1), 1) * runif(1, 0.01, 0.03)
        
        duration <- sample(8:20, 1)
        end_idx <- min(idx + duration, n_samples)
        valid_indices <- idx:end_idx
        
        if(length(valid_indices) > 0) {
          pupil_size[valid_indices] <- pupil_size[valid_indices] + size_change
          x_pos[valid_indices] <- x_pos[valid_indices] + x_change
          y_pos[valid_indices] <- y_pos[valid_indices] + y_change
        }
      }
      return(list(pupil_size=pupil_size, x_pos=x_pos, y_pos=y_pos))
    }
    
    # Apply left-only artifacts
    left_result <- apply_individual_artifact(left_only_indices, pupil_size_left, x_pos_left, y_pos_left)
    pupil_size_left <- left_result$pupil_size
    x_pos_left <- left_result$x_pos
    y_pos_left <- left_result$y_pos
    
    # Apply right-only artifacts
    right_result <- apply_individual_artifact(right_only_indices, pupil_size_right, x_pos_right, y_pos_right)
    pupil_size_right <- right_result$pupil_size
    x_pos_right <- right_result$x_pos
    y_pos_right <- right_result$y_pos
    
  } else if(eye == "left") {
    # Original code for left eye only condition
    left_result <- apply_artifacts(pupil_size_left, x_pos_left, y_pos_left, n_samples, artifact_prob)
    pupil_size_left <- left_result$pupil_size
    x_pos_left <- left_result$x_pos
    y_pos_left <- left_result$y_pos
  } else if(eye == "right") {
    # Original code for right eye only condition
    right_result <- apply_artifacts(pupil_size_right, x_pos_right, y_pos_right, n_samples, artifact_prob)
    pupil_size_right <- right_result$pupil_size
    x_pos_right <- right_result$x_pos
    y_pos_right <- right_result$y_pos
  }
  
  # Create data frames for each eye
  if(eye == "both") {
    # Create separate data frames for left and right eyes
    left_data <- tibble(
      participant_id = participant_id,
      trial_id = trial_id,
      eye = "left",
      time_ms = time_ms,
      pupil_size = pupil_size_left,
      x_pos = x_pos_left,
      y_pos = y_pos_left
    )
    
    right_data <- tibble(
      participant_id = participant_id,
      trial_id = trial_id,
      eye = "right",
      time_ms = time_ms,
      pupil_size = pupil_size_right,
      x_pos = x_pos_right,
      y_pos = y_pos_right
    )
    
    # Combine both eyes' data
    return(bind_rows(left_data, right_data))
  } else if(eye == "left") {
    return(tibble(
      participant_id = participant_id,
      trial_id = trial_id,
      eye = "left",
      time_ms = time_ms,
      pupil_size = pupil_size_left,
      x_pos = x_pos_left,
      y_pos = y_pos_left
    ))
  } else {
    return(tibble(
      participant_id = participant_id,
      trial_id = trial_id,
      eye = "right",
      time_ms = time_ms,
      pupil_size = pupil_size_right,
      x_pos = x_pos_right,
      y_pos = y_pos_right
    ))
  }
}

## 2. Function to generate data for multiple participants and trials
generate_pupil_data <- function(n_participants = 3, n_trials = 3, eye = "both", hz = 60, 
                                trial_length = NULL, min_length = 100, max_length = 300,
                                x_center = 0.5, y_center = 0.5, position_var = 0.02) {
  set.seed(123)  # For reproducibility
  
  # Define trial lengths based on input parameters
  if (is.null(trial_length)) {
    # Variable trial lengths if trial_length is NULL
    trial_lengths <- sample(min_length:max_length, n_participants * n_trials, replace = TRUE)
  } else if (length(trial_length) == 1) {
    # Same fixed length for all trials
    trial_lengths <- rep(trial_length, n_participants * n_trials)
  } else if (length(trial_length) == (n_participants * n_trials)) {
    # User-specified vector of lengths for each trial
    trial_lengths <- trial_length
  } else {
    stop("trial_length must be NULL, a single value, or a vector with length equal to n_participants * n_trials")
  }
  
  # Generate data for each participant and trial
  data <- expand_grid(
    participant_id = paste0("P", sprintf("%03d", 1:n_participants)),
    trial_id = paste0("T", sprintf("%03d", 1:n_trials))
  ) %>%
    mutate(trial_length = trial_lengths) %>%
    pmap_dfr(~ generate_trial_data(..3, ..1, ..2, eye = eye, hz = hz, 
                                   x_center = x_center, y_center = y_center, 
                                   position_var = position_var))
  
  return(data)
}


## 3. Additional utility: Check correlation between left and right eye measurements
check_eye_correlation <- function(data, participant, trial) {
  # Get data for specific participant and trial
  subset_data <- data %>%
    filter(participant_id == participant, trial_id == trial)
  
  # Reshape to have left and right eye data in the same rows
  wide_data <- subset_data %>%
    select(eye, time_ms, pupil_size, x_pos, y_pos) %>%
    tidyr::pivot_wider(
      names_from = eye,
      values_from = c(pupil_size, x_pos, y_pos)
    )
  
  # Calculate correlations
  pupil_cor <- cor(wide_data$pupil_size_left, wide_data$pupil_size_right, use = "complete.obs")
  x_cor <- cor(wide_data$x_pos_left, wide_data$x_pos_right, use = "complete.obs")
  y_cor <- cor(wide_data$y_pos_left, wide_data$y_pos_right, use = "complete.obs")
  
  # Return correlations
  return(list(
    pupil_size_correlation = pupil_cor,
    x_position_correlation = x_cor,
    y_position_correlation = y_cor
  ))
}