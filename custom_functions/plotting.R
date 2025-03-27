# a plotting function for 1 trial
plot_trial <- function(df){
  df %>% 
    ggplot(aes(x= time_zero, y= pupil)) + 
    geom_line() +
    scale_x_continuous(breaks = seq(0, 40000, 10000),
                       labels = seq(0, 40, 10)) +
    ggtitle("Raw Pupil Signal") +
    labs(x="time (s)", y="pupil (mm)") +
    facet_wrap(~eye)
}

## and now by condition and facet trials
## this is for the first half of trials (I split them up because they are easier to see that way)
plot_trialc1 <- function(df){
  df %>% 
    ggplot(aes(x= time_zero, y= pupil,
               colour = condition)) + 
    geom_line() +
    scale_x_continuous(breaks = seq(0, 40000, 10000),
                       labels = seq(0, 40, 10)) +
    scale_color_brewer(palette = "Dark2") +
    labs(x="time (s)", y="pupil (mm)") +
    facet_grid_paginate(trial~eye, ncol = 2, nrow = 10, page = 1)
}

## second half of trials
plot_trialc2 <- function(df){
  df %>% 
    ggplot(aes(x= time_zero, y= pupil,
               colour = condition)) + 
    geom_line() +
    scale_x_continuous(breaks = seq(0, 40000, 10000),
                       labels = seq(0, 40, 10)) +
    scale_color_brewer(palette = "Dark2") +
    labs(x="time (s)", y="pupil (mm)") +
    facet_grid_paginate(trial~eye, ncol = 2, nrow = 10, page = 2)
}

## summary plot of all trials
plot_trial_summary <- function(plot1, plot2){
  (plot1 | plot2) +
    plot_layout(axes = "collect",
                guides = "collect") +
    plot_annotation(title = "Raw pupil data")
}
# Function to save each plot
save_raw_plot <- function(subject, plot_summary) {
  ggsave(filename = paste0("figures/raw/raw_plot_pid", subject, ".png"), 
         plot = plot_summary, width = 10, height = 10, dpi = 300)
}