
ggplot_histo_pareto <- function (data, bin_width) {
  # Cumulative data uses same bins as histogram
  data <- sort(data)
  rounding_factor <- 1 / bin_width
  data_rounded <- round(data * rounding_factor) / rounding_factor

  # Calculate highest count so axes can be adjusted
  highest_count <- max(table(data_rounded))
  data_pareto = seq(1, length(data), by = 1) / length(data) * highest_count

  # Calculate stats for overlay on histogram
  data_max <- max(data)
  data_mean <- mean(data)
  data_median <- median(data)

  p <- ggplot() +
    geom_histogram(aes(x = data),
                   color = "white",
                   binwidth = bin_width) +
    scale_x_continuous(breaks = seq(0, data_max + bin_width, by = bin_width)) +
    geom_line(aes(x = data_rounded,
                  y = data_pareto,
                  color="Cumulative")) +
    geom_vline(aes(xintercept = data_mean,
                   color = "Mean")) +
    geom_vline(aes(xintercept = data_median,
                   color = "Median")) +
    scale_y_continuous(name = "Count",
                       breaks = seq(0, highest_count, by = 1),
                       sec.axis = sec_axis(~./highest_count, name = "Cumulative Percentage of Count")) +
    scale_color_manual(name="Stats",
                       breaks = c("Cumulative", "Mean", "Median"),
                       values = c("Cumulative" = "red",
                                  "Mean" = "green",
                                  "Median" = "blue")) +
    labs(x = "Value")
  return(p)
}

walking_time_averaged <- walking_times %>%
  group_by(id) %>%
  summarize(average_mod_brisk = mean(moderate_walking + brisk_walking))

plot_mod_brisk <- ggplot_histo_pareto(walking_time_averaged$average_mod_brisk, 0.25)
print(plot_mod_brisk)