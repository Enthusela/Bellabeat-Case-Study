# Declare generic functions for use in the analysis below

round_data_to_bin <- function(data, bin_width) {
  rounding_factor <- 1 / bin_width
  return(round(data * rounding_factor) / rounding_factor)
}

get_histogram_max_count <- function(data, bin_width) {
  rounding_factor <- 1 / bin_width
  # Round data to nearest multiple of bin_width
  data_rounded <- round_data_to_bin(data, bin_width)
  max_count <- max(table(data_rounded))
  return(max_count)
}

rescale_plot <- function(p, x_min = 0, x_max = 10, x_step = 1, y_min = 0, y_max = 10, y_step = 1) {
  p <- p +
    scale_x_continuous(breaks = seq(x_min, x_max, by=x_step),
                       labels = scales::comma_format(),
                       limits=c(x_min, x_max)) +
    scale_y_continuous(breaks = seq(y_min, y_max, by=y_step),
                       labels = scales::comma_format(),
                       limits=c(y_min, y_max))
  return(p)
}

plot_histo_pareto <- function(data, bin_width) {
  # Cumulative data uses same bins as histogram
  data <- sort(data)
  data_rounded <- round_data_to_bin(data, bin_width)

  # Calculate highest count so axes can be adjusted
  highest_count <- get_histogram_max_count(data, bin_width)
  data_pareto <- seq(1, length(data), by = 1) / length(data) * highest_count

  # Calculate stats for overlay on histogram
  data_max <- max(data)
  data_mean <- round(mean(data), digits = 2)
  data_median <- round(median(data), digits = 2)
  sum_stats <- data.frame(Statistics = c("Mean", "Median"),
                          value = c(data_mean, data_median))
  label_text <- paste("Mean =", data_mean, ", Median =", data_median)

  p <- ggplot() +
    geom_histogram(aes(x = data),
                   color = "white",
                   binwidth = bin_width) +
    geom_line(aes(x = data_rounded,
                  y = data_pareto),
              color="forestgreen") +
    geom_vline(data = sum_stats,
               aes(xintercept = value,
                   linetype = Statistics,
                   color = Statistics),
               size = 1) +
    scale_x_continuous(breaks = seq(0, data_max + bin_width, by = bin_width)) +
    scale_y_continuous(name = "Users",
                       breaks = seq(0, highest_count, by = 1),
                       sec.axis = sec_axis(~./highest_count, name = "Cumulative Percentage of Users")) +
    # scale_linetype_manual(values = c("solid", "dashed", "solid"),
    #                       labels = c("Mean", "Median", "Cumulative")) +
    # scale_color_manual(name = "Legend",
    #                    values = c("red", "blue", "forestgreen")) +
    # annotate("text", x = Inf, y = Inf, label = label_text,
    #        hjust = 1, vjust = 1, size = 4) +
    labs(x = "Value",
         caption = label_text)
  return(p)
}

get_time_coefficients <- function(ids, timestamps, values) {
  tbl <- tibble(
    id = ids,
    timestamp = timestamps,
    value = values
  )

  coeffs <- tbl %>%
    mutate(day_of_year = yday(timestamp)) %>%
    group_by(id) %>%
    summarize(correlation = cor(day_of_year, value))

  return(coeffs)
}

plot_time_coefficients <- function(coeffs) {
  bin_width <- 0.1
  max_count <- get_histogram_max_count(coeffs$correlation, bin_width)

  coeffs_mean <- round(mean(coeffs$correlation, na.rm = TRUE), digits = 2)
  coeffs_median <- round(median(coeffs$correlation, na.rm = TRUE), digits = 2)
  sum_stats <- data.frame(Statistics = c("Mean", "Median"),
                          value = c(coeffs_mean, coeffs_median))
  label_text <- paste("Mean =", coeffs_mean, "\nMedian =", coeffs_median)

  ggplot(coeffs, aes(x = correlation)) +
    geom_histogram(binwidth = bin_width, color = "white") +
    geom_vline(data = sum_stats,
               aes(xintercept = value,
                   linetype = Statistics,
                   color = Statistics),
               size = 1) +
    scale_x_continuous(breaks = seq(-1.0, 1.0, by=bin_width)) +
    scale_y_continuous(breaks = seq(0, max_count, by=1)) +
    annotate("text", x = Inf, y = Inf, label = label_text,
             hjust = 1, vjust = 1, size = 4) +
    labs(title = "Correlation Coefficient with Time",
         caption = "Positive values imply variable of interest generally increased over time.",
         x = "Correlation Coefficient",
         y = "Count")
}

get_time_coefficients_plot <- function(ids, timestamps, values) {
  coeffs <- get_time_coefficients(ids, timestamps, values)
  p <- plot_time_coefficients(coeffs)
  return(p)
}

wide_to_stacked_bar_plot <- function(data_wide, key, value, key_order) {
  if(!("id" %in% colnames(data_wide))) {
    print("ERROR: data does not include \"id\" column: cannot convert.")
  } else {
    # Convert the data from wide to long. Set the factor levels to control the stacking order of the bars
    data_long <- data_wide %>%
      tidyr::gather(key = !!sym(key), value = !!sym(value), -id) %>%
      mutate(!!sym(key) := factor(!!sym(key), levels = key_order))

    # Order IDs in wide data based on value of first key, then rearrange long data.
    # This ensures the resultant plot sorts the IDs in ascending order of the first key
    first_key <- key_order[1]
    data_wide <- data_wide %>% arrange(!!sym(first_key))
    data_long$id <- factor(
      data_long$id,
      levels = data_wide$id)

    # Plot graph with angled ID labels
    p <- ggplot(data_long, aes(x= id, y= !!sym(value), fill= !!sym(key))) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # scale_y_continuous(breaks = seq(0, 24, by = 1))
  }
}

scatter_with_LOBF <- function(data_x, data_y, labels = NULL) {
  data <- tibble(
    x = data_x,
    y = data_y
  )

  p <- ggplot(data,
              aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                color = "blue") +
    stat_cor(mapping=aes(label=..rr.label..),
             method="pearson",
             label.x=-Inf,
             label.y=Inf,
             hjust = -0.1,
             vjust = 1.1) +
    geom_text(aes(label = sprintf("Gradient: %.2f", coef(lm(y ~ x, data = data))[2])),
              x = -Inf,
              y= Inf,
              hjust = -0.05,
              vjust = 3.5)

  if(!is.null(labels) && length(labels > 1)) {
    p <- p + labs(title = paste(labels[1], "vs.", labels[2]),
                  x = labels[1],
                  y = labels[2])
  }

  return(p)
}

daily_intensity_proportions <- intensity_sum_days_wide %>%
  mutate(minutes_active = minutes_fairly_active + minutes_very_active,
         proportion_very_active = ifelse(minutes_active > 0,
                                         minutes_very_active / minutes_active,
                                         0))

viz_lightly_active_time_coeffs <- get_time_coefficients_plot(
  daily_intensity_proportions$id,
  daily_intensity_proportions$activity_day,
  daily_intensity_proportions$proportion_very_active
)
print(viz_lightly_active_time_coeffs)