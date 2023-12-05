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

    p <- ggplot(data_long, aes(x= id, y= !!sym(value), fill= !!sym(key))) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # scale_y_continuous(breaks = seq(0, 24, by = 1))
  }
}

data <- distance_vs_intensity %>%
  select(id,
         pct_distance_very_active,
         pct_distance_moderately_active,
         pct_distance_lightly_active,
         pct_distance_sedentary)

stack_order <- c("pct_distance_very_active",
                 "pct_distance_moderately_active",
                 "pct_distance_lightly_active",
                 "pct_distance_sedentary")

p <- wide_to_stacked_bar_plot(data_wide = data,
                              key = "intensity",
                              value = "pct_distance",
                              key_order = stack_order)

print(p)