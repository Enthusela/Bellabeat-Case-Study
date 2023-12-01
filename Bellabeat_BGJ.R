days2secs <- 60*60*24
sleep_ranges_stt = seq(as.POSIXct("2016-04-12 20:00:00", tz = "UTC"),
                      as.POSIXct("2016-05-12 20:00:00", tz = "UTC"),
                      by=days2secs)
sleep_ranges_end = seq(as.POSIXct("2016-04-13 06:00:00", tz = "UTC"),
                      as.POSIXct("2016-05-13 06:00:00", tz = "UTC"),
                      by=days2secs)
sleep_ranges <- tibble(stt = sleep_ranges_stt,
                       end = sleep_ranges_end)

hr_data_gaps <- heartrate_src_seconds_tall %>%
  group_by(id) %>%
  nest() %>%
  mutate(next_poll = map(data, ~ lead(.x$heart_rate_second))) %>%
  mutate(gap = map(data, ~ difftime(lead(.x$heart_rate_second), .x$heart_rate_second, units = "hours"))) %>%
  unnest(cols = c(data, next_poll, gap)) %>%
  filter(gap >= 1)

get_date_range_overlap <- function(date1_stt, date1_end, date2_stt, date2_end) {
  # Returns a percentage indicating the amount that date2 range overlaps date1 range
  if (zGap_range_stt >= zSleep_range_end | zGap_range_end <= zSleep_range_stt) {
    overlap <- 0
  } else {
    duration <- as.double(difftime(date1_end, date1_stt, units = "hours"))
    overlap = min(1, max(0,
                         duration
                         - max(0, as.double(difftime(date2_stt, date1_stt)))
                         - max(0, as.double(difftime(date1_end, date2_end)))
                         ) / duration)
  }
  return(overlap)
}

for (hr_id in unique(hr_data_gaps$id)) {
  id_gaps <- hr_data_gaps %>% filter(id == hr_id)
  for (i in 1:nrow(sleep_ranges)) {
    zSleep_range_stt <- sleep_ranges$stt[i]
    zSleep_range_end <- sleep_ranges$end[i]
    duration <- as.double(difftime(zSleep_range_end, zSleep_range_stt))
    polling_coverage <- 1.0
    # How much does the gap overlap the target region?
    for (j in 1:nrow(id_gaps)) {
      # Stop checking gaps if range is already completely covered
      if (polling_coverage <= 0) {
        break
      }
      # Ignore gaps that end before the sleep range
      zGap_range_end = id_gaps$next_poll[j]
      if (zGap_range_end <= zSleep_range_stt) {
        next
      }
      # Skip all gaps that start after the sleep range
      zGap_range_stt = id_gaps$heart_rate_second[j]
      if (zGap_range_stt >= zSleep_range_end) {
        break
      }
      overhang_lhs <- max(0, as.double(difftime(zGap_range_stt, zSleep_range_stt, units = "hours")))
      overhang_rhs <- max(0, as.double(difftime(zSleep_range_end, zGap_range_end, units = "hours")))
      overlap = min(1, max(0, duration - overhang_lhs - overhang_rhs) / duration)
      polling_coverage <- max(0, polling_coverage - overlap)
      cat("update polling_coverage:", polling_coverage,"\n")
    }
    cat("final polling_coverage:", polling_coverage,"\n")
  }
}

# for (i in 1:nrow(sleep_ranges)) {
#   zSleep_range_stt <- sleep_ranges$stt[i]
#   zSleep_range_end <- sleep_ranges$end[i]
#   sleep_duration <- as.double(difftime(zSleep_range_end, zSleep_range_stt))
#   for (hr_id in unique(hr_data_gaps$id)) {
#     # How much does the gap overlap the target region?
#     id_gaps <- hr_data_gaps %>% filter(id == hr_id)
#     for (j in 1:nrow(id_gaps)) {
#       zGap_range_stt = id_gaps$heart_rate_second[j]
#       zGap_range_end = id_gaps$next_poll[j]
#       if (zGap_range_stt >= zSleep_range_end | zGap_range_end <= zSleep_range_stt) {
#         overlap <- 0
#       } else {
#         overlap = min(1, max(0,
#                              (sleep_duration
#                               - max(0, as.double(difftime(zGap_range_stt, zSleep_range_stt)))
#                               - max(0, as.double(difftime(zSleep_range_end, zGap_range_end)))
#                              )) / sleep_duration)
#       }
#       cat("Overlap:", overlap,"\n")
#       # if (zGap_range_end < zSleep_range_stt | zGap_range_stt > zSleep_range_end) { # No overlap
#       #   overlap <- overlap + 0
#       # } else if (zGap_range_stt < zSleep_range_stt & zGap_range_end <= zSleep_range_end) { # Gap overlaps LHS of sleep range
#       #   overlap <- overlap + difftime(zSleep_range_stt, zGap_range_end)
#       # } else if (zGap_range_stt >= zSleep_range_stt & zGap_range_end > zSleep_range_end) { # Gap overlaps RHS of sleep range
#       #   overlap <- overlap + difftime(zGap_range_stt, zSleep_range_end)
#       # } else { # Gap contained within sleep range
#       #   overlap <- difftime(zGap_range_stt, zGap_range_end)
#       # }
#     }
#   }
# }