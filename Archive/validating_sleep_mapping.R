df_sleepDay_rounding_test <- df_minuteSleep_merged %>%
  mutate(date_original = mdy_hms(date)) %>%
  mutate(date_rounded = round_date(date_original, unit = "days")) %>%
  mutate(Unique_ID = paste(Id, date_rounded, sep = "_")) %>%
  group_by(Id, date_rounded, Unique_ID) %>%
  summarize(
    "minutes_awake"    = sum(case_when(value == 3 ~ 1, TRUE ~ 0)),
    "minutes_restless" = sum(case_when(value == 2 ~ 1, TRUE ~ 0)),
    "minutes_asleep"   = sum(case_when(value == 1 ~ 1, TRUE ~ 0)),
    "minutes_in_bed"   = n()
  ) %>%
  arrange(Id, date_rounded)

df_sleepDay_UID <- df_sleepDay_merged %>%
  mutate(SleepDay_original = mdy_hms(SleepDay)) %>%
  mutate(SleepDay_floored = floor_date(SleepDay_original, unit = "days")) %>%
  mutate(Unique_ID = paste(Id, SleepDay_floored, sep = "_")) %>%
  group_by(Id, SleepDay_floored, Unique_ID)

df_sleepDay_comp <- df_sleepDay_UID %>%
  with(
    merge(df_sleepDay_UID,
          df_sleepDay_rounding_test,
          by = "Unique_ID",
          all = TRUE)
  ) %>%
  mutate(sleepDiff = TotalMinutesAsleep - minutes_asleep)