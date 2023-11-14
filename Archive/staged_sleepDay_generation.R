df_minuteSleep_summed_by_log <- df_minuteSleep_merged %>%
  mutate(date_typed = mdy_hms(date)) %>%
  mutate(date_floored = floor_date(date_typed, unit = "days")) %>%
  group_by(logId) %>%
  summarize(
    "Id" = min(Id),
    "SleepDay" = max(date_floored),
    "Id_SleepDay" = paste(Id, SleepDay, sep = "_"),
    # "SleepDay_stt" = min(date_floored),
    # "SleepDay_diff" = SleepDay - SleepDay_stt,
    "minutes_in_bed"   = n(),
    "minutes_awake"    = sum(case_when(value == 3 ~ 1, TRUE ~ 0)),
    "minutes_restless" = sum(case_when(value == 2 ~ 1, TRUE ~ 0)),
    "minutes_asleep"   = sum(case_when(value == 1 ~ 1, TRUE ~ 0))
  ) %>%
  arrange(Id)

View(df_minuteSleep_summed_by_log)

df_sleepDay_merged_custom_staged <- df_minuteSleep_summed_by_log %>%
  group_by(Id, SleepDay) %>%
  summarize(
    "TotalSleepRecords" = n(),
    "TotalMinutesAsleep_2" = sum(minutes_asleep),
    "TotalTimeInBed_2" = sum(minutes_in_bed),
    "TotalMinutesAwake" = sum(minutes_awake),
    "TotalMinutesRestless" = sum(minutes_restless),

  ) %>%
  mutate("Id_SleepDay_UID" = paste(Id, SleepDay, sep = "_")) %>%
  arrange(Id_SleepDay_UID)
View(df_sleepDay_merged_custom_staged)