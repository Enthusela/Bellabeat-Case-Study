# Bulk recast of variables with more than one occurrence
retypes <- data.frame (
  name = character(0),
  new_type = character(0)
)
retypes <- retypes %>%
  rbind(., data.frame(name = "activity_day"   , new_type = "datetime" )) %>%
  rbind(., data.frame(name = "activity_hour"  , new_type = "datetime" )) %>%
  rbind(., data.frame(name = "activity_minute", new_type = "datetime" )) %>%
  rbind(., data.frame(name = "date"           , new_type = "datetime" )) %>%
  rbind(., data.frame(name = "id"             , new_type = "character")) %>%
  rbind(., data.frame(name = "log_id"         , new_type = "character")) %>%
  rbind(., data.frame(name = "time"           , new_type = "datetime" ))

convert_to_type <- function(df, col_name, col_type_new) {
  df %>%
    mutate({{col_name}} := case_when(
      col_type_new == "datetime" ~ anytime({{col_name}}),
      col_type_new == "character" ~ as.character({{col_name}}),
      TRUE ~ {{col_name}}))
}

for (df_name in df_names) {
  print(df_name)
  str(get(df_name))
  df <- get(df_name)
  for (i in 1:nrow(retypes)) {
    col_name <- retypes$name[i]
    col_type_new <- retypes$new_type[i]
    if (col_name %in% colnames(df)) {
      df <- convert_to_type(df, col_name, col_type_new)
      str(df)
    }
  }
  assign(df_name, df)
  str(get(df_name))
}

activity_sum_days_wide <- activity_sum_days_wide %>%
  mutate(date := anytime(date))