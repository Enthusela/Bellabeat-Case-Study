for (df_name in df_names) {
  print(df_name)
  df <- get(df_name)
  for (i in 1:nrow(retypes)) {
    col_name <- retypes$name[i]
    col_type_new <- retypes$new_type[i]
    if (col_name %in% colnames(df)) {
      cat("INFO: Converting ", df_name, "$", col_name, " to ", col_type_new, "\n", sep = "")
      if (col_type_new == "datetime") {
        df <- df %>% mutate("{col_name}" := anytime(!!sym(col_name)))
      } else if (col_type_new == "chr") {
        # mutate needs the name of the column only, so we use glue syntax to interpolate the correct name from the env-variable chr vector
        # as.character requires the actual data of the column,so we turn the col_name env-var, in this case a string, into a symbol so it can point to the right data column, then we unquote it to get the actual data itself
        df <- df %>% mutate("{col_name}" := as.character(!!sym(col_name)))
      } else {
        print("col_type_new not found: not converting.")
      }
    }
  }
  # assign(df_name, df)
  # str(get(df_name))
}
# rm(df)
# rm(col_name)
# rm(col_type_new)