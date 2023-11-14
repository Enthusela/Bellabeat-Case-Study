# For each table
for (df_name in df_names) {
  # Print "before" column names
  cat("\ncolnames(", df_name, ") pre-rename: \n", sep = "")
  print(colnames(get(df_name)))
  for (col_name in colnames(get(df_name))) {
    cat("Renaming: ", df_name, "$", col_name, "... ", sep = "")
    col_name_match_indexes <- which(renames$old == col_name)
    if(length(col_name_match_indexes) == 0) {
      cat("Not found. Done.", "\n", sep = "")
    } else {
      cat(length(col_name_match_indexes), " matches found... ", sep = "")
      for (index in col_name_match_indexes) {
        table_name <- renames$table[index]
        cat("table_name = \"", table_name, "\"... ", sep = "")
        if (table_name != "" && df_name != table_name) {
          cat("Incorrect... ", sep = "")
        } else {
          old_name <- renames$old[index]
          new_name <- renames$new[index]
          cat("Correct. Replacing \"", old_name, "\" with \"", new_name, "\"... ", sep = "")
          # TODO: the actual replacing
          assign(df_name, df <- get(df_name) %>% rename(!!new_name := !!old_name))
          cat("Done.", "\n", sep = "")
          # Only replace on first match
          break()
        }
      }
    }
  }
  # Print "after" column names
  cat("\ncolnames(", df_name, ") post-rename: \n", sep = "")
  print(colnames(get(df_name)))
}
print("Renaming variables complete.")