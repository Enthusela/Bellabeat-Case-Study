# Setting up env ----

# Set up knitting options with the knitr package
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)

# Load all required packages
cat("Loading packages...\n", sep = "")

rqd_pkgs <- c(
  "dplyr",
  "anytime",
  "tidyverse",
  "janitor",
  "readr",
  "ggplot2",
  "kableExtra"
)

lapply(rqd_pkgs, function(pkg) {
  if(!requireNamespace(pkg, quietly = FALSE)) {
    cran_mirror <- "https://cran.r-project.org"
    install.packages(as.character(pkg), repos = cran_mirror)
  }
  library(pkg, character.only = TRUE)
})
rm(rqd_pkgs)
print("Loading packages complete.")

# Loading data ----

# Set the working directory of the R markdown environment to match that of the console
# Omitted from report: for working on my laptop specifically
dir <- "/Users/nathanweaver/Library/CloudStorage/OneDrive-Personal/Documents/Professional/Google Data Cert/Course 8 - Capstone Project/Bellabeat Case Study"
setwd(dir)
cat("Set working directory to:", dir, "\n", sep = "")

csv_dir <- "Fitabase_Data_Cleaned"
paths_dfs <- list.files(csv_dir, pattern = "*.csv", full.names = TRUE)

df_names <- paths_dfs %>%
  basename() %>%
  tools::file_path_sans_ext()

# Read in CSV files
cat("DEBUG\tReading CSVs...\n", sep = "")
for (i in 1:length(df_names)) {
  df_name <- df_names[i]
  cat("DEBUG\tReading ",df_name,".csv...\n", sep = "")
  assign(df_names[i], read_csv(paths_dfs[i], show_col_types = FALSE))
}
cat("DEBUG\tReading CSVs done.\n", sep = "")

# Cleaning data ----

## Clean and update variable names
cat("DEBUG\tCleaning variable names...\n", sep = "")
for (df_name in df_names) {
  cat("DEBUG\tCleaning ",df_name,"...\n", sep = "")
  assign(df_name, get(df_name) %>% clean_names())
}
cat("DEBUG\tCleaning variable names complete.\n", sep = "")

# Setting up var_mods ----

var_mods <- data.frame(
  var_old = character(0),
  var_new  = character(0),
  type_new = character(0),
  tbl = character(0)
)
# WARNING: changes without a specified tbl will ALWAYS be applied to matching column names, even if this interferes with a tbl-specific change
# Contain the names/types in the same dataframe so there's a SSOT for the new name/type pair
var_mods <- var_mods %>%
  rbind(.,data.frame(var_old="date",                       var_new="bodycomp_datetime",          type_new="mdy_hms", tbl="bodycomp_src_logs_wide")) %>%
  rbind(.,data.frame(var_old="time",                       var_new="heart_rate_second",          type_new="mdy_hms", tbl="heartrate_src_seconds_tall")) %>%
  rbind(.,data.frame(var_old="value",                      var_new="heart_rate",                 type_new="",        tbl="heartrate_src_seconds_tall")) %>%
  rbind(.,data.frame(var_old="date",                       var_new="sleep_minute",               type_new="mdy_hms", tbl="sleep_src_mins_tall")) %>%
  rbind(.,data.frame(var_old="value",                      var_new="sleep_rank",                 type_new="",        tbl="sleep_src_mins_tall")) %>%
  rbind(.,data.frame(var_old="",                           var_new="activity_hour",              type_new="mdy_hms", tbl="")) %>%
  rbind(.,data.frame(var_old="",                           var_new="activity_minute",            type_new="mdy_hms", tbl="")) %>%
  rbind(.,data.frame(var_old="",                           var_new="sleep_day",                  type_new="mdy_hms", tbl="")) %>%
  rbind(.,data.frame(var_old="",                           var_new="id",                         type_new="chr",     tbl="")) %>%
  rbind(.,data.frame(var_old="",                           var_new="log_id",                     type_new="chr",     tbl="")) %>%
  rbind(.,data.frame(var_old="activity_date",              var_new="activity_day",               type_new="mdy",     tbl="")) %>%
  rbind(.,data.frame(var_old="fairly_active_distance",     var_new="distance_fairly_active",     type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="fairly_active_minutes",      var_new="minutes_fairly_active",      type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="light_active_distance",      var_new="distance_lightly_active",    type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="light_active_minutes",       var_new="minutes_lightly_active",     type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="lightly_active_distance",    var_new="distance_lightly_active",    type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="lightly_active_minutes",     var_new="minutes_lightly_active",     type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="logged_activities_distance", var_new="distance_logged_activities", type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="me_ts",                      var_new="mets",                       type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="sedentary_active_distance",  var_new="distance_sedentary",         type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="sedentary_distance",         var_new="distance_sedentary",         type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="sedentary_active_minutes",   var_new="minutes_sedentary",          type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="sedentary_minutes",          var_new="minutes_sedentary",          type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="step_total",                 var_new="steps_total",                type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="total_distance",             var_new="distance_total",             type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="total_intensity",            var_new="intensity_total",            type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="total_minutes_asleep",       var_new="sleep_minutes_asleep_total", type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="total_sleep_records",        var_new="sleep_records_total",        type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="total_time_in_bed",          var_new="sleep_minutes_bed_total",    type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="tracker_distance",           var_new="distance_tracker",           type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="very_active_distance",       var_new="distance_very_active",       type_new="",        tbl="")) %>%
  rbind(.,data.frame(var_old="very_active_minutes",        var_new="minutes_very_active",        type_new="",        tbl=""))

var_mods_rename <- var_mods %>%
  filter(var_old != "" & var_new != "")
var_mods_recast <- var_mods %>%
  filter(type_new != "")

# Rename ----
cat("DEBUG\tRenaming variables...\n", sep = "")
rename_variables <- function(df_name) {
  cat("DEBUG\tRenaming ",df_name,"...\n", sep = "")
  df <- get(df_name)
  # Check each var name requiring correction against the var names in the df
  for (i in 1:nrow(var_mods_rename)) {
    var_old = var_mods$var_old[i]
    if (!(var_old %in% colnames(df))) {
      next
    }
    # If found, make sure the conversion is applicable to this or all dfs
    tbl <- var_mods$tbl[i]
    if (tbl != "" && tbl != df_name) {
      next
    }
    # Perform the conversion if all checks passed
    var_new = var_mods$var_new[i]
    cat("DEBUG\tdf: ",df_name, "\tvar_old: ",var_old,"\t",sep="")
    cat("var_new: ",var_new,"\t", sep="")
    cat("tbl: ",tbl,"    ", sep="")
    cat("Replacing... ", sep = "")
    df <- df %>% rename(!!var_new := !!var_old)
    cat("Done.\n", sep = "")
  }
  assign(df_name, df)
}
lapply(df_names, rename_variables)
cat("DEBUG\tRenaming variables complete.\n", sep = "")

# Recast ----

cat("DEBUG\tGenerating list of original column types...\n", sep = "")
get_df_col_info <- function(df_name) {
  df <- get(df_name)
  cat("DEBUG\tGenerating for ", df_name, "...\n", sep = "")
  data.frame(
    column = names(df),
    type = sapply(df, function(col) class(col)[1])
  )
}
results_list_before <-lapply(df_names, get_df_col_info)
names(results_list_before) <- df_names
cat("DEBUG\tGenerating list of original column types complete.\n", sep = "")

cat("DEBUG\tGenerating list of target column types...\n", sep = "")
results_list_goal <- results_list_before
for (i in 1:length(results_list_goal)) {
  df_name <- names(results_list_goal)[i]
  cat("DEBUG\tGenerating for ", df_name, "...\n", sep = "")
  df <- results_list_goal[[i]]
  for (j in 1:nrow(df)) {
    row_name <- rownames(df)[j]
    for(m in 1:nrow(var_mods_recast)) {
      var_new <- var_mods_recast$var_new[m]
      type_new <- var_mods_recast$type_new[m]
      if(row_name == var_new) {
        cat("DEBUG\tUpdating ", row_name," from ", df[j, "type"], " to ", type_new, "...\n", sep = "")
        df[j, "type"] <- case_when(
          type_new == "chr" ~ "character",
          type_new == "mdy" ~ "Date",
          type_new == "mdy_hms" ~ "POSIXct",
          TRUE ~ "NA"
        )
      }
    }
  results_list_goal[[i]] <- df
  }
  cat("DEBUG\tGenerating for ", df_name, " complete.\n", sep = "")
}
cat("DEBUG\tGenerating list of target column types complete.\n", sep = "")

cat("DEBUG\tRecasting variables...\n", sep = "")
for (df_name in df_names) {
  cat("DEBUG\tRecasting ",df_name,"...\n", sep = "")
  df <- get(df_name)
  for (i in 1:nrow(var_mods_recast)) {
    var_new <- var_mods_recast$var_new[i]
    type_new <- var_mods_recast$type_new[i]
    if (var_new %in% colnames(df)) {
      cat("DEBUG\tConverting ",df_name,"$",var_new," to ",type_new, "... ", sep = "")
      if (type_new == "chr") {
        df <- df %>% mutate("{var_new}" := as.character(!!sym(var_new)))
      } else if (type_new == "mdy") {
        df <- df %>% mutate("{var_new}" := mdy(!!sym(var_new)))
      } else if (type_new == "mdy_hms") {
        df <- df %>% mutate("{var_new}" := mdy_hms(!!sym(var_new)))
      } else {
        cat("type_new not found: not converting.", sep = "")
      }
      cat("Done.\n", sep = "")
    }
  }
  assign(df_name, df)
  cat("DEBUG\tConverting ",df_name," complete.\n", sep = "")
}
cat("DEBUG\tRecasting variables complete.\n", sep = "")

# Testing ----
cat("DEBUG\tGenerating list of updated column types...\n", sep = "")
results_list_after <- lapply(df_names, get_df_col_info)
names(results_list_after) <- df_names
cat("DEBUG\tGenerating list of updated column types complete.\n", sep = "")

cat("DEBUG\tChecking updated column types against target types...\n", sep = "")
are_identical_lists <- function(list1, list2) {
  if (length(list1) != length(list2)) {
    return(FALSE)
  }
  for (i in seq_along(list1)) {
    if(!identical(list1[[i]], list2[[i]])) {
      cat("Non-identical lists at list1[",i,"]. Exiting.\n", sep = "")
      print(list1[[i]])
      print(list2[[i]])
      return(FALSE)
    }
  }
  return(TRUE)
}
test_succeeded <- are_identical_lists(results_list_after, results_list_goal)

cat("DEBUG\tChecking updated column types against target types complete.\n", sep = "")
cat("Data recasting ", case_when(test_succeeded ~ "succeeded", TRUE ~"failed"), ".", sep = "")

# Writing results back to global environment ----
