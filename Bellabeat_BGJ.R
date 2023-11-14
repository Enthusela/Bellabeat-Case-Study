# Setting up env ----

rm(list = ls())

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
# cat("Set working directory to:", dir, "\n", sep = "")
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

# Setting up var_mods ----

var_mods <- data.frame(
  var_old = character(0),
  var_new  = character(0),
  type_new = character(0),
  tbl = character(0)
)

# WARNING: Ensure table-specific modifications (tbl != "") are positioned above non-specific modifications with matching var_old/var_new values: only the first modification in the list will be applied to matching variables.
# TODO: Eliminate this issue by modifying code to warn/handle conflicting rows

var_mods <- var_mods %>%
  rbind(.,data.frame(var_old="date",                       var_new="bodycomp_datetime",          type_new="POSIXct", tbl="bodycomp_src_logs_wide")) %>%
  rbind(.,data.frame(var_old="time",                       var_new="heart_rate_second",          type_new="POSIXct", tbl="heartrate_src_seconds_tall")) %>%
  rbind(.,data.frame(var_old="value",                      var_new="heart_rate",                 type_new="",        tbl="heartrate_src_seconds_tall")) %>%
  rbind(.,data.frame(var_old="date",                       var_new="sleep_minute",               type_new="POSIXct", tbl="sleep_src_mins_tall")) %>%
  rbind(.,data.frame(var_old="value",                      var_new="sleep_rank",                 type_new="",        tbl="sleep_src_mins_tall")) %>%
  rbind(.,data.frame(var_old="",                           var_new="activity_hour",              type_new="POSIXct", tbl="")) %>%
  rbind(.,data.frame(var_old="",                           var_new="activity_minute",            type_new="POSIXct", tbl="")) %>%
  rbind(.,data.frame(var_old="",                           var_new="sleep_day",                  type_new="POSIXct", tbl="")) %>%
  rbind(.,data.frame(var_old="",                           var_new="id",                         type_new="character",     tbl="")) %>%
  rbind(.,data.frame(var_old="",                           var_new="log_id",                     type_new="character",     tbl="")) %>%
  rbind(.,data.frame(var_old="activity_date",              var_new="activity_day",               type_new="Date",     tbl="")) %>%
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

# TODO: relocate or eliminate these variables
var_mods_rename <- var_mods %>%
  filter(var_old != "" & var_new != "")
var_mods_recast <- var_mods %>%
  filter(type_new != "")

# Setting up functions ----

rename_df_variables <- function(df_name) {
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
  cat("DEBUG\tRenaming ",df_name," complete.\n", sep = "")
  return(df)
}

rename_df_variables_byGlobalDF <- function(df) {
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
    cat("DEBUG\tvar_old: ",var_old,"\t",sep="")
    cat("var_new: ",var_new,"\t", sep="")
    cat("tbl: ",tbl,"    ", sep="")
    cat("Replacing... ", sep = "")
    df <- df %>% rename(!!var_new := !!var_old)
    cat("Done.\n", sep = "")
  }
}

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

# Rename ----

cat("DEBUG\tCleaning variable names...\n", sep = "")
for(df_name in df_names) {
  cat("DEBUG\tCleaning ",df_name,"...\n", sep = "")
  assign(df_name, get(df_name) %>% clean_names())
}
cat("DEBUG\tCleaning variable names complete.\n", sep = "")

cat("DEBUG\tRenaming variables...\n", sep = "")
for(df_name in df_names) {
  cat("DEBUG\tRenaming ",df_name,"...\n", sep = "")
  # df <- rename_df_variables(df_name)
  assign(df_name, rename_df_variables(df_name))
  cat("DEBUG\tRenaming ",df_name," complete.\n", sep = "")
}
cat("DEBUG\tRenaming variables complete.\n", sep = "")

# Recast Testing: Generate list of target values ----

# Testing Process:
  # Get complete list of target variable types
  # Execute re-cast
  # Get complete list of current variable types
  # Compare target to current


get_df_var_types <- function(df_name) {
  cat("DEBUG\tGetting current variable types for ", df_name, "...", sep = "")
  df <- get(df_name)
  var_types <- data.frame(
    var = names(df),
    type = sapply(df, function(col) class(col)[1])
  )
  cat("Done.\n", sep = "")
  return(var_types)
}

get_df_target_var_types <- function(df_name) {
  cat("DEBUG\tGetting target variable types for ", df_name, "...", sep = "")
  var_types <- get_df_var_types(df_name)
  # Iterate over rows in current var_types
  for (var_row in 1:nrow(var_types)) {
    # Check if var name is in var_mods_recast
    var_name <- var_types$var[var_row]
    for(mods_row in 1:nrow(var_mods_recast)) {
      var_name_mods <- var_mods_recast$var_new[mods_row]
      # If no, skip: if yes, replace type with target
      if(var_name == var_name_mods) {
        type_new <- var_mods_recast$type_new[mods_row]
        cat("DEBUG\tUpdating ", var_name, " from ", var_types$type[var_row], " to ", var_mods_recast$type_new[mods_row], "...\n", sep = "")
        var_types$type[var_row] <- type_new
      }
    }
  }
  cat("Done.\n", sep = "")
  return(var_types)
}

# Generate list of all current variable types for all tables, not just the ones that need to change

cat("DEBUG\tGenerating list of original column types for testing...\n", sep = "")
df_types_original <-lapply(df_names, get_df_var_types)
names(df_types_original) <- df_names
cat("DEBUG\tGenerating list of original column types complete.\n", sep = "")

cat("DEBUG\tGenerating list of target column types for testing...\n", sep = "")
df_types_target <-lapply(df_names, get_df_target_var_types)
names(df_types_target) <- df_names
cat("DEBUG\tGenerating list of target column types complete.\n", sep = "")

# Recast ----

cat("DEBUG\tRecasting variables...\n", sep = "")
for (df_name in df_names) {
  cat("DEBUG\tRecasting ",df_name,"...\n", sep = "")
  df <- get(df_name)
  for (i in 1:nrow(var_mods_recast)) {
    var_new <- var_mods_recast$var_new[i]
    type_new <- var_mods_recast$type_new[i]
    if (var_new %in% colnames(df)) {
      cat("DEBUG\tConverting ",df_name,"$",var_new," to ",type_new, "... ", sep = "")
      if (type_new == "character") {
        df <- df %>% mutate("{var_new}" := as.character(!!sym(var_new)))
      } else if (type_new == "Date") {
        df <- df %>% mutate("{var_new}" := mdy(!!sym(var_new)))
      } else if (type_new == "POSIXct") {
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


# Recast Testing: Compare list of actual values to target ----
cat("DEBUG\tGenerating list of updated column types...\n", sep = "")
df_types_after <- lapply(df_names, get_df_var_types)
names(df_types_after) <- df_names
cat("DEBUG\tGenerating list of updated column types complete.\n", sep = "")

cat("DEBUG\tChecking updated column types against target types...\n", sep = "")
test_succeeded <- are_identical_lists(df_types_after, df_types_target)
cat("DEBUG\tChecking updated column types against target types complete.\n", sep = "")
cat("Data recasting ", case_when(test_succeeded ~ "succeeded", TRUE ~"failed"), ".", sep = "")
