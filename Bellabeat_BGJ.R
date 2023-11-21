# feature_tibble <- tibble(id = unique_ids)
# for (feature_name in feature_usage$feature) {
#   feature_tibble <- add_column(feature_tibble, !!feature_name := FALSE)
# }
# cat("looping...\n")
# for (i in 1:nrow(feature_tibble)) {
#   id <- feature_tibble$id[i]
#   for (j in 1:nrow(feature_usage)) {
#     feature <- feature_usage$feature[j]
#     id_list <- feature_usage$id_list[[j]]
#     feature_used <- FALSE
#     if (id %in% id_list) {
#       cat("Found id \"",id,"\" using ",feature,".\n",sep="")
#       feature_used <- TRUE
#     } else {
#       cat("Did not find id \"",id,"\" using ",feature,".\n",sep="")
#     }
#     feature_index <- which(colnames(feature_tibble) == feature)
#     cat("Feature index:",feature_index,"\n")
#     feature_tibble[i, feature_index] <- feature_used
#   }
# }
# cat("looping done")

# This method guarantees a zero-row tibble before adding values
feature_tibble_long <- tibble() %>%
  add_column(id := character(0)) %>%
  add_column(feature := character(0)) %>%
  add_column(used := logical(0))

for (unique_id in unique_ids) {
  for (j in 1:nrow(feature_usage)) {
    feature_name <- feature_usage$feature[j]
    id_list <- feature_usage$id_list[[j]]
    feature_used <- FALSE
    if (unique_id %in% id_list) {
      cat("Found id \"",unique_id,"\" using ",feature_name,".\n",sep="")
      feature_used <- TRUE
    } else {
      cat("Did not find id \"",unique_id,"\" using ",feature_name,".\n",sep="")
    }
    feature_tibble_long <- feature_tibble_long %>%
      add_row(id=unique_id,feature=feature_name,used=feature_used)
  }
}

ggplot(feature_tibble_long, aes(x = feature, y = id, color = used)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Feature Usage by ID",
       x = "Feature",
       y = "ID",
       color = "Feature Used")