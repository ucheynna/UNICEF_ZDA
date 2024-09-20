# Missing indicator calculator (using hc57)
missing_hc57 <- lapply(merged_datasets, function(x) {
  return(!"hc57" %in% names(x))
})

# Get the indices of dataframes missing `hc57`
missing_indices <- which(unlist(missing_hc57))

# Show the dataframes in the list that are missing `hc57`
missing_dataframes <- merged_datasets[missing_indices]

# Print the SurveyId of the dataframes missing `hc57`
for (i in seq_along(missing_dataframes)) {
  print(paste("SurveyId with missing hc57:", missing_dataframes[[i]]$SurveyId[1]))
}

# Optionally, display the first few rows of the dataframes missing `hc57`
for (df in missing_dataframes) {
  print(head(df))
}

# For NA values
# Identify and print dataframes where `hc57` is present but contains only `NA` values
na_hc57 <- lapply(merged_datasets, function(x) {
  return("hc57" %in% names(x) && all(is.na(x$hc57)))
})

# Get the indices of dataframes where `hc57` is all `NA`
na_indices <- which(unlist(na_hc57))

# Show the dataframes in the list where `hc57` is all `NA`
na_dataframes <- merged_datasets[na_indices]

# Print the SurveyId of the dataframes where `hc57` is all `NA`
for (i in seq_along(na_dataframes)) {
  print(paste("SurveyId with all NAs in hc57:", na_dataframes[[i]]$SurveyId[1]))
}

# Optionally, display the first few rows of the dataframes where `hc57` is all `NA`
for (df in na_dataframes) {
  print(head(df))
}

# Removing dataframes where `hc57` is missing or all `NA`
na_or_missing_hc57 <- lapply(merged_datasets, function(x) {
  return(!"hc57" %in% names(x) || all(is.na(x$hc57)))
})

# Get the indices of dataframes where `hc57` is missing or all `NA`
na_or_missing_indices <- which(unlist(na_or_missing_hc57))

# Remove the dataframes where `hc57` is missing or all `NA`
filtered_datasets <- merged_datasets[-na_or_missing_indices]

# Verify the removal by printing the SurveyId of the remaining datasets
for (i in seq_along(filtered_datasets)) {
  print(paste("Remaining SurveyId:", filtered_datasets[[i]]$SurveyId[1]))
}
