process_files <- function(grouped_folders_list, categories, base_path, output_base_path, target_cat) {
  cont <- target_cat
  for (sample_name in names(grouped_folders_list)) {
    folders_list <- grouped_folders_list[[sample_name]]
    for (j in seq_along(folders_list)) {
      sample_pred_files <- list.files(path = folders_list[j], pattern = "_samplePredictions.csv$", full.names = TRUE)
      for (i in seq_along(sample_pred_files)) {
        if (cont == target_cat) {
          tryCatch({
            file <- read.csv(sample_pred_files[i])
            message("Processing file: ", sample_pred_files[i])
            
            if (grepl("sample_\\d+_new", sample_pred_files[i])) {
              sample_pattern <- regmatches(sample_pred_files[i], regexpr("sample_\\d+_new", sample_pred_files[i]))
              category <- categories[cont]
              subfolder <- file.path(output_base_path, sample_pattern)
              message("Creating subfolder: ", subfolder)
              if (!dir.exists(subfolder)) {
                dir.create(subfolder, recursive = TRUE)
                message("Subfolder created: ", subfolder)
              }
              
              new_file_path <- file.path(subfolder, paste(sample_name, '_', basename(folders_list[j]), '_', i-1, '.csv', sep = ''))
              
              if (grepl(paste0("_", category, "$"), basename(folders_list[j]))) {
                write.csv(file, new_file_path, row.names = FALSE)
                message("File written to: ", new_file_path)
              } else {
                message("File does not match category: ", sample_pred_files[i])
              }
            } else {
              message("File does not match pattern: ", sample_pred_files[i])
            }
          }, error = function(e) {
            message("Error processing file: ", sample_pred_files[i], " - ", e$message)
          })
        }
      }
    }
  }
}