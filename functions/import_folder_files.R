import_folder_files <- function(file_pattern = '*.csv', 
                          verbose = TRUE) {
  # Get the list of files
  files <- list.files(pattern = file_pattern)
  
  # Initialize an empty list to store data frames
  df_list <- list()
  
  for (file in files) {
    df <- read_csv(file, col_names = c("soc", "power")) %>%
      mutate(file_name = sub("\\.csv$", "", file)) %>%
      separate(file_name, into = c("model", "pack_energy", "notes"), sep = "_", extra = "merge")
    
    # Add file_name column
    
    df_list[[file]] <- df  # Store in the list
  }
  
  # Combine all data frames into one
  profiles <- bind_rows(df_list)

  return(profiles)
}





