invisible(sapply(list.files(
  "./functions", 
  full.names = TRUE, 
  recursive = T,
  pattern = "*.R"), source))