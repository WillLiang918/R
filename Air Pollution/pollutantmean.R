pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  data_dir <- paste(dir_0,directory,sep = "/") 
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
  tables <- lapply(files_list,read.csv)
  my.data <- do.call(rbind, tables)
  my.data.df <- data.frame(my.data)
  x <- my.data.df[my.data.df$ID %in% c(id), ]
  myVect <- x[,pollutant]                          
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  result <- mean(myVect, na.rm = TRUE)
  result
}

