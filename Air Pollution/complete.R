complete <- function(directory, id = 1:332) {

  data_dir <- paste(dir_0,directory,sep = "/") 
  
  files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
  
  options(stringsAsFactors = FALSE)
  
  Table <- data.frame("id" = integer(), "nobs" = integer())
  
  for (i in id)
  {
    idSelect <- read.csv(files_list[i])
    valid <- complete.cases(idSelect)
    nobs <- nrow(idSelect[valid,])
    Table <- rbind(Table, data.frame("id" = i,"nobs" = nobs))
  }  
  
  Table
}
