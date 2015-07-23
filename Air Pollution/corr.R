corr <- function(directory, threshold = 0) {

  data_dir <- paste(dir_0,directory,sep = "/") 
  
  files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
  
  options(stringsAsFactors = FALSE)
  
  Table <- data.frame()
  
  for (i in 1:332){
    idSelect <- read.csv(files_list[i])
    valid <- complete.cases(idSelect)
    nobs <- nrow(idSelect[valid,])
    good <- idSelect[valid,]
    if (nobs > threshold){
      x <- cor(good[,"sulfate"], good[,"nitrate"])
      Table <- rbind(Table, data.frame(x))
    }
  }
  Vector <- unlist(Table)
}
      


  
  
  
  
  
  
  
  
  
