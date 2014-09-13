pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  # Data Store
  data<-data.frame()
  
  #Load data into Data Store
  for(file_prefix in id){
    file <- paste(directory,"/",sprintf("%03d",file_prefix),".csv",sep = "")  
    df<-read.csv(file,header = TRUE)
    data<-rbind(data,df)
  }
  
  # Return the mean of a column "pollutant" type
  #colMeans(data[pollutant],na.rm = TRUE, dims = 1)
  mean(as.matrix.data.frame(data[pollutant]),na.rm = TRUE)
}