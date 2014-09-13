complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  # Data Store
  data<-data.frame()
  
  #Load data into Data Store
  for(file_prefix in id){
    file<-paste(directory,"/",sprintf("%03d",file_prefix),".csv",sep = "")  
    df<-read.csv(file,header = TRUE)
  
    #get values
    nobs<-NROW(na.omit(df))
    id<-file_prefix
    
    #set the new row
    newrow<-cbind(id,nobs)  
    
    #insert data
    data<-rbind(data,newrow)
  }
    
  #print result
  data

}