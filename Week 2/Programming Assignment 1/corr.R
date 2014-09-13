corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Get files names
  files<-list.files(directory, pattern = "^[0-9]{3}.csv")
  #For debug
  #files<-list.files(directory, pattern = "^[0..9]{3}.csv")
  
  # Data Store
  output<-numeric()
  
  #Load data into Data Store
  for(file in files){
    file<-paste(directory,"/",file,sep = "")  
    df<-read.csv(file,header = TRUE)
    
    #get values
    nobs<-NROW(na.omit(df))
    
    if(nobs>threshold){
      #Calculate Correlation for complete observatios
      cor_nitrate_sulfate<-cor(df$nitrate,df$sulfate,use = "complete.obs")
      
      #Bind results
      output<-c(output,cor_nitrate_sulfate)
    }
    
  }
  # Return output
  as.numeric(output)
  
}