pollutantmean <- function(directory, pollutant, id=1:332){
  
  id_max=332
  idm=id[length(id)]
  id_max_length = nchar(id_max)
  mean_pollutant_array<-vector("numeric")
  nobs<-vector("numeric")
  
  n=1
  for(i in seq_along(id)){
    id_length = nchar(id[i])
    file_id = as.character(id[i])
    nmax = id_max_length - id_length
    j=0
    while(j<nmax){                  ## Concatenation of strings to obtain the
                                    ## 3-character sequence of file names, e.g. "001.csv"  
      file_id = paste0("0",file_id)
      j=j+1
    }
   
    file_id=paste0(directory,"/",file_id,".csv")
    in_data=read.table(file_id,header=TRUE,sep=",")
    pollute=in_data[pollutant]
    x<-pollute[,pollutant]
    x<-x[complete.cases(x)]           ## Removes rows with NA or invalid entries
    mean_here=mean(x)
    
    if(complete.cases(mean_here)==TRUE){    ## Include only monitor ID's with at least
                                            ## one valid entry. Those with entirely invalid
      nobs[n]=length(x)                     ## entries will give mean value NaN
      mean_pollutant_array[n]=mean_here
      n = n+1
    }
    
  }
  mean_pollutant = sum(nobs*mean_pollutant_array)/sum(nobs)
   
  return(mean_pollutant)
  
}
