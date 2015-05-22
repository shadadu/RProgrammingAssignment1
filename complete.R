complete<- function(directory, id=1:332){
  
  id_max=332                             # Default for the last possible file so the default for the 
  idm=id[length(id)]                     # maximum number of characters in the file name can be set
  id_max_length = nchar(id_max)
  nobs<- vector("numeric")
  
  
  for(i in seq_along(id)){
    
    id_length = nchar(id[i])
    
    file_id = as.character(id[i])
    j=0                                   # Concatenates the id vector elements with extra 0's 
    while(j< id_max_length - id_length){  # to give the three character file names. For instance,
                                          # 1 becomes "001" for calling the file "001.csv", but
      file_id = paste0("0",file_id)       # 200 becomes "200.csv"
      j=j+1
    }
    
    
    file_id=paste0(directory,"/",file_id,".csv")
    nLines = length(readLines(file_id))   # counts the number of rows of the file, excluding the header
    
    in_data=read.table(file_id,header=TRUE,sep=",") 
    
    
    x<-in_data[,"sulfate"]              # x and y temporary variables to hold the sulfate and nitrate 
    y<-in_data[,"nitrate"]              # observed values
    
    n=0
    for(k in 1:nLines){                 # Goes through the rows of the file, line by line
      
      if(complete.cases(in_data[k,"sulfate"])==TRUE & complete.cases(in_data[k,"nitrate"])==TRUE){  
                                        # this if conditional is to include only                          
        n = n+1                         # the rows for which both sulfate and                           
      }                                 # nitrate have valid observation (not NA)               
      nobs[i]=n 
      
    }
        
    
  }
  
  df<-data.frame(id, nobs)
  df
  
}



