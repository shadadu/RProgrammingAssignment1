corr <- function(directory, threshold = 0){
  id=1:332
  id_max=id[length(id)]
  id_max_length = nchar(id_max)
  crrl<-c(rep(NA,id_max))
  
  n=0
  for(i in id){                       
    id_length = nchar(id[i])          
    file_id = as.character(id[i])
    
    j=0
    while(j< id_max_length - id_length){ ## Concatenates with "0"s to give the 3-character
      file_id = paste0("0",file_id)      ## file names such as "015.csv"
      j=j+1
    }
    
    file_id=paste0(directory,"/",file_id,".csv")
    in_data=read.table(file_id,header=TRUE,sep=",")
    pollute_matrix<- in_data[,"sulfate"]
    pollute_matrix<-cbind(pollute_matrix,in_data[,"nitrate"])     
    x<-in_data[,"sulfate"]
    y<-in_data[,"nitrate"]
    
    ##compute correlations for id's that have the minimum number of valid entries
    if(length(x[complete.cases(x)]) >threshold & length(y[complete.cases(y)]) >threshold){
      
      pollute_matrix=pollute_matrix[complete.cases(pollute_matrix),]
      crrl[i]<-cor(pollute_matrix[,1],pollute_matrix[,2])   
      
    } 
    
  }
  crrl = crrl[complete.cases(crrl)]
  
  crrl
   
}
