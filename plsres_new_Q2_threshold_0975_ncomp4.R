rm(list=ls())
gc() 
library(ncdf4)
library(raster)
#mydata <- read.table("D:/r_script_me/pls_tif/fi_ndvi_yearmax.txt")
plsres <- read.table("D:/griddata_fi/mon_hou/plsres_season.txt",fill=FALSE, strip.white=TRUE,header = TRUE)
# x=mydata[,2]
#sum(is.na(plsres))
#sum(is.infinite(as.matrix(plsres)))
new<-matrix(, nrow = 3911, ncol = 13)

for (i in 1:3911){
  new[i,1]=i
 
  Q2_1<-plsres[i,10]
  Q2_2<-plsres[i,11]
  Q2_3<-plsres[i,12]
  Q2_4<-plsres[i,13]
  
  if (is.na(Q2_1)|is.na(Q2_2)|is.na(Q2_3)|is.na(Q2_4)){
    new[i,2]<-NA
    new[i,3]<-NA
    new[i,4]<-NA
    new[i,5]<-NA
    
    new[i,6]<-NA
    new[i,7]<-NA
    new[i,8]<-NA
    new[i,9]<-NA
    
    new[i,10]<-NA
    new[i,11]<-NA
    new[i,12]<-NA
    new[i,13]<-NA
  
  
  
  }else if (Q2_1>0.0975 & Q2_2>0.0975 & Q2_3>0.0975 & Q2_4>0.0975){
    new[i,2]<-plsres[i,2]# R2X[1]
    new[i,3]<-plsres[i,3]
    new[i,4]<-plsres[i,4]
    new[i,5]<-plsres[i,5]
    
    new[i,6]<-plsres[i,6]# R2Y[1]
    new[i,7]<-plsres[i,7]# R2Y[2]
    new[i,8]<-plsres[i,8]
    new[i,9]<-plsres[i,9]
    
    new[i,10]<-plsres[i,10] #Q2_1
    new[i,11]<-plsres[i,11]
    new[i,12]<-plsres[i,12]
    new[i,13]<-plsres[i,13]
    
  }else if(Q2_1>0.0975 & Q2_2>0.0975 & Q2_3>0.0975 & Q2_4<=0.0975){
    new[i,2]<-plsres[i,2]# R2X[1]
    new[i,3]<-plsres[i,3]
    new[i,4]<-plsres[i,4]
    new[i,5]<-NA
    
    new[i,6]<-plsres[i,6]# R2Y[1]
    new[i,7]<-plsres[i,7]
    new[i,8]<-plsres[i,8]
    new[i,9]<-NA
    
    new[i,10]<-plsres[i,10] #Q2_1
    new[i,11]<-plsres[i,11]
    new[i,12]<-plsres[i,12]
    new[i,13]<-NA
    
  }  else if (Q2_1>0.0975 & Q2_2>0.0975 & Q2_3<=0.0975){
    new[i,2]<-plsres[i,2]# R2X[1]
    new[i,3]<-plsres[i,3]
    new[i,4]<-NA
    new[i,5]<-NA
    
    new[i,6]<-plsres[i,6]# R2Y[1]
    new[i,7]<-plsres[i,7]
    new[i,8]<-NA
    new[i,9]<-NA
    
    new[i,10]<-plsres[i,10] #Q2_1
    new[i,11]<-plsres[i,11]
    new[i,12]<-NA
    new[i,13]<-NA
    
  }else if (Q2_1>0.0975 & Q2_2<=0.0975){
    new[i,2]<-plsres[i,2]# R2X[1]
    new[i,3]<-NA
    new[i,4]<-NA
    new[i,5]<-NA
    
    new[i,6]<-plsres[i,6]# R2Y[1]
    new[i,7]<-NA
    new[i,8]<-NA
    new[i,9]<-NA
    
    new[i,10]<-plsres[i,10] #Q2_1
    new[i,11]<-NA
    new[i,12]<-NA
    new[i,13]<-NA
    
  }else{
    new[i,2]<-NA
    new[i,3]<-NA
    new[i,4]<-NA
    new[i,5]<-NA
    
    new[i,6]<-NA
    new[i,7]<-NA
    new[i,8]<-NA
    new[i,9]<-NA
    
    new[i,10]<-NA
    new[i,11]<-NA
    new[i,12]<-NA
    new[i,13]<-NA}
}
write.table(new, file="D:/plsres_new_0975_NA.txt", row.names=FALSE, col.names=TRUE)

  
  
  
 
  