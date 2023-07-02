rm(list=ls())
gc() 
# library(ncdf4)
# library(raster)
#mydata <- read.table("D:/r_script_me/pls_tif/fi_ndvi_yearmax.txt")
plsres <- read.table("D:/griddata_fi/new2018/txt/pls_result/plsres_jun_mean_ncomp1.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
# x=mydata[,2]
#sum(is.na(plsres))
#sum(is.infinite(as.matrix(plsres)))
new<-matrix(, nrow = 7888, ncol = 4)

for (i in 1:7888){
  new[i,1]=i
 
  Q2_1<-plsres[i,4]
  
  if (is.na(Q2_1)|(Q2_1 < -9900)){
    new[i,2]<--9999
    new[i,3]<--9999
    new[i,4]<--9999

  }else if (Q2_1>0.0975){
    new[i,2]<-plsres[i,2]# R2X[1]
    new[i,3]<-plsres[i,3]# R2Y[1]
    new[i,4]<-plsres[i,4]#Q2_1
    
  }else{
    new[i,2]<--9999
    new[i,3]<--9999
    new[i,4]<--9999
  }
}
write.table(new, file="D:/griddata_fi/new2018/txt/pls_result/use_Q2_0975/pls_use_jun_ncomp1_Q2_0975.txt",na = "-9999",sep=",",  row.names=FALSE, col.names=FALSE)

  
  
  
 
  