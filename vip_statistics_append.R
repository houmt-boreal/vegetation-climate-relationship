rm(list=ls())
gc() 
library(ncdf4)
library(raster) 
#mydata <- read.table("D:/r_script_me/pls_tif/fi_ndvi_yearmax.txt")
vip_v <- read.table("D:/mvc_bimonth_raw_3067/detrend/vip_v_jja_mean_ncomp1.txt",fill=FALSE, strip.white=TRUE,header = TRUE)
vip_loc <- read.table("D:/mvc_bimonth_raw_3067/detrend/vip_loc_jja_mean_ncomp1.txt",fill=FALSE, strip.white=TRUE,header = TRUE)
res=matrix(, nrow = 6, ncol = 13) # nrow = 6  means   there are six independent variables
vector=NULL
for (tot in 2:7){
  res[tot-1,1]=tot-1
  
  z=vip_v[,tot] 
  loc=vip_loc[,tot]
  
  # vector=NULL
  # for (ng in 1:3911){
  #   if ( !is.na(loc[ng]) & is.na(z[ng])){
  #     vector <- append(vector, ng)
  #   }
  # }
  vip_ave=NULL  
  vip_freq=NULL
  
  for (i in 1:6){
    vvv=which(loc==i)
    s=length(vvv)
    vip_freq[i]=s
    
    if (s>0){
      a=0
      for (j in 1:s){
        zzz=z[vvv[j]]
        if (is.na(zzz)){vector <- append(vector, vvv[j])} # vector should be NULL
        else{a=a+zzz}
      }
      vip_ave[i]=a/s
      
    }else{
      vip_ave[i]=NA
    }
  }
  #resold=cbind(1:24,vip_freq,vip_ave)
  res[,tot]=vip_freq
  res[,tot+6]=vip_ave
}


write.table(res, file="D:/mvc_bimonth_raw_3067/detrend/vip_statistics_jja.txt", row.names=FALSE, col.names=TRUE)




 