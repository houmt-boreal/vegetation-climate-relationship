rm(list=ls())
gc() 

vip_v <- read.table("D:/griddata_fi/new2018/txt/pls_result/use_after_lc_mask_for_statistics/vip_v_summer_ncomp1_lc_mask.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
vip_loc <- read.table("D:/griddata_fi/new2018/txt/pls_result/use_after_lc_mask_for_statistics/vip_loc_summer_ncomp1_lc_mask.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
 
res=matrix(, nrow = 18, ncol = 37) # nrow = 18  means   there are 18 independent variables; 37=1+18+18
vector=NULL
for (tot in 2:19){
  res[tot-1,1]=tot-1
  
  z=vip_v[,tot] 
  loc=vip_loc[,tot]
  
  # vector=NULL
  # for (ng in 1:3911){
  #   if ( !is.na(loc[ng]) & is.na(z[ng])){
  #     vector <- append(vector, ng)
  #   }
  # }
  vip_ave<--9999 
  vip_freq<--9999
  
  for (i in 1:18){
    vvv=which(loc==i)
    s=length(vvv)
    vip_freq[i]=s
    
    if (s>0){
      a=0
      for (j in 1:s){
        zzz=z[vvv[j]]
        if (is.na(zzz)| (zzz < -9900)){vector <- append(vector, vvv[j])} # vector should be NULL
        else{a=a+zzz}
      }
      vip_ave[i]=a/s
      
    }else{
      vip_ave[i]=-9999
    }
  }
  #resold=cbind(1:24,vip_freq,vip_ave)
  res[,tot]=vip_freq
  res[,tot+18]=vip_ave
}


write.table(res, file="D:/griddata_fi/new2018/txt/pls_result/use_after_lc_mask_for_statistics/vip_statistics_summer_lc_mask.txt",sep=" ", row.names=FALSE, col.names=FALSE)




 