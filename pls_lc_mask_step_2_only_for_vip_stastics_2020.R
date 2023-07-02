
rm(list=ls())
gc() 

lcmask <- read.table("D:/griddata_fi/new2018/txt/lc_mask_2002_2018.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)

vip_v <- read.table("D:/griddata_fi/new2018/txt/pls_result/vip_v_summer_ncomp1.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
vip_loc <- read.table("D:/griddata_fi/new2018/txt/pls_result/vip_loc_summer_ncomp1.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)

vip_v<-data.matrix(vip_v)
vip_loc<-data.matrix(vip_loc)

vip_v_new <-matrix(, nrow = 7888, ncol = 19) 
vip_loc_new <-matrix(, nrow = 7888, ncol = 19) 

for (i in 1:7888){
  lc<-lcmask[i,1]
  vip_v_new[i,1] <-i
  vip_loc_new[i,1] <-i
  if (lc==1){   # here, lc has only two values, 1 means- non forest
    vip_v_new[i,2:19] <--9999
    vip_loc_new[i,2:19] <--9999}
  else{
    vip_v_new[i,2:19] <-vip_v[i,2:19]
    vip_loc_new[i,2:19] <-vip_loc[i,2:19]
  }
}

write.table(vip_v_new, file="D:/griddata_fi/new2018/txt/pls_result/use_after_lc_mask_for_statistics/vip_v_summer_ncomp1_lc_mask.txt",sep=",",row.names=FALSE, col.names=FALSE)
write.table(vip_loc_new, file="D:/griddata_fi/new2018/txt/pls_result/use_after_lc_mask_for_statistics/vip_loc_summer_ncomp1_lc_mask.txt",sep=",",row.names=FALSE, col.names=FALSE)

