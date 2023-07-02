rm(list=ls())
gc() 
library(mixOmics)
#packageDescription("mixOmics")["Version"]
#remove.packages("mixOmics")

#mydata <- read.xlsx("D:\\ppt_June\\s7501.xlsx", 1)

# read in the worksheet named mysheet
# mydata <- read.xlsx("D:\\ppt_June\\s7501.xlsx", sheetName = "Sheet1", 
#                     header = TRUE,startRow=2)

mydata <- read.table("D:/mvc_bimonth_raw_3067/from_10km/mean/mam_mean.txt",sep=" ",fill=FALSE, strip.white=TRUE,header = TRUE)
pre  = read.table("D:/griddata_fi/mon_hou/txt_pls_season/pre_season.txt",sep=" ",fill=FALSE, strip.white=TRUE,header = TRUE)
spei = read.table("D:/griddata_fi/mon_hou/txt_pls_season/spei3_season.txt",sep=" ",fill=FALSE, strip.white=TRUE,header = TRUE)
Tmax = read.table("D:/griddata_fi/mon_hou/txt_pls_season/Tmax_season.txt",sep=" ",fill=FALSE, strip.white=TRUE,header = TRUE)
Tmin = read.table("D:/griddata_fi/mon_hou/txt_pls_season/Tmin_season.txt",sep=" ",fill=FALSE, strip.white=TRUE,header = TRUE)
rad = read.table("D:/griddata_fi/mon_hou/txt_pls_season/rad_season.txt",sep=" ",fill=FALSE, strip.white=TRUE,header = TRUE)
Tmean = read.table("D:/griddata_fi/mon_hou/txt_pls_season/Tmean_season.txt",sep=" ",fill=FALSE, strip.white=TRUE,header = TRUE)
#sum(is.infinite(as.matrix(spei)))

x_pre=matrix(, nrow = 4, ncol = 34)
x_spei=matrix(, nrow = 4, ncol = 34)
x_tmax=matrix(, nrow = 4, ncol = 34)
x_tmin=matrix(, nrow = 4, ncol = 34) 
x_tmean=matrix(, nrow = 4, ncol = 34)
x_rad=matrix(, nrow = 4, ncol = 34)

plsres<-matrix(, nrow = 3911, ncol = 13)
vip_v<-matrix(, nrow = 3911, ncol = 7)
vip_loc<-matrix(, nrow = 3911, ncol = 7)

  
#,For X & Y, rows corresponding to observations and columns to variables
for (i in 1:3911){
  Y<-t(mydata[i,4:37])
  plsres[i,1]=i
  vip_v[i,1]=i
  vip_loc[i,1]=i
  for (j in 1:34){
    st<-4*(j-1)+1
    end<-4*(j-1)+4
    x_pre[,j]<-t(pre[i,st:end])
    x_spei[,j]<-t(spei[i,st:end])
    x_tmax[,j]<-t(Tmax[i,st:end])
    x_tmin[,j]<-t(Tmin[i,st:end])
    x_tmean[,j]<-t(Tmean[i,st:end])
    x_rad[,j]<-t(rad[i,st:end])}
    
  
     
    v1<-x_pre[2,]  # 2 represents climate values in MAM 
    v2<-x_spei[2,]# for spei,include -Inf values
    v3<-x_tmax[2,]
    v4<-x_tmin[2,]
    v5<-x_tmean[2,]
    v6<-x_rad[2,]
    
    X<-cbind(v1,v2,v3,v4,v5,v6)
    vecy<-as.vector(Y)
    
    if (any(is.na(X))|any(is.infinite(X))|any(is.na(Y))|all(vecy == vecy[1])){
      plsres[i,2:13]=NA
      vip_v[i,2:7]=NA
      vip_loc[i,2:7]=NA
    }else{
    ncomp = 4 
    pls_res <- pls(X, Y, ncomp = ncomp)
    
    pls_res.vip <- vip(pls_res)  # vip values also are affected by ncomp
    R2Y = explained_variance(pls_res$Y, pls_res$variates$X, ncomp =ncomp)
    R2X = explained_variance(pls_res$X, pls_res$variates$X, ncomp =ncomp) 

    # get R2X
    plsres[i,2]<-unname(R2X[1])
    plsres[i,3]<-unname(R2X[2])
    plsres[i,4]<-unname(R2X[3])
    plsres[i,5]<-unname(R2X[4])
    
    plsres[i,6]<-unname(R2Y[1])
    plsres[i,7]<-unname(R2Y[2])
    plsres[i,8]<-unname(R2Y[3])
    plsres[i,9]<-unname(R2Y[4])
    
    #
    #
    # # for validation-after fitting
    tune.pls = perf(pls_res, validation = "Mfold", folds = 5, criterion = "all",
                    progressBar = FALSE)

    # #tune.pls$R2
    # #tune.pls$Q2.total

    Q2_1<-tune.pls$Q2.total[1] # Q2 for comp 1
    Q2_2<-tune.pls$Q2.total[2] # Q2 for comp 2
    Q2_3<-tune.pls$Q2.total[3] # Q2 for comp 3
    Q2_4<-tune.pls$Q2.total[4]

    plsres[i,10]<-unname(Q2_1)
    plsres[i,11]<-unname(Q2_2)
    plsres[i,12]<-unname(Q2_3)
    plsres[i,13]<-unname(Q2_4)

    # get vip
    if (Q2_1>0.0975 & Q2_2>0.0975 & Q2_3>0.0975 & Q2_4>0.0975)
    {ttt<-as.vector(pls_res.vip[,4])# for the 2nd component
    s<-length(ttt)
    for (ss in 1:s){
      jj<-sort(ttt, TRUE)[ss]
      mm<-sort(ttt, index.return=TRUE)$ix[s-ss+1]  # the location of the 1st vip
      if (jj>1){
        vip_v[i,1+ss]<-jj
        vip_loc[i,1+ss]<-mm

      }else{
        vip_v[i,1+ss]<-NA
        vip_loc[i,1+ss]<-NA}
     }
    }

    else if (Q2_1>0.0975 & Q2_2>0.0975 & Q2_3>0.0975 & Q2_4<=0.0975)
    {ttt<-as.vector(pls_res.vip[,3])# for the 2nd component
      s<-length(ttt)
      for (ss in 1:s){
        jj<-sort(ttt, TRUE)[ss]
        mm<-sort(ttt, index.return=TRUE)$ix[s-ss+1]  # the location of the 1st vip
        if (jj>1){
          vip_v[i,1+ss]<-jj
          vip_loc[i,1+ss]<-mm

        }else{
          vip_v[i,1+ss]<-NA
          vip_loc[i,1+ss]<-NA}
      }
    }



    else if (Q2_1>0.0975 & Q2_2>0.0975 & Q2_3<=0.0975)
    {ttt<-as.vector(pls_res.vip[,2])# for the 2nd component
      s<-length(ttt)
      for (ss in 1:s){
        jj<-sort(ttt, TRUE)[ss]
        mm<-sort(ttt, index.return=TRUE)$ix[s-ss+1]  # the location of the 1st vip
        if (jj>1){
          vip_v[i,1+ss]<-jj
          vip_loc[i,1+ss]<-mm

        }else{
          vip_v[i,1+ss]<-NA
          vip_loc[i,1+ss]<-NA}
      }
    }


    else if (Q2_1>0.0975 & Q2_2<=0.0975)
    {ttt<-as.vector(pls_res.vip[,1])# for the 2nd component
      s<-length(ttt)
      for (ss in 1:s){
        jj<-sort(ttt, TRUE)[ss]
        mm<-sort(ttt, index.return=TRUE)$ix[s-ss+1]  # the location of the 1st vip
        if (jj>1){
          vip_v[i,1+ss]<-jj
          vip_loc[i,1+ss]<-mm

        }else{
          vip_v[i,1+ss]<-NA
          vip_loc[i,1+ss]<-NA}
      }
    }
    else
    {vip_v[i,2:7]<-NA
    vip_loc[i,2:7]<-NA}
    }
}
write.table(plsres, file="D:/plsres_mam_comp4.txt", row.names=FALSE, col.names=TRUE)
write.table(vip_v, file="D:/vip_v_mam_comp4.txt", row.names=FALSE, col.names=TRUE)
write.table(vip_loc, file="D:/vip_loc_mam_comp4.txt", row.names=FALSE, col.names=TRUE)

#pixel_no_use_PLS<-(sum(is.na(plsres)))/12

# X<-dfff[21:54,] # from nc_pls.R
#  # Matrix Transpose ,10 means the 10th point in nc file
# #Y<-as.matrix(Y1)
##write.table(X,"D:/xs396.txt") 
##write.table(Y,"D:/ys396.txt") 
# ncomp = 1 
# pls_res <- pls(X, Y, ncomp = ncomp)
# 
# pls_res.vip <- vip(pls_res)  # vip values also are affected by ncomp
# R2Y = explained_variance(pls_res$Y, pls_res$variates$X, ncomp =ncomp)
# R2X = explained_variance(pls_res$X, pls_res$variates$X, ncomp =ncomp) 
# 
# ttt<-as.vector(pls_res.vip)
# s<-length(ttt)
# jj1<-sort(ttt, TRUE)[1]
# jj2<-sort(ttt, TRUE)[2]
# mm1<-sort(ttt, index.return=TRUE)$ix[s]  # the location of the 1st vip
# mm2<-sort(ttt, index.return=TRUE)$ix[s-1]
# res[i,1]=
#   res[i,2]=
#   res[i,3]=
#   res[i,4]=unname(R2X)
# res[i,5]=unname(R2Y)
# res[i,6]=jj1
# res[i,7]=mm1
# 
# # for validation-after fitting
# tune.pls = perf(pls_res, validation = "Mfold", folds = 5, criterion = "all",
#                 progressBar = FALSE)
# tune.pls$Q2.total[1] # Q2 for comp 1
# 
# #tune.pls$R2
# tune.pls$Q2
