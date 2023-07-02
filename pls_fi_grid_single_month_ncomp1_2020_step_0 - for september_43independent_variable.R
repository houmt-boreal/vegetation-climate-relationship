rm(list=ls())
gc() 
# http://www.bioconductor.org/packages/release/bioc/html/mixOmics.html
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("mixOmics")

library(mixOmics)
library(pracma) 
#packageVersion("mixOmics")
pre  = read.table("D:/griddata_fi/new2018/txt/pre_2002_2018.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
spei = read.table("D:/griddata_fi/new2018/txt/spei1_2002Jau_2018Dec.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
Tmax = read.table("D:/griddata_fi/new2018/txt/mean_max_temp_2002_2018.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
Tmin = read.table("D:/griddata_fi/new2018/txt/mean_min_temp_2002_2018.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
rad = read.table("D:/griddata_fi/new2018/txt/rad_2002_2018.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)
Tmean = read.table("D:/griddata_fi/new2018/txt/mean_temp_2002_2018.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)

mydata <- read.table("D:/griddata_fi/new2018/txt/mean_ppi_sep_2002_2018.txt",sep=",",fill=FALSE, strip.white=TRUE,header = FALSE)


x_pre=matrix(, nrow = 7, ncol = 17)
x_spei=matrix(, nrow = 7, ncol = 17)
x_tmax=matrix(, nrow = 7, ncol = 17)
x_tmin=matrix(, nrow = 7, ncol = 17) 
x_tmean=matrix(, nrow = 7, ncol = 17)
x_rad=matrix(, nrow = 7, ncol = 17)

plsres<-matrix(, nrow = 7888, ncol = 4) # id + r2x r2y q2
vip_v<-matrix(, nrow = 7888, ncol = 43)  # id + 18 independent variables
vip_loc<-matrix(, nrow = 7888, ncol = 43)
coefficients<-matrix(, nrow = 7888, ncol = 43)


tes = NULL # To check for Q2_1, sum(is.na(tes))
  
#,For X & Y, rows corresponding to observations and columns to variables
for (i in 1:7888){
  Y<-t(mydata[i,1:17])
  plsres[i,1]=i
  vip_v[i,1]=i
  vip_loc[i,1]=i
  coefficients[i,1]=i
  
  for (j in 1:17){
    st<-12*(j-1)+3
    end<-12*(j-1)+9
    x_pre[,j]<-t(pre[i,st:end])
    x_spei[,j]<-t(spei[i,st:end])
    x_tmax[,j]<-t(Tmax[i,st:end])
    x_tmin[,j]<-t(Tmin[i,st:end])
    x_tmean[,j]<-t(Tmean[i,st:end])
    x_rad[,j]<-t(rad[i,st:end])}
    
  
       
    v1<-x_pre[1,] # 1 represents lag -2
    v2<-x_pre[2,] # 2 represents lag -1
    v3<-x_pre[3,] # 3 represents lag 0
    v4<-x_pre[4,] # 1 represents lag -2
    v5<-x_pre[5,]  
    v6<-x_pre[6,]
    v7<-x_pre[7,]  
 
    
    
    
    v8<-x_spei[1,]# for spei,include -Inf values
    v9<-x_spei[2,]
    v10<-x_spei[3,]
    v11<-x_spei[4,]# for spei,include -Inf values
    v12<-x_spei[5,]
    v13<-x_spei[6,]
    v14<-x_spei[7,]#  
   
    
    
    v15<-x_tmax[1,]
    v16<-x_tmax[2,]
    v17<-x_tmax[3,]
    v18<-x_tmax[4,]
    v19<-x_tmax[5,]
    v20<-x_tmax[6,]
    v21<-x_tmax[7,]
 
    
    
    v22<-x_tmin[1,]
    v23<-x_tmin[2,]
    v24<-x_tmin[3,]
    v25<-x_tmin[4,]
    v26<-x_tmin[5,]
    v27<-x_tmin[6,]
    v28<-x_tmin[7,]
 
    
    
    v29<-x_tmean[1,]
    v30<-x_tmean[2,]
    v31<-x_tmean[3,]
    v32<-x_tmean[4,]
    v33<-x_tmean[5,]
    v34<-x_tmean[6,]
    v35<-x_tmean[7,]
 
    v36<-x_rad[1,]
    v37<-x_rad[2,]
    v38<-x_rad[3,]
    v39<-x_rad[4,]
    v40<-x_rad[5,]
    v41<-x_rad[6,]
    v42<-x_rad[7,]
 
    
    X<-cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,
             v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30,v31,v32,v33,v34,v35,v36,
             v37,v38,v39,v40,v41,v42)
    vecy<-as.vector(Y)
    # if (any(is.na(X))|any(is.infinite(X))|any(is.na(Y))|all(vecy == vecy[1])){
    if (any(is.na(X))|any(is.infinite(X))|any(X < -9900)|any(is.na(Y))|any(Y < 0)){  
      plsres[i,2:4]=-9999
      vip_v[i,2:43]=-9999
      vip_loc[i,2:43]=-9999
      coefficients[i,2:43]=-9999
    }else{
    #X<-detrend(X, tt = 'linear')
    #Y<-detrend(Y, tt = 'linear')
    X<-scale(X)
    Y<-scale(Y)
    ncomp = 1 
    pls_res <- pls(X, Y, ncomp = ncomp)
    
    pls_res.vip <- vip(pls_res)  # vip values also are affected by ncomp
    R2Y = explained_variance(pls_res$Y, pls_res$variates$X, ncomp =ncomp)
    R2X = explained_variance(pls_res$X, pls_res$variates$X, ncomp =ncomp) 

    # get R2X
    plsres[i,2]<-unname(R2X[1])

    plsres[i,3]<-unname(R2Y[1])

    
    # for validation-after fitting
    tune.pls = perf(pls_res, validation = "Mfold", folds = 10, progressBar = FALSE)
    
    #tune.pls$R2
    #tune.pls$Q2.total
    
    Q2_1<-tune.pls$Q2.total[1] # Q2 for comp 1

    tes = append(tes,unname(Q2_1))
    plsres[i,4]<-unname(Q2_1)

    
    # get vip
    if (Q2_1>0.0975)
    {ttt<-as.vector(pls_res.vip[,1])# for the 1st component
    s<-length(ttt)
    for (ss in 1:s){
      jj<-sort(ttt, TRUE)[ss]
      mm<-sort(ttt, index.return=TRUE)$ix[s-ss+1]  # the location of the 1st vip
      if (jj>1){  # 1 means vip threshold
        vip_v[i,1+ss]<-jj
        vip_loc[i,1+ss]<-mm
        coefficients[i,1+ss]=as.vector(pls_res$mat.c)[mm]  # only for ncomp=1,sorted by vip in descending order 
      }else{
        coefficients[i,1+ss]<--9999
        vip_v[i,1+ss]<--9999
        vip_loc[i,1+ss]<--9999}
     }
    } else{
      coefficients[i,2:43]<--9999
      vip_v[i,2:43]<--9999
      vip_loc[i,2:43]<--9999}
    }
}
write.table(plsres, file="D:/griddata_fi/new2018/txt/pls_result/sep_lag9/plsres_sep_mean_ncomp1.txt", na = "-9999",sep=",", row.names=FALSE, col.names=FALSE)
write.table(vip_v, file="D:/griddata_fi/new2018/txt/pls_result/sep_lag9/vip_v_sep_ncomp1.txt",na = "-9999",sep=",",  row.names=FALSE, col.names=FALSE)
write.table(vip_loc, file="D:/griddata_fi/new2018/txt/pls_result/sep_lag9/vip_loc_sep_ncomp1.txt",na = "-9999",sep=",",  row.names=FALSE, col.names=FALSE)
write.table(coefficients, file="D:/griddata_fi/new2018/txt/pls_result/sep_lag9/coefficients_sep_ncomp1.txt",na = "-9999",sep=",",  row.names=FALSE, col.names=FALSE)
 
#sum(is.na(tes))

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
