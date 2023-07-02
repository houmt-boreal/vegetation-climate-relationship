 rm(list=ls())
gc() 
library(xlsx)
library(mixOmics)


x_n2 = read.table("D:/r_script_me/pls_tif/x_ncomp_2scal.txt",sep="",fill=FALSE, strip.white=TRUE,header = FALSE)
y_n2 = read.table("D:/r_script_me/pls_tif/y_ncomp_2scal.txt",sep="",fill=FALSE, strip.white=TRUE,header = FALSE)
X<-as.matrix(x_n2) 
Y<-as.matrix(y_n2) 
#,For X & Y, rows corresponding to observations and columns to variables
 
ncomp = 2
pls_res <- pls(X, Y, ncomp = ncomp,mode = "regression")

pls_res.vip <- vip(pls_res)  # every ncomp has its own vip values
R2Y = explained_variance(pls_res$Y, pls_res$variates$X, ncomp =ncomp)
R2X = explained_variance(pls_res$X, pls_res$variates$X, ncomp =ncomp) 

# get R2X
rrrr1<-unname(R2X[1])
rrrr2<-unname(R2X[2])
rrrr3<-unname(R2X[3])

# get coefficients
coefficients<-as.vector(pls_res$mat.c)

# get vip
ttt<-as.vector(pls_res.vip[,2])# for n=2
s<-length(ttt)
vip_v <- vector(mode="numeric", length=s)
vip_loc <- vector(mode="numeric", length=s)
for (ss in 1:s){
  jj<-sort(ttt, TRUE)[ss]
  mm<-sort(ttt, index.return=TRUE)$ix[s-ss+1]  # the location of the 1st vip
  if (jj>1){
    vip_v[ss]<-jj
    vip_loc[ss]<-mm
    }else{
      vip_v[ss]<-NA
      vip_loc[ss]<-NA
    }
}


# for validation-after fitting
tune.pls = perf(pls_res, validation = "Mfold", folds = 5, criterion = "all",
                progressBar = FALSE)

#tune.pls$R2
tune.pls$Q2.total

Q2_1<-tune.pls$Q2.total[1] # Q2 for comp 1
Q2_2<-tune.pls$Q2.total[2] # Q2 for comp 2
Q2_3<-tune.pls$Q2.total[3] # Q2 for comp 3




# # For another X and Y
# # read in the worksheet named mysheet
# # mydata <- read.xlsx("D:\\ppt_June\\s7501.xlsx", sheetName = "Sheet1", 
# #                     header = TRUE,startRow=2)
# 
# mydata <- read.table("D:/r_script_me/pls_tif/fi_ndvi_yearmax.txt")
# dfff = read.table("D:/r_script_me/pls_tif/X_diff.txt",sep=" ",fill=FALSE, strip.white=TRUE,header = TRUE)
# X<-as.matrix(dfff) # from nc_pls.R
# Y<-t(mydata[10,4:37]) # Matrix Transpose ,10 means the 10th point in nc file
# test<-cbind(Y,X)
# # write.table(test, file="D:/test_pls1.txt", row.names=FALSE, col.names=FALSE)

