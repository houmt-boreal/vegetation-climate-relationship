rm(list=ls())
gc() 

dat_ao <- read.csv("D:/griddata_fi/new2018/NAO/monthly.ao.index.b50.current.ascii", sep="",header = FALSE, skip = 624)
dat_nao <- read.csv("D:/griddata_fi/new2018/NAO/norm.nao.monthly.b5001.current.ascii", sep="",header = FALSE, skip = 624)

ce<-unname(t(dat_ao))
ce2<-unname(t(dat_nao))

ao_may<-ce[3,seq(from=5,to=204,by=12)] 
nao_may<-ce2[3,seq(from=5,to=204,by=12)]
