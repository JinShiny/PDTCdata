
#####20211204 PDTC代码

rm(list = ls())
 
library(car)
library(ggplot2)
library(glmnet)
library(MASS)
library(survival)
library(rms)
library(survminer)
library(ggridges)
library(pROC)
library(plotROC)
library(riskRegression)
library(magrittr)
library(DynNom)
library(packrat)
library(rsconnect)

options(stringsAsFactors = FALSE)
setwd("E:\\fron_in_onco")
data <- read.csv("1203111.csv")
str(data)
library(readxl)
data <- read_excel("12031111.xls")
a <- data

Outcome <- "Surv(Survivalmonths,specific)"
CandidateVariables <- c("Age", "Sex", "Race", "T", "N", "M","Surgery","Radiation","Maritalstatus","Sequencenumber","Tumorsize")
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(CandidateVariables, collapse=" + ")))
model.full <- coxph(Formula, data=data,x=TRUE)
model.step <- stepAIC(model.full, direction="both")
summary(model.step)

#单因素分析CSS
cox <- coxph(Surv(Survivalmonths,specific)~N,data)
summary(cox) 

# 绘制单因素KM曲线
setwd("E:\\fron_in_onco\\km图\\km1")
fit <- survfit(Surv(Survivalmonths, specific) ~Race, data = data)
ggsurvplot(fit, # 创建的拟合对象
           data = a,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           risk.table.col = "strata", # 根据分层更改风险表颜色
           xlab = "PDTC CSS(m)", # 指定x轴标签
           ggtheme = theme_bw()) #添加网格线

ddist <- datadist(data)
options(datadist='ddist')

fos <- cph(Surv(Survivalmonths,specific)~Age+T+N+M+Surgery+Radiation
           +Tumorsize ,
           data=data,x=T,y=T,time.inc=1,surv =T)
surv <- Survival(fos)
nomos <- nomogram(fos,
                  fun=list( function(x) surv(5, x), function(x) surv(10, x)),
                  lp=F,
                  funlabel=c("5-year CSS", "10-year CSS"),
                  maxscale=100,fun.at=c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5,
                                        0.4, 0.3,0.2,0.1,0.05))
png("CSS.png",width=10000,height=9200,res=1000)
plot(nomos)
dev.off() 

par(oma = c(3,2,2,2))
data$Survivalyear <- data$Survivalmonths/12

s <- Surv(data$Survivalyear,data$status,type="right")
f <- cph(s~Age+Sex+T+N+M+Surgery+Radiation+Maritalstatus+Tumorsize 
         ,data=data,x=T,y=T,time.inc=10,surv =T)
cal <- calibrate(f,u=10,cmethod="KM",m=200)
png("10OS.png",width=5500,height=4000,res=1000)
plot(cal,xlim=c(0,1),ylim=c(0,1),lwd=2,errbar.col=c(rgb(1,1,1,maxColorValue=255)),
     col=c(rgb(1,1,1,maxColorValue=255)))
abline(0,1,lty=3,lwd=2,col=c(rgb(0,0,0,maxColorValue=255)))
dev.off() 

s <- Surv(data$Survivalyear,data$specific,type="right")
f <- cph(s~Age+T+N+M+Surgery+Radiation+Tumorsize 
         ,data=data,x=T,y=T,time.inc=10,surv =T)
cal <- calibrate(f,u=10,cmethod="KM",m=200)
png("10CSS.png",width=5500,height=4000,res=1000)
plot(cal,xlim=c(0,1),ylim=c(0,1),lwd=2,errbar.col=c(rgb(1,1,1,maxColorValue=255)),
     col=c(rgb(1,1,1,maxColorValue=255)))
abline(0,1,lty=3,lwd=2,col=c(rgb(0,0,0,maxColorValue=255)))
dev.off() 


library(timeROC)
library(survival)
library(tidyverse)

feng_multi_ROC <- timeROC(a$Survivalmonths, a$status,  a$Age,  cause = 1, times = c(60,120), ROC = TRUE)

tmie <- 60
plotROC(Score(list("model.step"=model.step),Surv(Survivalmonths,status)~1,data=data,times=time, plots="roc",metrics=c("AUC")))

data$status<-as.numeric(data$status==1)

#DCA曲线
library(rms)
library(ggDCA)
library(survival) 
PDTC_OS <- coxph(Surv(Survivalmonths,status)~Age+Sex+T+N+M+Surgery+Radiation+Maritalstatus+Tumorsize ,data=a)

Nomogram <- coxph(Surv(Survivalmonths,specific)~Age+T+N+M+Surgery+Radiation+Tumorsize,data=a)
T <- coxph(Surv(Survivalmonths,specific)~T,data=a)
N <- coxph(Surv(Survivalmonths,specific)~N,data=a)
M <- coxph(Surv(Survivalmonths,specific)~M,data=a)

dca2<- dca(Nomogram,T,N,M,times=c(60,120))
ggplot(dca2,
       linetype =F,
       lwd = 1.2)+theme_classic()+  
  
  theme(legend.position=c(0.9,0.8))

dca1<-dca(PDTC_CSS,
          new.data = NULL,
          times=c(60,120))
ggplot(dca1)






















data$Age <- recode(data$Age,"c(1,2,3,4,5,6,7,8,9,10,11)='<65';else='??65'")
data$Year <- recode(data$Year,"c(2004,2005,2006,2007)='2004-2007';else='2008-2015'")
ordered.item  <- c("Married","Divorced/Separated","Single","Widowed")
ordered.item <- factor(1:length(ordered.item),labels = ordered.item)
data$Marital<-factor(data$Marital,levels = levels(ordered.item))
data$SurgPrimSite <- recode(data$SurgPrimSite,"c(0)='No';else='Yes'")

a <- data  ##????????????????????

Outcome <- "Surv(Survivalmonths,Status)"
CandidateVariables <- c("Age","Sex", "Year","Mstage", 
                        "Nstage", "SurgPrimSite", "Radiation", 
                        "Chemotherapy" ,"Sequencenumber","Marital")
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(CandidateVariables, collapse=" + ")))

model.full <- coxph(Formula, data=data,x=TRUE)
model.step <- stepAIC(model.full, direction="both")
summary(model.step)

data1 <- subset(data,data$Specific<2)
Outcome <- "Surv(Survivalmonths,Specific)"
CandidateVariables <- c("Age", "Year","Mstage", 
                        "Nstage", "SurgPrimSite", "Radiation", 
                        "Chemotherapy" ,"Sequencenumber","Marital")
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(CandidateVariables, collapse=" + ")))

model.full <- coxph(Formula, data=data,x=TRUE)
model.step <- stepAIC(model.full, direction="both")
summary(model.step)

a1 <- data1

fit2 <- survfit(Surv(Survivalmonths,Status)~Marital,data=a1,
                conf.type="log-log",type="kaplan-meier")
ggsurvplot(fit2, pval = TRUE)


table<-table(data1$Sequencenumber,data1$Marital)
table
chisq.test(table)

cox <- coxph(Surv(Survivalmonths,Status)~ Sequencenumber ,data1)
summary(cox)
##os??10?۽?????֤
N_folds <- 10   #10?۽?????֤

data$random <- sample(1:nrow(data), nrow(data), replace=F)
data$group <- data$random %% N_folds + 1

table(data$group)

c_harrell <- 0 
c_time <- 0
brier_test <- 0

time <- 36
time <- 60

for (i in 1:N_folds){
  
  data.train <- subset(data,data$group != i)
  
  # any data driven variable transformation or variable selection need to be added in each loop
  
  # define outcome and predictors
  Outcome <- "Surv(Survivalmonths,Status)"
  CandidateVariables <- c("Age","Sex", "Year","Mstage", 
                          "Nstage", "SurgPrimSite", "Radiation", 
                          "Chemotherapy" ,"Sequencenumber","Marital")
  Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                           paste(CandidateVariables, collapse=" + ")))
  
  # fit a model with all candidate varaibles
  
  
  model.full <- coxph(Formula, data=data.train,x=TRUE)
  
  # stepwise selection (as an example, other selection methods can be used as well)
  
  model.train <- stepAIC(model.full, direction="both")
  
  data.test <- subset(data,data$group == i)
  
  data.test$p_lp <- predict(model.train, data.test, type="lp")
  
  
  
  c_harrell[i] <- (cph(Surv(Survivalmonths,Status)~p_lp, data=data.test,x=TRUE,y=TRUE)$stats["Dxy"]+1)/2
  
  c_time[i] <- Score(list("model.step"=model.step),Surv(Survivalmonths,Status)~1,data=data.test,times=time, plots="cal",metrics=c("AUC"))$AUC$score$AUC
  
  brier_test[i] <- Score(list("model.step"=model.step),Surv(Survivalmonths,Status)~1,data=data.test,times=time, plots="cal",metrics=c("Brier"))$Brier$score$Brier[-1]
}

c_harrell
c_time
brier_test

summary(c_harrell)
summary(c_time)
summary(brier_test)

##CSS??10?۽?????֤
a <- data ##????һ??ԭʼ????Ϊa

data <- subset(data,data$Specific<2)

N_folds <- 10   #10?۽?????֤

data$random <- sample(1:nrow(data), nrow(data), replace=F)
data$group <- data$random %% N_folds + 1

table(data$group)

c_harrell <- 0 
c_time <- 0
brier_test <- 0

time <- 36
time <- 60

for (i in 1:N_folds){
  
  data.train <- subset(data,data$group != i)
  
  # any data driven variable transformation or variable selection need to be added in each loop
  
  # define outcome and predictors
  Outcome <- "Surv(Survivalmonths,Status)"
  CandidateVariables <- c("Age", "Year","Mstage", 
                          "Nstage", "SurgPrimSite", "Radiation", 
                          "Chemotherapy" ,"Sequencenumber","Marital")
  Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                           paste(CandidateVariables, collapse=" + ")))
  
  # fit a model with all candidate varaibles
  
  
  model.full <- coxph(Formula, data=data.train,x=TRUE)
  
  # stepwise selection (as an example, other selection methods can be used as well)
  
  model.train <- stepAIC(model.full, direction="both")
  
  data.test <- subset(data,data$group == i)
  
  data.test$p_lp <- predict(model.train, data.test, type="lp")
  
  
  
  c_harrell[i] <- (cph(Surv(Survivalmonths,Status)~p_lp, data=data.test,x=TRUE,y=TRUE)$stats["Dxy"]+1)/2
  
  c_time[i] <- Score(list("model.step"=model.step),Surv(Survivalmonths,Status)~1,data=data.test,times=time, plots="cal",metrics=c("AUC"))$AUC$score$AUC
  
  brier_test[i] <- Score(list("model.step"=model.step),Surv(Survivalmonths,Status)~1,data=data.test,times=time, plots="cal",metrics=c("Brier"))$Brier$score$Brier[-1]
}

c_harrell
c_time
brier_test

summary(c_harrell)
summary(c_time)
summary(brier_test)



ddist <- datadist(data)
options(datadist='ddist')

fos <- cph(Surv(Survivalmonths,Status)~Age+Sex+YearofDiagnosis+Mstage+Nstage
           +SurgPrimSite+Radiation+Sequence+Maritalstatus ,
         data=data,x=T,y=T,time.inc=1,surv =T)
surv <- Survival(fos)
nomos <- nomogram(fos,
                fun=list( function(x) surv(3, x), function(x) surv(5, x)),
                lp=F,
                funlabel=c("3-year OS", "5-year OS"),
                maxscale=100,fun.at=c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5,
                                      0.4, 0.3,0.2,0.1,0.05))
png("OS1112.png",width=5500,height=6200,res=72*10)
plot(nomos)
dev.off() 

data1 <- subset(data,data$Specific<2)

fcss <- cph(Surv(Survivalmonths,Status)~Age+YearofDiagnosis+Mstage+Nstage+
         SurgPrimSite+Radiation+Chemotherapy+Maritalstatus+Sequence
         ,data=data1,x=T,y=T,time.inc=1,surv =T)
surv <- Survival(fcss)
nomcss <- nomogram(fcss,
                fun=list( function(x) surv(3, x), function(x) surv(5, x)),
                lp=F,
                funlabel=c("3-year CSS", "5-year CSS"),
                maxscale=100,fun.at=c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5,
                                      0.4, 0.3,0.2,0.1,0.05))
png("CSS1112.png",width=5500,height=6200,res=72*10)
plot(nomcss)
dev.off()

par(oma = c(3,2,2,2))

s <- Surv(data$Survivalyear,data$Status,type="right")
f <- cph(s~Age+Sex+YearofDiagnosis+Mstage+Nstage+
  SurgPrimSite+Radiation+Maritalstatus+Sequence 
,data=data,x=T,y=T,time.inc=5,surv =T)
cal <- calibrate(f,u=5,cmethod="KM",m=300)
png("5OS1112.png",width=5500,height=4000,res=72*10)
plot(cal,xlim=c(0,1),ylim=c(0,1),lwd=2,errbar.col=c(rgb(1,1,1,maxColorValue=255)),
     col=c(rgb(1,1,1,maxColorValue=255)))
abline(0,1,lty=3,lwd=2,col=c(rgb(0,0,0,maxColorValue=255)))
dev.off() 


s <- Surv(data1$Survivalyear ,data1$Status,type="right")
f <- cph(s~Age+YearofDiagnosis+Mstage+Nstage+
           SurgPrimSite+Radiation+Chemotherapy+Maritalstatus+Sequence
         ,data=data1,x=T,y=T,time.inc=5,surv =T)
cal <- calibrate(f,u=5,cmethod="KM",m=250) #os?˴?m=1800
png("5CSS1112.png",width=5500,height=4000,res=72*10)
plot(cal,xlim=c(0,1),ylim=c(0,1),lwd=2,errbar.col=c(rgb(1,1,1,maxColorValue=255)),
     col=c(rgb(1,1,1,maxColorValue=255)))
abline(0,1,lty=3,lwd=2,col=c(rgb(0,0,0,maxColorValue=255)))
dev.off() 


write.csv(data,"20201006.csv")

