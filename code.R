rm(list = ls())

library(readxl)
data<-read_excel("data.xlsx",sheet="all")


################## 表6：PPS队列人口学信息 ######################################
library(tableone)
shapiro.test(data$Age)  # 0.06
shapiro.test(data$BMI)  # 0.65

vars<-c("Age","Sex","BMI")
factorVars<-c("Sex")
tab<-CreateTableOne(vars=vars,strata="TagA",data=data,factorVars = factorVars)
tab<-print(tab, showAllLevels = TRUE, formatOptions = list(big.mark = ","))



##################  表7_表8：PPS队列疗效评价  ##################################

data$ControlA=ifelse(data$TagA==0, data$AHI1, data$AHI2)
data$TreatAC=ifelse(data$TagA==1, data$AHI1, data$AHI2)

data$ControlO=ifelse(data$TagA==0, data$ODI1, data$ODI2)
data$TreatOC=ifelse(data$TagA==1, data$ODI1, data$ODI2)

data$ControlS=ifelse(data$TagA==0, data$Sleep_min1, data$Sleep_min2)
data$TreatS=ifelse(data$TagA==1, data$Sleep_min1, data$Sleep_min2)

## normality test
shapiro.test(data$ControlA) # 0.07
shapiro.test(data$TreatAC) # 0.0005
shapiro.test(data$ControlO) # 2.17E-5
shapiro.test(data$TreatOC)# 0.044


## AHI
median(data$TreatAC)
IQR(data$TreatAC)
median(data$ControlA)
IQR(data$ControlA)
wilcox.test( as.numeric(data$TreatAC),as.numeric(data$ControlA), paired = T)

## ODI
median(data$TreatOC)
IQR(data$TreatOC)
median(data$ControlO)
IQR(data$ControlO)
wilcox.test( as.numeric(data$TreatOC), as.numeric(data$ControlO),paired = T,exact = FALSE)

## Sleep
median(data$TreatS)
IQR(data$TreatS)
median(data$ControlS)
IQR(data$ControlS)
wilcox.test(as.numeric(data$TreatS), as.numeric(data$ControlS), paired = T,exact = FALSE)


################## 表9：中轻度队列人口学信息 ######################################
dat2<-data[data$ControlA>=5&data$AHI_base<=30,]#中轻度病人

shapiro.test(dat2$Age)  # 0.10
shapiro.test(dat2$BMI)  # 0.40

vars<-c("Age","Sex","BMI")
factorVars<-c("Sex")
tab<-CreateTableOne(vars=vars,strata="TagA",data=dat2,factorVars = factorVars)
tab<-print(tab, showAllLevels = TRUE, formatOptions = list(big.mark = ","))


##################  表10_表11：中轻度队列疗效评价  ##################################

## normality test
shapiro.test(dat2$ControlA) # 0.007
shapiro.test(dat2$TreatAC) # 0.249
shapiro.test(dat2$ControlO) # 3.04E-6
shapiro.test(dat2$TreatOC)# 0.061

## AHI
median(dat2$TreatAC)
IQR(dat2$TreatAC)
median(dat2$ControlA)
IQR(dat2$ControlA)
wilcox.test( as.numeric(dat2$TreatAC), as.numeric(dat2$ControlA),paired = T)

## ODI
median(dat2$TreatOC)
IQR(dat2$TreatOC)
median(dat2$ControlO)
IQR(dat2$ControlO)
wilcox.test( as.numeric(dat2$TreatOC), as.numeric(dat2$ControlO),paired = T,exact = FALSE)




