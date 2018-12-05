require('car')


backupData <- read.csv("~/regression/laptopPrice.csv", na.strings="?")
View(backupData)
laptopPrice<-backupData

###############
#Preprocessing#
###############
##Handling missing values
sum(is.na(laptopPrice))
#146
sum(is.na(laptopPrice$Max.Horizontal.Resolution))
#69, left 77
sum(is.na(laptopPrice$Memory.Technology))
#63, left 14
sum(is.na(laptopPrice$Installed.Memory))
#12, left 2
sum(is.na(laptopPrice$Processor.Speed))
#2, left 0

completeRows<-laptopPrice[!as.logical(apply(is.na(laptopPrice),1,sum)),]
incompleteRows<-laptopPrice[as.logical(apply(is.na(laptopPrice),1,sum)),]

##one loop to impute all missing values
for(k in 1:length(incompleteRows[,1])){
  test<-incompleteRows[k,]
  x1=completeRows
  for(i in c(2,5:15)){
    if(!is.na(test[i])){
      x1<-x1[which(x1[,i]==as.character(test[[i]])),]
    }
  }
  if(length(x1[,1])>1){
    min1<-sum((x1[1,c(1,3,4,16,17)]-test[c(1,3,4,16,17)])^2,na.rm=T)
    min2<-sum((x1[2,c(1,3,4,16,17)]-test[c(1,3,4,16,17)])^2,na.rm=T)
    min1.i=1
    min2.i=2
    for(i in 2:length(x1[,1])){
      minC<-sum((x1[i,c(1,3,4,16,17)]-test[c(1,3,4,16,17)])^2,na.rm=T)
      if(minC<min1){
        min2=min1
        min1=minC
        min2.i=min1.i
        min1.i=i
      }
      if(minC<min2 & minC>min1){
        min2=minC
        min2.i=i
      }
    }
    for(i in c(1,3,4,16,17)){
      if(is.na(test[i])){
        test[i]=(x1[min1.i,i]+x1[min2.i,i])/2
      }
    }
    for(i in c(2,5:15)){
      if(is.na(test[i])){
        test[i]=x1[min1.i,i]
      }
    }
  }else if(length(x1[,1])==1){
    for(i in 1:17){
      if(is.na(test[i])){
        test[i]=x1[1,i]
      }
    }
  }else{
    x1<-completeRows
    min1<-sum((x1[1,c(1,3,4,16,17)]-test[c(1,3,4,16,17)])^2,na.rm=T)
    min1.i=1
    for(i in 2:length(x1[,1])){
      minC<-sum((x1[i,c(1,3,4,16,17)]-test[c(1,3,4,16,17)])^2,na.rm=T)
      if(minC<min1){
        min1=minC
        min1.i=i
      }
    }
    for(i in 1:17){
      if(is.na(test[i])){
        test[i]=x1[min1.i,i]
      }    
    }
  }
  incompleteRows[k,]<-test
}

##missing values handled
laptopPrice<-rbind(completeRows,incompleteRows)
sum(is.na(laptopPrice))
#0 means no more NA in dataset


onehot.Enc<-function(x,namex){
  n=length(x)
  types=unique(x)
  zeros<-rep(0,n)
  for(i in 1:(length(types)-1)){
    initZ<-rep(0,n)
    initZ[which(x==types[i])]<-1
    zeros=cbind(zeros,initZ)
    colnames(zeros)[i+1]<-paste(namex,i,sep = "")
  }
  return(zeros[,-1])
}

set.seed(1)
laptopPrice<-laptopPrice[sample(nrow(laptopPrice)),]
laptopPrice2<-laptopPrice
RAM.T<-onehot.Enc(laptopPrice$Memory.Technology,"RAM.Type.")
Processor.T<-onehot.Enc(laptopPrice$Processor,"Processor.")
Manufact.T<-onehot.Enc(laptopPrice$Manufacturer,"Manufacturer.")
OS.T<-onehot.Enc(laptopPrice$Operating.System,"OS.")
laptopPrice=cbind("MaxRes"=laptopPrice[,1],RAM.T,laptopPrice[,3:4],Processor.T,Manufact.T,laptopPrice[,7:14],OS.T,laptopPrice[,16:17])
laptopPrice$Infrared<-as.numeric(laptopPrice$Infrared=="YES")
laptopPrice$Bluetooth<-as.numeric(laptopPrice$Bluetooth=="YES")
laptopPrice$Docking.Station<-as.numeric(laptopPrice$Docking.Station=="YES")
laptopPrice$Port.Replicator<-as.numeric(laptopPrice$Port.Replicator=="YES")
laptopPrice$Fingerprint<-as.numeric(laptopPrice$Fingerprint=="YES")
laptopPrice$Subwoofer<-as.numeric(laptopPrice$Subwoofer=="YES")
laptopPrice$External.Battery<-as.numeric(laptopPrice$External.Battery=="YES")
laptopPrice$CDMA<-as.numeric(laptopPrice$CDMA=="YES")
View(laptopPrice)

##############
#Partitioning#
##############
total.size=length(laptopPrice[,1])
set.seed(1)
train.index<-sample(1:total.size, 0.9*total.size)
train.set<-laptopPrice[train.index,]
test.set<-laptopPrice[-train.index,]

##############
#ModelFitting#
##############
lmodel1<-lm(train.set$Price~.,data = train.set, na.action = na.omit)
summary(lmodel1)
#coefficients of OS.6 is NA, multicollinearity seems to be the culprit 
vif(lmodel1)
alias(lmodel1)
#OS.6 is perfectly dependent on other regressors, dropped

lmodel2<-lm(train.set$Price~.-OS.6,data = train.set, na.action = na.omit)
vif(lmodel2)
max(vif(lmodel2))
#VIF for Manufacturer.4 is 144.784, dropped

lmodel3<-lm(train.set$Price~.-OS.6-Manufacturer.4,data = train.set, na.action = na.omit)
vif(lmodel3)
max(vif(lmodel3))
#VIF for Processor.2 is 32.16267, dropped

lmodel4<-lm(train.set$Price~.-OS.6-Manufacturer.4-Processor.2,
            data = train.set, na.action = na.omit)
vif(lmodel4)
max(vif(lmodel4))
#VIF for RAM.Type.1 is 27.81833, dropped

lmodel5<-lm(train.set$Price~.-OS.6-Manufacturer.4-Processor.2-RAM.Type.1,
            data = train.set, na.action = na.omit)
vif(lmodel5)
max(vif(lmodel5))
#VIF for OS.1 is 19.09984, dropped

lmodel6<-lm(train.set$Price~.-OS.6-Manufacturer.4-Processor.2-RAM.Type.1-OS.1,
            data = train.set, na.action = na.omit)
vif(lmodel6)
max(vif(lmodel6))
#VIF for Docking.Station is 7.124568, dropped

lmodel7<-lm(train.set$Price~.-OS.6-Manufacturer.4-Processor.2-RAM.Type.1-OS.1-Docking.Station,
            data = train.set, na.action = na.omit)
vif(lmodel7)
max(vif(lmodel7))
#VIF for all variables less than 3, model doesnt have any more multicollinearity
#final model: Price~MaxRes + RAM.Type.2 + Installed.Memory + Processor.Speed 
#                   + Processor.1 + Processor.3 + Processor.4 + Processor.5 + Processor.6
#                   + Processor.7 + Manufacturer.1 + Manufacturer.2 + Manufacturer.3 
#                   + Manufacturer.5 + Manufacturer.6 + Manufacturer.7 + Manufacturer.8 
#                   + Manufacturer.9 + Manufacturer.10 + Manufacturer.11 + Infrared 
#                   + Bluetooth + Port.Replicator + Fingerprint + Subwoofer External.Battery 
#                   + CDMA + OS.2 + OS.3 + OS.4 + OS.5 + Warranty.Days

#model selection
lmodel8<-step(lmodel7,direction = "both")
summary(lmodel8)

#removing insignificant variables
lmodel9<-lm(formula = train.set$Price ~ MaxRes + Installed.Memory + 
              Processor.Speed + Processor.1 + Processor.4 + 
              Processor.7 + Manufacturer.1 + 
              Manufacturer.2 + Manufacturer.3 + Manufacturer.5 + Manufacturer.7 + 
              Manufacturer.9 + Manufacturer.10 + Manufacturer.11 + Infrared + 
              Bluetooth + Port.Replicator + Subwoofer + 
              CDMA + OS.2 + OS.3 + OS.4 + OS.5 + Warranty.Days, data = train.set, 
            na.action = na.omit)
summary(lmodel9)

###############
#ModelAdequecy#
###############

r=rstudent(lmodel9)
predictions<-predict.lm(lmodel9,train.set)
plot(predictions, r,xlab = "Predictions", ylab = "Residuals", main = "Residual Plot", pch=20)
plot(cooks.distance(lmodel9), pch=20, ylab = "Cook's Distance")
qqnorm(lmodel9$residuals, pch=20)
qqline(lmodel9$residuals, pch=20)
hist(lmodel9$residuals, xlab = "Residuals")


#############
#Predictions#
#############
test.predictions<-predict.lm(lmodel9,test.set)
SSE=sum((test.set$Price-test.predictions)^2)
test.set=cbind(slno=1:84,test.set)

plot(test.predictions, test.set$Price, ylab = "Predicted", xlab = "Actual", main="Test Set Performance", pch=20)
text(test.predictions, test.set$Price, labels=test.set$slno, cex=0.7, pos=3)
abline(v=1000, h=1000)
best.buy=test.set[which(test.set$slno==24),2:40]
best.buy
apply(RAM.T, 2, sum)
summary(laptopPrice2$Memory.Technology)
#DDR2
apply(Processor.T, 2, sum)
summary(laptopPrice2$Processor)
#Intel Core Duo
apply(Manufact.T, 2, sum)
summary(laptopPrice2$Manufacturer)
#Toshiba
apply(OS.T, 2, sum)
summary(laptopPrice2$Operating.System)
#WinXP_Pro

#########################################################################################
# CONCLUSION: Best Laptop (Value for money): 768Max.Resolution,DDR2 RAM TYPE,512MB RAM  #
# 1.830 GHz Processor Speed, Intel Core Duo Processor, Toshiba with Docking Station and #
# Windows XP Professional with 3 years warranty at $1600                                #
#########################################################################################