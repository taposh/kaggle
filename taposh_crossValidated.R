#--------------------------------------------------------
#  Conditional Inference Tree for kaggle-bike-sharing
#  This method uses cross validation.
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

#libraries Used
library("MASS")
library(caret)
library(Metrics)
library(stats)
#library(pROC)
library(FSelector)
library(ggplot2)
library(neuralnet)

#File Access
setwd("/Users/taposh/workspace/kagglebikesharing")
BIKE<-read.csv("train.csv")
SUB<-read.csv("sampleSubmission.csv")
TEST<-read.csv("test.csv")
head(BIKE)

ACTUAL<-c()
COUNTRESULT <-c()
CASUAL<-c()
REGISTERED<-c()
COUNTRESULT<-cbind("count"=COUNTRESULT,BIKE[,"count"])
ACTUAL<-cbind("count"=ACTUAL,BIKE[,"count"])
CASUAL<-cbind(CASUAL,BIKE[,"casual"])
REGISTERED<-cbind(REGISTERED,BIKE[,"registered"])

CASUAL<-log(CASUAL)
CASUAL[CASUAL[,1]<0,1]<-0

REGISTERED<-log(REGISTERED)
REGISTERED[REGISTERED[,1]<0,1]<-0

ACTUAL<-log(ACTUAL)
#ACTUAL[ACTUAL[,1]<0,1]<-0

trainTimes<-paste(BIKE[,"datetime"])
TIMES<-paste(TEST[,"datetime"])

BIKE<-cbind(BIKE,"hour"=format(as.POSIXct(BIKE[,"datetime"], format="%Y-%m-%d %H:%M"), format="%H"),
            "day"=format(as.POSIXct(BIKE[,"datetime"], format="%Y-%m-%d %H:%M"), format="%d"),
            "month"=format(as.POSIXct(BIKE[,"datetime"], format="%Y-%m-%d %H:%M"), format="%m"),
            "year"=format(as.POSIXct(BIKE[,"datetime"], format="%Y-%m-%d %H:%M"), format="%Y")
)
BIKE[,c("hour","day","month","year")]<-as.matrix(BIKE[,c("hour","day","month","year")])

for(i in 1:nrow(BIKE)){
  for(j in c("hour","day","month","year")){
    if(is.na(BIKE[i,j])){
      BIKE[i,j]<-mean(as.numeric(BIKE[i-1,j]),as.numeric(BIKE[i+1,j]))}
  }}

TEST<-cbind(TEST,"hour"=format(as.POSIXct(TEST[,"datetime"], format="%Y-%m-%d %H:%M"), format="%H"),
            "day"=format(as.POSIXct(TEST[,"datetime"], format="%Y-%m-%d %H:%M"), format="%d"),
            "month"=format(as.POSIXct(TEST[,"datetime"], format="%Y-%m-%d %H:%M"), format="%m"),
            "year"=format(as.POSIXct(TEST[,"datetime"], format="%Y-%m-%d %H:%M"), format="%Y")
)
TEST[,c("hour","day","month","year")]<-as.matrix(TEST[,c("hour","day","month","year")])

START<-ISOdate(min(BIKE[,"year"]),min(BIKE[,"month"]),min(BIKE[,"day"]),min(BIKE[,"hour"]))

BIKETIME<-c()
TESTTIME<-c()
for(i in 1:nrow(BIKE)){
  BIKETIME<-rbind(BIKETIME,difftime(ISOdate(BIKE[i,"year"],BIKE[i,"month"],BIKE[i,"day"],BIKE[i,"hour"]),START,units="hours"))}
for(i in 1:nrow(TEST)){
  TESTTIME<-rbind(TESTTIME, difftime(ISOdate(TEST[i,"year"],TEST[i,"month"],TEST[i,"day"],TEST[i,"hour"]),START,units="hours"))}

head(BIKE,40)

#create Sunday variable
BIKE$sunday[BIKE$day == "02"] <- "1"
BIKE$sunday[BIKE$day != "02"] <- "0"

TEST$sunday[TEST$day == "02"] <- "1"
TEST$sunday[TEST$day != "02"] <- "0"

#Monday
BIKE$monday[BIKE$day == "03"] <- "1"
BIKE$monday[BIKE$day != "03"] <- "0"

TEST$monday[TEST$day == "03"] <- "1"
TEST$monday[TEST$day != "03"] <- "0"

#Tuesday
BIKE$tuesday[BIKE$day == "04"] <- "1"
BIKE$tuesday[BIKE$day != "04"] <- "0"

TEST$tuesday[TEST$day == "04"] <- "1"
TEST$tuesday[TEST$day != "04"] <- "0"

#Wednesday
BIKE$wednesday[BIKE$day == "05"] <- "1"
BIKE$wednesday[BIKE$day != "05"] <- "0"

TEST$wednesday[TEST$day == "05"] <- "1"
TEST$wednesday[TEST$day != "05"] <- "0"

#Thursday
BIKE$thursday[BIKE$day == "06"] <- "1"
BIKE$thursday[BIKE$day != "06"] <- "0"

TEST$thursday[TEST$day == "06"] <- "1"
TEST$thursday[TEST$day != "06"] <- "0"

#Friday
BIKE$friday[BIKE$day == "07"] <- "1"
BIKE$friday[BIKE$day != "07"] <- "0"

TEST$friday[TEST$day == "07"] <- "1"
TEST$friday[TEST$day != "07"] <- "0"

#Saturday
BIKE$saturday[BIKE$day == "01"] <- "1"
BIKE$saturday[BIKE$day != "01"] <- "0"

TEST$saturday[TEST$day == "01"] <- "1"
TEST$saturday[TEST$day != "01"] <- "0"

head(BIKE)

dayof<-function(X){
  month<-as.matrix(as.numeric(X[,"month"]))
  month[month==1]<-13
  month[month==2]<-14
  day<-as.matrix(as.numeric(X[,"day"]))
  year<-cbind(as.numeric(X[,"year"]),as.numeric(X[,"month"]))
  for(i in 1:nrow(year)){
    if((year[i,2]<=2)==TRUE){year[i,1]<-year[i,1]-1}
  }
  year<-as.matrix(year[,1])
  y<-year %% 2000
  c<-year - y
  c<-c/100
  DAY<-(day+floor(13*(month+1)/5)+y+floor(y/4)+floor(c/4)-c) %% 7
  return(DAY)}

BIKE<-cbind(BIKE,"dayof"=dayof(BIKE))
TEST<-cbind(TEST,"dayof"=dayof(TEST))
TEST<-TEST[,colnames(TEST)!="datetime"]
TEST<-TEST[,colnames(TEST)!="day"]
TEST<-TEST[,colnames(TEST)!="year"]
BIKE<-BIKE[,colnames(TEST)]


#head(BIKE)

A<-cbind(BIKE[,"hour"],BIKE[,"dayof"])
B<-cbind(TEST[,"hour"],TEST[,"dayof"])

A2<-c()
B2<-c()

for(i in unique(B[,1])){
  for(j in unique(B[,2])){
    TEMPA<-matrix(data=0,ncol=1,nrow=nrow(BIKE))
    TEMPB<-matrix(data=0,ncol=1,nrow=nrow(TEST))
    TEMPA[A[,1]==i & A[,2]==j,1]<-1
    TEMPB[B[,1]==i & B[,2]==j,1]<-1
    A2<-cbind(A2,TEMPA)
    B2<-cbind(B2,TEMPB)
  }}

#head(BIKE)
#TEST<-TEST[,colnames(TEST)!=c("month","hour","dayof")]
#BIKE<-BIKE[,colnames(BIKE)!=c("month","hour","dayof")]

BIKE<-cbind(BIKE,
            temp2=(BIKE[,"temp"])^2,
            atemp2=(BIKE[,"atemp"])^2,
            humid2=(BIKE[,"humidity"])^2,
            windspeed2=(BIKE[,"windspeed"])^2,
            temp3=(BIKE[,"temp"])^3,
            atemp3=(BIKE[,"atemp"])^3,
            humid3=(BIKE[,"humidity"])^3,
            windspeed3=(BIKE[,"windspeed"])^3)

TEST<-cbind(TEST,
            temp2=TEST[,"temp"]^2,
            atemp2=TEST[,"atemp"]^2,
            humid2=TEST[,"humidity"]^2,
            windspeed2=(TEST[,"windspeed"])^2,
            temp3=(TEST[,"temp"])^3,
            atemp3=(TEST[,"atemp"])^3,
            humid3=(TEST[,"humidity"])^3,
            windspeed3=(TEST[,"windspeed"])^3)


# BIKE<-cbind(BIKE,
#             sqrttemp=sqrt(BIKE[,"temp"]),
#             sqrtatemp=sqrt(BIKE[,"atemp"]),
#             sqrthumid=sqrt(BIKE[,"humidity"]),
#             sqrtwindspeed=sqrt(BIKE[,"windspeed"]))
# 
# TEST<-cbind(TEST,
#             sqrttemp=sqrt(TEST[,"temp"]),
#             sqrtatemp=sqrt(TEST[,"atemp"]),
#             sqrthumid=sqrt(TEST[,"humidity"]),
#             sqrtwindspeed=sqrt(TEST[,"windspeed"]))

BIKE<-cbind(BIKE,
            logtemp=log(BIKE[,"temp"]),
            logatemp=log(BIKE[,"atemp"]))
            #loghumid=log(BIKE[,"humidity"]))
            #logwindspeed=log(BIKE[,"windspeed"]))

TEST<-cbind(TEST,
            logtemp=log(TEST[,"temp"]),
            logatemp=log(TEST[,"atemp"]))
            #loghumid=log(TEST[,"humidity"]))
            #logwindspeed=log(TEST[,"windspeed"]))

# humid and windspeed
TEST<-cbind(TEST,
            sinhumid=sin(2*pi*TEST[,"humidity"]/4),
            coshumid=cos(2*pi*TEST[,"humidity"]/4),
            sinhumid2=sin(2*pi*TEST[,"humid2"]/10),
            coshumid2=cos(2*pi*TEST[,"humid2"]/10),
            sinhumid3=sin(2*pi*TEST[,"humid3"]/(10)),
            coshumid3=cos(2*pi*TEST[,"humid3"]/(10)),
            sinwind=sin(2*pi*TEST[,"windspeed"]/(10)),
            coswind=cos(2*pi*TEST[,"windspeed"]/(10)),
            sinwind2=sin(2*pi*TEST[,"windspeed2"]/(0.00001)),
            coswind2=cos(2*pi*TEST[,"windspeed2"]/(0.00001)),
            sinwind3=sin(2*pi*TEST[,"windspeed3"]/(0.00001)),
            coswind3=cos(2*pi*TEST[,"windspeed3"]/(0.00001)))



BIKE<-cbind(BIKE,
            sinhumid=sin(2*pi*BIKE[,"humidity"]/4),
            coshumid=cos(2*pi*BIKE[,"humidity"]/4),
            sinhumid2=sin(2*pi*BIKE[,"humid2"]/10),
            coshumid2=cos(2*pi*BIKE[,"humid2"]/10),
            sinhumid3=sin(2*pi*BIKE[,"humid3"]/(10)),
            coshumid3=cos(2*pi*BIKE[,"humid3"]/(10)),
            sinwind=sin(2*pi*BIKE[,"windspeed"]/(10)),
            coswind=cos(2*pi*BIKE[,"windspeed"]/(10)),
            sinwind2=sin(2*pi*BIKE[,"windspeed2"]/(0.00001)),
            coswind2=cos(2*pi*BIKE[,"windspeed2"]/(0.00001)),
            sinwind3=sin(2*pi*BIKE[,"windspeed3"]/(0.00001)),
            coswind3=cos(2*pi*BIKE[,"windspeed3"]/(0.00001)))



# #temp and atemp
BIKE<-cbind(BIKE,
                sintemp2=sin(2*pi*BIKE[,"temp2"]/(10000)),
                costemp2=cos(2*pi*BIKE[,"temp2"]/(10000)),
                sintemp3=sin(2*pi*BIKE[,"temp3"]/(1000000)),
                costemp3=cos(2*pi*BIKE[,"temp3"]/(1000000)),
                sinatemp2=sin(2*pi*BIKE[,"atemp2"]/(10000)),
                cosatemp2=cos(2*pi*BIKE[,"atemp2"]/(10000)),
                sinatemp3=sin(2*pi*BIKE[,"atemp3"]/(1000000)),
                cosatemp3=cos(2*pi*BIKE[,"atemp3"]/(1000000)))


TEST<-cbind(TEST,
            sintemp2=sin(2*pi*TEST[,"temp2"]/(10000)),
            costemp2=cos(2*pi*TEST[,"temp2"]/(10000)),
            sintemp3=sin(2*pi*TEST[,"temp3"]/(1000000)),
            costemp3=cos(2*pi*TEST[,"temp3"]/(1000000)),
            sinatemp2=sin(2*pi*TEST[,"atemp2"]/(10000)),
            cosatemp2=cos(2*pi*TEST[,"atemp2"]/(10000)),
            sinatemp3=sin(2*pi*TEST[,"atemp3"]/(1000000)),
            cosatemp3=cos(2*pi*TEST[,"atemp3"]/(1000000)))


##-----tanhh--------### 
#  TEST<-cbind(TEST,tanhhwind2=tanh(2*pi*TEST[,"windspeed2"]),tanhwind3=tanh(2*pi*TEST[,"windspeed3"]),tanhumid2=tanh(2*pi*TEST[,"humid2"]),tanhhumid3=tanh(2*pi*TEST[,"humid3"]),tanhwind24=tanh(4*pi*TEST[,"windspeed2"]),tanhwind34=tanh(4*pi*TEST[,"windspeed3"]),tanhumid24=tanh(4*pi*TEST[,"humid2"]),tanhhumid34=tanh(4*pi*TEST[,"humid3"]))
# #             
#  BIKE<-cbind(BIKE,tanhwind2=tanh(2*pi*BIKE[,"windspeed2"]),tanhwind3=tanh(2*pi*BIKE[,"windspeed3"]),tanhumid2=tanh(2*pi*BIKE[,"humid2"]),tanhhumid3=tanh(2*pi*BIKE[,"humid3"]),tanhwind24=tanh(4*pi*BIKE[,"windspeed2"]),tanhwind34=tanh(4*pi*BIKE[,"windspeed3"]),tanhumid24=tanh(4*pi*BIKE[,"humid2"]),tanhhumid34=tanh(4*pi*BIKE[,"humid3"]))
                        
                        

#Weather : removing 4
BIKE$weather[BIKE$weather == 4] <- 3
TEST$weather[TEST$weather == 4] <- 3

Categorical<-c("holiday","workingday","weather","month","hour","dayof")
for(i in Categorical){
  BT<-BIKE[i]
  TT<-TEST[i]
  BT2<-c()
  TT2<-c()
  for(j in unique(BT[,i])){
    TempMat<-matrix(data=0,nrow=nrow(BT),ncol=1)
    TempMat[BT==j]<-1
    BT2<-cbind(BT2,mystr=TempMat)
    TempMat2<-matrix(data=0,nrow=nrow(TT),ncol=1)
    TempMat2[TT==j]<-1
    TT2<-cbind(TT2,TempMat2)
  }
  mystr <- paste("tmpmat", toString(j),toString(i), sep="_")
  #print(mystr)
  #colnames(BT2[,1])<- mystr
  #colnames(TT2[,1])<- mystr
  BIKE<-cbind(BIKE[,colnames(BIKE)!=i], mystr=BT2)
  #colnames(BIKE[,colnames(BIKE)!=i]) <- mystr
  TEST<-cbind(TEST[,colnames(TEST)!=i], mystr=TT2)
}
BIKE<-cbind(BIKE,A2)
TEST<-cbind(TEST,B2)



TEST<-cbind(TEST,sin(4*pi*TESTTIME/(24)),
            sin(4*pi*TESTTIME/(1000)),
            cos(4*pi*TESTTIME/(24)),
            cos(4*pi*TESTTIME/(1000)),
            tanh(2*pi*TESTTIME/(24)),
            #tanh(2*pi*TESTTIME/(1000)),
            tanh(2*pi*TESTTIME/(365*24)),
            tanh(4*pi*TESTTIME/(24)),
            #tanh(4*pi*TESTTIME/(1000)),
            tanh(4*pi*TESTTIME/(365*24)),
            sin(2*pi*TESTTIME/(24)),
            sin(2*pi*TESTTIME/(1000)),
            cos(2*pi*TESTTIME/(24)),
            cos(2*pi*TESTTIME/(1000)),
            sin(4*pi*TESTTIME/(356*24)),
            cos(4*pi*TESTTIME/(365*24)),
            sin(2*pi*TESTTIME/(365*24)),cos(2*pi*TESTTIME/(365*24)),TESTTIME)
BIKE<-cbind(BIKE,sin(4*pi*BIKETIME/(24)),
            sin(4*pi*BIKETIME/(1000)),
            cos(4*pi*BIKETIME/(24)),cos(4*pi*BIKETIME/(1000)),
            tanh(2*pi*BIKETIME/(24)),#tanh(2*pi*BIKETIME/(1000)),
            tanh(2*pi*BIKETIME/(365*24)),
            tanh(4*pi*BIKETIME/(24)),#tanh(4*pi*BIKETIME/(1000)),
            tanh(4*pi*BIKETIME/(365*24)),
            sin(2*pi*BIKETIME/(24)),sin(2*pi*BIKETIME/(1000)),
            cos(2*pi*BIKETIME/(24)),cos(2*pi*BIKETIME/(1000)),
            sin(4*pi*BIKETIME/(356*24)),
            cos(4*pi*BIKETIME/(365*24)),
            sin(2*pi*BIKETIME/(365*24)),cos(2*pi*BIKETIME/(365*24)),BIKETIME)

BIKE<-cbind(BIKE,Number=matrix(data=1,nrow=nrow(BIKE),ncol=1))
TEST<-cbind(TEST,Number=matrix(data=1,nrow=nrow(TEST),ncol=1))

BIKE<-data.matrix(BIKE)
TEST<-data.matrix(TEST)


head(BIKE)



BikeInv<-ginv(as.matrix(BIKE))
ACT<-BikeInv %*% ACTUAL
CAU<-BikeInv %*% CASUAL
REG<-BikeInv %*% REGISTERED

head(CAU)
head(REG)


#train to output

 mybike <- cbind(COUNTRESULT,BIKE)
 colnames(mybike)[1] <- "count"
 head(mybike)
write.table(mybike,file="train_factors_h2o.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# #test to output
write.table(TEST,file="test_factors_h2o.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)

# See feature Correlations
#subset <- cfs(count~., as.data.frame(as.table(mybike)))
#f <- as.simple.formula(subset, count)
#print(f)



###############
#Validations
###############

bikeevaluate <- function (data, pred) {
  return(sqrt(1/nrow(data)*sum((log(pred+1)-log(data+1))^2)))
}

TrainPredictions<-cbind(exp(BIKE %*% ACT))
TrainPredictions[TrainPredictions[,1]<0,1]<-0
compare <- cbind(COUNTRESULT,TrainPredictions)

auc(COUNTRESULT,TrainPredictions)
rmsle(COUNTRESULT,TrainPredictions)
bikeevaluate(COUNTRESULT,TrainPredictions)

myroc <- roc(COUNTRESULT,TrainPredictions)
myci <- ci(COUNTRESULT,TrainPredictions)
plot(myroc)


TrainPredictions1<-cbind(exp(BIKE %*% CAU))
TrainPredictions1[TrainPredictions1[,1]<0,1]<-0
TrainPredictions2<-cbind(exp(BIKE %*% REG))
TrainPredictions2[TrainPredictions2[,1]<0,1]<-0
TrainPredictionsCombo <- cbind(rowSums(TrainPredictions1,TrainPredictions2))

auc(COUNTRESULT,TrainPredictionsCombo)
rmsle(COUNTRESULT,TrainPredictionsCombo)
myroc1 <- roc(COUNTRESULT,TrainPredictionsCombo)
myci1 <- ci(COUNTRESULT,TrainPredictionsCombo)
plot(myroc1)



#Predictions count
#Predictions<-cbind(TIMES,exp(TEST %*% ACT))
cPredictions<-cbind(exp(TEST %*% ACT))
cPredictCau<-cbind(exp(TEST %*% CAU)) 
cPredictReg<-cbind(exp(TEST %*% REG)) 

#head(cPredictions)
#head(cPredictCau)
#head(cPredictReg)

cPredictions[cPredictions[,1]<0,1]<-0
cPredictCau[cPredictCau[,1]<0,1]<-0
cPredictReg[cPredictReg[,1]<0,1]<-0

theCombo<-cbind(((cPredictCau)+(cPredictReg))) 
#head(theCombo)
#AUC(ACTUAL,theCombo)

Predictions_comb<-cbind(TIMES,theCombo)
colnames(Predictions_comb)<-c("datetime","count")
head(Predictions_comb)
#head(theCombo)


Predictions<-cbind(TIMES,cPredictions)
colnames(Predictions)<-c("datetime","count")
head(Predictions)
#auc(COUNTRESULT,Predictions)

curtime <- Sys.time()
timeStamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
timeStamp1 <-paste("Submission_combo_",timeStamp,".csv", sep="_")
timeStamp2 <-paste("Submission",timeStamp,".csv", sep="_")

#timeStamp1
#timeStamp2

########################################################
## Output
########################################################

write.table(Predictions_comb,file=timeStamp1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
write.table(Predictions,file=timeStamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)

########################################################
## Try Ensemble
########################################################
train_factor<-cbind(count=COUNTRESULT,BIKE)
colnames(train_factor)[1] <- "count"
train_factor_no_count<-as.data.frame(train_factor[,-1])
head(train_factor)


# define training control
train_control <- trainControl(method="cv", number=10)

formula <- count ~.
formula

# # train the model 
themodel <- train(formula, data=train_factor, trControl=train_control, method="ctree")
# #predict for count using train 
predict.train.count <-predict(themodel,train_factor_no_count )

#head(BIKE)
#actual <- data.frame(train_factor[1])
sim <-data.frame(predict.train.count)

auc(COUNTRESULT,sim)
rmsle(COUNTRESULT,sim)
myroc2 <- roc(COUNTRESULT,sim)
myci2 <- ci(COUNTRESULT,sim)
plot(myroc2)


# #build a dataframe with our results
#valid.train.count <- data.frame(datetime = train$datetime, count=predict.train.count)
#head(valid.train.count)

# ###################

#Neural Net
#train your data.  note that this is a neural network with 5 hidden layers of 7, 8, 9, 8, and 7 respectively.
fit <- neuralnet(formula,data=train_factor,threshold=.03,stepmax=1e+06,learningrate=.001,algorithm="rprop+",lifesign="full",likelihood=T)

predict.neural.net <- compute(fit,train_factor[-1])
sim1 <-data.frame(predict.neural.net)

auc(COUNTRESULT,sim1)
rmsle(COUNTRESULT,sim1)
myroc3 <- roc(COUNTRESULT,sim1)
myci3 <- ci(COUNTRESULT,sim1)
plot(myroc3)











