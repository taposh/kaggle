#--------------------------------------------------------
#  Factor Engineering for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

actual<-c()
countresult <-c()
causal<-c()
registered<-c()
countresult<-cbind("count"=countresult,bike[,"count"])
actual<-cbind("count"=actual,bike[,"count"])
causal<-cbind(causal,bike[,"casual"])
registered<-cbind(registered,bike[,"registered"])

causal<-log(causal)
causal[causal[,1]<0,1]<-0

registered<-log(registered)
registered[registered[,1]<0,1]<-0

actual<-log(actual)
#actual[actual[,1]<0,1]<-0

traintimes<-paste(bike[,"datetime"])
times<-paste(test[,"datetime"])

bike<-cbind(bike,"hour"=format(as.POSIXct(bike[,"datetime"], format="%Y-%m-%d %H:%M"), format="%H"),
            "day"=format(as.POSIXct(bike[,"datetime"], format="%Y-%m-%d %H:%M"), format="%d"),
            "month"=format(as.POSIXct(bike[,"datetime"], format="%Y-%m-%d %H:%M"), format="%m"),
            "year"=format(as.POSIXct(bike[,"datetime"], format="%Y-%m-%d %H:%M"), format="%Y")
)
bike[,c("hour","day","month","year")]<-as.matrix(bike[,c("hour","day","month","year")])

for(i in 1:nrow(bike)){
  for(j in c("hour","day","month","year")){
    if(is.na(bike[i,j])){
      bike[i,j]<-mean(as.numeric(bike[i-1,j]),as.numeric(bike[i+1,j]))}
  }}

test<-cbind(test,"hour"=format(as.POSIXct(test[,"datetime"], format="%Y-%m-%d %H:%M"), format="%H"),
            "day"=format(as.POSIXct(test[,"datetime"], format="%Y-%m-%d %H:%M"), format="%d"),
            "month"=format(as.POSIXct(test[,"datetime"], format="%Y-%m-%d %H:%M"), format="%m"),
            "year"=format(as.POSIXct(test[,"datetime"], format="%Y-%m-%d %H:%M"), format="%Y")
)
test[,c("hour","day","month","year")]<-as.matrix(test[,c("hour","day","month","year")])

START<-ISOdate(min(bike[,"year"]),min(bike[,"month"]),min(bike[,"day"]),min(bike[,"hour"]))

bikeTIME<-c()
testTIME<-c()
for(i in 1:nrow(bike)){
  bikeTIME<-rbind(bikeTIME,difftime(ISOdate(bike[i,"year"],bike[i,"month"],bike[i,"day"],bike[i,"hour"]),START,units="hours"))}
for(i in 1:nrow(test)){
  testTIME<-rbind(testTIME, difftime(ISOdate(test[i,"year"],test[i,"month"],test[i,"day"],test[i,"hour"]),START,units="hours"))}

head(bike,40)

#create Sunday variable
bike$sunday[bike$day == "02"] <- "1"
bike$sunday[bike$day != "02"] <- "0"

test$sunday[test$day == "02"] <- "1"
test$sunday[test$day != "02"] <- "0"

#Monday
bike$monday[bike$day == "03"] <- "1"
bike$monday[bike$day != "03"] <- "0"

test$monday[test$day == "03"] <- "1"
test$monday[test$day != "03"] <- "0"

#Tuesday
bike$tuesday[bike$day == "04"] <- "1"
bike$tuesday[bike$day != "04"] <- "0"

test$tuesday[test$day == "04"] <- "1"
test$tuesday[test$day != "04"] <- "0"

#Wednesday
bike$wednesday[bike$day == "05"] <- "1"
bike$wednesday[bike$day != "05"] <- "0"

test$wednesday[test$day == "05"] <- "1"
test$wednesday[test$day != "05"] <- "0"

#Thursday
bike$thursday[bike$day == "06"] <- "1"
bike$thursday[bike$day != "06"] <- "0"

test$thursday[test$day == "06"] <- "1"
test$thursday[test$day != "06"] <- "0"

#Friday
bike$friday[bike$day == "07"] <- "1"
bike$friday[bike$day != "07"] <- "0"

test$friday[test$day == "07"] <- "1"
test$friday[test$day != "07"] <- "0"

#Saturday
bike$saturday[bike$day == "01"] <- "1"
bike$saturday[bike$day != "01"] <- "0"

test$saturday[test$day == "01"] <- "1"
test$saturday[test$day != "01"] <- "0"

head(bike)

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

bike<-cbind(bike,"dayof"=dayof(bike))
test<-cbind(test,"dayof"=dayof(test))
test<-test[,colnames(test)!="datetime"]
test<-test[,colnames(test)!="day"]
test<-test[,colnames(test)!="year"]
bike<-bike[,colnames(test)]


#head(bike)

A<-cbind(bike[,"hour"],bike[,"dayof"])
B<-cbind(test[,"hour"],test[,"dayof"])

A2<-c()
B2<-c()

for(i in unique(B[,1])){
  for(j in unique(B[,2])){
    TEMPA<-matrix(data=0,ncol=1,nrow=nrow(bike))
    TEMPB<-matrix(data=0,ncol=1,nrow=nrow(test))
    TEMPA[A[,1]==i & A[,2]==j,1]<-1
    TEMPB[B[,1]==i & B[,2]==j,1]<-1
    A2<-cbind(A2,TEMPA)
    B2<-cbind(B2,TEMPB)
  }}

#head(bike)
#test<-test[,colnames(test)!=c("month","hour","dayof")]
#bike<-bike[,colnames(bike)!=c("month","hour","dayof")]

bike<-cbind(bike,
            temp2=(bike[,"temp"])^2,
            atemp2=(bike[,"atemp"])^2,
            humid2=(bike[,"humidity"])^2,
            windspeed2=(bike[,"windspeed"])^2,
            temp3=(bike[,"temp"])^3,
            atemp3=(bike[,"atemp"])^3,
            humid3=(bike[,"humidity"])^3,
            windspeed3=(bike[,"windspeed"])^3)

test<-cbind(test,
            temp2=test[,"temp"]^2,
            atemp2=test[,"atemp"]^2,
            humid2=test[,"humidity"]^2,
            windspeed2=(test[,"windspeed"])^2,
            temp3=(test[,"temp"])^3,
            atemp3=(test[,"atemp"])^3,
            humid3=(test[,"humidity"])^3,
            windspeed3=(test[,"windspeed"])^3)


# bike<-cbind(bike,
#             sqrttemp=sqrt(bike[,"temp"]),
#             sqrtatemp=sqrt(bike[,"atemp"]),
#             sqrthumid=sqrt(bike[,"humidity"]),
#             sqrtwindspeed=sqrt(bike[,"windspeed"]))
# 
# test<-cbind(test,
#             sqrttemp=sqrt(test[,"temp"]),
#             sqrtatemp=sqrt(test[,"atemp"]),
#             sqrthumid=sqrt(test[,"humidity"]),
#             sqrtwindspeed=sqrt(test[,"windspeed"]))

bike<-cbind(bike,
            logtemp=log(bike[,"temp"]),
            logatemp=log(bike[,"atemp"]))
#loghumid=log(bike[,"humidity"]))
#logwindspeed=log(bike[,"windspeed"]))

test<-cbind(test,
            logtemp=log(test[,"temp"]),
            logatemp=log(test[,"atemp"]))
#loghumid=log(test[,"humidity"]))
#logwindspeed=log(test[,"windspeed"]))

# humid and windspeed
test<-cbind(test,
            sinhumid=sin(2*pi*test[,"humidity"]/4),
            coshumid=cos(2*pi*test[,"humidity"]/4),
            sinhumid2=sin(2*pi*test[,"humid2"]/10),
            coshumid2=cos(2*pi*test[,"humid2"]/10),
            sinhumid3=sin(2*pi*test[,"humid3"]/(10)),
            coshumid3=cos(2*pi*test[,"humid3"]/(10)),
            sinwind=sin(2*pi*test[,"windspeed"]/(10)),
            coswind=cos(2*pi*test[,"windspeed"]/(10)),
            sinwind2=sin(2*pi*test[,"windspeed2"]/(0.00001)),
            coswind2=cos(2*pi*test[,"windspeed2"]/(0.00001)),
            sinwind3=sin(2*pi*test[,"windspeed3"]/(0.00001)),
            coswind3=cos(2*pi*test[,"windspeed3"]/(0.00001)))



bike<-cbind(bike,
            sinhumid=sin(2*pi*bike[,"humidity"]/4),
            coshumid=cos(2*pi*bike[,"humidity"]/4),
            sinhumid2=sin(2*pi*bike[,"humid2"]/10),
            coshumid2=cos(2*pi*bike[,"humid2"]/10),
            sinhumid3=sin(2*pi*bike[,"humid3"]/(10)),
            coshumid3=cos(2*pi*bike[,"humid3"]/(10)),
            sinwind=sin(2*pi*bike[,"windspeed"]/(10)),
            coswind=cos(2*pi*bike[,"windspeed"]/(10)),
            sinwind2=sin(2*pi*bike[,"windspeed2"]/(0.00001)),
            coswind2=cos(2*pi*bike[,"windspeed2"]/(0.00001)),
            sinwind3=sin(2*pi*bike[,"windspeed3"]/(0.00001)),
            coswind3=cos(2*pi*bike[,"windspeed3"]/(0.00001)))



# #temp and atemp
bike<-cbind(bike,
            sintemp2=sin(2*pi*bike[,"temp2"]/(10000)),
            costemp2=cos(2*pi*bike[,"temp2"]/(10000)),
            sintemp3=sin(2*pi*bike[,"temp3"]/(1000000)),
            costemp3=cos(2*pi*bike[,"temp3"]/(1000000)),
            sinatemp2=sin(2*pi*bike[,"atemp2"]/(10000)),
            cosatemp2=cos(2*pi*bike[,"atemp2"]/(10000)),
            sinatemp3=sin(2*pi*bike[,"atemp3"]/(1000000)),
            cosatemp3=cos(2*pi*bike[,"atemp3"]/(1000000)))


test<-cbind(test,
            sintemp2=sin(2*pi*test[,"temp2"]/(10000)),
            costemp2=cos(2*pi*test[,"temp2"]/(10000)),
            sintemp3=sin(2*pi*test[,"temp3"]/(1000000)),
            costemp3=cos(2*pi*test[,"temp3"]/(1000000)),
            sinatemp2=sin(2*pi*test[,"atemp2"]/(10000)),
            cosatemp2=cos(2*pi*test[,"atemp2"]/(10000)),
            sinatemp3=sin(2*pi*test[,"atemp3"]/(1000000)),
            cosatemp3=cos(2*pi*test[,"atemp3"]/(1000000)))


##-----tanhh--------### 
#  test<-cbind(test,tanhhwind2=tanh(2*pi*test[,"windspeed2"]),tanhwind3=tanh(2*pi*test[,"windspeed3"]),tanhumid2=tanh(2*pi*test[,"humid2"]),tanhhumid3=tanh(2*pi*test[,"humid3"]),tanhwind24=tanh(4*pi*test[,"windspeed2"]),tanhwind34=tanh(4*pi*test[,"windspeed3"]),tanhumid24=tanh(4*pi*test[,"humid2"]),tanhhumid34=tanh(4*pi*test[,"humid3"]))
# #             
#  bike<-cbind(bike,tanhwind2=tanh(2*pi*bike[,"windspeed2"]),tanhwind3=tanh(2*pi*bike[,"windspeed3"]),tanhumid2=tanh(2*pi*bike[,"humid2"]),tanhhumid3=tanh(2*pi*bike[,"humid3"]),tanhwind24=tanh(4*pi*bike[,"windspeed2"]),tanhwind34=tanh(4*pi*bike[,"windspeed3"]),tanhumid24=tanh(4*pi*bike[,"humid2"]),tanhhumid34=tanh(4*pi*bike[,"humid3"]))



#Weather : removing 4
bike$weather[bike$weather == 4] <- 3
test$weather[test$weather == 4] <- 3

Categorical<-c("holiday","workingday","weather","month","hour","dayof")
for(i in Categorical){
  BT<-bike[i]
  TT<-test[i]
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
  bike<-cbind(bike[,colnames(bike)!=i], mystr=BT2)
  #colnames(bike[,colnames(bike)!=i]) <- mystr
  test<-cbind(test[,colnames(test)!=i], mystr=TT2)
}
bike<-cbind(bike,A2)
test<-cbind(test,B2)



test<-cbind(test,sin(4*pi*testTIME/(24)),
            sin(4*pi*testTIME/(1000)),
            cos(4*pi*testTIME/(24)),
            cos(4*pi*testTIME/(1000)),
            tanh(2*pi*testTIME/(24)),
            #tanh(2*pi*testTIME/(1000)),
            tanh(2*pi*testTIME/(365*24)),
            tanh(4*pi*testTIME/(24)),
            #tanh(4*pi*testTIME/(1000)),
            tanh(4*pi*testTIME/(365*24)),
            sin(2*pi*testTIME/(24)),
            sin(2*pi*testTIME/(1000)),
            cos(2*pi*testTIME/(24)),
            cos(2*pi*testTIME/(1000)),
            sin(4*pi*testTIME/(356*24)),
            cos(4*pi*testTIME/(365*24)),
            sin(2*pi*testTIME/(365*24)),cos(2*pi*testTIME/(365*24)),testTIME)
bike<-cbind(bike,sin(4*pi*bikeTIME/(24)),
            sin(4*pi*bikeTIME/(1000)),
            cos(4*pi*bikeTIME/(24)),cos(4*pi*bikeTIME/(1000)),
            tanh(2*pi*bikeTIME/(24)),#tanh(2*pi*bikeTIME/(1000)),
            tanh(2*pi*bikeTIME/(365*24)),
            tanh(4*pi*bikeTIME/(24)),#tanh(4*pi*bikeTIME/(1000)),
            tanh(4*pi*bikeTIME/(365*24)),
            sin(2*pi*bikeTIME/(24)),sin(2*pi*bikeTIME/(1000)),
            cos(2*pi*bikeTIME/(24)),cos(2*pi*bikeTIME/(1000)),
            sin(4*pi*bikeTIME/(356*24)),
            cos(4*pi*bikeTIME/(365*24)),
            sin(2*pi*bikeTIME/(365*24)),cos(2*pi*bikeTIME/(365*24)),bikeTIME)

bike<-cbind(bike,Number=matrix(data=1,nrow=nrow(bike),ncol=1))
test<-cbind(test,Number=matrix(data=1,nrow=nrow(test),ncol=1))

bike<-data.matrix(bike)
test<-data.matrix(test)


head(bike)

