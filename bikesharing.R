#--------------------------------------------------------
#  Main Program for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

setwd("/Users/taposh/workspace/kagglebikesharing")

#sink the output
#sink("bikeshare.log", split = T)
#source the libraries
source("mylibraries.R")
#input data
source("inputdata.R")
#View the data
head(bike)
#Factorengineering
source("factorengineering.R")
#Columns Created
colnames(bike)

#Flat Data sets with factors for Web-H2o Consumption
train_factor <- cbind(countresult,bike)
colnames(train_factor)[1] <- "count"
colnames(train_factor)
#write.table(train_factor,file="train_factors_h2o.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
#write.table(test,file="test_factors_h2o.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)


########################################################
## Forumula
########################################################
#All factors determined

#forumla_all <- toString(paste(unique(colnames(as.data.frame(bike))), collapse=" + "))
#forumla_all
formula <- count ~. #forumla_all

###################################
# Algorithms
##################################

### Method #1
#1 Matrix Computation
#source("matrix.R")


##################################
# Method 2 : CTREE
##################################
#source("ctree.R")
#head(Predictions)
#write.table(Predictions,file=timestamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)


##################################
# Method 3: Neural Net
##################################
# #train your data.  note that this is a neural network with 5 hidden layers of 7, 8, 9, 8, and 7 respectively.
fit <- neuralnet(formula,data=train_factor,threshold=.03,stepmax=1e+06,learningrate=.001,algorithm="rprop+",lifesign="full",likelihood=T)
# 
predict.neural.net <- compute(fit,bike)
compare <- cbind(countresult,predict.neural.net)
head(compare)
auc_nn<-auc(countresult,predict.neural.net)
rmsle_nn <-rmsle(countresult,predict.neural.net)
myrocnn <- roc(countresult,predict.neural.net)
plot(myrocnn)

##
## Output
##

curtime <- Sys.time()
timestampnn <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
timestampnn1 <-paste("Submission_nn",timestamp,".csv", sep="_")

write.table(Predictions,file=timestampnn1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)

x<-paste(timestampnn1,"AUC",auc_nn,"Logloss",rmsle_nn,collapse="  ")
print(x)
write(x, file = "Results_compare.txt",append = TRUE, sep = " ")


##################################
# Method 4: Random Forests
##################################

model_rf <- randomForest( formula, data=train_factor)   
varImpPlot(model_rf)

predict.train.rf <- predict(model_rf, newdata=bike,type='response')
compare_rf <- cbind(countresult,predict.train.rf)
head(compare_rf)
auc_rf<-auc(countresult,predict.train.rf)
rmsle_rf <-rmsle(countresult,predict.train.rf)
myrocrf <- roc(countresult,predict.train.rf)
plot(myrocrf)


predict.test.rf <- predict(model_rf, newdata=test,type='response')
head(predict.test.rf)

curtime <- Sys.time()
timestamprf <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
timestamprf1 <-paste("Submission_rf",timestamp,".csv", sep="_")

write.table(predict.test.rf ,file=timestamprf1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)

xrf<-paste(timestamprf1,"AUC",auc_rf,"Logloss",rmsle_rf,collapse="  ")
print(xrf)
write(xrf, file = "Results_compare.txt",append = TRUE, sep = " ")

C.index <- somers2(prd.rf, test$y)["C"]
Brier <- mean((prd.rf-test$y)^2)
format(C.index)
format(Brier)






