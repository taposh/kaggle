
bikeInv<-ginv(as.matrix(bike))
ACT<-bikeInv %*% actual
CAU<-bikeInv %*% causal
REG<-bikeInv %*% registered

#Only Counts
TrainPredictions<-cbind(exp(bike %*% ACT))
TrainPredictions[TrainPredictions[,1]<0,1]<-0
compare <- cbind(countresult,TrainPredictions)
auc_Counts<-auc(countresult,TrainPredictions)
#ci_Counts <-ci(countresult,TrainPredictions)
rmsle_Counts <-rmsle(countresult,TrainPredictions)
myroc <- roc(countresult,TrainPredictions)
plot(myroc)

#registered & Causal
TrainPredictions1<-cbind(exp(bike %*% CAU))
TrainPredictions1[TrainPredictions1[,1]<0,1]<-0
TrainPredictions2<-cbind(exp(bike %*% REG))
TrainPredictions2[TrainPredictions2[,1]<0,1]<-0
TrainPredictionsCombo <- cbind(rowSums(TrainPredictions1,TrainPredictions2))
auc_Combo <- auc(countresult,TrainPredictionsCombo)
rmsle_Combo <- rmsle(countresult,TrainPredictionsCombo)
#ci_Combo <- ci(countresult,TrainPredictionsCombo)
#myroc1 <- roc(countresult,TrainPredictionsCombo)
#plot(myroc1)


#Present a standard model summary.
# print("Auc :")
# print(auc_Counts)
# print("Logloss :")
# print(rmsle_Counts)
# plot(myroc)

#Predictions count
cPredictions<-cbind(exp(test %*% ACT))
cPredictCau<-cbind(exp(test %*% CAU)) 
cPredictReg<-cbind(exp(test %*% REG)) 

cPredictions[cPredictions[,1]<0,1]<-0
cPredictCau[cPredictCau[,1]<0,1]<-0
cPredictReg[cPredictReg[,1]<0,1]<-0

#Count from Causal & registered
theCombo<-cbind(((cPredictCau)+(cPredictReg))) 
Predictions_comb<-cbind(times,theCombo)
colnames(Predictions_comb)<-c("datetime","count")
head(Predictions_comb)

#Count Predictions
Predictions<-cbind(times,cPredictions)
colnames(Predictions)<-c("datetime","count")
head(Predictions)

########################################################
## Output
########################################################

curtime <- Sys.time()
timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
timestamp1 <-paste("Submission_combo_Matrix",timestamp,".csv", sep="_")
timestamp2 <-paste("Submission_Matrix",timestamp,".csv", sep="_")

#write.table(Predictions_comb,file=timestamp1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
#write.table(Predictions,file=timestamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)

x<-paste(timestamp2,"AUC",auc_Counts,"Logloss",rmsle_Counts,collapse="  ")
print(x)
write(x, file = "Results_compare.txt",append = TRUE, sep = " ")
