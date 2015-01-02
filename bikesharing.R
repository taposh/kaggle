#--------------------------------------------------------
#  Main Program for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

setwd("/Users/taposh/workspace/kagglebikesharing")

#sink the output
sink("bikeshare.log", split = T)
#source the libraries
source("mylibraries.R")
#input data
source("inputdata.R")
#View the data
head(bike)
#Factorengineering
source("factorengineering.R")

#Flat Data sets with factors for Web-H2o Consumption
train_factor <- cbind(countresult,bike)
colnames(train_factor)[1] <- "count"
head(train_factor)
write.table(train_factor,file="train_factors_h2o.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
write.table(test,file="test_factors_h2o.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)


########################################################
## Forumula
########################################################
#All factors determined
formula <- count ~.

###################################
# Algorithms
##################################

### Method #1
#1 Matrix Computation
source("matrix.R")


##################################
# Method 2 : CTREE
##################################

# # define training control
# train_control <- trainControl(method="cv", number=10)
# 
# # # train the model 
# themodel <- train(formula, data=train_factor, trControl=train_control, method="ctree")
# 
# #predict for count using train 
# predict.train.count <-predict(themodel,BIKE )
# 
# #actual <- data.frame(train_factor[1])
# sim <-data.frame(predict.train.count)
# 
# auc(countresult,sim)
# rmsle(countresult,sim)
# myroc2 <- roc(countresult,sim)
# myci2 <- ci(countresult,sim)
# plot(myroc2)

# 
# # #build a dataframe with our results
# #valid.train.count <- data.frame(datetime = train$datetime, count=predict.train.count)
# #head(valid.train.count)
# 


##################################
# Method 3: Neural Net
##################################
# #train your data.  note that this is a neural network with 5 hidden layers of 7, 8, 9, 8, and 7 respectively.
# fit <- neuralnet(formula,data=train_factor,threshold=.03,stepmax=1e+06,learningrate=.001,algorithm="rprop+",lifesign="full",likelihood=T)
# 
# predict.neural.net <- compute(fit,train_factor[-1])
# sim1 <-data.frame(predict.neural.net)
# 
# auc(countresult,sim1)
# rmsle(countresult,sim1)
# myroc3 <- roc(countresult,sim1)
# myci3 <- ci(countresult,sim1)
# plot(myroc3)











