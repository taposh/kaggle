## ################################MNIST################################
## MNIST using PCA
## World-record parameters at http://learn.h2o.ai/content/hands-on_training/deep_learning.html
## From https://github.com/h2oai/h2o-training/blob/master/tutorials/unsupervised/anomaly/anomaly.R.md

mnist_train.hex = h2o.importFile(localH2O, path = "C:/Users/Documents/Data/H20/mnist_train.csv", key = "mnist_train.hex")
mnist_test.hex = h2o.importFile(localH2O, path = "C:/Users/Documents/Data/H20/mnist_test.csv", key = "mnist_test.hex")

predictors = c(1:784)
resp = 785

mnist_train.hex <- mnist_train.hex[,-resp]
mnist_test.hex <- mnist_test.hex[,-resp]

ae_model <- h2o.deeplearning(x=predictors,
                             y=42, #response (ignored - pick any non-constant column)
                             data=mnist_train.hex,
                             activation="Tanh",
                             autoencoder=T,
                             hidden=c(50),
                             ignore_const_cols=F,
                             epochs=1)
test_rec_error <- as.data.frame(h2o.anomaly(mnist_test.hex, ae_model))
test_features_deep <- h2o.deepfeatures(mnist_test.hex, ae_model, layer=1)
summary(test_features_deep)



plotDigit <- function(mydata, rec_error) {
  len<-nrow(mydata)
  N<-ceiling(sqrt(len))
  op <- par(mfrow=c(N,N),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
  for (i in 1:nrow(mydata)) {
    colors<-c('white','black')
    cus_col<-colorRampPalette(colors=colors)
    z<-array(mydata[i,],dim=c(28,28))
    z<-z[,28:1]
    image(1:28,1:28,z,main=paste0("rec_error: ", round(rec_error[i],4)),col=cus_col(256))
  }
  on.exit(par(op))
}

plotDigits <- function(data, rec_error, rows) {
  row_idx <- order(rec_error[,1],decreasing=F)[rows]
  my_rec_error <- rec_error[row_idx,]
  my_data <- as.matrix(as.data.frame(data[row_idx,]))
  plotDigit(my_data, my_rec_error)
}

test_recon <- h2o.predict(ae_model, mnist_test.hex)
summary(test_recon)

plotDigits(test_recon, test_rec_error, c(1:25))
plotDigits(mnist_test.hex,   test_rec_error, c(1:25))

plotDigits(test_recon, test_rec_error, c(4988:5012))
plotDigits(mnist_test.hex,   test_rec_error, c(4988:5012))

plotDigits(test_recon, test_rec_error, c(9976:10000))
plotDigits(mnist_test.hex,   test_rec_error, c(9976:10000))


plotDigits(test_recon, test_rec_error, c(9991:10000))
plotDigits(mnist_test.hex,   test_rec_error, c(9991:10000))
