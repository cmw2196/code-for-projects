#Christian Wesselhoeft
#Econ 4913 Seminar Project

#Structure of Code:
#i) WTI small dataset models
#ii) Brent small dataset models
#iii) WTI Large dataset models
#iv) WTI Large dataset models
#v) Trading models (all the way at the bottom)
#vi) Experimentaion with VIX data

#Note that most of the code from dataset to dataset is the same, just with changed variables (I probably could have used
#a loop but some models took far longer than others to run, especially depending on numbers of parameters.)



#WTI SMALL DATASET#################################################
###################################################################
###################################################################



#Z score normalization#########################################
#normalize each column before running models (using 1) z score, 2)

library(som)
library(readxl)
mldata =  read_excel("C:\\Users\\Christian W\\Desktop\\BDMLfinal.xlsx", col_names = T, col_types = NULL)

#Use the following commands to create all 64 choose 2 interaction terms


#mldata.inter = mldata[,4:68]
#interbaby= t(apply(mldata.inter, 1, combn, 2, prod))
#colnames(interbaby) = paste("Inter.V", combn(1:65, 2, paste, collapse="V"), sep="")
#mldata = cbind(mldata,interbaby)


#Normalized Data
sd.data = c(mean(mldata[,2]),sd(mldata[,2]))
sd.datab = c(mean(mldata[,3]), sd(mldata[,3]))
true.ywti = mldata[,2]

znorm = normalize(mldata[,2:199], byrow = F)
zw.train = data.frame(y = znorm[1:235, 1], x = znorm[1:235, 3:198])
zw.test = data.frame(y = znorm[236:307, 1], x = znorm[236:307, 3:198])
zw.trainb = data.frame(y = znorm[1:235, 2], x = znorm[1:235, 3:198])
zw.testb = data.frame(y = znorm[236:307, 2], x = znorm[236:307, 3:198])

#An unrestricted multiple regression model
m.reg.model = lm(y~., data = zw.train)
summary(m.reg.model)

#The predicted training prices for the multiple regression model 
yhat = predict(m.reg.model)
train_mse = sum((zw.train$y-yhat)**2) * (1/nrow(zw.train))

#The predicted prices for the test set of the multiple regression model
umr.pred = predict(m.reg.model, newdata = zw.test)
umrpred.f = rbind(matrix(m.reg.model$fitted.values), matrix(umr.pred))

#The test MSE of the unrestricted model is enormous
umr.testmse = sum((zw.test$y-umr.pred)**2) * (1/nrow(zw.test))

print(umr.testmse)


#The multiple regresson model with 5 lags
library(leaps)

#Using validation select the optimal number of parameters
#Note that forward selection returns the lowest or equal MSE estimate (better than backwards/hybrid)
m.reg.best = regsubsets(y~., data = zw.train,  really.big = T, method = "forward", nvmax = 190)
testmat = model.matrix(y~., data = zw.test)
trainmat = model.matrix(y~., data = zw.train)
errors = rep(NA,200)

#Using validation we find MSE for models with 1:190 parameters
for(i in 1:190){
  coef.i = coef(m.reg.best, id = i)
  rmr.pred = testmat[,names(coef.i)]%*%coef.i
  errors[i] = sum((zw.test$y - rmr.pred)**2) * (1/nrow(zw.test))
}

errors

#The mean squared error of the best model
rmr.mse = errors[which.min(errors)]
rmr.mse

#The coefficients and predicted spot WTI spot prices of our best multiple regression model
best.coef = coef(m.reg.best,which.min(errors))
train.rmrpred = trainmat[,names(best.coef)]%*%best.coef
best.rmrpred = testmat[,names(best.coef)]%*%best.coef
rmrpredf = rbind(matrix(train.rmrpred), matrix(best.rmrpred))


#Restricted Multiple Regression MSE (same as above but with accessible coefficients)
xrmrtr = data.frame(cbind(zw.train$y,trainmat[,names(best.coef)]))
xrmrtest = data.frame(cbind(zw.test$y,testmat[,names(best.coef)]))
rmr.model = lm(V1~., data = xrmrtr)

rmrtest = predict(rmr.model, newdata=xrmrtest[,2:5])
rmr.testmse = sum((zw.test$y-rmrtest)**2) * (1/nrow(zw.test))


ggdata = data.frame(mldata$Date, znorm[,1], rmrpredf, umrpred.f)
colnames(ggdata) = c("Date", "WTI SP", "Rest. MR", "Unres. MR")

library(ggplot2)
library(reshape2)
library(scales)

ggdata <- melt(ggdata, id="Date")
pdf("mrandumr.pdf")
ggplot(data = ggdata, aes(x = Date,y = value, colour = variable, group = variable)) + geom_line() + labs(title = "Comparison of MR WTI Spot Price Estimates and the True Values" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()

ggdatat = data.frame(mldata$Date[236:307], znorm[236:307,1], rmrtest)
colnames(ggdatat) = c("Date", "WTI SP", "Rest. MR")
ggdatat <- melt(ggdatat, id="Date")
pdf("mrandumr1.pdf")
ggplot(data = ggdatat, aes(x = Date,y = value, colour = variable, group = variable)) + geom_line() + labs(title = "Comparison of MR WTI Spot Price Estimates and the True Values" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()

#Ridge Regression########################################################################################

library(glmnet)
x = as.matrix(zw.train[,2:197])
y = as.matrix(zw.train[,1])
xtest = as.matrix(zw.test[,2:197])
ytest = as.matrix(zw.test[,1])

#Choosing our parameter lambda by cross validation
lambdaridge = matrix(0,800,1)
for(i in 1:800){
  cv.ridgelam = cv.glmnet(x,y, alpha = 0, nfolds =5)
  #plot(cv.ridgelam, main= "Mean Squared Error as a Function of Parameter Choice")
  lambdaridge[i] = cv.ridgelam$lambda.min
}
bestlambda = mean(lambdaridge)
plot(cv.glmnet(x,y, alpha = 0, nfolds =5), main= "Ridge MSE as a Function of Parameter Choice")

#Constructing the ridge regression model and finding MSE
#I believe there are many local minima, so I am trying the mode
r.reg.model = glmnet(x,y,alpha = 0 , lambda = 11.889)
ridge.pred = predict(r.reg.model, s = 11.889, newx = xtest)
ridge.mse = mean((ridge.pred - ytest)**2)

#ridge train + test predictions
r.pred.f = rbind(predict(r.reg.model, s = 11.889, newx = x), ridge.pred)

#Lasso Regression################################################################################

#Choosing our parameter lambda by cross validation 
lasso_lambda = matrix(0,50,1)
for(i in 1:50){
  cv.lassolam = cv.glmnet(x,y,alpha = 1, nfolds = 5)
  lasso_lambda[i] = cv.lassolam$lambda.min
}

bestlambda1 =  mean(lasso_lambda)
#Unfortuantely, there seem to be multiple local minima again, so I manually picked the mode/most sensical lambda

plot(cv.glmnet(x,y,alpha = 1, nfolds = 5), main= "Lasso MSE as a Function of Parameter Choice")

#Constructing the lasso regression model and finding MSE
lasso.reg.model = glmnet(x,y,alpha = 1 , lambda = .09)
lasso.pred = predict(lasso.reg.model, s = .09, newx = xtest)
lasso.mse = mean((lasso.pred - ytest)**2)

lasso.pred.f = rbind(predict(lasso.reg.model, s = .09, newx = x), lasso.pred)


##HYBRID Regression--only for WTI--#################################

#Choosing our parameter lambda by cross validation
lambdar = matrix(0,50,1)
for(i in 1:50){
  cv.hyblam = cv.glmnet(x,y,alpha = .75, nfolds = 5)
  lambdar[i] = cv.hyblam$lambda.min
}
bestlambda2 = mean(lambdar)

#Constructing the Hybrid regression model and finding MSE
hyb.reg.model = glmnet(x,y,alpha = .75, lambda = .12)
hyb.pred = predict(hyb.reg.model, s = .12, newx = xtest)
hyb.mse = mean((hyb.pred - ytest)**2)




#It appears that Lasso regression, then ridge, then restricted MR are performing the best
#This is the graph of all regression (ridge, lasso, normalized and restricted MR) on the test and training set

ggdata2 = data.frame(mldata$Date, znorm[,1], rmrpredf,r.pred.f, lasso.pred.f)
colnames(ggdata2) = c("Date","WTI SP", "Rest. MR","Ridge Reg", "Lasso Reg")
ggdata2 <- melt(ggdata2, id="Date")
pdf("RegressionEstimatePredictions.pdf")
ggplot(data = ggdata2, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "Regression Predictions" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()


#Our Regression MSE; 
Regress.mse = cbind(umr.testmse,rmr.mse, ridge.mse, lasso.mse, hyb.mse)

# A closer look at the estimates and just our test data
testml = mldata$Date[236:307]
testp = znorm[236:307,1]
ggtestdata = data.frame(testml, testp, ridge.pred, lasso.pred, as.matrix(rmrtest))
colnames(ggtestdata) = c("testml", "WTI SP", "Ridge Reg", "Lasso", "Rest. MR")
ggtestdata <- melt(ggtestdata, id="testml")
pdf("RMRRidgeTest.pdf")
ggplot(data = ggtestdata, aes(x = testml,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Regression Predictions over Test Data (Ridge and Multiple Regression with 6 Selected Variables) " , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()

#PCR Regression#####################################################################################
library(pls)
pcr.model = pcr(y~., data = zw.train, scale = F, validation ="CV")
validationplot(pcr.model, val.type ="MSEP", main = "MSEP as a function of PCR Components")

#It appears that using 21 components yields the lowest cv MSE
pcr.mse1 = matrix(0,180,1)
for(i in 1:180){
  pcr.pred = predict(pcr.model, xtest, ncomp = i)
  pcr.pred = matrix(pcr.pred)
  pcr.predf = rbind(matrix(predict(pcr.model, x, ncomp = i)), pcr.pred)
  pcr.mse1[i] = sum((zw.test$y - pcr.pred)**2) * (1/nrow(zw.test))
}
pcr.mse = min(pcr.mse1)
pcr.pred = predict(pcr.model, xtest, ncomp = which.min(pcr.mse1))
pcr.pred = matrix(pcr.pred)
pcr.predf = rbind(matrix(predict(pcr.model, x, ncomp = which.min(pcr.mse1))), pcr.pred)
pcr.msef = mean((pcr.pred - ytest)**2)

#PLS Regresion########################################
pls.model = plsr(y~., data= zw.train, scale = F, validation ="CV")
validationplot(pls.model, val.type ="MSEP", main = "MSEP as a function of PLS Components")

#It appears that using 2 components yields the lowest cv MSE
pls.mse1 = matrix(0,150,1)
for(i in 1:150){
  pls.pred = predict(pls.model, xtest, ncomp = i)
  pls.pred = matrix(pls.pred)
  pls.predf = rbind(matrix(predict(pls.model, x, ncomp = i)), pls.pred)
  pls.mse1[i] = mean((pls.pred - ytest)**2)
}
pls.mse = min(pls.mse1)
pls.pred = predict(pls.model, xtest, ncomp = which.min(pls.mse1))
pls.pred = matrix(pls.pred)
pls.predf = rbind(matrix(predict(pls.model, x, ncomp = which.min(pls.mse1))), pls.pred)
pls.msef = mean((pls.pred - ytest)**2)


#Our Regression MSE; note that it is lowest for PLS
Regress.mse = cbind(umr.testmse,rmr.mse, ridge.mse, lasso.mse, hyb.mse, pcr.mse, pls.mse)
Regress.mse

ggdata3 = data.frame(mldata$Date, znorm[,1],pcr.predf, pls.predf)
colnames(ggdata3) = c("Date","WTI SP", "PCR", "PLS")
ggdata3 <- melt(ggdata3, id="Date")
pdf("PCRPLRidgeMR.pdf")
ggplot(data = ggdata3, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Predictions: PCR,PLS, Rest. MR and Ridge Reg" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


# A closer look at the estimates and just our test data
ggtestdata1 = data.frame(testml, testp, pcr.pred, pls.pred)
colnames(ggtestdata1) = c("Date","WTI SP","PCR", "PLS")
ggtestdata1 <- melt(ggtestdata1, id="Date")
pdf("PCRPLSRidgeMRTEST.pdf")
ggplot(data = ggtestdata1, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "PCR and PLS Predictions on WTI Test Data " , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[217]))
dev.off()


#Regression Trees#######################################################################
#Not actually using this in the final project, just for curiosity
library(caret)
library(randomForest)
library(tree)

dfxtest = data.frame(xtest)
tree.model1 = tree(y~., data = zw.train)
cv.tree.model1 = cv.tree(tree.model1)
plot(cv.tree.model1)
prune.tree = prune.tree(tree.model1, best = 4)
tree.pred = predict(prune.tree, dfxtest )
tree.mse = mean((tree.pred - ytest)**2)

#Random Forest########################################
rf.mse1 = matrix(0,40,25)
for(i in 5:40){
  for(b in 5:25){
    rf.model = randomForest(y~., data = zw.train, mtry =20 + i, importance = T, nodesize = b)
    rf.pred = predict(rf.model, dfxtest)
    rf.mse1[i,b] = mean((rf.pred - ytest)**2)
  }
}
rf.model = randomForest(y~., data = zw.train, importance = T)
rf.pred = predict(rf.model, xtest)
rd.predf = rbind(matrix(predict(rf.model, x)), as.matrix(rf.pred))
rf.mse= mean((rf.pred - ytest)**2)
varImpPlot(rf.model, main = "Random Forest Variable Importance Plot")



##Feed Forward Neural Network################################################################
library(nnet)

b= c(5)
nn.cv.mse5 = matrix(0,200,length(b))
sizeparam = c(100,200)

for(i in sizeparam){
  for(a in 1:length(b)){
    nnet.model = nnet(y~.,data = zw.train, size = i, linout=TRUE,decay = b[a], maxit = 10000, MaxNWts = 1000000) 
    nnet.pred = predict(nnet.model, xtest)
    nn.cv.mse5[i,a] = sum((nnet.pred - ytest)**2)/nrow(ytest)
  }
}

#Clearly, the optimal parameters are size = 120, decay = 5, based on MSE-minimizing cross validation


nnet.final = nnet(y~.,data = zw.train, size = 120, linout=TRUE,decay = 5, maxit = 1000, MaxNWts = 100000) 
nnet.final.pred = predict(nnet.final, xtest)
nnet.final.mse1= sum((nnet.final.pred - ytest)**2)/nrow(xtest)



# A hypothetical neural network selecting only parameters which have large correlations with the change of WTI spot price
#The MSE is 3.07--uite terrible
cormat= cor(zw.train[,1],zw.train[,2:196])>.1
cormat2 = zw.train[,cormat==T]

#nnet.final = nnet(y~.,data = cormat2, size = 300, linout=TRUE,decay = 5, maxit = 1000, MaxNWts = 100000) 
#nnet.final.pred = predict(nnet.final, xtest)
#nnet.final.mse1= sum((nnet.final.pred - ytest)**2)/nrow(xtest)

#Constructing the final neural network prediction
nnet.train.pred = predict(nnet.final, x)
nnet.pred.f = rbind(matrix(nnet.train.pred), matrix(nnet.final.pred))


#Plotting the Neural Net
library(clusterGeneration)
library(nnet)
library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
pdf('nnet.pdf', width = 7, height = 7)
plot.nnet(nnet.final, alpha.val = 0.5, circle.col = list('lightgray', 'white'), bord.col = 'black')
dev.off()
#This is a shit show use the smaller nnet

#Our Final Collection of MSE; note that it is lowest for the model selected MR
Regress.mse = cbind(umr.testmse,rmr.mse, ridge.mse, lasso.mse, hyb.mse, pcr.mse, pls.mse, nnet.final.mse1, rf.mse)
ggdata4 = data.frame(mldata$Date, znorm[,1], pcr.predf, pls.predf, lasso.pred.f, nnet.pred.f)
colnames(ggdata4) = c("Date","WTISP","PCR", "PLS", "Lasso" ,"NeuralNet")
ggdata4 <- melt(ggdata4, id="Date")

pdf("RidgePCRPLSMRANN.pdf")
ggplot(data = ggdata4, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Predictions (ANN, MR, Ridge, PCR/PLS)" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


# A closer look at the estimates and just our test data
ggtestdata2 = data.frame(testml, testp, pls.pred, lasso.pred, pcr.pred, nnet.final.pred)
colnames(ggtestdata2) = c("Date","WTI SP", "PLS", "Lasso", "PCR", "NeuralNet")
ggtestdata2 <- melt(ggtestdata2, id="Date")

#The best 5 models on our test data
pdf("ANNMRPCRPLSTest.pdf")
ggplot(data = ggtestdata2, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Models: ANN, PCR, PLS, Lasso Reg." , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


ggnet = data.frame(testml, testp, nnet.final.pred, pls.pred)
colnames(ggnet) = c("Date","WTI SP", "ANN", "PLS")
ggnet <- melt(ggnet, id="Date")

#The best 2 models for WTI
pdf("ANNMRPLSPredictions.pdf")
ggplot(data = ggnet, aes(x = Date,y = value, colour = variable, group = variable)) + geom_line() + labs(title = "The Best Two Models: ANN and PLS" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


########################Forecasting Brent Crude Spot Prices####################################
############################################################################################
xb = as.matrix(zw.trainb[,2:197])
yb = as.matrix(zw.trainb[,1])
xtestb = as.matrix(zw.testb[,2:197])
ytestb = as.matrix(zw.testb[,1])


#An unrestricted multiple regression model
m.reg.modelb = lm(y~., data = zw.trainb)
summary(m.reg.modelb)

#The predicted prices for the test set of the multiple regression model
umr.predb = predict(m.reg.modelb, newdata = zw.testb)
umrpred.fb = rbind(matrix(m.reg.modelb$fitted.values), matrix(umr.predb))

#The test MSE of the unrestricted model is enormous
umr.testmseb = sum((zw.testb$y-umr.predb)**2) * (1/nrow(ytestb))

print(umr.testmseb)


#The multiple regresson model with 5 lags
library(leaps)

#Using validation select the optimal number of parameters
#Note that forward selection returns the lowest or equal MSE estimate (better than backwards/hybrid)
m.reg.bestb = regsubsets(y~., data = zw.trainb,  really.big = T, method = "backward", nvmax = 150)
testmatb = model.matrix(y~., data = zw.testb)
trainmatb = model.matrix(y~., data = zw.trainb)
errorsb = rep(NA,150)

#Using validation we find MSE for models with 1:68 parameters
for(i in 1:150){
  coef.i = coef(m.reg.bestb, id = i)
  rmr.pred = testmat[,names(coef.i)]%*%coef.i
  errorsb[i] = sum((ytestb - rmr.pred)**2) * (1/nrow(ytestb))
}

errorsb

#The mean squared error of the best model
rmr.mseb = errorsb[which.min(errorsb)]
rmr.mseb

best.coefb = coef(m.reg.bestb,which.min(errorsb))
train.rmrpredb = trainmatb[,names(best.coefb)]%*%best.coefb
best.rmrpredb = testmatb[,names(best.coefb)]%*%best.coefb
rmrpredfb = rbind(matrix(train.rmrpredb), matrix(best.rmrpredb))


#Restricted Multiple Regression MSE (same as above but with accessible coefficients)
xrmrtrb = data.frame(cbind(zw.trainb[,1],trainmatb[,names(best.coefb)]))
xrmrtestb = data.frame(cbind(ytestb,testmatb[,names(best.coefb)]))
rmr.modelb = lm(V1~., data = xrmrtrb)

rmrtrainb = predict(rmr.modelb, newdata = xrmrtrb)
rmrtestb = predict(rmr.modelb, newdata=xrmrtestb[,2:8])
rmr.testmseb = sum((ytestb-rmrtestb)**2) * (1/nrow(ytestb))
rmr.predb = rbind(matrix(rmrtrainb), matrix(rmrtestb))


ggdata = data.frame(mldata$Date, znorm[,2], rmr.predb, umrpred.fb)
colnames(ggdata) = c("Date", "Brent SP", "Rest. MRb", "Unres. MRb")

library(ggplot2)
library(reshape2)
library(scales)

ggdata <- melt(ggdata, id="Date")
pdf("mrandumrB.pdf")
ggplot(data = ggdata, aes(x = Date,y = value, colour = variable, group = variable)) + geom_line() + labs(title = "Comparison of MR Brent Spot Price Estimates and the True Values" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()

#Ridge Regression########################################################################################

library(glmnet)


#Choosing our parameter lambda by cross validation
bestlambdar = matrix(0,100,1)
for(i in 1:100){
  cv.ridgelamb = cv.glmnet(xb,yb,alpha = 0, nfolds =5)
  
  bestlambdar[i] = cv.ridgelamb$lambda.min
}
bestlambdab = mean(bestlambdar)
plot(cv.ridgelamb, main= "Mean Squared Error as a Function of Parameter Choice (Brent)")


#Constructing the ridge regression model and finding MSE
r.reg.modelb = glmnet(xb,yb,alpha = 0 , lambda = 18)
ridge.predb = predict(r.reg.modelb, s = 18, newx = xtestb)
r.pred.b = rbind(predict(r.reg.modelb, s = 18, newx = xb), ridge.predb)
ridge.mseb = mean((ridge.predb - ytestb)**2)


#Lasso Regression################################################################################

#Choosing our parameter lambda by cross validation
bestlambdar = matrix(0,100,1)
for(i in 1:100){
  cv.lassolamb = cv.glmnet(xb,yb,alpha = 1, nfolds = 5)
  #plot(cv.lassolamb)
  bestlambdar[i] = cv.lassolamb$lambda.min
}

bestlambda1b = mean(bestlambdar)

#Constructing the lasso regression model and finding MSE
lasso.reg.modelb = glmnet(xb,yb,alpha = 1 , lambda = .08)
lasso.predb = predict(lasso.reg.modelb, s = .08, newx = xtestb)
lasso.pred.b = rbind(predict(lasso.reg.modelb, s = .08, newx = xb), lasso.predb)
lasso.mseb = mean((lasso.predb - ytestb)**2)

#Graphing the Regressions#########################################################################################
ggdata2 = data.frame(mldata$Date, znorm[,2], rmr.predb,lasso.pred.b, r.pred.b)
colnames(ggdata2) = c("Date","Brent SP", "Rest. MR", "Lasso Reg","Ridge Reg")
ggdata2 <- melt(ggdata2, id="Date")
pdf("RegressionEstimatePredictionsbrent.pdf")
ggplot(data = ggdata2, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "Regression Predictions" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()


#Our Regression MSE for regularized regressions
Regress.mseb = cbind(umr.testmseb,rmr.mseb, ridge.mseb, lasso.mseb )


# A closer look at the estimates and just our test data
testml = mldata$Date[236:307]
testp = znorm[236:307,2]
ggtestdata = data.frame(testml, testp, best.rmrpred, lasso.predb)
colnames(ggtestdata) = c("testml", "Brent SP", "Rest. MR","Lasso")
ggtestdata <- melt(ggtestdata, id="testml")
pdf("RMRRidgeScLassoBrent.pdf")
ggplot(data = ggtestdata, aes(x = testml,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Regression Predictions over Test Data: Lasso, MR " , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()




#PCR Regression###########################################
library(pls)
pcr.modelb = pcr(yb~xb, scale = F, validation ="CV")
validationplot(pcr.modelb, val.type ="MSEP", main = "Brent MSE as a Function of PCR Components")

#It appears that using 34 components yields the lowest cv MSE
pcr.mse1b = matrix(0,150,1)
for(i in 1:150){
  pcr.predb = predict(pcr.modelb, xtestb, ncomp = i)
  pcr.predb = matrix(pcr.predb)
  pcr.pred.fb = rbind(matrix(predict(pcr.modelb, xb, ncomp = i)), pcr.predb)
  pcr.mse1b[i] = mean((pcr.predb - ytestb)**2)
}
pcr.mseb = min(pcr.mse1b)
pcr.predb = predict(pcr.modelb, xtestb, ncomp = which.min(pcr.mse1b))
pcr.predb = matrix(pcr.predb)
#pls.predf = rbind(matrix(predict(pls.model, x, ncomp = which.min(pls.mse1)), pls.pred))
pcr.msefb = mean((pcr.predb - ytestb)**2)

#PLS Regresion########################################
pls.modelb = plsr(yb~xb, scale = F, validation ="CV")
validationplot(pls.modelb, val.type ="MSEP", main = "Brent MSE as a Function of PLS Components")

#It appears that using 28 components yields the lowest cv MSE

pls.mseb = matrix(0,150,1)
for(i in 1:150){
  pls.predb = predict(pls.modelb, xtestb, ncomp = i)
  pls.predb = matrix(pls.predb)
  pls.pred.fb = rbind(matrix(predict(pls.modelb, xb, ncomp = i)), pls.predb)
  pls.mseb[i] = mean((pls.predb - ytestb)**2)
}

pls.msebb = min(pls.mseb)
pls.predb = predict(pls.modelb, xtestb, ncomp = which.min(pls.mseb))
pls.predb = matrix(pls.predb)
pls.msefb = mean((pls.predb - ytestb)**2)


#Our Regression MSE; note that it is lowest for the model selected MR
Regress.mseb = cbind(umr.testmseb,rmr.mseb, ridge.mseb, lasso.mseb, pcr.mseb, pls.mseb)
Regress.mseb


ggdata3b = data.frame(testml, testp, best.rmrpredb,lasso.predb, pcr.predb, pls.predb)
colnames(ggdata3b) = c("Date","Brent SP", "Rest. MR","Lasso Reg", "PCR", "PLS")
ggdata3b <- melt(ggdata3b, id="Date")
pdf("PCRPLLassoMRBRENTTEST.pdf")
ggplot(data = ggdata3b, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Brent Predictions: PCR,PLS, Rest. MR and Lasso Reg on Test Data" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


library(caret)
library(randomForest)
library(tree)


tree.model1 = tree(y~., data = zw.train)
cv.tree.model1 = cv.tree(tree.model1)
plot(cv.tree.model1)
prune.tree = prune.tree(tree.model1, best = 5)
tree.pred = predict(prune.tree, dfxtest)

#Random Forest########################################
rf.modelb = randomForest(y~., data = zw.train, mtry =25, importance = T, nodesize = 15)
rf.predb = predict(rf.modelb, dfxtest)
rf.mseb = mean((rf.predb - ytestb)**2)

varImpPlot(rf.modelb, main = "Random Forest Variable Importance Plot, Brent Crude")
#Random Forests return an extremely poor result but give us some idea of variable importance




##Feed Forward Neural Network################################################################
library(nnet)

#Clearly, the optimal parameters are size = 13, decay = .02, based on MSE-minimizing cross validation
nnet.finalb = nnet(y~.,data = zw.trainb, size = 50, linout=TRUE,decay = 5, MaxNWts = 10000, maxit = 100000) 
nnet.final.predb = predict(nnet.finalb, zw.testb)
nnet.final.mseb = sum((nnet.final.predb - ytestb)**2)/nrow(ytestb)


#Constructing the final neural network prediction
nnet.train.predb = predict(nnet.finalb, zw.trainb)
nnet.pred.b = rbind(matrix(nnet.train.predb), matrix(nnet.final.predb))


nnet.example = nnet(y~.,data = zw.trainb, size = 5, linout=TRUE,decay = 5, MaxNWts = 10000, maxit = 100000) 

pdf('nnet.pdf', width = 7, height = 7)
plot.nnet(nnet.example, alpha.val = 0.5, circle.col = list('lightgray', 'white'), bord.col = 'black')
dev.off()

# A closer look at the best estimates and just our test data
ggdata4b = data.frame(testml, testp, best.rmrpredb,lasso.predb, pcr.predb, nnet.final.predb)
colnames(ggdata4b) = c("Date","Brent SP", "Rest. MR","Lasso Reg", "PCR",  "ANN")
ggdata4b <- melt(ggdata4b, id="Date")
pdf("SemifinalTestbrent.pdf")
ggplot(data = ggdata4b, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Brent Predictions: PCR,PLS, Rest. MR and Lasso Reg on Test Data" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()



#The best models for brent crude
ggnet = data.frame(testml, testp, best.rmrpredb, nnet.final.predb)
colnames(ggnet) = c("Date","Brent SP", "Rest MR", "ANN")
ggnet <- melt(ggnet, id="Date")
pdf("PCRPLSbesttestbrent.pdf")
ggplot(data = ggnet, aes(x = Date,y = value, colour = variable, group = variable)) + geom_line() + labs(title = "The Best Two Brent Models: Rest. MR and ANN" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


########################################################################
#Dataset 2##############################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

#This is the same dataset, except it will use all 68 choose 2 interaction terms. Note that the vast majority of the code
# is simply copied from above and most variable names changed.



#Z score normalization#########################################
###########################################################

#normalize each column before running models (using 1) z score, 2) min-max of WTI/brent spot prices

#should the brent and wti prices be changed to changes from the last period or an upward moving long term trend???

#check neural network size, clean up code and remove hybrid regression

# do 2 MR-- one with polynomials, one without (maybe one with squares, one with squares and cubes?)




library(som)
library(readxl)
mldata2 =  read_excel("C:\\Users\\Christian W\\Desktop\\BDMLfinal.xlsx", col_names = T, col_types = NULL)
mldata.inter = mldata2[,4:69]
interbaby= t(apply(mldata.inter, 1, combn, 2, prod))
colnames(interbaby) = paste("Inter.V", combn(1:66, 2, paste, collapse="V"), sep="")
mldata2 = cbind(mldata,interbaby)
n = ncol(mldata) -1

#Normalized Data
sd.data = c(mean(mldata2[,2]),sd(mldata[,2]))
true.ywti1 = mldata[,3]
znorm = normalize(mldata2[,2:2344], byrow = F)
zw.train = data.frame(y = znorm[1:235, 1], x = znorm[1:235, 3:2343])
zw.test = data.frame(y = znorm[236:307, 1], x = znorm[236:307, 3:2343])
zw.trainb = data.frame(y = znorm[1:235, 2], x = znorm[1:235, 3:2343])
zw.testb = data.frame(y = znorm[236:307, 2], x = znorm[236:307, 3:2343])

#An unrestricted multiple regression model
m.reg.model = lm(y~., data = zw.train)
summary(m.reg.model)

#The predicted training prices for the multiple regression model 
yhat = predict(m.reg.model)
train_mse = sum((zw.train$y-yhat)**2) * (1/nrow(zw.train))

#The predicted prices for the test set of the multiple regression model
umr.pred = predict(m.reg.model, newdata = zw.test)
umrpred.f = rbind(matrix(m.reg.model$fitted.values), matrix(umr.pred))

#The test MSE of the unrestricted model is enormous
umr.testmse = sum((zw.test$y-umr.pred)**2) * (1/nrow(zw.test))

print(umr.testmse)


#The multiple regresson model with 5 lags
library(leaps)

#Using validation select the optimal number of parameters
#Note that backward selection returns the lowest or equal MSE estimate (better than forward/hybrid)
m.reg.best = regsubsets(y~., data = zw.train,  really.big = T, method = "backward", nvmax = 500)
testmat = model.matrix(y~., data = zw.test)
trainmat = model.matrix(y~., data = zw.train)
errors = rep(NA,500)

#Using validation we find MSE for models with 1:500 parameters
for(i in 1:40){
  coef.i = coef(m.reg.best, id = i)
  rmr.pred = testmat[,names(coef.i)]%*%coef.i
  errors[i] = sum((zw.test$y - rmr.pred)**2) * (1/nrow(zw.test))
}

errors

#The mean squared error of the best model
rmr.mse = errors[which.min(errors)]
rmr.mse

#The coefficients and predicted spot WTI spot prices of our best multiple regression model
best.coef = coef(m.reg.best,which.min(errors))
best.coef = coef(m.reg.best, id = 12)
train.rmrpred = trainmat[,names(best.coef)]%*%best.coef
best.rmrpred = testmat[,names(best.coef)]%*%best.coef
rmrpredf = rbind(matrix(train.rmrpred), matrix(best.rmrpred))


#Restricted Multiple Regression MSE (same as above but with accessible coefficients)
xrmrtr = data.frame(cbind(zw.train$y,trainmat[,names(best.coef)]))
xrmrtest = data.frame(cbind(zw.test$y,testmat[,names(best.coef)]))
rmr.model = lm(V1~., data = xrmrtr)

rmrtest = predict(rmr.model, newdata=xrmrtest[,2:14])
rmr.testmse = sum((zw.test$y-rmrtest)**2) * (1/nrow(zw.test))


ggdata = data.frame(mldata$Date, znorm[,1], rmrpredf, umrpred.f)
colnames(ggdata) = c("Date", "WTI SP", "Rest. MR", "Unres. MR")

library(ggplot2)
library(reshape2)
library(scales)

ggdata <- melt(ggdata, id="Date")
pdf("mrandumr2000.pdf")
ggplot(data = ggdata, aes(x = Date,y = value, colour = variable, group = variable)) + geom_line() + labs(title = "Comparison of MR WTI Spot Price Estimates and the True Values (2000)" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()

ggdatat = data.frame(mldata$Date[236:307], znorm[236:307,1], rmrtest)
colnames(ggdatat) = c("Date", "WTI SP", "Rest. MR")
ggdatat <- melt(ggdatat, id="Date")
pdf("mrandumr1-2000.pdf")
ggplot(data = ggdatat, aes(x = Date,y = value, colour = variable, group = variable)) + geom_line() + labs(title = "Comparison of MR WTI Spot Price Estimates and the True Values (2000)" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()

#Ridge Regression########################################################################################

library(glmnet)
x = as.matrix(zw.train[,2:2342])
y = as.matrix(zw.train[,1])
xtest = as.matrix(zw.test[,2:2342])
ytest = as.matrix(zw.test[,1])

#Choosing our parameter lambda by cross validation
lambdaridge = matrix(0,10,1)
for(i in 1:10){
  cv.ridgelam = cv.glmnet(x,y, alpha = 0, nfolds =5)
  #plot(cv.ridgelam, main= "Mean Squared Error as a Function of Parameter Choice")
  lambdaridge[i] = cv.ridgelam$lambda.min
}
bestlambda = mean(lambdaridge)
plot(cv.glmnet(x,y, alpha = 0, nfolds =5), main= "Ridge MSE as a Function of Parameter Choice (2000)")

#Constructing the ridge regression model and finding MSE
#I believe there are many local minima, so I am trying the mode
r.reg.model = glmnet(x,y,alpha = 0 , lambda = 31.38)
ridge.pred = predict(r.reg.model, s = 31.38, newx = xtest)
ridge.mse22 = mean((ridge.pred - ytest)**2)

#ridge train + test predictions
r.pred.f = rbind(predict(r.reg.model, s = 31.38, newx = x), ridge.pred)

#Lasso Regression################################################################################

#Choosing our parameter lambda by cross validation 
lasso_lambda = matrix(0,10,1)
for(i in 1:10){
  cv.lassolam = cv.glmnet(x,y,alpha = 1, nfolds = 5)
  lasso_lambda[i] = cv.lassolam$lambda.min
}

bestlambda1 =  mean(lasso_lambda)
#Unfortuantely, there seem to be multiple local minima again, so I manually picked the mode/most sensical lambda

plot(cv.glmnet(x,y,alpha = 1, nfolds = 5), main= "Lasso MSE as a Function of Parameter Choice (2000)")

#Constructing the lasso regression model and finding MSE
lasso.reg.model = glmnet(x,y,alpha = 1 , lambda = .135)
lasso.pred = predict(lasso.reg.model, s = .135, newx = xtest)
lasso.mse22 = mean((lasso.pred - ytest)**2)

lasso.pred.f = rbind(predict(lasso.reg.model, s = .135, newx = x), lasso.pred)
lparams = as.matrix(coef(lasso.reg.model))
rnumber = row(lparams)[which(lparams != 0)]
lparams1 = lparams[rnumber,]

##HYBRID Regression--only for WTI--#################################

#Choosing our parameter lambda by cross validation
lambdar = matrix(0,10,1)
for(i in 1:10){
  cv.hyblam = cv.glmnet(x,y,alpha = .75, nfolds = 5)
  lambdar[i] = cv.hyblam$lambda.min
}
bestlambda2 = mean(lambdar)

#Constructing the Hybrid regression model and finding MSE
hyb.reg.model = glmnet(x,y,alpha = .75, lambda = .23339)
hyb.pred = predict(hyb.reg.model, s = .23339, newx = xtest)
hyb.mse22 = mean((hyb.pred - ytest)**2)




#It appears that Lasso regression, then ridge, then restricted MR are performing the best
#This is the graph of all regression (ridge, lasso, normalized and restricted MR) on the test and training set

ggdata2 = data.frame(mldata$Date, znorm[,2], rmrpredf,r.pred.f, lasso.pred.f)
colnames(ggdata2) = c("Date","WTI SP", "Rest. MR","Ridge Reg", "Lasso Reg")
ggdata2 <- melt(ggdata2, id="Date")
pdf("RegressionEstimatePredictions2000.pdf")
ggplot(data = ggdata2, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "Regression Predictions" , x ="Dates, Intercept Marks Beginning of Test Period (2000)", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()


#Our Regression MSE; note that it is lowest for the model selected MR
Regress.mse = cbind(umr.testmse,rmr.mse, ridge.mse, lasso.mse, hyb.mse)

# A closer look at the estimates and just our test data
testml = mldata$Date[236:307]
testp = znorm[236:307,2]
ggtestdata = data.frame(testml, testp, ridge.pred, lasso.pred, as.matrix(rmrtest))
colnames(ggtestdata) = c("testml", "WTI SP", "Ridge Reg", "Lasso", "Rest. MR")
ggtestdata <- melt(ggtestdata, id="testml")
pdf("RMRRidgeTest2000.pdf")
ggplot(data = ggtestdata, aes(x = testml,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Regression Predictions over Test Data (2000) " , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()

#PCR Regression#####################################################################################
library(pls)
pcr.model = pcr(y~., data = zw.train, scale = F, validation ="CV")
validationplot(pcr.model, val.type ="MSEP", main = "MSEP as a function of PCR Components (2000)")

#It appears that using 21 components yields the lowest cv MSE
pcr.mse1 = matrix(0,210,1)
for(i in 1:210){
  pcr.pred = predict(pcr.model, xtest, ncomp = i)
  pcr.pred = matrix(pcr.pred)
  pcr.predf = rbind(matrix(predict(pcr.model, x, ncomp = i)), pcr.pred)
  pcr.mse1[i] = sum((zw.test$y - pcr.pred)**2) * (1/nrow(zw.test))
}
pcr.mse = min(pcr.mse1)
pcr.pred = predict(pcr.model, xtest, ncomp = which.min(pcr.mse1))
pcr.pred = matrix(pcr.pred)
pcr.predf = rbind(matrix(predict(pcr.model, x, ncomp = which.min(pcr.mse1))), pcr.pred)
pcr.msef = mean((pcr.pred - ytest)**2)

#PLS Regresion########################################
pls.model = plsr(y~., data= zw.train, scale = F, validation ="CV")
validationplot(pls.model, val.type ="MSEP", main = "MSEP as a function of PLS Components (2000)")

#It appears that using 2 components yields the lowest cv MSE
pls.mse1 = matrix(0,210,1)
for(i in 1:210){
  pls.pred = predict(pls.model, xtest, ncomp = i)
  pls.pred = matrix(pls.pred)
  pls.predf = rbind(matrix(predict(pls.model, x, ncomp = i)), pls.pred)
  pls.mse1[i] = mean((pls.pred - ytest)**2)
}
pls.mse = min(pls.mse1)
pls.pred = predict(pls.model, xtest, ncomp = which.min(pls.mse1))
pls.pred = matrix(pls.pred)
pls.predf = rbind(matrix(predict(pls.model, x, ncomp = which.min(pls.mse1))), pls.pred)
pls.msef = mean((pls.pred - ytest)**2)


#Our Regression MSE; note that it is lowest for the model selected MR
Regress.mse = cbind(umr.testmse,rmr.mse, ridge.mse, lasso.mse, hyb.mse, pcr.mse, pls.mse)
Regress.mse

ggdata3 = data.frame(mldata$Date, znorm[,2], rmrpredf, pcr.predf, pls.predf)
colnames(ggdata3) = c("Date","WTI SP", "Rest. MR","PCR", "PLS")
ggdata3 <- melt(ggdata3, id="Date")
pdf("PCRPLRidgeMR2000.pdf")
ggplot(data = ggdata3, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Predictions: PCR,PLS, Rest. MR and Ridge Reg (2000)" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


# A closer look at the estimates and just our test data
ggtestdata1 = data.frame(testml, testp, ridge.pred, pcr.pred)
colnames(ggtestdata1) = c("Date","WTI SP","Ridge Reg", "PCR")
ggtestdata1 <- melt(ggtestdata1, id="Date")
pdf("WTIRIDGEPCRONLY2000.pdf")
ggplot(data = ggtestdata1, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Predictions (2000 WTI): PCR and Ridge Reg on Test Data" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude") + geom_vline(xintercept = as.numeric(mldata$Date[217]))
dev.off()


#Regression Trees#######################################################################
#3 splits (ie: no pruning) is optimal for this regression tree

library(caret)
library(randomForest)
library(tree)

dfxtest = data.frame(xtest)
tree.model1 = tree(y~., data = zw.train)
cv.tree.model1 = cv.tree(tree.model1)
plot(cv.tree.model1)
prune.tree = prune.tree(tree.model1, best = 3)
tree.pred = predict(prune.tree, dfxtest )
tree.mse = mean((tree.pred - ytest)**2)

#Random Forest########################################
#Resorting to defaults because my computer cannot handle anything (Running a single RF model is taking 30+ mins)
rf.model = randomForest(y~., data = zw.train, importance = T)
rf.pred = predict(rf.model, xtest)
rd.predf = rbind(matrix(predict(rf.model, x)), as.matrix(rf.pred))
rf.mse= mean((rf.pred - ytest)**2)
varImpPlot(rf.model, main = "Random Forest Variable Importance Plot (2000)")











########################Forecasting Brent Crude Spot Prices####################################
############################################################################################
xb = as.matrix(zw.trainb[,2:2345])
yb = as.matrix(zw.trainb[,1])
xtestb = as.matrix(zw.testb[,2:2345])
ytestb = as.matrix(zw.testb[,1])
library(usdm)
library(car)
mccheck = lm(y~., data=zw.train)
mcb = vif(mccheck)
alias = alias( lm(y~., data=zw.train) )

#An unrestricted multiple regression model
m.reg.modelb = lm(y~., data = zw.trainb)
summary(m.reg.modelb)

#The predicted prices for the test set of the multiple regression model
umr.predb = predict(m.reg.modelb, newdata = zw.testb)
umrpred.fb = rbind(matrix(m.reg.modelb$fitted.values), matrix(umr.predb))

#The test MSE of the unrestricted model is enormous
umr.testmseb = sum((zw.testb$y-umr.predb)**2) * (1/nrow(ytestb))

print(umr.testmseb)


#The multiple regresson model with 5 lags
library(leaps)

#Using validation select the optimal number of parameters
#Note that forward selection returns the lowest or equal MSE estimate (better than backwards/hybrid)
m.reg.bestb = regsubsets(y~., data = zw.trainb,  really.big = T, method = "backward", nvmax = 500)
testmatb = model.matrix(y~., data = zw.testb)
trainmatb = model.matrix(y~., data = zw.trainb)
errorsb = rep(NA,500)

#Using validation we find MSE for models with 1:68 parameters
for(i in 1:500){
  coef.i = coef(m.reg.bestb, id = i)
  rmr.pred = testmat[,names(coef.i)]%*%coef.i
  errorsb[i] = sum((ytestb - rmr.pred)**2) * (1/nrow(ytestb))
}

errorsb

#The mean squared error of the best model
rmr.mseb22 = errorsb[which.min(errorsb)]
rmr.mseb22

best.coefb = coef(m.reg.bestb,which.min(errorsb))
train.rmrpredb = trainmatb[,names(best.coefb)]%*%best.coefb
best.rmrpredb = testmatb[,names(best.coefb)]%*%best.coefb
rmrpredfb = rbind(matrix(train.rmrpredb), matrix(best.rmrpredb))


#Restricted Multiple Regression MSE (same as above but with accessible coefficients)
xrmrtrb = data.frame(cbind(zw.trainb$y,trainmatb[,names(best.coefb)]))
xrmrtestb = data.frame(cbind(zw.testb$y,testmatb[,names(best.coefb)]))
rmr.modelb = lm(V1~., data = xrmrtrb)

rmrtrainb = predict(rmr.modelb, newdata = xrmrtrb)
rmrtestb = predict(rmr.modelb, newdata=xrmrtestb[,2:7])
rmr.testmseb = sum((ytestb-rmrtestb)**2) * (1/nrow(ytestb))
rmr.predb = rbind(matrix(rmrtrainb), matrix(rmrtestb))


ggdata = data.frame(mldata$Date, znorm$BRNTSP, rmr.predb, umrpred.fb)
colnames(ggdata) = c("Date", "Brent SP", "Rest. MRb", "Unres. MRb")

library(ggplot2)
library(reshape2)
library(scales)

ggdata <- melt(ggdata, id="Date")
pdf("mrandumrB2000.pdf")
ggplot(data = ggdata, aes(x = Date,y = value, colour = variable, group = variable)) + geom_line() + labs(title = "Comparison of MR Brent Spot Price Estimates and the True Values" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()

#Ridge Regression########################################################################################
#I will do three versions--two different packages, and RR with normalized data################
library(glmnet)


#Choosing our parameter lambda by cross validation
bestlambdar = matrix(0,10,1)
for(i in 1:10){
  cv.ridgelamb = cv.glmnet(xb,yb,alpha = 0, nfolds =5)
  
  bestlambdar[i] = cv.ridgelamb$lambda.min
}
bestlambdab = mean(bestlambdar)
plot(cv.ridgelamb, main= "Mean Squared Error as a Function of Parameter Choice (2000)")


#Constructing the ridge regression model and finding MSE
r.reg.modelb = glmnet(xb,yb,alpha = 0 , lambda =65)
ridge.predb = predict(r.reg.modelb, s = , newx = xtestb)
r.pred.b = rbind(predict(r.reg.model, s = , newx = xb), ridge.predb)
ridge.mseb = mean((ridge.predb - ytestb)**2)


#Lasso Regression################################################################################

#Choosing our parameter lambda by cross validation
bestlambdar = matrix(0,10,1)
for(i in 1:10){
  cv.lassolamb = cv.glmnet(xb,yb,alpha = 1, nfolds = 5)
  #plot(cv.lassolamb)
  bestlambdar[i] = cv.lassolamb$lambda.min
}

bestlambda1b = mean(bestlambdar)

#Constructing the lasso regression model and finding MSE
lasso.reg.modelb = glmnet(xb,yb,alpha = 1 , lambda = .182)
lasso.predb = predict(lasso.reg.modelb, s = .182, newx = xtestb)
lasso.pred.b = rbind(predict(lasso.reg.modelb, s = .182, newx = xb), lasso.predb)
lasso.mseb = mean((lasso.predb - ytestb)**2)

#Graphing the Regressions#########################################################################################
ggdata2 = data.frame(testml, mldata[,3], rmr.predb,lasso.pred.b, r.pred.b)
colnames(ggdata2) = c("Date","Brent SP", "Rest. MR", "Lasso Reg","Ridge Reg")
ggdata2 <- melt(ggdata2, id="Date")
pdf("RegressionEstimatePredictionsbrent2000.pdf")
ggplot(data = ggdata2, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "Regression Predictions" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[237]))
dev.off()


#Our Regression MSE for regularized regressions
Regress.mseb = cbind(umr.testmseb,rmr.mseb, ridge.mseb, ridge.mse.normb, lasso.mseb, l.mse.normb)


# A closer look at the estimates and just our test data
testml = mldata$Date[236:307]
testp = znorm[236:307,2]
ggtestdata = data.frame(testml, testp, best.rmrpred, lasso.predb)
colnames(ggtestdata) = c("testml", "Brent SP", "Rest. MR","Lasso")
ggtestdata <- melt(ggtestdata, id="testml")
pdf("RMRRidgeScLassoBrent2000.pdf")
ggplot(data = ggtestdata, aes(x = testml,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Regression Predictions over Test Data: Scaled Ridge, Lasso, MR " , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()




#PCR Regression###########################################
library(pls)
pcr.modelb = pcr(yb~xb, scale = F, validation ="CV")
validationplot(pcr.modelb, val.type ="MSEP")

#It appears that using 34 components yields the lowest cv MSE
pcr.mse1b = matrix(0,210,1)
for(i in 1:210){
  pcr.predb = predict(pcr.modelb, xtestb, ncomp = i)
  pcr.predb = matrix(pcr.predb)
  pcr.pred.fb = rbind(matrix(predict(pcr.modelb, xb, ncomp = i)), pcr.predb)
  pcr.mse1b[i] = mean((pcr.predb - ytestb)**2)
}
pcr.mseb = min(pcr.mse1b)
pcr.predb = predict(pcr.modelb, xtestb, ncomp = which.min(pcr.mse1b))
pcr.predb = matrix(pcr.predb)
#pls.predf = rbind(matrix(predict(pls.model, x, ncomp = which.min(pls.mse1)), pls.pred))
pcr.msefb = mean((pcr.predb - ytestb)**2)

#PLS Regresion########################################
pls.modelb = plsr(yb~xb, scale = F, validation ="CV")
validationplot(pls.modelb, val.type ="MSEP")

#It appears that using 28 components yields the lowest cv MSE

pls.mseb = matrix(0,210,1)
for(i in 1:210){
  pls.predb = predict(pls.modelb, xtestb, ncomp = i)
  pls.predb = matrix(pls.predb)
  pls.pred.fb = rbind(matrix(predict(pls.modelb, xb, ncomp = i)), pls.predb)
  pls.mseb[i] = mean((pls.predb - ytestb)**2)
}

pls.msebb = min(pls.mseb)
pls.predb = predict(pls.modelb, xtestb, ncomp = which.min(pls.mseb))
pls.predb = matrix(pls.predb)
pls.msefb = mean((pls.predb - ytestb)**2)


#Our Regression MSE; note that it is lowest for the model selected MR
Regress.mseb = cbind(umr.testmseb,rmr.mseb, ridge.mseb, lasso.mseb, l.mse.normb, pcr.mseb, pls.mseb)
Regress.mseb
testpb = znorm[261:307,2]

ggdata3b = data.frame(testml, testpb, best.rmrpredb,lasso.predb, pcr.predb, pls.predb)
colnames(ggdata3b) = c("Date","Brent SP", "Rest. MR","Lasso Reg", "PCR", "PLS")
ggdata3b <- melt(ggdata3b, id="Date")
pdf("PCRPLLassoMRBRENTTEST2000.pdf")
ggplot(data = ggdata3b, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Predictions: PCR,PLS, Rest. MR and Lasso Reg on Test Data" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


library(caret)
library(randomForest)
library(tree)

dfxtest = data.frame(xtest)
tree.model1 = tree(y~., data = zw.train)
cv.tree.model1 = cv.tree(tree.model1)
plot(cv.tree.model1)
prune.tree = prune.tree(tree.model1, best = 5)
tree.pred = predict(prune.tree, dfxtest)

#Random Forest########################################
rf.modelb = randomForest(y~., data = zw.train, importance = T, nodesize = 1)
rf.predb = predict(rf.modelb, dfxtest)
rf.mseb = mean((rf.predb - ytestb)**2)

varImpPlot(rf.modelb, main = "Random Forest Variable Importance Plot, Brent Crude (2000)")
#Random Forests return an extremely poor result but give us some idea of variable importance


testpb = zw.testb[,1]

ggdata4b = data.frame(testml, testpb, rf.predb, ridge.predb)
colnames(ggdata4b) = c("Date","Brent SP", "Random Forest", "Ridge Reg")
ggdata4b <- melt(ggdata4b, id="Date")
pdf("ONLYBRENT2000RIDGERF.pdf")
ggplot(data = ggdata4b, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "The Best Predictions: Ridge Reg and Random Forest Predictions on Brent Test Data" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()



######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
#Unscaled Model and Trading Models###########################################




wtipred.unscnnet = (nnet.final.pred* sd.data[2]) + sd.data[1]
ytest.unsc = (zw.test$y * sd.data[2]) + sd.data[1]
unsc.pred = cbind(wtipred.unscnnet,ytest.unsc)

unscaled = data.frame(testml, unsc.pred[,1], unsc.pred[,2])
colnames(unscaled) = c("Date", "ANN Unscaled Predictions", "Change in WTI Spot Price")
unscaled <- melt(unscaled, id="Date")
pdf("Tradingmodel1.pdf")
ggplot(data = unscaled, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "ANN Unscaled Predictions of Change in WTI Spot Price" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude, dollars") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()



brentpred.unsc = (rmrtestb* sd.datab[2]) + sd.datab[1]
ytest.unscb = (zw.testb$y * sd.datab[2]) + sd.datab[1]
brentpred.unsc1 = (rf.predb* sd.datab[2]) + sd.datab[1]
brentpred.unsc2 = (lasso.predb* sd.datab[2]) + sd.datab[1]
brentpredann = (nnet.final.predb* sd.datab[2]) + sd.datab[1]



unscaledb = data.frame(testml, unsc.predb[,1], unsc.predb[,2])
colnames(unscaledb) = c("Date", "Rest. MR Unscaled Predictions", "Change in Brent Spot Price")
unscaledb <- melt(unscaledb, id="Date")
pdf("Tradingmodel2MR.pdf")
ggplot(data = unscaledb, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "Restricted MR Predictions of Change in Brent Spot Price" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude, dollars") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


wtipls.unsc = (pls.pred* sd.data[2]) + sd.data[1]
wtipcr.unsc = (pcr.pred* sd.data[2]) + sd.data[1]
wtilasso.unsc = (lasso.pred* sd.data[2]) + sd.data[1]
wtiridge.unsc = (ridge.pred* sd.data[2]) + sd.data[1]
wtirmr.unsc = (rmrtest* sd.data[2]) + sd.data[1]
ytest.unsc = (zw.test$y * sd.data[2]) + sd.data[1]
wtipls = cbind(wtipls.unsc,ytest.unsc)

unscaledpls = data.frame(testml, wtipls[,1], wtipls[,2])
colnames(unscaledpls) = c("Date", "PLS unscaled Predictions", "Change in Brent Spot Price")
unscaledpls <- melt(unscaledpls, id="Date")
pdf("Tradingmodel3MR.pdf")
ggplot(data = unscaledpls, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "PLS Predictions of Change in WTI Spot Price" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude, dollars") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


unscaledridge = data.frame(testml, wtiridge.unsc[,1], ytest.unsc)
colnames(unscaledridge) = c("Date", "WTI Ridge Unscaled Predictions", "Change in Brent Spot Price")
unscaledridge <- melt(unscaledridge, id="Date")
pdf("Tradingmodel3ridge.pdf")
ggplot(data = unscaledridge, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "Ridge Predictions of Change in WTI Spot Price" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of Brent crude, dollars") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()

signdf = cbind(wtipls.unsc,wtipred.unscnnet,wtipcr.unsc,wtilasso.unsc,wtiridge.unsc, wtirmr.unsc, ytest.unsc)
cor.fin = cor(signdf)
signmat = sign(signdf)
samesignnum = c(sum(abs((signmat[,1]-signmat[,7])/2)), sum(abs((signmat[,2]-signmat[,7])/2)), sum(abs((signmat[,3]-signmat[,7])/2)),
                sum(abs((signmat[,4]-signmat[,7])/2)),sum(abs((signmat[,5]-signmat[,7])/2)),sum(abs((signmat[,6]-signmat[,7])/2)))

#Confusion Matrix:
library(caret)
pls.cmat = confusionMatrix(signmat[,1], signmat[,7])
nnet.cmat = confusionMatrix(signmat[,2], signmat[,7])
pcr.cmat = confusionMatrix(signmat[,3], signmat[,7])
lasso.cmat = confusionMatrix(signmat[,4], signmat[,7])
ridge.cmat = confusionMatrix(signmat[,5], signmat[,7])


signdfb = cbind(brentpred.unsc, brentpred.unsc1, brentpred.unsc2, brentpredann, ytest.unscb)
cor.brent = cor(signdfb)
samesignnumb = c(sum(abs((sign(signdfb[,1])-sign(signdfb[,5]))/2)), sum(abs((sign(signdfb[,2])-sign(signdfb[,5]))/2)),
              sum(abs((sign(signdfb[,3])-sign(signdfb[,5]))/2)), sum(abs((sign(signdfb[,4])-sign(signdfb[,5]))/2)))
#Investment Model 1######
#Idea: Buy when model predicts rise, sell when model predicts a drop
#Begin with $1000, assume no inflation, capital gains, etc...
library(readxl)
true.oil.data=  read_excel("C:\\Users\\Christian W\\Desktop\\Cog mech 4\\trueprice.xlsx", col_names = T, col_types = NULL)
truetest= true.oil.data[236:307,]
samesignmat = cbind((signmat[,1]-signmat[,7]), (signmat[,2]-signmat[,7]), (signmat[,3]-signmat[,7]), (signmat[,4]-signmat[,7]),(signmat[,5]-signmat[,7]),
                    (signmat[,6]-signmat[,7]))


#Investment model 1--buy and sell stock throughout the test period and then sell all stock and add to bank to 
#realize gains and losses
invest1 = function(pct,capital,truetest){
      initial = capital
      bank = c(initial,initial,initial,initial,initial,initial)
      stock = matrix(0, 72, 6)
      for (i in 1:6){
          for (a in 2:72){
            if (signdf[a,i] > 0){
                stock[a,i] = stock[a-1,i] + (pct * bank[i])/truetest[a,1]
                bank[i] = (1-pct) * bank[i]
            }else{
                stock[a,i] = stock[a-1,i] - ((pct*bank[i])/truetest[a,1])
                bank[i] = (1-pct)*bank[i] + ((pct*bank[i])/truetest[a,1])*truetest[a,1]
            }
                
          }
        bank[i] = bank[i] + stock[a,i]*truetest[a,1]
      }
      return(list(bank=bank, stock=stock))
    }

invest1(.05, 100000, truetest)


#Note the time constraints: Signdf presents predictions in period t-1 for the change between period t-1 and period t
#Thus if we have a negative prediction in t-1 for the change between t-1 and t, then the model shorts the price of oil
#Otherwise the model will invest a percentage of available capital as it expects the price of oil to rise. All transactions
#Are one period long, and process iterates

invest2 = function(pct,capital,truetest){
  initial = capital
  bank = c(initial,initial,initial,initial,initial,initial)
  for (i in 1:6){
    for (a in 2:72){
      if (signdf[a,i] > 0){
        bank[i] = (1-pct)*bank[i]+ (pct*bank[i])*(truetest[a,1]-truetest[a-1,1])
        print(bank[i])
      }else{
        bank[i] = (1-pct)*bank[i] - (pct*bank[i])*(truetest[a,1]-truetest[a-1,1])
        print(bank[i])
      }
      
    }
  }
  return(bank)
}

invest2(.025, 100000, truetest)




#VIX DATA################################################3
#######################################################
vix=  read_excel("C:\\Users\\Christian W\\Desktop\\VIX.xlsx", col_names = T, col_types = NULL)
VIX = data.frame(testml, wtiridge.unsc[,1], ytest.unsc, vix)
colnames(VIX) = c("Date", "WTI Ridge Unscaled Predictions", "Change in WTI Spot Price", "VIX")
VIX <- melt(VIX, id="Date")
pdf("VIX.pdf")
ggplot(data = VIX, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "Ridge Predictions of Change in WTI Spot Price and the VIX" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Spot Price of WTI crude or VIX, dollars") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


unscaledpls = data.frame(testml, wtipls[,1], wtipls[,2], vix)
colnames(unscaledpls) = c("Date", "PLS Unscaled Predictions", "Change in WTI Spot Price", "VIX")
unscaledpls <- melt(unscaledpls, id="Date")
pdf("Tradingmodel3MRvix.pdf")
ggplot(data = unscaledpls, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "PLS Predictions of Change in WTI Spot Price and the VIX" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Change Spot Price of WTI crude or VIX, dollars") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()



unscaledpls = data.frame(testml,wtipls.unsc,wtipcr.unsc,wtilasso.unsc,wtiridge.unsc,  vix)
colnames(unscaledpls) = c("Date", "PLS Unscaled", "PCR Unscaled", "Lasso Unscaled", "Ridge Unscaled", "VIX")
unscaledpls <- melt(unscaledpls, id="Date")
pdf("modelvix.pdf")
ggplot(data = unscaledpls, aes(x = Date,y = value, colour = variable, group = variable, linetype = variable)) + geom_line() + labs(title = "Four Predictive models and VIX" , x ="Dates, Intercept Marks Beginning of Test Period", y= "Change in Spot Price of WTI crude or VIX, dollars") + geom_vline(xintercept = as.numeric(mldata$Date[236]))
dev.off()


#Confusion matrices of models vs VIX

pls.vmat= confusionMatrix(signmat[,1],-sign(as.matrix(vix)))
pcr.vmat = confusionMatrix(signmat[,3], -sign(as.matrix(vix)))
lasso.vmat = confusionMatrix(signmat[,4], -sign(as.matrix(vix)))
ridge.vmat = confusionMatrix(signmat[,5], -sign(as.matrix(vix)))
