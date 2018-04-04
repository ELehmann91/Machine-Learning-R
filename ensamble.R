##########################################################################
# Model procedure
# load data
# basic model
# model tuning (caret)
# diagnostic plots
######################################################################### 

######################################################################## load data

PFAD_IN = "H:/My Documents/R"
PFAD_DATEN = paste0(PFAD_IN , "00_Daten/")
PFAD_ZWERGEBNIS = paste0(PFAD_IN , "02_Ergebnisse/")

load(paste0( PFAD_ZWERGEBNIS, "/Train.Rda"))
load(paste0( PFAD_ZWERGEBNIS, "/Test.Rda"))

Anteil_ZV_in <- function(data, ZV, col, X, q){
  A <- data[order(data[, col], decreasing=TRUE),]
  A <- A[1:(nrow(A)* q), ] 
  A<-sum(A[, ZV] == X)/sum(data[, ZV] == X)
  print(A)
  rm(A)
}




#Adapted from the caret vignette
library("caret")
library("mlbench")
library("pROC")
library("rpart")
library("caretEnsemble")
library("caTools")
library("glm2")
library(ada)
library(kernlab)
library(mboost)
library(nnet)
library(LogicReg)
library(mda)
library(earth)
library(obliqueRF)
library(mboost)
library(rattle)
library(adabag)
library(fastAdaboost)
########################################################################

cvCtrl <- trainControl(method = "repeatedcv"
                       ,repeats = 2
                       ,number = 5
                       ,summaryFunction = twoClassSummary
                       ,classProbs = TRUE
                       ,returnData=F
                       ,savePredictions="final"
                       ,returnResamp="final"
                       ,verboseIter=T
                       ,allowParallel=T
                       )

model_list <- caretList(
  ZIELVERHALTEN~., data=Train30,
  trControl=cvCtrl,
  methodList=c("ada","LogitBoost","cforest","avNNet","svmRadial"), #"cforest",
  metric = "ROC")
  #,na.action=na.omit
 # tuneList=list(
#    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
#    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
#    nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
#  )


p <- as.data.frame(predict(model_list, newdata=head(Test)))
print(p)
xyplot(resamples(model_list))
modelCor(resamples(model_list))


############################################################################################################# greedy

greedy_ensemble <- caretEnsemble(model_list, 
                                metric="ROC",
                                   trControl=cvCtrl
                                )


greedy_ensemble_prb<-predict(greedy_ensemble,Test30,type="prob")
roc(Test$ZIELVERHALTEN,greedy_ensemble_prb)
plot.roc(roc(Test$ZIELVERHALTEN,greedy_ensemble_prb[,"A"]))

A<-greedy_ensemble_prb
A <- data.frame(A)
A$ZV <- Test$ZIELVERHALTEN
##############################################################################################################
summary(greedy_ensemble)

ZVHN <- Test$ZIELVERHALTEN
table(Test$ZIELVERHALTEN)
table(ZVHN)
levels(ZVHN) <- c(0,1)
table(model_preds$class)


model_preds <- lapply(model_list, predict, newdata=Test, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"A"])
model_preds <- data.frame(model_preds)

ens_preds <- predict(greedy_ensemble, newdata=Test, type="prob")
model_preds$ensemble_gred <- ens_preds


caTools::colAUC(model_preds, Test$ZIELVERHALTEN)



summary(model_preds)
#varImp(greedy_ensemble)

greedy_ensemble_trn_prd <- predict(greedy_ensemble, Test)
confusionMatrix(greedy_ensemble_trn_prd, Test$ZIELVERHALTEN)

############################################################################################################# glm

glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=cvCtrl
)

model_preds2 <- model_preds
model_preds$ensemble_glm <- predict(glm_ensemble, newdata=Test, type="prob")

CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
roc(model_preds2, Test$ZIELVERHALTEN)

CF/sum(CF)

summary(glm_ensemble)
######################################################################################################### gbm

library("gbm")
gbm_ensemble <- caretStack(
  model_list,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  metric="ROC",
  trControl=cvCtrl
)
gbm_ensemble$final_model
summary(gbm_ensemble)
varImp(gbm_ensemble)

model_preds2 <- model_preds
model_preds$ensemble_gbm <- predict(gbm_ensemble, newdata=Test, type="prob")
CF <- coef(gbm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds, Test$ZIELVERHALTEN, alg="ROC")

summary(model_preds2)

glm_ensemble_trn_prd <- predict(glm_ensemble, Test)
confusionMatrix(glm_ensemble_trn_prd, Test$ZIELVERHALTEN)

model_preds2$greedy_ensemble <- predict(greedy_ensemble, newdata=Test, type="prob")
model_preds2$ensemble_glm <- predict(glm_ensemble, newdata=Test, type="prob")
model_preds2$ensemble_gbm <- predict(gbm_ensemble, newdata=Test, type="prob")

roc(Test$ZIELVERHALTEN,model_preds2$greedy_ensemble)
roc(Test$ZIELVERHALTEN,model_preds2$ensemble_glm)
roc(Test$ZIELVERHALTEN,model_preds2$ensemble_gbm)

rpart_trn_prb$obs <-Test$ZIELVERHALTEN
head(rpart_trn_prb)
mnLogLoss(rpart_trn_prb, lev = levels(rpart_trn_prb$obs))

rpart_trn_prb$pred <- predict(rpart_trn,Test)

multiClassSummary(model_preds2$ensemble_gred, lev = levels(model_preds2$class[,1]))

gbm_ensemble$results

Anteil_ZV_in(data=model_preds2,ZV="class", col="ensemble_gred", X=1, q=0.05)





library("caTools")
model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds


caTools::colAUC(model_preds, testing$Class)






