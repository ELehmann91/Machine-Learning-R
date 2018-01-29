##########################################################################
# Model procedure
# load data
# basic model
# model tuning (caret)
# diagnostic plots
######################################################################### 

######################################################################## load data

PFAD_IN = "H:/My Documents/R/CC_Gold/"
PFAD_DATEN = paste0(PFAD_IN , "00_Daten/")
PFAD_ZWERGEBNIS = paste0(PFAD_IN , "02_Ergebnisse/")

load(paste0( PFAD_ZWERGEBNIS, "/Train.Rda"))
load(paste0( PFAD_ZWERGEBNIS, "/Test.Rda"))

library(kernlab)
library(pROC)
library(caret)

########################################################################

######################################################################## basic model


save(svm, file=paste0(PFAD_ZWERGEBNIS,"C50.RData" ))


#######################################################################

######################################################################## model tuning (caret)


cvCtrl <- trainControl(method = "repeatedcv", repeats = 2,number = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE
                       ,returnData=F
                       ,savePredictions=F
                       ,returnResamp="none")

set.seed(1)
svm_trn <- train(ZIELVERHALTEN~.
                 ,data=Train30,
                 method = "svmRadial",
                 # The default grid of cost parameters go from 2^-2,
                 # 0.5 to 1,
                 # Well fit 9 values in that sequence via the tuneLength
                 # argument.
                 tuneLength = 9,
                 ## Also add options from preProcess here too
                 #preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = cvCtrl)

save(svm_trn, file=paste0(PFAD_ZWERGEBNIS,"svm_trn.RData" ))

########################################################################

######################################################################## diagnostic plots

load(file=paste0(PFAD_ZWERGEBNIS,"svm_trn.RData" ))

plot(svm_trn, metric = "ROC", scales = list(x = list(log =   2)))
plot(svm_trn)

svm_trn_prd <- predict(svm_trn, Test)
confusionMatrix(svm_trn_prd, Test$ZIELVERHALTEN)

svm_trn_prb <- predict(svm_trn, Test, type = "prob")

svm_trn_rc<-roc(Test$ZIELVERHALTEN,svm_trn_prb[,"X"])
plot(svm_trn_rc,type="S",print.thres= .5)   
plot.roc(svm_trn_rc)

#######################################
