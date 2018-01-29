PFAD_IN = "H:/My Documents/R/CC_Gold/"
PFAD_DATEN = paste0(PFAD_IN , "00_Daten/")
PFAD_ZWERGEBNIS = paste0(PFAD_IN , "02_Ergebnisse/")

load(file=paste0(PFAD_ZWERGEBNIS,"rpart_trn.RData" ))
load(file=paste0(PFAD_ZWERGEBNIS,"c50_trn.RData"))
load(file=paste0(PFAD_ZWERGEBNIS,"svm_trn.RData"))
load(file=paste0(PFAD_ZWERGEBNIS,"logreg_trn.RData"))


Anteil_ZV_in <- function(data, ZV, col, X, q){
  A <- data[order(data[, col], decreasing=TRUE),]
  A <- A[1:(nrow(A)* q), ] 
  A<-sum(A[, ZV] == X)/sum(data[, ZV] == X)
  print(A)
  rm(A)
}

plot(rpart_trn)

plot(nn_trn)
plot()
plot()
plot()

cvValues <- resamples(list( CART=rpart_trn, C5.0=c50_trn, rf=rf_trn, nn=nn_trn))

summary(cvValues)

bwplot(cvValues)
splom(cvValues, metric = "ROC")


rocDiffs <- diff(cvValues, metric = "ROC")
summary(rocDiffs)
dotplot(rocDiffs, metric = "ROC")

evalResults <- data.frame(Class = Test$ZIELVERHALTEN)
#evalResults$CART <- predict(rpart_trn, Test)
#evalResults$SVM <- predict(svm_trn, Test)
evalResults$C5.0 <- predict(c50_trn, Test)
evalResults$rf <- predict(rf_trn, Test)
evalResults$nn <- predict(nn_trn, Test)

length(Test$ZIELVERHALTEN)
length(evalResults$CART)
#confusionMatrix(evalResults$CART, Test$ZIELVERHALTEN)
#confusionMatrix(evalResults$SVM, evalResults$Class)
confusionMatrix(evalResults$C5.0, evalResults$Class)
confusionMatrix(evalResults$rf, evalResults$Class)
confusionMatrix(evalResults$nn, evalResults$Class)

X <- "nn"

Stats <- list()
Stats$Anschreiben <- sum(table(evalResults[,X], Test$ZIELVERHALTEN)[2,])/sum(table(evalResults[,X], Test$ZIELVERHALTEN)) #% müssen angeschrieben werden
Stats$Zielverhalten <- sum(table(evalResults[,X], Test$ZIELVERHALTEN)[,2])/sum(table(evalResults[,X], Test$ZIELVERHALTEN)) #% Anteil mit Zielverhalten
Stats$Zielverhalten_nicht_erkannt <- sum(table(evalResults[,X], Test$ZIELVERHALTEN)[1,2])/sum(table(evalResults[,X], Test$ZIELVERHALTEN)) #% Zielverhalten aber nicht angesprochen
Stats$Zielverhalten_falsch_erkannt <- sum(table(evalResults[,X], Test$ZIELVERHALTEN)[2,1])/sum(table(evalResults[,X], Test$ZIELVERHALTEN)) #% Angesprochen aber kein Zielverhalten
Stats$Zielverhalten_richtig_erkannt <- sum(table(evalResults[,X], Test$ZIELVERHALTEN)[2,2])/sum(table(evalResults[,X], Test$ZIELVERHALTEN)) #% Angesprochen und  Zielverhalten
Stats$Anteil_Zielverhalten_erkann <- Stats$Zielverhalten_richtig_erkann/Stats$Zielverhalten  #Zielverhalten gefunden
Stats$Anteil_richtig_Angeschrieben <- Stats$Zielverhalten_richtig_erkann/Stats$Anschreiben  #richtig angeschrieben
Stats

table(evalResults$CART, Test$ZIELVERHALTEN)[2,2]/sum(table(evalResults$CART, Test$ZIELVERHALTEN)[1,2]) # davon richtige ansprachen
table(evalResults$CART, Test$ZIELVERHALTEN)[2,2]/sum(table(evalResults$CART, Test$ZIELVERHALTEN)[1,2])
table(evalResults$CART, Test$ZIELVERHALTEN)[2,2]/sum(table(evalResults$CART, Test$ZIELVERHALTEN)[1,2])

evalResults <- data.frame(Class = factor(Test$ZIELVERHALTEN))
#evalResults$CART <- predict(rpart_trn, Test, type = "prob")[,"X"]
#evalResults$SVM <- predict(svm_trn, Test, type = "prob")[,"X"]
evalResults$C5.0 <- predict(c50_trn, Test, type = "prob")[,"A"]
evalResults$rf <- predict(rf_trn, Test, type = "prob")[,"A"]
evalResults$nn <- predict(nn_trn, Test, type = "prob")[,"A"]

head(evalResults,20)
cor(evalResults[,2:6])




evalResults <- evalResults[ order(evalResults$Class, decreasing=TRUE), ] 
trellis.par.set(caretTheme())
#levels(evalResults$Class) <- c(0,1)
liftData <- lift(Class ~ rf + nn + C5.0 , data = evalResults, positive="A")
plot(liftData, values = 30, auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))




plot(roc(evalResults$Class,evalResults$C5.0), type = "S", col = "#BBBBBB")
plot(roc(evalResults$Class,evalResults$nn), add = TRUE, col = "#BBBBBB")
plot(roc(evalResults$Class,evalResults$rf),, add = TRUE, ,col = "#FF0000")



plot(roc(evalResults$Class,evalResults$SVM), add = TRUE, col = "#9E0142")
plot(roc(evalResults$Class,evalResults$C5.0), add = TRUE, col = "#9EBB42")
plot(roc(evalResults$Class,evalResults$rf), add = TRUE, col = "#995542")
plot(roc(evalResults$Class,evalResults$nn), add = TRUE, col = "#9955FF")

histogram(~evalResults$CART|Test$ZIELVERHALTEN, xlab = "Probability of Poor Segmentation")
histogram(~evalResults$C5.0|Test$ZIELVERHALTEN, xlab = "Probability of Poor Segmentation")
histogram(~evalResults$SVM|Test$ZIELVERHALTEN, xlab = "Probability of Poor Segmentation")
histogram(~evalResults$rf|Test$ZIELVERHALTEN, xlab = "Probability of Poor Segmentation")
histogram(~evalResults$nn|Test$ZIELVERHALTEN, xlab = "Probability of Poor Segmentation")


Anteil_ZV_in(data=evalResults,ZV="Class", col="CART", X="X", q=0.05)
Anteil_ZV_in(data=evalResults,ZV="Class", col="SVM",  X="X", q=0.05)
Anteil_ZV_in(data=evalResults,ZV="Class", col="C5.0", X="X", q=0.05)
Anteil_ZV_in(data=evalResults,ZV="Class", col="rf", X="X", q=0.05)
Anteil_ZV_in(data=evalResults,ZV="Class", col="nn", X="X", q=0.05)

A<-varImp(rpart_trn)[1]
varImp(svm_trn)
varImp(c50_trn)
varImp(logreg_trn)
varImp(nn)

A$importance[1:2,]
rownames(A$importance)
