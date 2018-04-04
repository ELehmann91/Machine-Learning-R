
windows()

for( j in names(Variablen )){

fit <- train(y=factor(Train$ABG),
             x= apply(as.matrix(Train[Variablen[[ j ]] ]),2,as.numeric)
             #  ,importance=TRUE
             , method="rpart", # "rf"
             metric="ROC",
             tuneGrid=expand.grid( .cp= exp(-c(10:2) )),
             # tuneGrid=expand.grid( .cp=c(3,6,9) ), 
             trControl=trainControl(method="cv", 
                                    number=5,repeats=10,
                                    ,summaryFunction=twoClassSummary, classProbs=T ,
                                    returnData=F,savePredictions=F,returnResamp="none"),
                                    control=rpart.control(minbucket=50,maxdepth=5),    
                                    na.action=na.omit)

      #save(fit, file= paste0(PFAD_ZWERGEBNIS,j,".Rda"))
    #  plot(fit)
      #savePlot(filename= paste0(PFAD_ZWERGEBNIS,j,".jpg" ),type="jpeg", device = dev.cur())
      
      plot(varImp(fit))
      #savePlot( filename= paste0(PFAD_ZWERGEBNIS,j,"_VarImp.jpg" ),type="jpeg")
      
      predicted.fit= predict(newdata=Test, fit, type = "prob")[,1] #class
      colAUC(predicted.fit, Test[,"ABG"],plot=T)
    #  savePlot( filename= paste0(PFAD_ZWERGEBNIS,j,"_roc.jpg" ),type="jpeg", device = dev.cur())
      
      plot(fit$finalModel)
      text(fit$finalModel,use.n=T)

predicted.fit= predict(newdata=Test, fit, type = "raw")
table(predicted.fit, Test$ABG)
Test$Pred<-predicted.fit

}        

predicted.fit= predict(newdata=Test, fit, type = "raw")
table(predicted.fit, Test$ABG)

Variablen


mean(Test$E_GES[Test$ABG=='X'&Test$Pred=='X'])
mean(Test$E_GES[Test$Pred=='X'])



