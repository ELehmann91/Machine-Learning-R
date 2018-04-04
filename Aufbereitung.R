library(caret)
library(ggplot2)
library(tree)
library(lattice)
library(randomForest)
library(caTools)
library(rpart)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


PFAD_IN = "H:/My Documents/R/"
PFAD_DATEN = paste0(PFAD_IN , "0Daten/")
PFAD_ZWERGEBNIS = paste0(PFAD_IN , "2ZwischenErgebnis/")


Test <- read.csv( paste0( PFAD_DATEN,"/Test.csv" ), sep=";" ,
                  colClasses=c("factor","factor","factor", rep("numeric",16),"factor","factor",
                               rep("numeric",4),"factor",rep("numeric",33) ) )
Train <- read.csv( paste0( PFAD_DATEN,"/Train.csv" ), sep=";",
                   colClasses=c("numeric","factor","factor", rep("numeric",16),"factor","factor",
                                rep("numeric",4),"factor",rep("numeric",33) ) )
summary(Train)
summary(Test)

### TODO.


###   NA = max
Train$C_lKo[is.na(Train$C_lKo)] <- max(Train$C_lKo, na.rm = TRUE)+1
Test$C_lKo[is.na(Test$C_lKo)] <- max(Test$C_lKo, na.rm = TRUE)+1


### A

rpartA<- rpart(A~G+ as.factor(F)+AK+Kd_Bez_Dauer+B
                   , data=Train, na.action=na.omit, method="anova")

Train$A[is.na(Train$A)] <-  predict(rpartA,Train[is.na(Train$A),])
Train$A[Train$A < 14] <-  predict(rpartA,Train[Train$A <  14,])
Train$A[Train$A > 99] <-  predict(rpartA,Train[Train$A >  99,])

rpartA2<- rpart(A~G+ as.factor(F)+AK+Kd_Bez_Dauer+B
                    , data=Test, na.action=na.omit, method="anova")

Test$A[is.na(Test$A)] <-  predict(rpartA2,Test[is.na(Test$A),])
Test$A[Test$A < 14] <-  predict(rpartA2,Test[Test$A <  14,])
Test$A[Test$A > 99] <-  predict(rpartA2,Test[Test$A >  99,])

### Anzahl Personen
table(Train$A)

Train$AP[Train$AP <0] <- NA
Test$AP[Test$AP <0] <- NA

TraAP<- rpart(AP~ as.factor(F)+AK, data=Train, na.action=na.omit, method="anova")
TesAP<- rpart(AP~ as.factor(F)+AK, data=Test, na.action=na.omit, method="anova")

Train$AP[is.na(Train$AP)] <-  predict(TraAP,Train[is.na(Train$AP),])
Test$AP[is.na(Test$AP)] <-  predict(TesAP,Test[is.na(Test$AP),])

### Beruf löschen
Train$Beruf<-0
Test$Beruf<-0
table(Test$Beruf)

########################################################################## NA's durch Mode ersetzen
Train$N[Train$N==-1]<- NA
Test$N[Test$N==-1]<- NA

Var_Mode = list()
Var_Mode<-c("B","G","N","F")
            
for( i in Var_Mode){
  Train[,i][is.na(Train[,i])] <- Mode(Train[,i])
  Train[,i][Train[,i] ==''] <- Mode(Train[,i])
  Train[,i][Train[,i] =='?'] <- Mode(Train[,i])
  Train[,i] <- Train[,i][drop=TRUE]
}
for( i in Var_Mode){
  Test[,i][is.na(Test[,i])] <- Mode(Test[,i])
  Test[,i][Test[,i] ==''] <- Mode(Test[,i])
  Test[,i][Test[,i] =='?'] <- Mode(Test[,i])
  Test[,i] <- Test[,i][drop=TRUE]
}
summary(Test$B)
summary(Test$G)
summary(Test$N)
summary(Test$F)

########################################################################### NA's durch Null ersetzen

Var_Null = list()
Var_Null<-c("Prod_AG_1M")

for( i in Var_Null){
  Train[,i][is.na(Train[,i])] <- 0
}
        
for( i in Var_Null){
    Test[,i][is.na(Test[,i])] <- 0
}
summary(Train)


#Train<-na.omit(Train)
#Test<-na.omit(Test)


