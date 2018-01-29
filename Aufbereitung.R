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


PFAD_IN = "H:/My Documents/R/churn/"
PFAD_DATEN = paste0(PFAD_IN , "0Daten/")
PFAD_ZWERGEBNIS = paste0(PFAD_IN , "2ZwischenErgebnis/")
#"P:/Zentral_Einheiten/ZPK/2_Projektdaten/DataBaseMarketing/0SAMBA/cb2lehk/alt"

Test <- read.csv( paste0( PFAD_DATEN,"/Test.csv" ), sep=";" ,
                  colClasses=c("factor","factor","factor", rep("numeric",16),"factor","factor",
                               rep("numeric",4),"factor",rep("numeric",33) ) )
Train <- read.csv( paste0( PFAD_DATEN,"/Train.csv" ), sep=";",
                   colClasses=c("numeric","factor","factor", rep("numeric",16),"factor","factor",
                                rep("numeric",4),"factor",rep("numeric",33) ) )
summary(Train)
summary(Test)

### TODO.


### Letzter Kontakt NA = max
Train$C_lKo[is.na(Train$C_lKo)] <- max(Train$C_lKo, na.rm = TRUE)+1
Test$C_lKo[is.na(Test$C_lKo)] <- max(Test$C_lKo, na.rm = TRUE)+1


### Alter

rpartAlter<- rpart(Alter~Geschlecht+ as.factor(Famstand)+Anz_Kind+Kd_Bez_Dauer+Beruf2
                   , data=Train, na.action=na.omit, method="anova")

Train$Alter[is.na(Train$Alter)] <-  predict(rpartAlter,Train[is.na(Train$Alter),])
Train$Alter[Train$Alter < 14] <-  predict(rpartAlter,Train[Train$Alter <  14,])
Train$Alter[Train$Alter > 99] <-  predict(rpartAlter,Train[Train$Alter >  99,])

rpartAlter2<- rpart(Alter~Geschlecht+ as.factor(Famstand)+Anz_Kind+Kd_Bez_Dauer+Beruf2
                    , data=Test, na.action=na.omit, method="anova")

Test$Alter[is.na(Test$Alter)] <-  predict(rpartAlter2,Test[is.na(Test$Alter),])
Test$Alter[Test$Alter < 14] <-  predict(rpartAlter2,Test[Test$Alter <  14,])
Test$Alter[Test$Alter > 99] <-  predict(rpartAlter2,Test[Test$Alter >  99,])

### Anzahl Personen
table(Train$Alter)

Train$ANZPERS[Train$ANZPERS <0] <- NA
Test$ANZPERS[Test$ANZPERS <0] <- NA

TraAP<- rpart(ANZPERS~ as.factor(Famstand)+Anz_Kind, data=Train, na.action=na.omit, method="anova")
TesAP<- rpart(ANZPERS~ as.factor(Famstand)+Anz_Kind, data=Test, na.action=na.omit, method="anova")

Train$ANZPERS[is.na(Train$ANZPERS)] <-  predict(TraAP,Train[is.na(Train$ANZPERS),])
Test$ANZPERS[is.na(Test$ANZPERS)] <-  predict(TesAP,Test[is.na(Test$ANZPERS),])

### Beruf löschen
Train$Beruf<-0
Test$Beruf<-0
table(Test$Beruf)

########################################################################## NA's durch Mode ersetzen
Train$NAEHFIL[Train$NAEHFIL==-1]<- NA
Test$NAEHFIL[Test$NAEHFIL==-1]<- NA

Var_Mode = list()
Var_Mode<-c("Beruf2","Geschlecht","NAEHFIL","Famstand")
            
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
summary(Test$Beruf2)
summary(Test$Geschlecht)
summary(Test$NAEHFIL)
summary(Test$Famstand)

########################################################################### NA's durch Null ersetzen

Var_Null = list()
Var_Null<-c("Prod_AG_1M","Prod_AG_2M","Prod_AG_3M","Prod_AG_6M","V01_AUC","V02_AUM","V01_AUC_1",
         "V02_AUM_1","V01_AUC_2","V02_AUM_2","V01_AUC_3","V02_AUM_3","V01_AUC_6","V02_AUM_6","Anz_Umsa","Umsa_Vol_H","Umsa_Vol_S","Anz_Umsa_1",
         "Umsa_Vol_H_1","Umsa_Vol_S_1","Anz_Umsa_2","Umsa_Vol_H_2", "Umsa_Vol_S_2", "Anz_Umsa_3","Umsa_Vol_H_3", "Umsa_Vol_S_3", "Anz_Umsa_6","Umsa_Vol_H_6",
         "Umsa_Vol_S_6","FilWe3","FilWe6","BerWe3","BerWe6"      )

for( i in Var_Null){
  Train[,i][is.na(Train[,i])] <- 0
}
        
for( i in Var_Null){
    Test[,i][is.na(Test[,i])] <- 0
}
summary(Train)


#Train<-na.omit(Train)
#Test<-na.omit(Test)