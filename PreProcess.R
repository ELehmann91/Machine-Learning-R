
############################################################################################ libraries & functions

library(e1071)
library(ggplot2)
library(caTools)
library(Hmisc)
library(plyr)
library(dplyr)
library(caret)
library(ipred)
library(readr)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



PFAD_IN = ""
PFAD_DATEN = paste0(PFAD_IN , "05_Daten/")
PFAD_ZWERGEBNIS = paste0(PFAD_IN , "02_Ergebnisse/")

Input <- read_delim( paste0( PFAD_DATEN,"/CHURN_SAMPLE.csv" ), ";", escape_double = FALSE, trim_ws = TRUE)


dropvars<- names(Input) %in% c("N","D","l","id","ID","ID_F")       #100% NA / 100% Mode werden gefiltert
Input<-Input[!dropvars]
Input$k <- factor(ifelse(Input$k=='1', 'Yes', 'No'))

Input$BW[is.na(Input$BW)]<-0
Input$d[is.na(Input$d)]<-0
Input$kw[is.na(Input$kw)]<-0
Input$KWK_D[is.na(Input$KWK_D)]<-0
Input$sd[is.na(Input$sd)]<-0
Input$c[is.na(Input$c)]<-0

table(Input$k)
summary(Input$k)
######################################################################################### NAs -> NA, FActor

for( i in 1:length(Input)){
  if(length(table(Input[,i]) ) <10) { #print(length(table(Input[,i])))}
   Input[,i]<- as.factor(Input[[i]])}
}

NAs<- matrix(, nrow = length(Input), ncol = 6)
colnames(NAs) <- c("Name","NA","Mode%","Skew","Auspraeg","Type")

NAs <- data.frame(name = NAs[,1],                                   #Variablenname
                  Na_perc = as.numeric(NAs[,2]),                    #Anteil NAs
                  Mode_perc = as.numeric(NAs[,3]),                  #Anteil der häufigsten Ausprägung
                  Skew = as.numeric(NAs[,4]),                       #Schiefe
                  Auspraegungen = as.numeric(NAs[,5]),              #Anzahl Ausprägungen
                  Type = (NAs[,6])                                  #Variablentype
)

for( i in 1:length(Input)){
  NAs[i,1] <- names(Input)[i]
  NAs[i,2] <- sum(is.na(Input[,i]))/dim(Input)[1]
  NAs[i,3] <- tabulate(match(Input[,i],Mode(na.omit(Input[,i]))))/dim(Input)[1]
 # NAs[i,4] <- skewness(Input[,i],na.rm=T)
  NAs[i,5] <- length(unique(Input[,i],na.rm=T))
  NAs[i,6] <- class(Input[[i]])
}
#########################################################################################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

NA_MODE_99<- names(Input) %in% c(NAs$name[(NAs$Na_perc + NAs$Mode_perc)>0.80])      #100% NA / 100% Mode werden gelöscht
Input<-Input[!NA_MODE_99]

Factor_vars<- names(Input) %in% c(NAs$name[NAs$Type=="factor"])                     # Alle FactorVar
Numeric_vars<- names(Input) %in% c(NAs$name[NAs$Type=="numeric"])                    # Alle Numerischen Var

NA_05<- names(Input) %in% c(NAs$name[NAs$Na_perc > 0.05 & NAs$Type!="factor"])      # kein Faktor und mehr als 10% NA

#for( i in 1:length(Input)){                                                         #          
  #if(NA_29[i]) {Input[,i][is.na(Input[,i])] <- 0}
  #if(Factor_vars[[i]]) {
    # Input[,i] <- addNA(Input[,i])
   # Input[,i] <- as.character(Input[,i])
 #   Input[[i]][is.na(Input[[i]])] <- 'Kat_NA'
  #  Input[,i][Input[,i]=="NUL4"] <- 'NUL2'
 #   Input[,i] <- factor(Input[[i]])
#  }
#}


# Faktorvariablen sind NAs egal bzw werden als eigene Gruppe angesehen, Verbesserung durch Transformation nicht möglich
################################################################# Numeric
Numeric <- Input[NA_05]
NA_05<- names(Input) %in% c(NAs$name[NAs$Na_perc > 0.0001 & NAs$Type!="factor"])      # kein Faktor und mehr als 10% NA

for( i in 1:length(Input)){                                                         #          
  #  if(NA_29[i]) {Input[,i][is.na(Input[,i])] <- 0}
  if(Factor_vars[i]) {
    # Input[,i] <- addNA(Input[,i])
    #Input[,i] <- as.character(Input[,i])
    Input[,i][is.na(Input[,i])] <- 'Kat_NA'
    Input[,i] <- factor(Input[[i]])
  }
}


# Faktorvariablen sind NAs egal bzw werden als eigene Gruppe angesehen, Verbesserung durch Transformation nicht möglich
################################################################# Numeric
Numeric <- Input[NA_05]

for( i in 1:length(Input[NA_05])){              # _ / ? -> NA
  Input[NA_05][,i][Input[NA_05][,i] ==''] <- NA
  Input[NA_05][,i][Input[NA_05][,i] =='?'] <- NA
  Input[NA_05][,i] <- Input[NA_05][,i][drop=TRUE]
}

Numeric_TR<- matrix(, nrow = length(Numeric), ncol = 8)
colnames(Numeric_TR) <- c("Name","Type","Base","Null","Mean","Mode","Min","Max")


Numeric_TR <- data.frame(name = Numeric_TR[,1],
                         Type = (Numeric_TR[,2]),
                         Base = as.numeric(Numeric_TR[,3]),
                         Na_Null = as.numeric(Numeric_TR[,4]),
                         Na_Mean = as.numeric(Numeric_TR[,5]),
                         Na_Mode = as.numeric(Numeric_TR[,6]),
                         Na_Min = (Numeric_TR[,7]),
                         NaMax = (Numeric_TR[,8])
)

for( i in 1:length(Input[NA_05])){                                        #NA's ersetzen 1. step
  Numeric_TR[i,1] <- names(Input[NA_05][i])
  Numeric_TR[i,2] <- class(Input[NA_05][[i]])
  Numeric_TR[i,3] <- colAUC(as.numeric(Input[NA_05][[i]]),Input$k,alg=c("ROC"))
  Trans <- Input[NA_05][[i]]
  Trans[is.na(Trans)]<- 0
  Numeric_TR[i,4] <- colAUC(as.numeric(Trans) ,Input$k,alg=c("ROC"))
  Trans <- Input[NA_05][[i]]
  Trans[is.na(Trans)]<- mean(na.omit(Input[NA_05][[i]]))
  Numeric_TR[i,5] <- colAUC(as.numeric(Trans) ,Input$k,alg=c("ROC"))
  Trans <- Input[NA_05][[i]]
  Trans[is.na(Trans)]<- Mode(na.omit(Input[NA_05][[i]]))
  Numeric_TR[i,6] <- colAUC(as.numeric(Trans) ,Input$k,alg=c("ROC"))
  Trans <- Input[NA_05][[i]]
  Trans[is.na(Trans)]<- min(na.omit(Input[NA_05][[i]]))
  Numeric_TR[i,7] <- colAUC(as.numeric(Trans) ,Input$k,alg=c("ROC"))
  Trans <- Input[NA_05][[i]]
  Trans[is.na(Trans)]<- max(na.omit(Input[NA_05][[i]]))
  Numeric_TR[i,8] <- colAUC(as.numeric(Trans) ,Input$k,alg=c("ROC"))
}
########################################################################## Hilfstabelle, beste Ersetzung
Numeric_TR2<- matrix(, nrow = length(Numeric), ncol = 3)
colnames(Numeric_TR2) <- c("Name","Max","Max2")

for( i in 1:length(Numeric)){   
  Numeric_TR2[i,1] <- Numeric_TR[i,1]
  Numeric_TR2[i,2] <- match(max(Numeric_TR[i,4:8]),Numeric_TR[i,4:8])
  Numeric_TR2[i,3] <- max(Numeric_TR[i,4:8])
}
########################################################################## Umsetzung beste Ersetzung
Numeric_TR3 <- data.frame(Numeric)
for( i in 1:length(Numeric)){   
  if(Numeric_TR2[i,2]=="1") {Input[NA_05][,i][is.na(Input[NA_05][,i])] <- 0}  
  if(Numeric_TR2[i,2]=="2") {Input[NA_05][,i][is.na(Input[NA_05][,i])] <- mean(na.omit(Input[NA_05][,i]))}
  if(Numeric_TR2[i,2]=="3") {Input[NA_05][,i][is.na(Input[NA_05][,i])] <- Mode(na.omit(Input[NA_05][,i]))}
  if(Numeric_TR2[i,2]=="4") {Input[NA_05][,i][is.na(Input[NA_05][,i])] <- min(na.omit(Input[NA_05][,i]))-1}
  if(Numeric_TR2[i,2]=="5") {Input[NA_05][,i][is.na(Input[NA_05][,i])] <- max(na.omit(Input[NA_05][,i]))+1}
}


######################################################################################## korrel


preProcValues <- preProcess(Input,method = c("center", "scale", "nzv","knnImpute"))#,"pca")) #or *bagImpute* / *knnImpute*

Input <- predict(preProcValues, Input)


Numeric_vars<- names(Input) %in% c(NAs$name[NAs$Type=="numeric"])  
descrCor <- cor(na.omit(Input[,Numeric_vars]))
summary(descrCor[upper.tri(descrCor)])
corel <- data.frame(descrCor)

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
names(corel[highlyCorDescr])

dropvars <- names(Input) %in% names(corel[highlyCorDescr])
Input <- Input[!dropvars]

names(Input[,highlyCorDescr])

#Numeric_vars<- names(trainTransformed) %in% c(NAs$name[NAs$Type!="factor"])    
#descrCor2 <- cor(trainTransformed[,Numeric_vars])
#summary(descrCor2[upper.tri(descrCor2)])



############################################################################## Top30 selection

Selected_Input<- matrix(, nrow = length(Input), ncol = 2)
colnames(Selected_Input) <- c("Name","Max")

Selected_Input<-data.frame(Selected_Input)

for( i in 1:length(Input)){
  Selected_Input[i,1] <- names(Input[i])
  Selected_Input[i,2] <- colAUC(as.numeric(Input[[i]]) ,Input[[1]])
}
Selected_Input <- data.frame(Selected_Input)
Selected_Input <- Selected_Input[ order(Selected_Input$Max, decreasing=TRUE), ] 

Top30<- names(Input) %in% c( as.character(Selected_Input[1:30,1]))   

Input<-Input[Top30]


save(Input,file=paste0( PFAD_DATEN, "Input30.Rda"))



#rm(filteredDescr, NAs, trainIndex, trainTransformed, dropvars, Factor_vars, Numeric_vars,
#   NA_29, NA_MODE_99, nzv, preProcValues, Input, i, trainTransformed_Train, trainTransformed_Test,
#   Top30, Selected_Input)




