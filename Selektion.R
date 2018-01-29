library(pROC)
library(base)

set.seed(100)
trainIndex <- createDataPartition(Train$ID, p = .30,
                                  list = FALSE,
                                  times = 1)
Train1 <- Train[ trainIndex,]
Train2  <- Train[-trainIndex,]


Variablen = list()
Variablen$Produkte=   c("C_NPr","C_PSp","C_PDi","C_PKK","C_PKr","C_PWP","C_PDa","C_PBa")
Variablen$Stamm=      c("Alter"  ,"Anz_Kind","Kd_Bez_Dauer"  ,"NAEHFIL") #,"Geschlecht","Famstand","Beruf2",
Variablen$Person=     c("KT_pers","KT_mail","KT_schr","KT_tele","E_GES","E_ZVS","FilWe3","FilWe6","BerWe3","BerWe6")
Variablen$VarM=       c("V01_AUC","V02_AUM","Anz_Umsa","Umsa_Vol_H","Umsa_Vol_S")
Variablen$VarM1=      c("Prod_AG_1M","V01_AUC_1","V02_AUM_1","Anz_Umsa_1","Umsa_Vol_H_1","Umsa_Vol_S_1")
Variablen$VarM2=      c("Prod_AG_2M","V01_AUC_2","V02_AUM_2","Anz_Umsa_2","Umsa_Vol_H_2","Umsa_Vol_S_2")
Variablen$VarM3=      c("Prod_AG_3M","V01_AUC_3","V02_AUM_3","Anz_Umsa_3","Umsa_Vol_H_3","Umsa_Vol_S_3")
Variablen$VarM6=      c("Prod_AG_6M","V01_AUC_6","V02_AUM_6","Anz_Umsa_6","Umsa_Vol_H_6","Umsa_Vol_S_6")

Variablen$VarD= NULL#     c("Anz_Umsa_D1","Anz_Umsa_D2","Anz_Umsa_D3","Anz_Umsa_D6","Anz_Umsa_D1_q")


for( j in names(Variablen)){         
for( i in Variablen[[j]]){
  print( paste0(i, " ", colAUC( as.numeric(Train[,i]), Train[,"ABGANG3"]))) 
}}

Train$Anz_Umsa_D1<- log((Train$Anz_Umsa+1)/(Train$Anz_Umsa_1+1))
Train$Anz_Umsa_D2<- log((Train$Anz_Umsa+1)/(Train$Anz_Umsa_2+1))
Train$Anz_Umsa_D3<- log((Train$Anz_Umsa+1)/(Train$Anz_Umsa_3+1))
Train$Anz_Umsa_D6<- log((Train$Anz_Umsa+1)/(Train$Anz_Umsa_6+1))

Train$Anz_Umsa_D1_q <- with(Train, cut(Anz_Umsa_D1, breaks=quantile(Anz_Umsa_D1, probs=seq(0,1, by=0.20)), include.lowest=TRUE))
Train$Anz_Umsa_D2_q <- with(Train, cut(Anz_Umsa_D2, breaks=quantile(Anz_Umsa_D2, probs=seq(0,1, by=0.20)), include.lowest=TRUE))
Train$Anz_Umsa_D3_q <- with(Train, cut(Anz_Umsa_D3, breaks=quantile(Anz_Umsa_D3, probs=seq(0,1, by=0.20)), include.lowest=TRUE))
Train$Anz_Umsa_D6_q <- with(Train, cut(Anz_Umsa_D6, breaks=quantile(Anz_Umsa_D6, probs=seq(0,1, by=0.20)), include.lowest=TRUE))

Train$Anz_Umsa_D1<- log((Train$Anz_Umsa+1)/(Train$Anz_Umsa_1+1))
Train$Anz_Umsa_D2<- log((Train$Anz_Umsa+1)/(Train$Anz_Umsa_2+1))
Train$Anz_Umsa_D3<- log((Train$Anz_Umsa+1)/(Train$Anz_Umsa_3+1))
Train$Anz_Umsa_D6<- log((Train$Anz_Umsa+1)/(Train$Anz_Umsa_6+1))

Train$Anz_Umsa_D1_q <- with(Train, cut(Anz_Umsa_D1, breaks=quantile(Anz_Umsa_D1, probs=seq(0,1, by=0.20)), include.lowest=TRUE))
Train$Anz_Umsa_D2_q <- with(Train, cut(Anz_Umsa_D2, breaks=quantile(Anz_Umsa_D2, probs=seq(0,1, by=0.20)), include.lowest=TRUE))
Train$Anz_Umsa_D3_q <- with(Train, cut(Anz_Umsa_D3, breaks=quantile(Anz_Umsa_D3, probs=seq(0,1, by=0.20)), include.lowest=TRUE))
Train$Anz_Umsa_D6_q <- with(Train, cut(Anz_Umsa_D6, breaks=quantile(Anz_Umsa_D6, probs=seq(0,1, by=0.20)), include.lowest=TRUE))

colAUC( as.numeric(Train$Anz_Umsa_D1_q), Train[,"ABGANG3"])
colAUC( as.numeric(Train$Anz_Umsa_D2_q), Train[,"ABGANG3"])
colAUC( as.numeric(Train$Anz_Umsa_D3_q), Train[,"ABGANG3"])
colAUC( as.numeric(Train$Anz_Umsa_D6_q), Train[,"ABGANG3"])


table(Train$ABGANG3,Train$Anz_Umsa_D1_q)
table(Train$ABGANG3,Train$Anz_Umsa_D2_q)
plot(Train$ABGANG3,Train$Anz_Umsa_D3_q)
plot(Train$ABGANG3,Train$Anz_Umsa_D6_q)

Train1 <- Train[ trainIndex,]
Train2  <- Train[-trainIndex,]

plot(as.party(rpart(ABGANG1~ , data=Train1, control=rpart.control(maxdepth=10))))
fit4<-(rpart(ABGANG3~Anz_Umsa+Anz_Umsa_D1_q + Anz_Umsa_D3_q + Anz_Umsa_D6_q, data=Train1, control=rpart.control(maxdepth=3)))

predicted.fit4= predict(newdata=Train2, fit4, type = "class")
table(predicted.fit4, Train2$ABGANG3)
