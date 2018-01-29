PFAD_IN = "H:/My Documents/R/CC_Gold/"
PFAD_DATEN = paste0(PFAD_IN , "00_Daten/")
PFAD_ZWERGEBNIS = paste0(PFAD_IN , "02_Ergebnisse/")

library(proxy)
library(caret)

tag <- read.csv(file=paste0(PFAD_ZWERGEBNIS,"tag_data.csv"))#, row.names = 1))
tag <- as.matrix(tag)

tag <- data.frame(tag)
tag
## Select only models for regression
regModels <- as.matrix(tag[tag[,"Classification"] == 1,])
regModels <- as.matrix(regModels[regModels[,"Ensemble.Model"] == 1,])


all <- 1:nrow(regModels)
## Seed the analysis with the SVM model
start <- grep("(AdaBag)", regModels[,1], fixed = TRUE)
pool <- all[all != start]

## Select 4 model models by maximizing the Jaccard
## dissimilarity between sets of models
regModels_num <- regModels

class(regModels_num) <- "numeric"
nextMods <- maxDissim(regModels_num[start,,drop = FALSE],
                      regModels_num[pool, ],
                     method = "Jaccard",
                      n = 6)

regModels[,1][c(start, nextMods)]

all
start
pool
nextMods

tag[1,]


regModels
