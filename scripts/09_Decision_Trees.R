#Practical work on Clustering
#Karina Gibert (KEMLG at IDEAI, UPC)
#
#Follow comments and instructions
#
#
# 
#In a general case: Retrieve the data saved AFTER the preprocessing practice...... 
#this means data is already cleaned
#  READING CREDSCO.csv

setwd("D:/karina/docencia/01areferenciesPPT/0DadesPractiques/CREDSCO/")
dd <- read.csv("data/interim/CredScoClean.csv",header=T, sep=";")

dim(dd)

names(dd)
missingDictamen<-which(is.na(dd[,1]))

ddUsefull<-dd[-missingDictamen,]
dim(ddUsefull)

attach(ddUsefull)
sapply(ddUsefull, class)
summary(ddUsefull)

#ASSUMING PREPROCESSED DATA
# WHICH RESPONSE?
# WHICH ARE CATEGORICAL AND WHICH ARE CONTINUOUS
#check in "environment" window the correct type of all variables in dd


#declare type of response variable and all factors
# DECISION TREES   CART

library(rpart)

#by default
dtot <- data.frame(Dictamen,Antiguedad.Trabajo,Vivienda,Plazo,Edad, Estado.civil, Registros, Tipo.trabajo, Gastos,Ingresos,Patrimonio,Cargas.patrimoniales,
                   Importe.solicitado,Precio.del.bien.financiado,Estalvi, RatiFin)

#don't predict Dictamen=NA, missing data on the response variable makes no sense



# HOLDOUT OF A 1/3 OF DATA TO ESTIMATE THE MISCLASSIFICATION PROB. OF THE CHOSEN TREE 

n <- nrow(dtot)
learn <- sample(1:n, round(0.67*n))
head(learn)

nlearn <- length(learn)
nlearn
ntest <- n - nlearn

#verify that all modalities of all qualitative explanatory variables are appearing 
#in the training set, if not, rerun the sampling or sample with stratification


#build a dt by default parameters


tree1 = rpart(Dictamen~Antiguedad.Trabajo+Vivienda+Plazo+Edad+Estado.civil+Registros+Tipo.trabajo+Gastos+Ingresos+Patrimonio+Cargas.patrimoniales+
                Importe.solicitado+Precio.del.bien.financiado+Estalvi+RatiFin, data=dtot[learn,])
class(tree1)

print(tree1)

summary(tree1)


plot(tree1)
text(tree1, use.n=TRUE, all=TRUE, cex=0.8) 
#click zoom

#ojo con las protecciones del disco donde esta instalado R
#install.packages("rpart.plot")
library(rpart.plot)

prp(tree1)
#diferents formats estetics
prp(tree1, type=1)


#diferent informacio presentada
prp(tree1, type=1, extra=4)


# Analyze control parameters: maxdepth

tree2 = rpart(Dictamen~Antiguedad.Trabajo+Plazo+Edad+Gastos+Ingresos+Patrimonio+Cargas.patrimoniales+
                Importe.solicitado+Precio.del.bien.financiado+RatiFin+Estalvi+Vivienda+Estado.civil+Registros+Tipo.trabajo, data=dtot[learn,], control=rpart.control(maxdepth=3))

prp(tree2, type=1, extra= 4)
print(tree2)

#complexity parameter
tree3 = rpart(Dictamen~Antiguedad.Trabajo+Plazo+Edad+Gastos+Ingresos+Patrimonio+Cargas.patrimoniales+
                Importe.solicitado+Precio.del.bien.financiado+RatiFin+Estalvi+Vivienda+Estado.civil+Registros+Tipo.trabajo, data=dtot[learn,], control=rpart.control(cp=0.005))

prp(tree3, type=1, extra=4)

#please take your tree and place in extra window 
#and check another cp value


tree4 = rpart(Dictamen~Antiguedad.Trabajo+Plazo+Edad+Gastos+Ingresos+Patrimonio+Cargas.patrimoniales+
                Importe.solicitado+Precio.del.bien.financiado+RatiFin+Estalvi+Vivienda+Estado.civil+Registros+Tipo.trabajo, data=dtot[learn,], control=rpart.control(cp=0.06))
prp(tree4, type=1, extra=4)

#minsplit
tree5 = rpart(Dictamen~Antiguedad.Trabajo+Plazo+Edad+Gastos+Ingresos+Patrimonio+Cargas.patrimoniales+
                Importe.solicitado+Precio.del.bien.financiado+RatiFin+Estalvi+Vivienda+Estado.civil+Registros+Tipo.trabajo, data=dtot[learn,], control=rpart.control(minsplit=200))
prp(tree5, type=1, extra= 3)
#this are the basics. Other criteria available, complexity parameter and cross-validation not introduced


# CACULATE THE ERROR RATE IN THE LEARNING SAMPLE

predictions=predict(tree5,data=dtot[learn,])
head(predictions)

# LETS HAVE A LOOK ON THE PREDICTION (conditional probability of LeafLabel)

dim(predictions)
predictions[1:10]
predictions[1:10,1]
predictions[1:10,2]
length(predictions[,1])
dim(dtot)
nlearn
dim(dtot[learn,])

table(Dictamen,exclude=FALSE)
table(ddUsefull[learn,1])



table(ddUsefull[,1])


# CACULATE THE ERROR RATE IN THE LEARNING SAMPLE

l<-0.5
l<-0.2
predClass=NULL
predClass[predictions[,1]<l]="pred_neg.learn"
predClass[predictions[,1]>=l]="pred_pos.learn"
predictions[1:5]
predClass[1:5]

confusionMatrix <- table(Dictamen[learn],predClass)
confusionMatrix
error_rate.learn <- 100*(confusionMatrix[1,1]+confusionMatrix[2,2])/nlearn 
error_rate.learn


# ERROR RATE IN THE TEST SAMPLE

tPredictions=predict(tree5,newdata=dtot[-learn,])
summary(tPredictions)
dim(tPredictions)

tpredClass=NULL
tpredClass[tPredictions[,1]<0.5]="pred_neg.test"
tpredClass[tPredictions[,1]>=0.5]="pred_pos.test"
table(Dictamen[-learn],tpredClass)
TestConfusionMatrix <- table(Dictamen[-learn],tpredClass)
error_rate.test <- 100*(TestConfusionMatrix[1,1]+TestConfusionMatrix[2,2])/ntest 
error_rate.test


# ROC CURVE

Dict.test <- Dictamen[-learn]
table(Dict.test)
npos <- table(Dict.test)[1]
nneg <- ntest - npos

pred.test <- tPredictions[,1]

# RANKING THE PREDICTIONS

rank_pred.test <- rank(pred.test)

acum_fals.pos <- 100*cumsum(rev(as.numeric(tapply(Dict.test=="negatiu",rank_pred.test,sum))))/nneg

# COMPUTING HOW MANY POSITIVE ARE IN EACH LEAVE (LEAVE = EQUAL RANK INDIVIDUALS)

acum_true.pos <- 100*cumsum(rev(as.numeric(tapply(Dict.test=="positiu",rank_pred.test,sum))))/npos


# PLOT

plot(acum_fals.pos,acum_true.pos,type="l", main="ROC curve")
lines(acum_fals.pos, acum_fals.pos, col="red")

sensitivity<-1-acum_fals.pos

acum_fals.neg<- 100*cumsum(rev(as.numeric(tapply(Dict.test=="positiu",rank_pred.test,sum))))/npos
plot(acum_fals.neg,acum_fals.pos,type="l", main="ROC curve")
lines(acum_fals.neg,acum_fals.pos,  col="red")

# OR
ord_pred.test <- order(pred.test,decreasing=T)
ac_neg_test <- 100*cumsum(Dict.test[ord_pred.test]=="negatiu")/nneg
ac_pos_test <- 100*cumsum(Dict.test[ord_pred.test]=="positiu")/npos

plot(ac_neg_test,ac_pos_test,type="l", main="Concentration Curve")
lines(ac_neg_test, ac_neg_test, col="red")

# CONCENTRATION CURVE

# THE TOTAL NUMBER OF TEST INDIVIDUALS IN EACH LEAVE 

totn <- table(-pred.test)/ntest
ac_totn <- 100*cumsum(as.numeric(totn))

# ANOTHER WAY OF DOING THE CONCENTRATION CURVE

ac_tot <- 100*(1:ntest)/ntest

ord_pred.test <- order(pred.test,decreasing=T)


plot(ac_tot,ac_pos_test,type="l", main="Concentration Curve")
lines(ac_tot, ac_tot, col="red")
