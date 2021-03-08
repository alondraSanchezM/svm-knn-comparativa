library(caret)
library(kernlab)
library(ggplot2)
library(e1071)
library(readr)
library(dplyr) 
library(gmodels)

#---------------------------Comparativa de modelos: SVM Lineal y KNN 

#Lectura del archivo
dataSetEbay<-read.csv("eBayAuctions.csv",header = TRUE, sep = ",")
View(dataSetEbay)

#Preprocesamiento de datos
dataSetEbay <-  cbind(dataSetEbay[1], dataSetEbay[3], dataSetEbay[6:8])
colnames(dataSetEbay) = c("Category", "sellerRating", "ClosePrice","OpenPrice","Competitive")
dataSetEbay <- dataSetEbay %>% mutate(Competitive = ifelse(Competitive  == 1, "si", "no"))
View(dataSetEbay)

#Creación conjuntos de trainning y test
set.seed(128)
indices <- createDataPartition(dataSetEbay$Competitive, p = .70, list = FALSE)
train_Ebay = dataSetEbay[indices, ]
test_Ebay = dataSetEbay[-indices, ] 

View(train_Ebay)
View(test_Ebay)


train10CV <- trainControl(method = "cv", number = 10, classProbs = TRUE)
#---------------SVM
svmLineal <- train(Competitive ~ ., 
                data = train_Ebay, 
                method = "svmLinear", 
                trControl = train10CV, 
                preProc = c("center", "scale"))
svmLineal

#Predecir los datos de test
predictionResult = predict(svmLineal, newdata = test_Ebay[-5]) #sin considerar la variable dependiente Competitive
predictionResult

#Creación de matriz de confución
cm = table(test_Ebay[, 5], predictionResult)
cm

presicionA <- ((sum(diag(cm)))/(sum(cm))*100)
presicionA

#---------------KNN
knnModel <- train(Competitive ~ ., data = train_Ebay, method = "knn", trControl = train10CV, preProc = c("center", "scale"))
knnModel

#Predicción KNN
predKnn <- predict(knnModel, newdata = test_Ebay[-5])
predKnn
summary(predKnn)  

pres <- (289/301)*100
pres

#Comparativa de modelos
results <- resamples(list(svmLineal = svmLineal, kNN = knnModel))
summary(results)
dotplot(results)