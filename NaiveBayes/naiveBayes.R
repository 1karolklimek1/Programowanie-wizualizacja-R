wine = read.table("wine.data", sep=",", header = FALSE)
#View( wine )
library(dplyr)
colnames(wine) = c("Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash", "Magnesium", "Total_phenols",  "Flavanoids",
                  "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity", "Hue", "OD280/OD315_of_diluted_wines", "Proline")
#View(wine)
str(wine)
#install.packages("caret")
library(caret)

#wine$Class <- as.numeric( wine$Class )
wine$Alcohol <- as.numeric( wine$Alcohol )
wine$Malic_Acid <- as.numeric( wine$Malic_Acid )
wine$Ash <- as.numeric( wine$Ash )
wine$Alcalinity_of_Ash <- as.numeric( wine$Alcalinity_of_Ash )
#wine$Magnesium <- as.numeric( wine$Magnesium )
wine$Total_phenols <- as.numeric( wine$Total_phenols )
wine$Flavanoids <- as.numeric( wine$Flavanoids )
wine$Nonflavanoid_phenols <- as.numeric( wine$Nonflavanoid_phenols )
wine$Proanthocyanins <- as.numeric( wine$Proanthocyanins )
wine$Color_intensity <- as.numeric( wine$Color_intensity )
wine$Hue <- as.numeric( wine$Hue )
wine$`OD280/OD315_of_diluted_wines`<- as.numeric( wine$`OD280/OD315_of_diluted_wines` )
#wine$Proline <- as.numeric( wine$Proline )

wine <- select( wine, Class, Alcohol, Malic_Acid, Magnesium, Proline )
set.seed(123) # Ustawienie ziarna dla powtarzalności
index <- createDataPartition(wine$Class, p = 0.7, list = FALSE)
train_data <- wine[index, ]
test_data <- wine[-index, ]

#install.packages("e1071")
library(e1071)
# Dostosowanie modelu naiwnego Bayesa
model <- naiveBayes(Class ~ ., data = train_data)

predictions <- predict(model, test_data)
predictions

confusion_matrix <- confusionMatrix(predictions, as.factor(test_data$Class))

print(confusion_matrix)


