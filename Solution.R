
#Clear Variable
rm(list=ls())

# carregando biblioteca
library(readr)
library(dplyr)
library(rpart)
library(caret)

#carrega dados
file_path = "http://mlr.cs.umass.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"

dados <- read_delim(file_path, ";", escape_double = FALSE,col_types = cols(quality = col_factor()), trim_ws = TRUE)
#plot(dados)
head(dados)

colnames(dados)
gsub(" ", "_", colnames(dados))
colnames(dados) = gsub(" ", "_", colnames(dados))

#4. split data into training and test set

y=dplyr::select(dados,quality)
x=dplyr::select(dados, -quality)

set.seed(123)
ids = sort(sample(nrow(y), nrow(y)*0.2))

y_test = y[ids, ]
y_train = y[-ids,]
x_test = x[ids, ]
x_train = x[-ids, ]

#dados_test = dados[ids,]
#dados_train = dados[-ids,]

x_test_norm = data.frame(id=1:nrow(x_test))
x_train_norm= data.frame(id=1:nrow(x_train)) 

#normalizacao
for (col in colnames(x_train))
{
  cat(sprintf("%s", col))
  media = mean(x_train[[col]])
  dv = (var(x_train[[col]]))^0.5

  new_col = paste(col, "norm", sep="_")
  
  x_test_norm[, new_col] = (x_test[, col]-media)/dv
  x_train_norm[, new_col] = (x_train[, col]-media)/dv
}

summary(x_test)
diag(var(x_test_norm))
summary(x_train)
diag(var(x_train_norm))

x_test_norm$id = NULL
x_train_norm$id = NULL

head(x_test_norm)
#plot(x_test_norm)

#monta a arvore de treinamento por classificação
trainTree = rpart::rpart( quality ~ ., data=cbind(y_train, x_train_norm), method="class");

#plota arvore
plot(trainTree)
text(trainTree, pretty=0, cex=0.6)

#type class gives already most likely
testTree.predict = predict(trainTree, cbind(y_test, x_test_norm), type="class")
head(testTree.predict)

table(testTree.predict, y_test$quality)
