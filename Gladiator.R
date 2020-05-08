# Clear all
rm(list = ls())

# bibliotecas
library(readr)
library(dplyr)
library(rpart);
# library(randomForest);
library(ROCR);
library(caret);


# carrega base de dados
file_path = "http://mlr.cs.umass.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
dados <- readr::read_delim(file_path, ";", escape_double = FALSE, trim_ws = TRUE, col_types = cols(quality = col_factor()))

# imprime primeiras linhas nos dados
head(dados)

# ajusta nome das colunas
colnames(dados) = gsub(" ", "_", colnames(dados))

# plota relacao de colunas
# plot(dados)


# 4. Split data into training and test sets
# y = dplyr::select(dados, quality)
# y = dados %>% dplyr::select(quality)

# x = dplyr::select(dados, -quality)

# set random seed for replicability
set.seed(123)

# Selecao de base de teste
ids = sort(base::sample(nrow(dados), nrow(dados)*0.2))

# y_teste = y[ids, ]
# y_train = y[-ids, ]
# 
# x_teste = x[ids, ]
# x_train = x[-ids, ]

dados_teste = dados[ids, ]
dados_train = dados[-ids, ]

# Normalizacao de colunas
for (col in colnames(dados_teste)) {
  
  if(col == "quality")
  {
    cat(sprintf("Pulando coluna: %s\n", col))
    next;
  }
  
  cat(sprintf("Normalizando coluna: %s\n", col))

  media = mean(dados_train[[col]])
  dv = (var(dados_train[[col]]))^0.5
  
  new_col = paste(col, "norm", sep="_")
  
  dados_teste[ , new_col] = (dados_teste[ ,col] - media)/dv
  dados_train[ , new_col] = (dados_train[ ,col] - media)/dv
}

# check
summary(dados_teste)
diag(var(dados_teste))

class(dados_train$quality)

## Monta a arvore de treinamento por classificacao
trainTree = rpart::rpart(quality ~ fixed_acidity_norm + volatile_acidity_norm + citric_acid_norm + residual_sugar_norm + chlorides_norm + free_sulfur_dioxide_norm + total_sulfur_dioxide_norm + density_norm + pH_norm + sulphates_norm + alcohol_norm, data=dados_train, method = "class");


## Plota a arvore de classificacao
plot(trainTree);
text(trainTree, pretty=0 ,cex=0.6)

## Faz a previsao
testTree.predict = predict(trainTree, dados_teste, type = "class");

table(testTree.predict, dados_teste$quality);


sum(testTree.predict == 5)
