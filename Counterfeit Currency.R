# limpeza da base de dados
rm(list = ls())

# bibliotecas utilizadas
library(dplyr)
library(tidyr)
library(C50)
library(tree)

# carrega base de dados
data("banknote", package = "mclust")

#  mostra o inicio dos dados
head(banknote)

# mostra a quantidade de va variavel Status
table(banknote$Status)

# Fixa a semente de reproducao
set.seed(2011)

# busca o tamanho da amostra
N = nrow(banknote)

# retira uma amostra de tamanho 150 (~ 75%)
train <- sample(1:N, 150, FALSE)

# mostra o inicio da amostra
head(train)

# faz uma arvore utilizando o pacote C5.0
fitc <- C50::C5.0(Status ~ ., data = banknote[train,])
plot(fitc)

# Mostra as regras do pacote
fitc_rules <- C50::C5.0(Status ~ ., data = banknote[train,], rules=TRUE)
summary(fitc_rules)

# faz a previsao "In Sample"
predc_train <- predict( fitc, newdata = banknote[train,], type = "class")
head(predc_train)

# Confusion Matrix "In Sample"
table(banknote$Status[train], predc_train, dnn=c("observed Class","Predicted Class"))

# Faz previsao "Out of Sample"
predc <- predict( fitc, newdata = banknote[-train,], type = "class")

# Confusion Matrix "Out of Sample"
table(banknote$Status[-train], predc, dnn=c("observed Class","Predicted Class"))

# Step 5 - improvinf Model Performance
# we use the package tree
fit <- tree::tree(Status ~ . , data = banknote[train, ], split = "deviance")

# Viewing the decision tree 
plot(fit)
text(fit)

# Train set performance
summary(fit)

# Previsao "Out of Sample"
pred <- predict(fit, newdata=banknote[-train, ])

# fim da previsao
tail(pred, 5)

pred.class <- colnames(pred)[max.col(pred, ties.method = c("random"))]

tail(pred.class, 5)

table(banknote$Status[-train], pred.class, dnn=c("observed Class","Predicted Class"))
