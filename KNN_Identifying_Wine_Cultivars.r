# clear all
rm(list = ls())

# load libraries ----
library(ggplot2)
library(corrplot)
library(reshape2)
library(knnGarden)

# load database ----
data ("wine",package ="rebmix")

colnames(wine) = c(
  "Alcohol",
  "Malic.Acid",
  "Ash",
  "Alcalinity.of.Ash",
  "Magnesium",
  "Total.Phenols",
  "Flavanoids",
  "Nonflavanoid.Phenols",
  "Proanthocyanins",
  "Color.Intensity",
  "Hue",
  "Diluted.Wines",
  "Proline",
  "Cultivar")


# Step 1 – Collecting and Exploring the Data ----
# plot bargraph
ggplot(wine) + geom_bar(aes(x=Cultivar), fill="gold")


# plot correlation (retiro coluna cultivar)
corrplot(cor(wine[,-14]), method = "ellipse", type = "upper")

# Step 2 – Preparing the Data ----
# plot box-plot
wine.melt = melt(wine, id.vars = "Cultivar")
ggplot(wine.melt, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=variable)) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))

# normalizacao da amaostra
data_sample <- wine[ ,1:13]
data_sample <- scale(data_sample)

# plot box plot
data_sample.melt = melt(data_sample)
colnames(data_sample.melt) = c("id", "variable", "value")
ggplot(data_sample.melt, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=variable)) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))


# fixa semente de random sample
set.seed(2015)
n= nrow ( data_sample )
train <- sample (1:n, 89, replace = FALSE )

# imprime a amostra de treino
head(train)

# Step 3 - Train Model using Train Set ----
fit1 <- knnVCN(data_sample[train ,],
                 wine$Cultivar[ train ],
                 data_sample[-train ,],
                 K = 2,
                 method = "canberra")

# Step 4 - Evaluate Model Performance ----
tab1 <- table(fit1$TstXIBelong, wine$Cultivar[-train])

print(tab1)
Error.rate = (sum(tab1) - sum(diag(tab1)))/sum(tab1)
Accuracy.rate = 1-Error.rate

cat(sprintf("\nError rate: %0.3f\nAccuracy rate: %0.3f", Error.rate, Accuracy.rate))

# Step 5 - Improving Model Performance ----
fit2 <- knnVCN(data_sample[train ,],
               wine$Cultivar[ train ],
               data_sample[-train ,],
               K = 2,
               method = "euclidean")

tab2 <- table(fit2$TstXIBelong, wine$Cultivar[-train])

print(tab2)
Error.rate2 = (sum(tab2) - sum(diag(tab2)))/sum(tab2)
Accuracy.rate2 = 1-Error.rate2

cat(sprintf("\nError rate: %0.3f\nAccuracy rate: %0.3f", Error.rate2, Accuracy.rate2))

# Changing k
fit3 <- knnVCN(data_sample[train ,],
               wine$Cultivar[ train ],
               data_sample[-train ,],
               K = 3,
               method = "euclidean")

tab3 <- table(fit3$TstXIBelong, wine$Cultivar[-train])

print(tab3)
Error.rate3 = (sum(tab3) - sum(diag(tab3)))/sum(tab3)
Accuracy.rate3 = 1-Error.rate2

cat(sprintf("\nError rate: %0.3f\nAccuracy rate: %0.3f", Error.rate3, Accuracy.rate3))

