# Clear all
rm(list = ls())

# libraries
library(svmpath)

# Load Database
data ("PimaIndiansDiabetes2", package ="mlbench")

# dimensoes da base de dados
dim(PimaIndiansDiabetes2)

# mostra a estrutura da base de dados
str(PimaIndiansDiabetes2)

# funcao que calcula o total de na em um vetor
total.na <- function(x){ return(sum(is.na(x))); }

# Investiga o total de na base de dados
sapply(PimaIndiansDiabetes2, total.na)

# copio a base de dados e retiro as colunas insulin e triceps. 
# Alem disso tambem "filtramos" a amostra apenas para os casos completos.
data <- (PimaIndiansDiabetes2)
data$insulin <- NULL
data$triceps <- NULL
data <- na.omit(data)

# Separamos o nosso y
y <- as.numeric(data$diabetes)
y[y==1] <- -1
y[y==2] <- 1
y <- as.matrix(y)

# determino o x da minha amostra
x <- data
x$diabetes <- NULL
x <- as.matrix(x)

# Support vector machine kernels generally depend on the inner
# product of attribute vectors. Very large values might cause
# numerical problems.
x <- scale(x)


set.seed(103)
n <- nrow(x)
train <- sample(1:n, 600, FALSE)


fit <- svmpath(x[train ,],
               y[train,],
               kernel.function = radial.kernel,
               trace = TRUE)


head(fit$lambda, 3)
head(fit$Error, 3)

plot(fit$lambda, type = "l")
plot(fit$Error, type = "l")

# Each minimum error is associated with a unique regularization/cost value.
# We would like to use the smallest of these values as a parameter in our 
# test set model.

# lista o menor erro
error <- with(fit, Error[Error == min(Error)])
# seleciona a linha correspondente aos menores erros
min_err_row <- which(fit$Error == min(fit$Error))
# seleciona os lambdas com os menores erros
temp_lamdba <- fit$lambda[ min_err_row ]
# seleciona a linha do menor lambda
loc <- which(fit$lambda[ min_err_row ] == min(fit$lambda[min_err_row]))


lambda <- temp_lamdba[loc]
lambda

# In-sample prediction
pred_train <- predict(fit, newx =x[train ,], lambda=lambda, type="class")

# Confusion matrix
table(y[train ,], pred_train, dnn=c("Observed", "Predicted"))

# Step 4 - Evaluate Model Performance
pred_test <- predict(fit , newx =x[-train ,], lambda =lambda, type="class")

# Confusion matrix
table(y[-train ,], pred_test, dnn=c("Observed", "Predicted"))



# One of the quickest ways to improve performance is to choose an alternative kernel.
# Let’s estimate another SVM, this time using a radial basis function kernel:
fitP <- svmpath(x[train ,],
                y[train ,],
                kernel.function = poly.kernel,
                trace = FALSE )

# Check for linearity of the classification
fitP$linear
fit$linear

error <- with(fitP, Error[ Error == min(Error) ])
min_err_row <- which(fitP$Error == min(fitP$Error))
temp_lamdba <- fitP$lambda[ min_err_row ]
loc <- which ( fitP$lambda[ min_err_row ] == min(fitP$lambda [ min_err_row ]))
lambdaP <- temp_lamdba[loc]
lambdaP

error[1]/600

1/lambdaP


predP_train <- predict(fitP, newx=x[train ,],
                       lambda = lambdaP,
                       type="class")

table(predP_train, y[train ,],
      dnn = c("Observed" , "Predicted"))


predP <- predict(fitP , newx =x[-train ,],
                  lambda= lambdaP ,
                  type="class")

table(predP, y[-train ,], dnn=c("Observed", "Predicted"))
