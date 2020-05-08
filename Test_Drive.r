# clear all
rm(list=ls())

#  libraries
library(readxl)
library(dplyr)
library(tidyr)

# load data
data <- read_excel("Databases/DriveData.xlsx")

head(data)

# Calcula a Entropia da Varaivel Test Drive
col.distrib = data %>% count(Test_Drive)
total.testDrive = sum(col.distrib$n)

E.Test_Drive = 0
for (row in 1:nrow(col.distrib)) {
  E.Test_Drive = E.Test_Drive + 
    -(col.distrib[row, 2]/total.testDrive)*(log(col.distrib[row, 2]/total.testDrive))/log(nrow(col.distrib))
}

# imprime entropia
print(E.Test_Drive)

# limpa variaveis nao mais utilizadas
rm(list = c("col.distrib", "row", "total.testDrive"))

# Calcula a entropia de cada variavel

# tabela que guarda a entropia de cada variavel
tb_variable = tibble(Variable = colnames(data[, !(colnames(data) %in% c("Obs", "Test_Drive"))]), Entropy = 0, Reduction = 0)



for (variable in tb_variable$Variable) {
  
  # variable = "Fuel_Economy"
  
  # distribuicao conjunta de Test Drive e outra variavel
  col.distrib = data %>% group_by_at(c(eval(variable), "Test_Drive")) %>% summarize( Count = n()) %>% pivot_wider(names_from = eval(variable), values_from = Count)

  # Total de elementos na amostra
  Total = sum(col.distrib[, colnames(col.distrib) !="Test_Drive"])
  
  # Vetor que contem a entropia de cada caminho da variavel
  Entropy_vector = tibble(path = colnames(col.distrib[,colnames(col.distrib) !="Test_Drive"]), Entropy = NA, prob = NA)
  
  # calcula a entropia de cada caminho
  for (path.count in 1:nrow(Entropy_vector)) {
    
    # valor do caminho atual
    path.value = Entropy_vector[[path.count, 1]]
    
    # inicializa o total
    total.path = sum(col.distrib[ , path.value])
    
    # inicializa a entropia do caminho
    E.path = 0 
    
    for (row in 1:nrow(col.distrib)) {
      E.path = E.path + -(col.distrib[row, path.value]/total.path)*(log(col.distrib[row, path.value]/total.path))/log(nrow(col.distrib))
    }
    
    # atualiza vetor de entropia
    Entropy_vector[path.count, 2]=  E.path
    Entropy_vector[path.count, 3]=  total.path/Total
  }
  
  tb_variable[tb_variable$Variable == eval(variable), 2] = sum(Entropy_vector$Entropy * Entropy_vector$prob)
}

tb_variable$Reduction = E.Test_Drive$n - tb_variable$Entropy

print(tb_variable)
