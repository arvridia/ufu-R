library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)

# QUESTAO 1

matriz_distancias <- as.dist(matrix(c(0,9,3,6,11,9,0,7,5,10,3,7,0,9,2,6,5,9,0,8,11,10,2,8,0), nrow=5))

arvore_a <- hclust(matriz_distancias, "complete")
plot(arvore_a)
abline(h = 10, col = "red")
rect.hclust(arvore_a, k = 2)

arvore_b <- hclust(matriz_distancias, "single")
plot(arvore_b)
abline(h = 5.5, col = "red")
rect.hclust(arvore_b, k = 2)

# -----------------------------------------------------

# QUESTAO 2

ataque <- read.table("heart.txt", header = TRUE, sep = ",")
ataque$chest_pain <- as.factor(ataque$chest_pain)
ataque$sex <- as.factor(ataque$sex)
ataque$smokes <- as.factor(ataque$smokes)
ataque$exercises <- as.factor(ataque$exercises)
ataque$heart_attack <- as.factor(ataque$heart_attack)
str(ataque)

chestpain <- function(vector){
  if(vector[1] == "yes"){
    return("yes")
  }else{
    if(vector[4] == "yes"){
      return("yes")
    }else{
      return("no")
    }
  }
}

# -----------------------------------------------------

# QUESTAO 3

# pré-processamento dos dados ---

hospital <- read.csv("SBI.csv", header = TRUE)

for(i in 1:nrow(hospital)){
  if(hospital$sbi[i] != "NotApplicable"){
    hospital$infection[i] <- "yes"
  }else{
    hospital$infection[i] <- "no"
  }
}

hospital$infection <- as.factor(hospital$infection)
hospital$sex <- as.factor(hospital$sex)
hospital$prevAB <- as.factor(hospital$prevAB)
hospital <- hospital[, -c(1,2,8)]
hospital <- hospital[sample(nrow(hospital)), ]

str(hospital)

tamanho <- floor(nrow(hospital)*0.8)
treino <- hospital[1:tamanho,]
teste <- hospital[(tamanho+1):nrow(hospital), ]

str(treino)
str(teste)

# arvore de decisao usando indice de gini ---

modelo <- rpart(infection ~., data = treino, parms = list(split = "gini"))
rpart.plot(modelo, extra=102)
predicao <- predict(modelo, newdata = teste, type = "class")

mean(teste$infection == predicao) 
table(teste$infection, predicao)

# floresta de decisao

modelo_floresta <- randomForest(infection ~., data = treino)
predicao_floresta <- predict(modelo_floresta, newdata = teste)
mean(teste$infection == predicao_floresta)
table(teste$infection, predicao_floresta)
