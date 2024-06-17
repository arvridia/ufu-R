#--------------------------------------------------------
# QUESTAO 1

q1a <- seq(1, 20, 1)
q1b <- seq(20, 1, -1)
q1c <- c(seq(1, 20, 1), seq(19, 1, -1))

#--------------------------------------------------------
# QUESTAO 2

fq2 <- function(x){
  return(exp(x)*cos(x))
}

q2 <- funcao_questao_2(seq(3, 6, 0.1))

#--------------------------------------------------------
# QUESTAO 3

f3a <- function(i){
  return(i^3 + 4*i^2)
}

f3b <- function(i){
  return((2^i)/i + (3^i)/i^2)
}

q3a <- sum(f3a(10:100))
q3b <- sum(f3b(10:25))

#--------------------------------------------------------
# QUESTAO 4

xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

q4a <- which(xVec %% 2 == 1)
q4b <- yVec[2:length(yVec)] - xVec[1:length(xVec)-1]
q4c <- sin(yVec[2:length(yVec)])/cos(xVec[1:length(xVec)-1])
q4d <- xVec[1:(length(xVec)-2)] + 2*xVec[2:(length(xVec)-1)] - xVec[3:length(xVec)]

#--------------------------------------------------------
# QUESTAO 5

xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

q5a <- which(xVec > 600) //errou
q5b <- yVec[which(yVec > 600)]
q5c <- xVec[which(yVec > 600)]
mediaxVec <- mean(xVec)
q5d <- sqrt(abs(xVec - mediaxVec))
valorMax <- max(q5d)
q5e <- sum(sqrt(abs(yVec - valorMax)) <= 200) porra é essa
q5f <- sum(xVec %% 2 == 0) que porra é essa
q5g <- xVec[order(yVec)]
q5h <- yVec[seq(1, 250, 3)]

#--------------------------------------------------------
# QUESTAO 6

num.impar <- function(v){
  qtd_impar <- 0
  for(i in 1:length(v)){
    if(v[i] %% 2 != 0){
      qtd_impar <- qtd_impar + 1
    }
  }
  return(qtd_impar)
}

num.impar2 <- function(v){
  # caso base: tamanho vetor = 1
  (length(v) == 1){
    if(v[1] %% 2 != 0){
      return(1)
    }
    return(0)
  }
  
  # caso geral
  else if(v[1] %% 2 != 0){
    return(1 + num.impar2(v[2:length(v)]))
  }
  
    return(num.impar2(v[2:length(v)]))
}

#--------------------------------------------------------
# QUESTAO 7

fibonacci <- function(n){
  if(n == 1 | n == 2){
    return(1)
  }
  
  return(fibonacci(n-1) + fibonacci(n-2))
}

#--------------------------------------------------------
# QUESTAO 8

tamanho.seq <- function(p){
  # X ~ Ber(p)
  # X = 1 <=> cara
  # X = 0 <=> coroa
  caras <- 0
  repeat{
    X <- sample(x=c(1,0), size=1, prob=c(p,(1-p)))
    if(X == 1){
      caras <- caras + 1
    }else{
      break
    }
  }
  return(caras)
}

maior.seq <- function(n){
  maior_seq <- 0
  for(i in 1:n){
    x <- tamanho.seq(0.5)
    if(x > maior_seq){
      maior_seq <- x
    }
  }
  return(maior_seq)
}

#--------------------------------------------------------
# QUESTAO 9

f <- function(n, k){
  if(n  == 1){
    return(1);
  }
  
  if(n == 2){
    return(1)
  }
  
  return(f(n-1, k) + k*f(n-2, k))
}

#--------------------------------------------------------
# QUESTAO 10

indice_maior <- function(v){
  maior <- 1
  for(i in 2:length(v)){
    if(v[i] > v[maior]){
      maior <- i
    }
  }
  return(maior)
}

indice_menor <- function(v){
  menor <- 1
  for(i in 2:length(v)){
    if(v[i] < v[menor]){
      menor <- i
    }
  }
  return(menor)
}

install.packages(dslabs)
library(dslabs)
dados <- murders

# A
a_media <- mean(dados$population)
a_maior <- dados$state[indice_maior(dados$population)]
a_menor <- dados$state[indice_menor(dados$population)]

# B
dados$taxa <- dados$population/10^4

# C
dados$state[order(dados$taxa)]

# D
dados$state[dados$taxa < 0.05]

# E
dados$state[dados$taxa > 0.5]

# F
south <- dados[dados$region == "South",]
mean(south$taxa)
sd(south$taxa)

west <- dados[dados$region == "West",]
mean(west$taxa)
sd(west$taxa)

northeast <- dados[dados$region == "Northeast",]
mean(northeast$taxa)
sd(northeast$taxa)

northcentral <- dados[dados$region == "North Central",]
mean(northcentral$taxa)
sd(northcentral$taxa)

# G
par(mfrow=c(2,2))

boxplot(south$taxa, ylim = c(0, 3800), main = "sul")
boxplot(west$taxa, ylim = c(0, 3800), main = "oeste")
boxplot(northeast$taxa, ylim = c(0, 3800), main = "nordeste")
boxplot(northcentral$taxa, ylim = c(0, 3800), main = "norte central")
