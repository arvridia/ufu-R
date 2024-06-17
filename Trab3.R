#Integrantes:
	Arthur Resende Santos
	Henrique Braga
	Pedro Henrique Marra
	Anderson Alves

#--------------------------------------------------------
# QUESTAO 1

d1 <- sample(1:6,size = 10^6, replace=TRUE)
d2 <- sample(1:6, size = 10^6, replace=TRUE)
d <- d1 + d2
q1 <-mean(d == 7 | d == 11 )

#--------------------------------------------------------
# QUESTAO 2

urna1 <- c(rep(1, 4), rep(0, 11))
urna2 <- c(rep(1, 7), rep(0, 8))
urna3 <- c(rep(1, 2), rep(0, 7))
urna4 <- c(rep(1, 8), rep(0, 3))

x <- c()
for(i in 1:10^6){
  dados <- sample(1:6, size = 2)
  soma <- sum(dados)
  if(soma < 4){
    x[i] <- sample(urna1, size=1)
  }else if(soma < 7){
    x[i] <- sample(urna2, size=1)
  }else if(soma == 7){
    x[i] <- sample(urna3, size=1)
  }else{
    x[i] <- sample(urna4, size=1)
  }
}

q2 <- mean(x)

#--------------------------------------------------------
# QUESTAO 3

x <- c()
for(i in 1:10^5){
  dados <- sample(1:6, size=2, replace=TRUE)
  soma <- sum(dados)
  if(soma == 7 | soma == 11){
    x[i] <- 1
  }else if(soma == 2 | soma == 3 | soma == 12){
    x[i] <- 0
  }else{
    repeat{
      dados1 <- sample(1:6, size=2, replace=TRUE)
      soma1 <- sum(dados1)
      if(soma1 == 7){
        x[i] <- 0
        break
      }
      if(soma1 == soma){
        x[i] <- 1
        break
      }
    }
  }
}

q3 <- mean(x)

#--------------------------------------------------------
# QUESTAO 4

# 1 = cara
# 0 = coroa

# consideramos que empate não constitui vitória... então jim só ganha quando suas 3 moedas são iguais e as de dwight não são

jim <- c(0, 0, 1)
dwight <- c(0, 1, 0)

jim_venceu <- c()

for(i in 1:10^6){
  moedas <- sample(c(0,1), size = 3, replace = TRUE)
  repeat{
    if(identical(jim, moedas)){
      jim_venceu[i] <- 1
      break
    }
    else if(identical(dwight, moedas)){
      jim_venceu[i] <- 0
      break
    }
    else{
      moedas <- c(moedas[2:3], sample(c(0,1), size=1))
    }
  }  
}

q4 <- mean(jim_venceu)

#--------------------------------------------------------
# QUESTAO 5

q5 <- function(passos){
  x <- c()
  for(i in 1:10^6){
    if(sum(sample(c(-1,1), size=passos, replace=TRUE)) == 0){
      x[i] <- 1
    }else{
      x[i] <- 0
    }
  }
  return(mean(x))
}

q5_i <- q5(4)
q5_ii <- q5(6)
q5_iii <- q5(10)
q5_iv <- q5(20)

#--------------------------------------------------------
# QUESTAO 6

# -1 = coroa
# 1 = cara


mapear <- function(vetor){
  x <- c()
  for(i in vetor){
    x[i] <- q6(vetor[i])
  }
  return(x)
}

q6 <- function(L){
  x <- c()

  for(i in 1:10^4){
    luke <- L
    
    while(luke != 0 & luke != 20){
      moeda <- sample(c(-1,1), size=1)
      luke <- luke + moeda
    }
    
    if(luke == 0){
      x[i] <- 1
    }else{
      x[i] <- 0
    }
  }
  
  return(mean(x))
}

plot(1:19, mapear(1:19), xlim = c(1,19), ylim = c(0,1), type = "l", xlab = "Posição inicial de Luke", ylab = "Probabilidade de Luke parar caindo no precipício")

#--------------------------------------------------------
# QUESTAO 7

f <- function(x){
  return(1/sqrt(2*pi)*exp(-x^2/2))
}

g <- function(x){
  return((cos(x))^2)
}

q7_a <- 3*mean(f(runif(10^6, -1, 2)))
q7_b <- pi*mean(g(runif(10^6, 0, pi)))

#--------------------------------------------------------
# QUESTAO 8

q8 <- function(n){
  x <- c()
  for(i in 1:n){
    prob_ac <- 0
    u <- runif(1)
    while(u > prob_ac){
      if(u <= 1/7){
        x[i] <- 1
        prob_ac <- prob_ac + 1/7
      }else if(u <= 3/7){
        x[i] <- 5
        prob_ac <- prob_ac + 2/7
      }else{
        x[i] <- 10
        prob_ac <- prob_ac + 4/7
      }
    }
  }
  return(x)
}

q8_a <- mean(q8(10^2) == 5)
q8_b <- mean(q8(10^3) == 5)
q8_c <- mean(q8(10^4) == 5)

#--------------------------------------------------------
# QUESTAO 9

q9 <- function(n){
  return((-1 + sqrt(1 + 8*runif(n,0,1)))/2)
}

q9a <- mean(q9(10^4) < 0.7)
q9b <- mean(q9(10^4))
