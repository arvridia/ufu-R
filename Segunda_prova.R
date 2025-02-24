library(tree)
library(rpart)
library(rpart.plot)

#Arthur Resende Santos - Prova 2

#Quest�o 1- 

#a)
papagaios <- read.table("papagaio.txt", header = TRUE, sep = ',')
head(papagaios)
tail(papagaios)
str(papagaios)
summary(papagaios)


#b)
papagaios <- papagaios[sample(1:500),]


#c)
Arctica <- papagaios[which(papagaios$especie == "arctica"),]
mean(Arctica$tamanho, na.rm = TRUE)
mean(Arctica$peso, na.rm = TRUE)
mean(Arctica$envergadura, na.rm = TRUE)
sd(Arctica$tamanho, na.rm = TRUE)
sd(Arctica$peso, na.rm = TRUE)
sd(Arctica$envergadura, na.rm = TRUE)

Corniculata <- papagaios[which(papagaios$especie == "corniculata"),]
mean(Corniculata$tamanho, na.rm = TRUE)
mean(Corniculata$peso, na.rm = TRUE)
mean(Corniculata$envergadura, na.rm = TRUE)
sd(Corniculata$tamanho, na.rm = TRUE)
sd(Corniculata$peso, na.rm = TRUE)
sd(Corniculata$envergadura, na.rm = TRUE)

Cirrhata <- papagaios[which(papagaios$especie == "cirrhata"),]
mean(Cirrhata$tamanho, na.rm = TRUE)
mean(Cirrhata$peso, na.rm = TRUE)
mean(Cirrhata$envergadura, na.rm = TRUE)
sd(Cirrhata$tamanho, na.rm = TRUE)
sd(Cirrhata$peso, na.rm = TRUE)
sd(Cirrhata$envergadura, na.rm = TRUE)

boxplot(Arctica$peso, Corniculata$peso, Cirrhata$peso)
#percebe-se tanto pelas medias e pelo boxplot que as esp�cies possuem grande diferencia��o em peso, com a esp�cie "cirrhata" possuindo o maior peso em geral, seguida pela "corniculata" e, por �ltimo, a "arctica", com os menores pesos


#d)
summary(papagaios)
papagaios$especie <- as.factor(papagaios$especie)
summary(papagaios)

#e)
0.8*500
Treino <- papagaios[1:400,]
Teste <- papagaios[401:500,]

#f)
str(papagaios)
modeloPapag <- rpart(especie ~., data = Treino, parms = list(split = "gini"), method = "class")
rpart.plot(modeloPapag, extra = 101)#mais f�cil visualiza��o

#g)
previsaoPapag <- predict(modeloPapag, newdata = Teste[, 1:3], type = "class")
previsaoPapag
mean(previsaoPapag == Teste$especie)
table(previsaoPapag, Teste$especie)
#o modelo de previsao atingiu taxa de acerto de cerca de 70%(neste teste). Ao observar a matriz confus�o, entende-se que os acertos (diagonal principal) s�o bem mais numeros que os erros(todos os outros algarismos). Al�m disso, a maior taxa de erro ocorreu na esp�cie corniculata, j� que apresentou (neste teste moment�neo) 16 erros, em contraste com 9 para a cirrhata e 2 para a arctica. Um fator que pode ter influenciado essa maior quantidade de erros � que (levando em considera��o as medias e o boxplot dos exerc�cios anteriores) a esp�cie arctica � mais distinta das outras duas que possuem certa aproxima��o nos valores ao serem comparadas, o que dificulta um pouco a classifica��o.



#Quest�o 2-

#a)
papagaios1 <- read.table("papagaio.txt", header = TRUE, sep = ',')

Arctica1 <- papagaios1[which(papagaios1$especie == "arctica"),]
Corniculata1 <- papagaios1[which(papagaios1$especie == "corniculata"),]
Cirrhata1 <- papagaios1[which(papagaios1$especie == "cirrhata"),]

#b)
coef_cor <- function(x,y)
{
  n   = length(x)
  n2  = length(y)
  xy  = x * y
  x2  = x * x 
  y2  = y * y
  r = ((n*sum(xy)) - (sum(x) * sum(y))) / (sqrt((((n*sum(x2))-(sum(x)^2)) * ((n*sum(y2))-(sum(y)^2)))))
  return(r)
}
resultArctica <- coef_cor(Arctica1$tamanho, Arctica1$peso)
resultArctica
resultCorniculata <- coef_cor(Corniculata1$tamanho, Corniculata1$peso)
resultCorniculata
resultCirrhata <- coef_cor(Cirrhata$tamanho, Cirrhata$peso)
resultCirrhata
#as vari�veis est�o mais relacionadas linearmente para a esp�cie "Corniculata", pois seu �ndice foi de 90%, enquanto a "Arctica" 68 e "Cirrhata" 71, aproximadamente.

#c)
modelCorni <- lm(Corniculata1$peso ~ Corniculata1$tamanho)
modelCorni

#d)
#peso <- 24.25 * tamanho - 300.59
#Uma varia��o de 0.5 no tamanho da ave proporcionaria uma altera��o de 12,125, pois construimos a fun��o peso <- 24.25 * tamanho - 300.59 a partir da reta reta de regress�o. O peso � dependente do tamanho e ao ser multiplicado 24.25 (coeficiente angular) por 0.5, o resultado altera em 12,125 pra mais se for positivo e pra -12,125 se for negativa essa varia��o.

#e)
min(Corniculata1$tamanho)
max(Corniculata1$tamanho)
calc_peso <- function(tam)
{
  if(tam < 23 || tam > 56){
    result <- "tamano fora do limite"
  }
  else{
    result <- (24.25*tam) - 300.59
  }
  return(result)
}
calc_peso(18)
calc_peso(41.01)
