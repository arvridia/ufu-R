#Questão 2-

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
#as variáveis estão mais relacionadas linearmente para a espécie "Corniculata", pois seu índice foi de 90%, enquanto a "Arctica" 68 e "Cirrhata" 71, aproximadamente.

#c)
modelCorni <- lm(Corniculata1$peso ~ Corniculata1$tamanho)
modelCorni

#d)
#peso <- 24.25 * tamanho - 300.59
#Uma variação de 0.5 no tamanho da ave proporcionaria uma alteração de 12,125, pois construimos a função peso <- 24.25 * tamanho - 300.59 a partir da reta reta de regressão. O peso é dependente do tamanho e ao ser multiplicado 24.25 (coeficiente angular) por 0.5, o resultado altera em 12,125 pra mais se for positivo e pra -12,125 se for negativa essa variação.

#e)