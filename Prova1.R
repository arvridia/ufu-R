# Aluno: Henrique Braga Alves Pereira
# Matricula: 12011BCC017
# EX 1
#a)
alt.cm <- 2.54*heights$height
heights <- cbind(heights, alt.cm)

#b) A Proporcao apresentada é de 812/1050.
sum(heights$sex == "Homens")

#c) A altura maxima apresentada na tabela é de210cm, a qual pertence ao um individuo masculino.
max(heights$alt.cm)
which(heights$alt.cm == 210)
heights[1017,]

#d) Na tabela esta presente esta duas pessoas com a altura minima de 127cm.
min(heights$alt.cm)
which(heights$alt.cm == 127)
heights[1032,]
heights[1045,]

#e)A media apresentada é de 173.5405cm e desvio padrao de 10.35969.
mean(heights$alt.cm)
sd(heights$alt.cm)

#f) O dataframe informa que a atulta dos homens possue media maior que os da mulheres, por torno de 12cm de comprimento. Outro fator é 
# desvio padrão, que apresenta um menor desvio ao ser comparado com os da mulhres.
females <- heights[heights$sex == "Mulheres", ]
males <- heights[heights$sex == "Homens", ]
mean(males$alt.cm)
sd(males$alt.cm)
mean(females$alt.cm)
sd(females$alt.cm)

# EX 2

# 0 = Coroa = neto(7 reais) +1
# 1 = Cara = vovó(18 reais) +1
# vovo juju ficar sem dinheiro

VovóJuju <- 18
Jorel <- 7

# j = número de jogadas
j <- 0
resultado <- c()
repeat{
  lançarMoeda <- sample(c(0,1), size = 1, replace = TRUE)
  j <- j + 1
  resultado <- c(resultado,lançarMoeda)
  if(lançarMoeda == 0){
    VovóJuju <- VovóJuju - 1 
    Jorel <- Jorel + 1
  }
  if(lançarMoeda == 1){
    VovóJuju <- VovóJuju + 1
    Jorel <- Jorel - 1
  }
  if(VovóJuju == 0 | Jorel == 0){
    break
  }
}
j
resultado

# PROBABILIDADE DE VOVO JUJU PERDER - Monte Carlo

VovóJuju <- 18
Jorel <- 7

# a = perdas da vovojuju
a <- 0
#b = perdas de jorel
b <- 0

# contador
x <- 0 

while(x != 100000)
{
  lançarMoeda <- sample(c(0,1), size = 1, replace = TRUE)
  x <- x + 1
  if(lançarMoeda == 0){
    VovóJuju <- VovóJuju - 1 
    Jorel <- Jorel + 1
  }
  if(lançarMoeda == 1){
    VovóJuju <- VovóJuju + 1
    Jorel <- Jorel - 1
  }
  if(VovóJuju == 0){
    a <- a + 1
  }
  if(Jorel == 0){
    b <- b + 1
  }
}
mean(a)/100000
mean(b)/100000


