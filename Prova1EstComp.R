#fx <- function(x) {
#  return(exp(2*x+(x^2)))
#}
#EstimativaIntegral <- function(a,b){
#  integrate(fx, a, b)
#}
#EstimativaIntegral(-1,2)
#Prova 1 - Arthur Resende Santos

#a)
alt.cm <- 2.54*heights$height
heights <- cbind(heights, alt.cm)

#b) Proporcao eh 812/1050
sum(heights$sex == "Male")

#c) altura maxima eh 210cm, que pertence a um homem
max(heights$alt.cm)
which(heights$alt.cm == 210)
heights[1017,]

#d)Ha duas pessoas com uma altura minima de 127cm, dois homens
min(heights$alt.cm)
which(heights$alt.cm == 127)
heights[1032,]
heights[1045,]

#e)