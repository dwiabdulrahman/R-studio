#nama:
#Girendra egie zuhrival (17523154)
#dwi abdul rahman (17523151)
#-------------------------------
#Trepezoidal Rule

library (matlib)

trapezoid <- function(f, a, b){
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')}
  
  h <- b - a
  
  fxdx <- (h / 2) * (f(a) + f(b))
  
  return(fxdx)
}


f <- function(x) {
  return(2+x^4)
}

trapezoid(f, 0,6)
#--------------------------------

#Simpson Rule

simpsons.rule <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / 2
  x0 <- a
  x1 <- a + h
  x2 <- b
  
  s <- (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
  
  return(s)
}

f2 <- function(x) {
  return(2+x^4)
}

simpsons.rule(f2, 0, 6)