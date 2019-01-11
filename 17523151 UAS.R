#Nama : Dwi Abdul Rahman
#Nim : 17523151
#UAS MATLAN 2
#-----------------------
#1

tabel11

model <- lm(yi ~ xi, data=tabel11)
summary(model)
# y= - 93.6+2.7x
#-----------------------
#2
predict(model , data.frame(xi=55))

#-----------------------
#3

library(polynom)
xi <- c(0, 1, 2, 3, 4)
yi <- c(1, 2.25, 3.75, 4.25, 5.65)
dat <- data.frame(cbind(xi, yi))
poly.calc(xi, yi)

#-----------------------
#4

f1 <- function(x){
  return (1 - 0.07916667*x + 2.19375*x^2 - 0.9958333*x^3 + 0.13125*x^4 )
}
f1(2.75)

#-----------------------
#5

plot(xi,yi)
curve(f1, add=TRUE)

#-----------------------
#11

library(pracma)
fx <- function(x){
  return (x^2-6)
}
trapzfun(fx,0,1)

#-----------------------
#12

trapezoid <- function(f, a, b){
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')}
  h <- b - a
  fxdx <- (h / 2) * (f(a) + f(b))
  return(fxdx)
}
f <- function(x) {
  return(x^3+4*x^2-10)
}
trapezoid(f, 1,2)

#-----------------------
#13 dan 14

h <- 0.1
x <- seq(0,1,by=h)
f <- function(x){
  return(x^2)
}
f0<-f(x[1])
fi<-sapply(x[2:10],f)
fn<-f(x[length(x)])

trap<- function(f0,fi,fn,h){
  #L<-h*(f0+2*sum(fi)+fn)/2
  L<-h*(f0+2*sum(fi)+fn)/2
  return(L)
}
trap(f0,fi,fn,h)

#-----------------------
#15

h <- 0.2
x <- seq(0,1,by=h)
f <- function(x){
  return(x^2)
}
f0<-f(x[1])
fi<-sapply(x[2:6],f)
fn<-f(x[length(x)])

trap<- function(f0,fi,fn,h){
  L<-h*(f0+2*sum(fi)+fn)/2
  return(L)
}
trap(f0,fi,fn,h)
#-----------------------