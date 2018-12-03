# Nama
# Dwi Abdul Rahman ( 17523151 )
# Girendra Egie Zuhrival (17523154)
#-----------------------------------------------------
#Data
data1<-read.csv(file="dataproduksi.csv", header=TRUE)
data1

#Linear Regresion
model <-lm(jamKerja ~ JumlahProduksi, data=data1)
summary(model)

plot(jamKerja ~ JumlahProduksi, data=data1)
abline(model, col = "red", lwd = 1)

# Predicting New Value based on our model
predict(model, data.frame(JumlahProduksi = 70))

#Polinomial Regresion
poly_model <- lm(jamKerja ~ poly(JumlahProduksi,degree=2), data = data1)
poly_model

x <- with(data1, seq(min(JumlahProduksi), max(JumlahProduksi), length.out=2000))
y <- predict(poly_model, newdata = data.frame(JumlahProduksi = x))

plot(jamKerja ~ JumlahProduksi, data = data1)
lines(x, y, col = "red")
