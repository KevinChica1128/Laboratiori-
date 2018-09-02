# Kevin García - 1533173
# Alejandro Vargas -
# Alejandro Soto -
# Laboratorio 1 - Multivariada

#Ingresamos los datos y conformamos la matriz
planta<-c(1,2,3,4,5,6,7,8,9,10)
x1<-c(1.89,1.92,1.95,2.11,1.78,2.12,2.06,2.19,1.93,1.84)
x2<-c(53,62,71,75,58,77,82,78,60,72)
x3<-c(4.5,5.2,5.4,6.1,5,6.5,7.6,7.3,5.5,5.7)
x4<-c(120,136,145,156,127,160,169,164,142,147)

x<-matrix(c(x1,x2,x3,x4),10,4)
colnames(x)<-c("Altura planta","Longitud radicular","Área foliar","Peso pulpa")

#Función desviación estandar sobre n:
sd2 <- function (x) {
  
  sqrt(sum((x - mean(x))^2) / (length(x)))
  
} 

#--------------------------------------------------------------------------#
#Punto 1:

g<-c(mean(x1),mean(x2),mean(x3),mean(x4)) #Individuo (Planta) Promedio

#Inercia con los datos originales
s<-c()
for (i in 1:10) {
  s[i]<-c((t(x[i,]-g))%*%(x[i,]-g))
}
ine<-sum(s)/10 #Inercia

#Inercia con datos centrados
#Centramos datos
X1<-(x1-mean(x1))
X2<-(x2-mean(x2))
X3<-(x3-mean(x3))
X4<-(x4-mean(x4))

X<-matrix(c(X1,X2,X3,X4),10,4)
colnames(X)<-c("Altura planta","Longitud radicular","Área foliar","Peso pulpa")

G<-c(mean(X1),mean(X2),mean(X3),mean(X4)) #Planta Promedio (Origen por datos centrados)

S<-c()
for (i in 1:10) {
  S[i]<-c((t(X[i,]-G))%*%(X[i,]-G))
}
In<-sum(S)/10 #Inercia

#--------------------------------------------------------------------------#
#Punto 2:
#Estandarización:
z1 <- (x1 - mean(x1))/sd2(x1) 
z2 <- (x2 - mean(x2))/sd2(x2)
z3 <- (x3 - mean(x3))/sd2(x3)
z4 <- (x4 - mean(x4))/sd2(x4)


#Matriz de dats estandarizada:
Z <- matrix(c(z1,z2,z3,z4),10,4)

gs<-c(mean(z1),mean(z2),mean(z3),mean(z4)) #Punto de gravedad con datos estandarizados

#Inercia con los datos originales
ss<-c()
for (i in 1:10) {
  ss[i]<-c((t(Z[i,]-gs))%*%(Z[i,]-gs))
}
Is<-sum(ss)/10 #Inercia (Como estan estandarizados debe ser igual al número de variables)
 
#--------------------------------------------------------------------------#
#Punto 3:

#Matriz de varianzas y covarianzas
V<-cov(x)

#--------------------------------------------------------------------------#
#Punto 4:

#Matriz de correlaciones
R<-cor(x)

#--------------------------------------------------------------------------#
#Punto 5:
#Matriz de varianzas y covarianzas:
dv <- eigen(V)  # Valores y Vectores propios de V
u <- dv$vectors
l <- dv$values

#Matriz de correlaciones:
dvR <- eigen(R)  # Valores y Vectores propios de R
uR <- dvR$vectors
lR <- dvR$values


#-------------------------------------------------------------------------#
#punto 8:
t<-x%*%u
T<-X%*%u
Tz<-Z%*%uR
#------------------------------------------------------------------------#
#punto 9:
install.packages("scatterplot3d")
library(scatterplot3d)
X11()
scatterplot3d(x1,x2,x3,angle = 40,xlab = "altura planta",ylab = "longitud radicular",zlab = "area foliar")
X11()
scatterplot3d(x1,x2,x3,angle = 120,xlab = "altura planta",ylab = "longitud radicular",zlab = "area foliar")
X11()
scatterplot3d(x1,x2,x3,angle = 200,xlab = "altura planta",ylab = "longitud radicular",zlab = "area foliar")

#-------------------------------------------------------------------------#
#Punto 10:
x11()
plot(t[,1],t[,2])
text(t[,1],t[,2]+0.22,c(1,2,3,4,5,6,7,8,9,10))

x11()
plot(T[,1],T[,2])
text(T[,1],T[,2]+0.25,c(1,2,3,4,5,6,7,8,9,10))

x11()
plot(Tz[,1],Tz[,2])
text(Tz[,1],Tz[,2]+0.05,c(1,2,3,4,5,6,7,8,9,10))


X11()
scatterplot3d(t[,1],t[,2],t[,3],xlab = "componente 1",ylab = "componente 2",zlab = "componente 3")
X11()
scatterplot3d(T[,1],T[,2],T[,3],xlab = "componente 1",ylab = "componente 2",zlab = "componente 3")
X11()
scatterplot3d(Tz[,1],Tz[,2],Tz[,3],xlab = "componente 1",ylab = "componente 2",zlab = "componente 3")

