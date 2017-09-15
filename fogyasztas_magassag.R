library(Rcmdr)
#########  Adatok beolvasása
oda <- readXL("C:/PE_GK/bjs/elokeszites.xlsx", rownames=FALSE, header=TRUE,
   na="", sheet="oda", stringsAsFactors=FALSE)
vissza <- readXL("C:/PE_GK/bjs/elokeszites.xlsx", rownames=FALSE,
  header=TRUE, na="", sheet="vissza", stringsAsFactors=FALSE)

plot(c(oda[,1],oda[,2],oda[,3],oda[,4],oda[,5]),c(oda[,6],oda[,7],oda[,8],oda[,9],oda[,10]),xlab="tangens",ylab="fogyasztás",type="n")
for (i in 1:5) points(oda[,i],oda[,i+5],col=i)
for (i in 1:5) points(vissza[,i],vissza[,i+5],pch=16,col=i)
# oda_tan_atlag=apply(oda[1:5],1,mean) nem müxik
# A=rowMeans(as.matrix(oda[1:5]))  ez sem müxik
oda_tan=c(oda[,1],oda[,2],oda[,3],oda[,4],oda[,5])
oda_fogy=c(oda[,6],oda[,7],oda[,8],oda[,9],oda[,10])
vissza_tan=c(vissza[,1],vissza[,2],vissza[,3],vissza[,4],vissza[,5])
vissza_fogy=c(vissza[,6],vissza[,7],vissza[,8],vissza[,9],vissza[,10])
plot(c(oda_fogy,vissza_fogy),c(oda_tan,vissza_tan),type="n",xlab="Fogyasztás",ylab="tangens")
points(oda_fogy,oda_tan)
points(vissza_fogy,vissza_tan,col=2)

m_oda=lm(oda_tan~oda_fogy)
m_vissza=lm(vissza_tan~vissza_fogy)
points(oda_fogy,predict(m_oda),col=3)
points(vissza_fogy,predict(m_vissza),col=4)

# Modell diagnosztika
op=par(mfrow=c(2,4))
plot(m_oda)
plot(m_vissza)
op=par(mfrow=c(1,1))

predict_oda=predict(m_oda,interval="predict")
predict_vissza=predict(m_vissza,interval="predict")
points(oda_fogy,predict_oda[,2],pch="*")
points(oda_fogy,predict_oda[,3],pch="*")
points(vissza_fogy,predict_vissza[,2],pch="*",col=2)
points(vissza_fogy,predict_vissza[,3],pch="*",col=2)

# Egyesítve az oda és a vissza adatokat
meredekseg=c(oda_tan,vissza_tan)
fogy=c(oda_fogy,vissza_fogy)
mod=lm(meredekseg~fogy)
plot(fogy,meredekseg,xlab="Fogyasztás",ylab="tangens",main="95%-os elõrejelzési intervallum")
pred=predict(mod,interval="predict")
points(fogy,pred[,1],col=2)
points(fogy,pred[,2],col=2,pch=16,cex=1.2)
points(fogy,pred[,3],col=2,pch=16,cex=1.2)