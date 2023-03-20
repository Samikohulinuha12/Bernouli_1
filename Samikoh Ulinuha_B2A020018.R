#Nama : Samikoh Ulinuha 
#NIM  : B2A020018
#Kelas:A
#______________________Teknik Simulasi___________________________


#Multiplicative
multiplicative_RNG<-function(a,z0,m,n){
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ", "Xj", "Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3])
  View(xj)
}

#Bernouli_1
Bernouli_1<-function(n,p){
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for(z in 1:i) ifelse(X[z]<=p, Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
#Angka 5=5
Bernouli_1(5, 0.65)
#Angka 7=5
Bernouli_1(7, 0.65)
#Angka 11=5
Bernouli_1(11, 0.65)
#Angka 17=4
Bernouli_1(17, 0.65)
#Angka 37=5
Bernouli_1(37, 0.65)
#Angka 43=4
Bernouli_1(43, 0.65)
#Angka 53=5
Bernouli_1(53, 0.65)
#Angka 61=4
Bernouli_1(61, 0.65)
#Angka 65=4
Bernouli_1(65, 0.65)
#Angka 67=4
Bernouli_1(67, 0.65)
#Angka 79=5
Bernouli_1(79, 0.65)
#Angka 83=4
Bernouli_1(83, 0.65)
#Angka 89=5
Bernouli_1(89, 0.65)
#Angka 91=5
Bernouli_1(91, 0.65)
#Angka 97=5
Bernouli_1(97, 0.65)
#Angka 103=4
Bernouli_1(103, 0.65)
#Angka 107=5
Bernouli_1(107, 0.65)
#Angka 109=5
Bernouli_1(109, 0.65)
#Angka 113=5
Bernouli_1(113, 0.65)
#Angka 125=4
Bernouli_1(125, 0.65)
#Angka 137=4
Bernouli_1(137, 0.65)


Bernouli_1(100,0.65)
