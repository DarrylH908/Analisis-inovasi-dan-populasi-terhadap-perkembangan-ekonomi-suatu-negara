library(car)
library(lmtest)
library(tseries)

inovasi <- read.csv("C:\\Users\\ASUS\\OneDrive\\Desktop\\Data Worldbank\\Data Inovasi.csv")
inovasi

#Kita bikin korelasi antara jumlah penduduk, inovasi dengan GDP Per kapita
print(cor(inovasi[,c('Populasi','Inovasi','GDP.Per.Kapita')]))
#Dari kita lihat hasilnya tidak ada lebih dari 0.8 hubungan antara variabel variabelnya

#Sekarang kita regresikan hubungan antara jumlah penduduk , inovasi terhadap GDP per Kapita
regressi <- lm(GDP.Per.Kapita~Populasi+Inovasi,data=inovasi)
summary(regressi)

#Kesimpulan 
#Semakin bertambah 1 penduduk maka GDP Per Kapita akan berkurang sebesar 2.866e-05
#Semakin nilai inovasi bertambah 1 poin maka GDP Per Kapita akan bertambah sebesar 1.575e+03

#Uji Multikolienaritas
vif(regressi)
#Kesimpulan
#Tidak terjadi multikolinearitas dikarenakan angka 1.005767 kurang dari 10

#Uji Heteroskedastisitas
bptest(regressi)
#Tidak erjadi heteroskedastisitas dikarenakan p valuenya lebih dari 0.05 dengan angka 0.4563

#Uji Normalitas
residus <- resid(regressi)
jarque.bera.test(residus)
plot(residus)
hist(residus)
#Residu tidak terdistribusi normal dan normalitas tidak terpenuhi karena p value < 0.00

#Uji Autokorelasi
dwtest(regressi)
#Tidak terjadi autokorelasi karena p value 0.7243