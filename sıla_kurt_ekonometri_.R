# Gerekli kütüphaneler
install.packages("corrplot")   # Korelasyon ısı haritası için
install.packages("lmtest")     # Breusch-Pagan ve Durbin-Watson testleri için
install.packages("car")        # VIF testi (çoklu doğrusal bağlantı) için

# Kütüphaneleri aktif et
library(corrplot)
library(lmtest)
library(car)





# Verilerin manuel girişi
enflasyon <- c(54, 55, 57, 59, 61, 62, 63, 65, 66, 67, 68, 69)
faiz      <- c(30, 32, 35, 35, 37, 40, 42, 45, 45, 47, 48, 50)
doviz     <- c(32.0, 32.4, 33.1, 33.8, 34.5, 35.2, 36.0, 36.8, 37.5, 38.0, 38.6, 39.2)

veri <- data.frame(enflasyon, faiz, doviz)
print(veri)


# Veri
enflasyon <- c(54, 55, 57, 59, 61, 62, 63, 65, 66, 67, 68, 69)
faiz      <- c(30, 32, 35, 35, 37, 40, 42, 45, 45, 47, 48, 50)
doviz     <- c(32.0, 32.4, 33.1, 33.8, 34.5, 35.2, 36.0, 36.8, 37.5, 38.0, 38.6, 39.2)

veri <- data.frame(enflasyon, faiz, doviz)

# Betimsel istatistikler
summary(veri)



sapply(veri, mean)
sapply(veri, sd)


# 1 satır 2 sütun: önce histogramlar, sonra boxplotlar
par(mfrow = c(2, 3))  # 2 satır, 3 sütun = toplam 6 grafik

# Histogramlar
hist(veri$enflasyon, main = "Enflasyonun Dağılımı", xlab = "Enflasyon (%)", col = "lightgray", border = "white")
hist(veri$faiz, main = "Faiz Oranının Dağılımı", xlab = "Faiz (%)", col = "lightgray", border = "white")
hist(veri$doviz, main = "Döviz Kurunun Dağılımı", xlab = "USD/TL", col = "lightgray", border = "white")

# Boxplotlar
boxplot(veri$enflasyon, main = "Enflasyon Boxplot", col = "lightgray", horizontal = TRUE)
boxplot(veri$faiz, main = "Faiz Boxplot", col = "lightgray", horizontal = TRUE)
boxplot(veri$doviz, main = "Döviz Boxplot", col = "lightgray", horizontal = TRUE)



# Korelasyon matrisi
kor <- cor(veri)
kor

# Isı haritası


corrplot(kor,
         method = "color",
         addCoef.col = "black",
         tl.col = "black",
         number.cex = 0.9,
         main = "Korelasyon Isi Haritasi")  # ö yerine o, ı yerine i


# Çoklu doğrusal regresyon modeli
model <- lm(enflasyon ~ faiz + doviz, data = veri)

# Model özet tablosu
summary(model)



install.packages("lmtest")
library(lmtest)

bptest(model)   # Breusch-Pagan testi


# Artıkları al
residuals_model <- residuals(model)

# Normallik testi
shapiro.test(residuals_model)

# QQ grafiği
qqnorm(residuals_model)
qqline(residuals_model, col = "red")


dwtest(model)

#Çoklu bağlantı 

install.packages("car")
library(car)

vif(model)


