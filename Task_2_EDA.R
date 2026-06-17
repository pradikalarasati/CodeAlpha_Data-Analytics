# ==========================================
# TASK 2: EXPLORATORY DATA ANALYSIS (EDA)
# ==========================================

# 1. Input Data
library(readxl)
Pajak_Hotel <- read_excel("~/PKL BPPKAD/Laporan/Pajak Hotel.xlsx")
mydata <- Pajak_Hotel

# 2. Konversi ke Runtun Waktu & Plot Eksplorasi Awal
Pajak_Hotel_ts <- ts(mydata$Data, start=c(2019,1), frequency=12)

cat("--- Statistik Deskriptif Awal ---\n")
summary(Pajak_Hotel_ts)

# 3. Uji Stasioneritas (Exploratory Testing)
library(tseries)
cat("\n--- Uji ADF Data Asli ---\n")
adf.test(Pajak_Hotel_ts) # Memeriksa p-value (> 0.05 berarti tidak stasioner)

cat("\n--- Uji ADF Setelah Differencing ---\n")
adf.test(diff(Pajak_Hotel_ts)) # Memeriksa p-value (< 0.05 berarti stasioner)

# 4. Identifikasi Orde Melalui ACF & PACF
par(mfrow=c(2,1))
acf(as.numeric(diff(Pajak_Hotel_ts)), lag.max = 24, main="ACF Data Differencing")
pacf(as.numeric(diff(Pajak_Hotel_ts)), lag.max = 24, main="PACF Data Differencing")

# 5. Evaluasi & Perbandingan Model Kompetitor (Mencari AIC Terkecil)
library(forecast)
model1 <- arima(Pajak_Hotel_ts, order = c(1,1,0))
model2 <- arima(Pajak_Hotel_ts, order = c(0,1,1))
model4 <- arima(Pajak_Hotel_ts, order = c(2,1,0))

cat("\n--- Perbandingan Nilai AIC Model Kandidat ---\n")
aic.model <- data.frame(
  Model = c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(2,1,0)"),
  AIC = c(model1$aic, model2$aic, model4$aic)
)
print(aic.model)

# 6. Uji Diagnostik Formal pada Sisaan Model Terbaik
sisaan <- model2$residuals

cat("\n--- Uji Formal Normalitas Sisaan (KS-Test) ---\n")
ks.test(sisaan, "pnorm")

cat("\n--- Uji Nilai Tengah Sisaan (t-Test) ---\n")
t.test(sisaan, mu = 0, alternative = "two.sided")

cat("\n--- Uji Autokorelasi Sisaan (Ljung-Box) ---\n")
Box.test(sisaan, lag = 23, type = "Ljung")

