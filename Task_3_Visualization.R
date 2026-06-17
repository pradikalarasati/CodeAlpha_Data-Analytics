# ==========================================
# TASK 3: DATA VISUALIZATION
# ==========================================

library(forecast)
library(ggplot2)

# Menggunakan data dari langkah sebelumnya
Pajak_Hotel_ts <- ts(mydata$Data, start=c(2019,1), frequency=12)

# 1. Visualisasi Tren Historis Utama
par(mfrow=c(1,1))
ts.plot(Pajak_Hotel_ts, col='blue', main="Tren Realisasi Pajak Hotel (2019-2024)", ylab="Nilai Pajak (Rp)", lwd=2)

# 2. Pemodelan Otomatis untuk Mengatasi Efek Musiman (SARIMA)
best_model <- auto.arima(Pajak_Hotel_ts, seasonal = TRUE, stepwise=FALSE, approximation=FALSE, D=1)

# 3. Visualisasi Kompilasi Diagnostik Residual (White Noise Check)
# Menampilkan 3 plot sekaligus: Residual Plot, ACF Residual, dan Histogram
checkresiduals(best_model)

# 4. Pembuatan Ramalan 12 Bulan ke Depan
forecast_result <- forecast(best_model, h = 12)

# 5. Visualisasi Hasil Forecast dengan Interval Keyakinan (80% & 95%)
autoplot(forecast_result) +
  ggtitle("Prediksi Realisasi Pajak Hotel 12 Bulan ke Depan") +
  ylab("Nilai Pajak (Rp)") +
  xlab("Tahun") +
  theme_minimal()

# 6. Visualisasi Perbandingan Komprehensif: Data Asli vs Fitted vs Prediksi
autoplot(Pajak_Hotel_ts, series = "Data Asli") +
  autolayer(fitted(best_model), series = "Fitted (Kecocokan Model)", lwd=1) +
  autolayer(forecast_result, series = "Prediksi", alpha=0.5) +
  ggtitle("Akurasi Model SARIMA: Data Asli, Fitted, dan Hasil Prediksi") +
  ylab("Nilai Pajak (Rp)") +
  xlab("Tahun") +
  guides(colour = guide_legend(title = "Keterangan:")) +
  theme_minimal()
