# An-interactive-Time-Series-Analysis-Dashboard-built-using-R-and-Shiny.
# Time Series Forecasting Dashboard using R Shiny

Hey there, this is an interactive RShiny app that allows users to perform basic Time Series Analysis( no code!). Feel free to drop your suggestions or collaborate with us to make this app better. Find a quick run through below:

---

## 🔍 Features

📁 **Upload Your Own CSV or Excel File**  

📊 **Comprehensive Time Series Analysis**  
- Summary statistics  
- Interactive line plots  
- Decomposition to detect trend/seasonality  
- ACF and PACF plots  
- Augmented Dickey-Fuller test for stationarity  
- Model fitting and forecasting using:
  - ARIMA
  - SARIMA (if seasonality is detected)
  - GARCH (if volatility clustering is detected)
  - Auto ARIMA

🧠 **Smart Model Recommendation Engine**  
- Automatically checks for seasonality and volatility.
- Recommends SARIMA if seasonality is found.
- Suggests GARCH if volatility clustering is detected (via residual ACF + squared ACF).

📉 **Residual Diagnostics**  
- Histogram of residuals  
- Shapiro-Wilk test for normality  
- Ljung-Box test  
- ACF plots for residuals and squared residuals  

📥 **Report Download**  
- Generate a downloadable report 
- Summary of selected model, forecast, plots, and diagnostics  
- Automatically reflects recommendations in the report  

---

## 📁 Files in This Repository

| File | Description |
|------|-------------|
| `app.R` | Main Shiny app code |
| `report_template.Rmd` | R Markdown template for dynamic report generation |
| `README.md` | This documentation file |

---

## ▶️ How to Run the App

1. Clone the repo:
   ```bash
   git clone https://github.com/your-username/your-repo-name.git

Developed by students of 'CHRIST Deemed to be University'- MSc Statistics (2024-26)
Elizabeth Joseph
Deeksha Reddy
Gayathri A
Gayathri Vijayan
Gowri MS

NOTE:
This project is for academic and educational purposes only.
Feel free to use or modify with credit to the authors.
