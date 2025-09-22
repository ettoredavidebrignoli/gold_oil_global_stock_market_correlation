install.packages("FinTS")
install.packages("fGarch")
install.packages("rmgarch")
install.packages("rugarch")

library(timeDate)
library(readr)
library(dplyr)
library(lubridate)
library(FinTS)
library(fGarch)
library(rmgarch)
library(rugarch)
library(strucchange)
library(tseries)
library(urca)
library(dynlm)
library(vars)
library(tidyr)
library(ggplot2)

# Definisci i percorsi dei file CSV
#ETTORE
pathG <- "D:\\Desktop\\Ettore\\Scuola\\4 Polimi\\Ingegneria matematica\\Anno 1\\Insurance and Econometrics\\Econometrics\\Project\\Datasets\\Gold.csv"
pathO <- "D:\\Desktop\\Ettore\\Scuola\\4 Polimi\\Ingegneria matematica\\Anno 1\\Insurance and Econometrics\\Econometrics\\Project\\Datasets\\Oil.csv"
pathM <- "D:\\Desktop\\Ettore\\Scuola\\4 Polimi\\Ingegneria matematica\\Anno 1\\Insurance and Econometrics\\Econometrics\\Project\\Datasets\\MSCI.csv"

# Definisci i percorsi dei file CSV
#AGATA
#pathG <- "C:\\Users\\HP\\Documents\\POLI\\1.2\\ECONOMETRICS\\Progetto\\Gold.csv"
#pathO <- "C:\\Users\\HP\\Documents\\POLI\\1.2\\ECONOMETRICS\\Progetto\\Oil.csv"
#pathM <- "C:\\Users\\HP\\Documents\\POLI\\1.2\\ECONOMETRICS\\Progetto\\MSCI.csv"

# Caricamento dei dati senza colonne "Unnamed"
Gold <- read_csv(pathG) 
Oil <- read_csv(pathO) 
MSCI <- read_csv(pathM)

# Controllo delle colonne e conversione della data
for (df in list(Gold, Oil, MSCI)) {
  if (!"Date" %in% names(df)) {
    names(df)[names(df) == "date"] <- "Date"
  }
}

# Merge con INNER JOIN per mantenere solo le date comuni
data <- Gold %>%
  inner_join(Oil, by="Date") %>%
  inner_join(MSCI, by="Date")

data$Date = as.Date(data$Date, format = "%m/%d/%Y")

data <- data[rev(1:nrow(data)), ]

# Visualizza i primi valori per controllo
head(data)

# Calcolo dei rendimenti cumulati rispetto al primo valore
df_returns <- data %>%
  mutate(across(-Date, ~ (. /dplyr::first(.) - 1) * 100)) %>%  # Rendimento percentuale
  pivot_longer(-Date, names_to = "Serie", values_to = "Rendimento")

# Plot
ggplot(df_returns, aes(x = Date, y = Rendimento, color = Serie)) +
  geom_line(size = 1) +
  labs(title = "Performance Percentuale Cumulata", 
       x = "Tempo", 
       y = "Rendimento (%)") +
  theme_minimal()


# Calcola i log ritorni per ciascuna variabile
lret <- data %>%
  transmute(
    Date = Date,
    lrG = log(Gold / lag(Gold)),
    lrO = log(Oil / lag(Oil)),
    lrM = log(MSCI / lag(MSCI))
  )

# Rimuovi la prima riga perch? avr? dei valori NA per i log ritorni (poich? manca il valore precedente)
lret <- lret %>%
  filter(!is.na(lrG) & !is.na(lrO) & !is.na(lrM))

# Plot con ggplot2
ggplot(lret, aes(x = Date, y = lrG)) +
  geom_line(color = "blue", size = 1) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linea dello zero
  labs(
    title = "Gold log returns",  # Titolo del grafico
    x = "Data", 
    y = "Log Return (%)"
  ) +
  theme_minimal()

ggplot(lret, aes(x = Date, y = lrO)) +
  geom_line(color = "green", size = 1) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linea dello zero
  labs(
    title = "Oil log returns",  # Titolo del grafico
    x = "Data", 
    y = "Log Return (%)"
  ) +
  theme_minimal()

ggplot(lret, aes(x = Date, y = lrM)) +
  geom_line(color = "orange", size = 1) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linea dello zero
  labs(
    title = "MSCI World log returns",  # Titolo del grafico
    x = "Data", 
    y = "Log Return (%)"
  ) +
  theme_minimal()

##########################################################################################################################

#Analisi preliminari sui dati completi

#Skeweness, Kurtosis, ecc...
summary(lret)

sd(lret$lrG)
skewness(lret$lrG)
kurtosis(lret$lrG)

sd(lret$lrO)
skewness(lret$lrO)
kurtosis(lret$lrO)

sd(lret$lrM)
skewness(lret$lrM)
kurtosis(lret$lrM)

plot(density(lret$lrG))
plot(density(lret$lrO))
plot(density(lret$lrM))

#Boxplot per outliers
df_long <- lret %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Return")

# Creiamo il box plot
ggplot(df_long, aes(x = Asset, y = Return, fill = Asset)) +
  geom_boxplot(outlier.size = 2, outlier.color = "red") +
  labs(title = "Distribuzione dei Log Returns per Asset", x = "Asset", y = "Log Return") +
  theme_minimal()



#Plottiamo ACF e PACF

#Goldhttp://127.0.0.1:34787/graphics/4432f874-65b2-4b09-af7b-d99c0f84fdc2.png
plot(acf(lret$lrG, 12, xlim = c(1,12))) # 12 is the maximum lag considered
plot(acf(lret$lrG, 12, type = "partial", xlim = c(1,12)))
res <- acf(lret$lrG, 12, type = "partial")
res

pacf(lret$lrG)


#Oil
plot(acf(lret$lrO, 12, xlim = c(1,12))) # 12 is the maximum lag considered
plot(acf(lret$lrO, 12, type = "partial", xlim = c(1,12)))
res <- acf(lret$lrO, 12, type = "partial")
res

#MSCI
plot(acf(lret$lrM, 12, xlim = c(1,12))) # 12 is the maximum lag considered
plot(acf(lret$lrM, 12, type = "partial", xlim = c(1,12)))
res <- acf(lret$lrM, 12, type = "partial")
res

#Boh ha senso farlo? Poi i plot fanni davvero schifo non so come cambiarli

#ADF test
adf.test(lret$lrG)
adf.test(lret$lrO)
adf.test(lret$lrM)

library(urca)

#Alternative method
summary(ur.df(lret$lrG, type = "none", lags = 5))
summary(ur.df(lret$lrO, type = "none", lags = 5))
summary(ur.df(lret$lrM, type = "none", lags = 5))

## Data are strongly stationary, and this is consistent across different lag values


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#Ora divido il dataset dei ritorni in pre e post covid
data_cutoff <- as.Date("2020-01-01")

# Dividere in due dataset
postcovid <- subset(lret, lret$Date >= data_cutoff)
precovid <- subset(lret, lret$Date < data_cutoff)

######
#Ora cominciamo l'analisi per la costruzione del modello precovid, dopo ripeteremo lo stesso per post covid
######

#VAR lag selection
y_pre <- cbind(precovid$lrG, precovid$lrO, precovid$lrM)
colnames(y_pre) <- c("Gold","Oil", "MSCI")

y_pre.VAR.IC <- VARselect(y_pre, type="const")
y_pre.VAR.IC$selection
y_pre.VAR.IC$criteria

# Select the lag number based on the BIC
nlags_pre <- y_pre.VAR.IC$selection["AIC(n)"]
nlags_pre

#Model estimation
model_pre <- VAR(y_pre, p = nlags_pre, type = "const")
summary(model_pre)

summary(ur.df(precovid$lrG, type = "none", lags = 5))
summary(ur.df(precovid$lrO, type = "none", lags = 5))
summary(ur.df(precovid$lrM, type = "none", lags = 5))

summary(ur.df(postcovid$lrG, type = "none", lags = 5))
summary(ur.df(postcovid$lrO, type = "none", lags = 5))
summary(ur.df(postcovid$lrM, type = "none", lags = 5))


### IMPULSE RESPONSES LHo copiato questa parte dal lab COintegration, dobbiamo fare le impulse response per ogni coppia?

variables <- c("Gold", "Oil", "MSCI")

# Initialize an empty list to store the IRFs
irf_results_pre <- list()

# Loop over all combinations of impulse and response variables
for (imp in variables) {
  for (resp in variables) {
    # Compute the IRF for each impulse-response pair
    irf_results_pre[[paste(imp, resp, sep = "_")]] <- irf(model_pre, 
                                                      impulse = imp, 
                                                      response = resp, 
                                                      n.ahead = 20, 
                                                      boot = TRUE)
  }
}

# Loop through the IRF results and plot each one
for (irf_name in names(irf_results_pre)) {
  plot(irf_results_pre[[irf_name]], main = paste("IRF of", irf_name))
}

# OIL - GOLD 

feir1 <- irf(model_pre, impulse = "Gold", response = "Oil",
             n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir1)

feir2 <- irf(model_pre, impulse = "Oil", response = "Gold",
             n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir2)

feir3 <- irf(model_pre, impulse = "Gold", response = "Oil",
             n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir3)
plot(feir1)

feir4 <- irf(model_pre, impulse = "Oil", response = "Gold",
             n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir4)
plot(feir2)

#Granger causality

causality(model_pre, cause = "Gold")
causality(model_pre, cause = "Oil")
causality(model_pre, cause = "MSCI")

#Granger Causality: does the past of variable x help improve the prediction of future values of y? 
#Instantaneous Causality: does knowing the future of x help me better predict the future of y?


######
#Postcovid
######

#VAR lag selection
y_post <- cbind(postcovid$lrG, postcovid$lrO, postcovid$lrM)
colnames(y_post) <- c("Gold","Oil", "MSCI")

y_post.VAR.IC <- VARselect(y_post, type="const")
y_post.VAR.IC$selection
y_post.VAR.IC$criteria

# Select the lag number based on the AIC
nlags_post <- y_post.VAR.IC$selection["AIC(n)"]
nlags_post

#Model estimation
model_post <- VAR(y_post, p = nlags_post, type = "const")
summary(model_post)

roots(model_post)

# OIL - GOLD 

feir1 <- irf(model_post, impulse = "Gold", response = "Oil",
             n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir1)

feir2 <- irf(model_post, impulse = "Oil", response = "Gold",
             n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir2)

feir3 <- irf(model_post, impulse = "Gold", response = "Oil",
             n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir3)
plot(feir1)

feir4 <- irf(model_post, impulse = "Oil", response = "Gold",
             n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir4)
plot(feir2)

#Granger causality

causality(model_post, cause = "Gold")
causality(model_post, cause = "Oil")
causality(model_post, cause = "MSCI")


#Sotto c'? la parte di GARCH che avevamo gi? fatto

archG <- ArchTest(lret$lrG, lags = 1, demean = TRUE)
archG

archO <- ArchTest(lret$lrO, lags = 1, demean = TRUE)
archO

archM <- ArchTest(lret$lrM, lags = 1, demean = TRUE)
archM

##################################### COSTRUIAMO IL GARCH PER LE 3 TIME SERIES #######################################################

## DCC - PRECOVID

#Gold
spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = 'eGARCH', garchOrder = c(1,1)), distribution.model = 'std')

egarch.fit.G_pre = ugarchfit(data = as.array(precovid$lrG), spec = spec)
egarch.fit.G_pre

#Oil
egarch.fit.O_pre = ugarchfit(data = as.array(precovid$lrO), spec = spec)
egarch.fit.O_pre

#MSCI
egarch.fit.M_pre = ugarchfit(data = as.array(precovid$lrM), spec = spec)
egarch.fit.M_pre

################################??
spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = 'eGARCH', garchOrder = c(1,1)), distribution.model = 'std')
dcc_spec <- dccspec(uspec = multispec(list(spec, spec, spec)),
                    dccOrder = c(1, 1), distribution = "mvt")

library(xts)

# Usa la colonna delle date in lret
precovid_xts <- xts(cbind(precovid$lrG, precovid$lrO, precovid$lrM), order.by = as.Date(precovid$Date))

# Imposta i nomi delle colonne
colnames(precovid_xts) <- c("Gold", "Oil", "MSCI")


dcc_fit_pre <- dccfit(dcc_spec, data = precovid_xts)
dcc_fit_pre

####################?
# Estrai le correlazioni dinamiche condizionali
correlations_pre <- rcor(dcc_fit_pre)
correlations_pre

# Visualizza la correlazione condizionale tra asset1 e asset2
plot(correlations_pre[1, 2, ], type = "l", col = "blue", 
     xlab = "Time", ylab = "Conditional Correlation",
     main = "Conditional Correlation between Asset 1 and Asset 2")

# Visualizza la correlazione condizionale tra asset1 e asset3
plot(correlations_pre[1, 3, ], type = "l", col = "red", 
     xlab = "Time", ylab = "Conditional Correlation",
     main = "Conditional Correlation between Asset 1 and Asset 3")

# Visualizza la correlazione condizionale tra asset2 e asset3
plot(correlations_pre[2, 3, ], type = "l", col = "green", 
     xlab = "Time", ylab = "Conditional Correlation",
     main = "Conditional Correlation between Asset 2 and Asset 3")

#######################################
 
# DCC - POSTCOVID

#Gold
spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = 'eGARCH', garchOrder = c(1,1)), distribution.model = 'std')

egarch.fit.G_post = ugarchfit(data = as.array(postcovid$lrG), spec = spec)
egarch.fit.G_post

#Oil
egarch.fit.O_post = ugarchfit(data = as.array(postcovid$lrO), spec = spec)
egarch.fit.O_post

#MSCI
egarch.fit.M_post = ugarchfit(data = as.array(postcovid$lrM), spec = spec)
egarch.fit.M_post

################################??
spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = 'eGARCH', garchOrder = c(1,1)), distribution.model = 'std')
dcc_spec <- dccspec(uspec = multispec(list(spec, spec, spec)),
                    dccOrder = c(1, 1), distribution = "mvt")

library(xts)

# Usa la colonna delle date in lret
postcovid_xts <- xts(cbind(postcovid$lrG, postcovid$lrO, postcovid$lrM), order.by = as.Date(postcovid$Date))

# Imposta i nomi delle colonne
colnames(postcovid_xts) <- c("Gold", "Oil", "MSCI")


dcc_fit_post <- dccfit(dcc_spec, data = postcovid_xts)
dcc_fit_post

####################?
# Estrai le correlazioni dinamiche condizionali
correlations_post <- rcor(dcc_fit_post)
correlations_post

# Visualizza la correlazione condizionale tra asset1 e asset2
plot(correlations_post[1, 2, ], type = "l", col = "blue", 
     xlab = "Time", ylab = "Conditional Correlation",
     main = "Conditional Correlation between Asset 1 and Asset 2")

# Visualizza la correlazione condizionale tra asset1 e asset3
plot(correlations_post[1, 3, ], type = "l", col = "red", 
     xlab = "Time", ylab = "Conditional Correlation",
     main = "Conditional Correlation between Asset 1 and Asset 3")

# Visualizza la correlazione condizionale tra asset2 e asset3
plot(correlations_post[2, 3, ], type = "l", col = "green", 
     xlab = "Time", ylab = "Conditional Correlation",
     main = "Conditional Correlation between Asset 2 and Asset 3")
