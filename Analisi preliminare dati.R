# ANALISI PRELIMINARE DEI DATI #
# TABELLA #

tabella <- data.frame(
  variabile = character(),
  media = numeric(),
  mediana = numeric(),
  sd = numeric(),
  varianza = numeric(),
  CV = numeric(),
  percent_fuori = numeric(),
  livello = character(),
  stringsAsFactors = FALSE
)

risultati <- list()

for (col in names (dati)) {   # CILO FOR # # Esegue il codice per tutte le variabili inserite #
  
  x <- dati[[col]]
  
  # === AGGIUNTA 1: PROTEZIONE ===
  # Se la colonna non è numerica (es. Nomi Trattamenti), il ciclo salta al prossimo giro
  if(!is.numeric(x)) next 
  # ==============================
  
  # Output a Video #
  cat("\n=====================================\n")
  cat("Analisi della variabile:", col, "\n")
  cat("=====================================\n")
  
  str(x)
  # print(head(x)) # Commentato per pulizia, decommenta se vuoi vederlo
  
  # Calcolo Statistiche Base (uso na.rm=TRUE per sicurezza)
  media <- mean(x, na.rm=TRUE)
  mediana <- median(x, na.rm=TRUE)
  varianza <- var(x, na.rm=TRUE)
  dev_std <- sd(x, na.rm=TRUE)
  range_val <- range(x, na.rm=TRUE)
  
  cv <- (dev_std / media) * 100 # Coefficente di Variazione #
  
  # Limiti per dispersione
  Upper_limit <- media + dev_std
  Lower_limit <- media - dev_std
  Upper_limit_2SD <- media + 2 * dev_std
  Lower_limit_2SD <- media - 2 * dev_std
  
  fuori <- ifelse(x < Lower_limit_2SD | x > Upper_limit_2SD, 1, 0)
  percent_fuori <- mean(fuori, na.rm=TRUE) * 100
  percent_dentro <- 100 - percent_fuori
  
  # Classificazione #
  if(cv <10){
    livello <- "Livello 1 - Ottimo"
  } else if(cv<30){
    livello <- "Livello 2 - Buono"
  } else if(cv <40){
    livello <- "Livello 3 - Problematico"
  } else{
    livello <- "Livello 4 - Critico"
  }
  
  if(percent_fuori > 10){ # Nota: Teoricamente nella normale è 5%, tu usi 10%
    livello <- paste(livello,"|Nota: Molti valori fuori dal +- 2SD")
  }
  
  # === AGGIUNTA 2: RILEVATORE OUTLIER (P-Value) ===
  # Qui calcoliamo chi è statisticamente "impossibile"
  
  # 1. Calcolo Z-Score puntuale
  z_scores <- abs((x - media) / dev_std)
  
  # 2. Calcolo P-Value (Probabilità)
  p_values <- 2 * (1 - pnorm(z_scores))
  
  # 3. Creiamo un mini-report temporaneo
  df_check <- data.frame(
    Riga = 1:length(x),
    Valore = x,
    Z = round(z_scores, 2),
    P_Value = p_values
  )
  
  # 4. Filtriamo quelli con probabilità bassissima (es. < 0.01 ovvero 1%)
  outliers_detect <- df_check[df_check$P_Value < 0.01 & !is.na(df_check$P_Value), ]
  
  # 5. Se ne trova, li stampa SUBITO a video per avvisarti
  if(nrow(outliers_detect) > 0) {
    cat("\n[!!!] ATTENZIONE: Trovati valori con P-Value < 0.01 (Molto anomali):\n")
    # Formattiamo il numero p-value per non vedere "0e+00"
    outliers_detect$P_Value <- format(outliers_detect$P_Value, scientific=FALSE, digits=4)
    print(outliers_detect)
    cat("\n")
  }
  # =================================================
  
  # Salvataggio Risultati #
  risultati[[col]] <- list(
    media = media,
    mediana = mediana,
    dev_std = dev_std,
    CV = cv,
    percent_fuori = percent_fuori,
    livello = livello
  )
  
  tabella <- rbind(tabella, data.frame(
    variabile = col,
    media = media,
    mediana = mediana,
    sd = dev_std,
    varianza = varianza,
    CV = cv,
    percent_fuori = percent_fuori,
    livello = livello,
    stringsAsFactors = FALSE
  ))
  
  # Grafici (Ho aggiunto par(mfrow) per vederli insieme, toglilo se non ti piace)
  par(mfrow=c(2,2)) 
  
  hist(
    x,
    main = paste("Istogramma", col),
    col.main ="Purple",
    probability = TRUE,
    breaks = 25,
    xlab = "valori",
    col.lab="darkred",
    ylab = "Frequenza",
    col = "#02F9F2",
    border = "white"
  )
  curve(
    dnorm(x, mean = media, sd = dev_std),
    col = "Red",
    lwd = 3,
    add = TRUE
  )
  boxplot(x, main = paste("Boxplot", col), col="orange")
  qqnorm(x, main=paste("QQ-Plot", col))
  qqline(x, col = "Red", lwd = 3)
  
  par(mfrow=c(1,1)) # Reset griglia grafici
}

View(risultati)
View(tabella)

# FINE #
