# PRELIMINARY DATA ANALYSIS #
# SUMMARY TABLE INITIALIZATION #

tabella <- data.frame(
  variable = character(),
  mean = numeric(),
  median = numeric(),
  sd = numeric(),
  variance = numeric(),
  CV = numeric(),
  percent_out = numeric(),
  level = character(),
  stringsAsFactors = FALSE
)

risultati <- list()

for (col in names(dati)) {   # FOR LOOP # # Executes the code for all provided variables #
  
  x <- dati[[col]]
  
  # === ADDITION 1: PROTECTION ===
  # If the column is not numeric (e.g., Treatment Names), the loop skips to the next iteration
  if(!is.numeric(x)) next 
  # ==============================
  
  # Console Output #
  cat("\n=====================================\n")
  cat("Analysis of variable:", col, "\n")
  cat("=====================================\n")
  
  str(x)
  # print(head(x)) # Commented for cleanliness, uncomment if you wish to see it
  
  # Basic Statistics Calculation (using na.rm=TRUE for safety)
  media <- mean(x, na.rm=TRUE)
  mediana <- median(x, na.rm=TRUE)
  varianza <- var(x, na.rm=TRUE)
  dev_std <- sd(x, na.rm=TRUE)
  range_val <- range(x, na.rm=TRUE)
  
  cv <- (dev_std / media) * 100 # Coefficient of Variation #
  
  # Dispersion Limits
  Upper_limit <- media + dev_std
  Lower_limit <- media - dev_std
  Upper_limit_2SD <- media + 2 * dev_std
  Lower_limit_2SD <- media - 2 * dev_std
  
  fuori <- ifelse(x < Lower_limit_2SD | x > Upper_limit_2SD, 1, 0)
  percent_fuori <- mean(fuori, na.rm=TRUE) * 100
  percent_dentro <- 100 - percent_fuori
  
  # Classification #
  if(cv < 10){
    livello <- "Level 1 - Optimal"
  } else if(cv < 30){
    livello <- "Level 2 - Good"
  } else if(cv < 40){
    livello <- "Level 3 - Problematic"
  } else {
    livello <- "Level 4 - Critical"
  }
  
  if(percent_fuori > 10){ # Note: Theoretically in a normal distribution it's 5%, you use 10%
    livello <- paste(livello, "| Note: Many values outside +- 2SD")
  }
  
  # === ADDITION 2: OUTLIER DETECTOR (P-Value) ===
  # Calculating statistically "improbable" values
  
  # 1. Pointwise Z-Score calculation
  z_scores <- abs((x - media) / dev_std)
  
  # 2. P-Value calculation (Probability)
  p_values <- 2 * (1 - pnorm(z_scores))
  
  # 3. Create a temporary mini-report
  df_check <- data.frame(
    Row = 1:length(x),
    Value = x,
    Z = round(z_scores, 2),
    P_Value = p_values
  )
  
  # 4. Filter values with very low probability (e.g., < 0.01 or 1%)
  outliers_detect <- df_check[df_check$P_Value < 0.01 & !is.na(df_check$P_Value), ]
  
  # 5. If found, print immediately to the console as a warning
  if(nrow(outliers_detect) > 0) {
    cat("\n[!!!] WARNING: