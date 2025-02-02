library(lubridate)
library(dplyr)
library(Rblpapi)
library(zoo)

#Helfer function to define dictionary to load forward data
dictionary_forwards <- function(holding_period) {
  list(
    EUR = paste0("EUR", holding_period, "M BGN Curncy"),
    GBP = paste0("GBP", holding_period, "M BGN Curncy"),
    JPY = paste0("JPY", holding_period, "M BGN Curncy"),
    AUD = paste0("AUD", holding_period, "M BGN Curncy"),
    CAD = paste0("CAD", holding_period, "M BGN Curncy"),
    CHF = paste0("CHF", holding_period, "M BGN Curncy"),
    CNY = paste0("CCN+", holding_period, "M BGN Curncy"),  # Updated
    HKD = paste0("HKD", holding_period, "M BGN Curncy"),
    SGD = paste0("SGD", holding_period, "M BGN Curncy"),
    SEK = paste0("SEK", holding_period, "M BGN Curncy"),
    KRW = paste0("KRW", holding_period, "M BGN Curncy"),
    NOK = paste0("NOK", holding_period, "M BGN Curncy"),
    NZD = paste0("NZD", holding_period, "M BGN Curncy"),
    INR = paste0("IRN+", holding_period, "M BGN Curncy"),  # Updated
    MXN = paste0("MXN", holding_period, "M BGN Curncy"),
    TWD = paste0("NTN", holding_period, "M BGN Curncy"),   # Updated
    ZAR = paste0("ZAR", holding_period, "M BGN Curncy"),
    BRL = paste0("BCN", holding_period, "M BGN Curncy"),   # Updated
    DKK = paste0("DKK", holding_period, "M BGN Curncy"),
    PLN = paste0("PLN", holding_period, "M BGN Curncy"),
    THB = paste0("THB", holding_period, "M BGN Curncy"),
    ILS = paste0("ILS", holding_period, "M BGN Curncy"),
    IDR = paste0("IHN+", holding_period, "M BGN Curncy"),  # Updated
    CZK = paste0("CZK", holding_period, "M BGN Curncy"),
    AED = paste0("AED", holding_period, "M BGN Curncy"),
    TRY = paste0("TRY", holding_period, "M BGN Curncy"),
    HUF = paste0("HUF", holding_period, "M BGN Curncy"),
    CLP = paste0("CHN", holding_period, "M BGN Curncy"),   # Updated
    SAR = paste0("SAR", holding_period, "M BGN Curncy"),
    PHP = paste0("PHP", holding_period, "M BGN Curncy"),
    MYR = paste0("MYR", holding_period, "M BGN Curncy"),
    COP = paste0("COP", holding_period, "M BGN Curncy"),
    RUB = paste0("RUB", holding_period, "M BGN Curncy"),
    RON = paste0("RON", holding_period, "M BGN Curncy"),
    PEN = paste0("PSN+", holding_period, "M BGN Curncy"),  # Updated
    BHD = paste0("BHD", holding_period, "M BGN Curncy"),
    BGN = paste0("BGN", holding_period, "M BGN Curncy"),
    ARS = paste0("APN", holding_period, "M BGN Curncy")    # Updated
  )
}

# Helper function to pull data and handle errors
pull_data <- function(security, fields, options) {
  blpConnect() # Connect to Bloomberg
  data <- tryCatch({
    bdh(
      securities = security,
      fields = fields,
      start.date = start_date,
      end.date = end_date,
      options = options
    )
  }, error = function(e) {
    warning("Error pulling data for ", security, ": ", e$message)
    return(data.frame(date = NA, field = NA))
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No data returned for ", security)
    return(data.frame(date = NA, field = NA))
  }
  return(data)
}


# Define functions
fx_data_pull <- function(currency = "EUR", holding_period = 1, start_date = Sys.Date(), end_date = Sys.Date()) {
  
  cat("Processing:", currency, "\n")
  
  
  # Pull historical prices
  fx_data <- pull_data(
    security = paste0(currency, "USD Curncy"),
    fields = c("PX_LAST"),
    options = c("periodicitySelection" = "MONTHLY")
  )
  colnames(fx_data)[2] <- "Spot" #in USD/CCY
  fx_data$Spot <- fx_data$Spot
  
  
  fx_data$Currency <- currency #add currency as variable
  
  
  # Pull data for purchasable NDF
  NDF_data <- pull_data(
    security = dictionary_forwards(holding_period)[[currency]],
    fields = c("BID", "ASK"),
    options = c("periodicitySelection" = "MONTHLY")
  )
  colnames(NDF_data) = c("date", "Outright_BID", "Outright_ASK")
  
  fx_data <- inner_join(fx_data, NDF_data, by = "date")
  
  fx_data$Outright_ASK <- fx_data$Outright_ASK / 10000# Convert points to CCY/USD terms
  fx_data$Outright_ASK <- fx_data$Outright_ASK + 1/fx_data$Spot # compute outright (formula in CCY/USD)
  fx_data$Outright_ASK <- 1/fx_data$Outright_ASK # Convert outright to USD/CCY terms
  
  fx_data$Outright_BID <- fx_data$Outright_BID / 10000# Convert points to CCY/USD terms
  fx_data$Outright_BID <- fx_data$Outright_BID + 1/fx_data$Spot # compute outright (formula in CCY/USD)
  fx_data$Outright_BID <- 1/fx_data$Outright_BID # Convert outright to USD/CCY terms
  
  
  # Pull REER data
  reer <- pull_data(
    security = paste0("CTTWBR", substring(currency, 1, 2), " Index"),
    fields = c("PX_LAST"),
    options = c("periodicitySelection" = "MONTHLY")
  )
  colnames(reer)[2] <- "REER"
  
  fx_data <- inner_join(x = fx_data, y = reer, by = "date")
  
  return(fx_data)
}

factor_computation <- function(fx_data, holding_period = 1){
  #initialize new data frame to store results
  fx_factors <- fx_data %>%
    select(-c("Spot","Outright_BID","Outright_ASK", "REER"))
  
  fx_factors$log_return <- 
    log((1/fx_data$Outright_BID - 1/lead(fx_data$Spot, holding_period)) * fx_data$Spot +1)
  
  fx_factors$Carry <-
    (1/fx_data$Outright_BID - 1/fx_data$Spot)/(1/fx_data$Spot)
  
  fx_factors$Liquidity <- #log-exp trick for computational stability
    exp(log(abs(fx_data$Outright_BID - fx_data$Outright_ASK)) - log((fx_data$Outright_ASK + fx_data$Outright_BID)/2))
  
  #A few Momentum horizons
  fx_factors$Momentum_1m <- log(fx_data$Spot) - log(lag(fx_data$Spot,1)) # 1 month
  fx_factors$Momentum_3m <- log(fx_data$Spot) - log(lag(fx_data$Spot,3)) # 3 months
  fx_factors$Momentum_6m_ex1 <- log(lag(fx_data$Spot,1)) - log(lag(fx_data$Spot,6)) # 6-1 months
  fx_factors$Momentum_12m_ex1 <- log(lag(fx_data$Spot,1)) - log(lag(fx_data$Spot,12)) # 12-1 months
  
  #Different REER factor ideas
  fx_factors$REER <- fx_data$REER
  fx_factors$REER_minus_100 <- 100-fx_data$REER
  fx_factors$REER_historic_mean <-
    zoo::rollapply(fx_data$REER, width = 60, FUN = mean, align = "right", fill = NA)
  fx_factors$REER_historic_mean_ratio <-
    fx_factors$REER_historic_mean / fx_data$REER
  
  return(na.omit(fx_factors))
}

start_date <- Sys.Date() %m-% years(26)
end_date <- as.Date("2024-11-30")
currencies <- c( #define FX universe
  "EUR", "GBP", "JPY", "AUD",
  "CAD", "CHF", "CNY", "HKD",
  "SGD", "SEK", "KRW", "NOK",
  "NZD", "INR", "MXN", "TWD",
  "ZAR", "BRL", "DKK", "PLN",
  "THB", "ILS", "IDR", "CZK",
  "AED", "TRY", "HUF", "CLP",
  "SAR", "PHP", "MYR", "COP",
  "RUB", "RON", "PEN", "BHD",
  "BGN", "ARS")

holding_period = 1 #in months

fx_factors <- list()
for (currency in currencies){
  #currency = "TRY" # for debugging
  fx_data <- fx_data_pull(currency = currency, holding_period = holding_period, start_date = start_date, end_date = end_date)
  fx_factors[[currency]] <- factor_computation(fx_data, holding_period = holding_period)
}
fx_factors <- bind_rows(fx_factors)

#export
save(fx_factors, file = "fx_factors.RData")

#export risk free rate USD
RF_EUR <- pull_data(
  security = "EUR012M Index",
  fields = "PX_LAST",
  c("periodicitySelection" = "MONTHLY"))
colnames(RF_EUR)[2] <- "RF_EUR"
save(RF_EUR, file = "RF_EUR.RData")

#export pricing data that was used for ARS
save(fx_data, file = "ARS_fx_data.RData")
