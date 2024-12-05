library(lubridate)
library(dplyr)
library(Rblpapi)


#The data pull encountered issues with missing indices: SOR6M, 
#BKBM6M, JIBAR6M, SAIBOR6M, CIBOR6M, WIBOR6M, TELBOR6M, EBOR6M, BUBOR6M, PRIBOR6M,
#PHIREF6M, KLIBOR6M, IBR6M, MOSPRIME6M.

#Discuss with Richard if IBORs, Bond Yields or sth else should be used.

deposit_rate_tickers <- list(
  USD = "US0003M Index",   # 3-Month USD LIBOR or SOFR-based term rate
  EUR = "EUR006M Index",  # 6-Month EURIBOR
  GBP = "BP0006M Index",   # 6-Month GBP LIBOR or SONIA-based term rate
  JPY = "JY0006M Index",   # 6-Month JPY TIBOR
  AUD = "AU0006M Index",   # 6-Month AUD BBSW (Bank Bill Swap Rate)
  CAD = "CDOR06 Index",    # 6-Month CAD CDOR (Canadian Dollar Offered Rate)
  CHF = "SF0006M Index",   # 6-Month CHF LIBOR or SARON
  CNY = "CNREPOFIX6M Index", # 6-Month CNY Repo Fixing Rate
  HKD = "HIHD05M Index",   # 5-Month HKD (Hong Kong Interbank Offered Rate)
  SGD = "SOR6M Index",     # 6-Month SGD SOR (Swap Offer Rate)
  SEK = "STIB6M Index",    # 6-Month SEK STIBOR (Stockholm Interbank Offered Rate)
  KRW = "KWCDC6M Index",   # 6-Month KRW CD Rate
  NOK = "NIBOR6M Index",   # 6-Month NOK NIBOR (Norwegian Interbank Offered Rate)
  NZD = "BKBM6M Index",    # 6-Month NZD BKBM (Bank Bill Benchmark Rate)
  INR = "MIBOR6M Index",   # 6-Month INR MIBOR (Mumbai Interbank Offered Rate)
  MXN = "MXIBTIIE6M Index", # 6-Month MXN TIIE (Interbank Equilibrium Interest Rate)
  TWD = "TAIBOR6M Index",  # 6-Month TWD TAIBOR (Taiwan Interbank Offered Rate)
  ZAR = "JIBAR6M Index",   # 6-Month ZAR JIBAR (Johannesburg Interbank Agreed Rate)
  BRL = "BRIPCDI6M Index", # 6-Month BRL CDI (Interbank Deposit Certificate)
  DKK = "CIBOR6M Index",   # 6-Month DKK CIBOR (Copenhagen Interbank Offered Rate)
  PLN = "WIBOR6M Index",   # 6-Month PLN WIBOR (Warsaw Interbank Offered Rate)
  THB = "BIBOR6M Index",   # 6-Month THB BIBOR (Bangkok Interbank Offered Rate)
  ILS = "TELBOR6M Index",  # 6-Month ILS TELBOR (Tel Aviv Interbank Offered Rate)
  IDR = "IDRFIX6M Index",  # 6-Month IDR JIBOR (Jakarta Interbank Offered Rate)
  CZK = "PRIBOR6M Index",  # 6-Month CZK PRIBOR (Prague Interbank Offered Rate)
  AED = "EBOR6M Index",    # 6-Month AED EIBOR (Emirates Interbank Offered Rate)
  TRY = "TRLIB6M Index",   # 6-Month TRY TRLIBOR (Turkish Lira Interbank Offered Rate)
  HUF = "BUBOR6M Index",   # 6-Month HUF BUBOR (Budapest Interbank Offered Rate)
  CLP = "CLIBOR6M Index",  # 6-Month CLP CLIBOR (Chilean Interbank Offered Rate)
  SAR = "SAIBOR6M Index",  # 6-Month SAR SAIBOR (Saudi Arabian Interbank Offered Rate)
  PHP = "PHIREF6M Index",  # 6-Month PHP PHIREF (Philippine Interbank Reference Rate)
  MYR = "KLIBOR6M Index",  # 6-Month MYR KLIBOR (Kuala Lumpur Interbank Offered Rate)
  COP = "IBR6M Index",     # 6-Month COP IBR (Indicador Bancario de Referencia)
  RUB = "MOSPRIME6M Index",# 6-Month RUB MOSPRIME (Moscow Prime Offered Rate)
  RON = "ROBOR6M Index",   # 6-Month RON ROBOR (Romanian Interbank Offered Rate)
  PEN = "LIMABOR6M Index", # 6-Month PEN LIMABOR (Lima Interbank Offered Rate)
  BHD = "BHIBOR6M Index",  # 6-Month BHD BHIBOR (Bahrain Interbank Offered Rate)
  BGN = "SOFIBOR6M Index", # 6-Month BGN SOFIBOR (Sofia Interbank Offered Rate)
  ARS = "BADLAR6M Index"   # 6-Month ARS BADLAR (Buenos Aires Deposits of Large Amount Rate)
)

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
fx_data_pull <- function(currency = "EUR", carry_horizon = 1, start_date = Sys.Date(), end_date = Sys.Date()) {
  
  cat("Processing:", currency, "\n")
  
  

  # Pull historical prices
  hist_prices <- pull_data(
    security = paste0(currency, "USD Curncy"),
    fields = c("PX_LAST", "BID", "ASK"),
    options = c("periodicitySelection" = "MONTHLY")
  )
  if (is.null(hist_prices)) return(NULL)
  hist_prices$PX_LAST <- 1 / hist_prices$PX_LAST  # Convert CCY/USD to USD/CCY
  
  # Pull forward spread data
  forward_spread <- pull_data(
    security = paste0(currency, carry_horizon, "M BGN Curncy"),
    fields = c("PX_LAST"),
    options = c("periodicitySelection" = "MONTHLY")
  )
  if (is.null(forward_spread)) return(NULL)
  
  # Merge forward rate with historical prices and construct forward rate
  
  all_dates <- hist_prices$date #need a workaround when some data is missing
  
  forward_rate <- forward_spread
  
  forward_rate <- full_join(#need a workaround when some data is missing
    data.frame(date = all_dates),
    forward_rate, 
    by = "date"
  )
  
  forward_rate[[2]] <- forward_rate[[2]] / 10000  # Convert points to USD terms
  forward_rate[[2]] <- forward_rate[[2]] + hist_prices$PX_LAST
  colnames(forward_rate)[2] <- "Forward_Price"
  hist_prices <- left_join(hist_prices, forward_rate, by = "date")
  
  # Pull risk-free rate data
  risk_free_rate <- pull_data(
    security = deposit_rate_tickers[[currency]],
    fields = c("PX_LAST"),
    options = c("periodicitySelection" = "MONTHLY")
  )
  if (!is.null(risk_free_rate)) {
    colnames(risk_free_rate)[2] <- "Risk_Free_Rate_Currency"
    hist_prices <- left_join(hist_prices, risk_free_rate, by = "date")
  }
  
  hist_prices$Currency <- currency
  
  # Pull REER data
  reer <- pull_data(
    security = paste0("CTTWBR", substring(currency, 1, 2), " Index"),
    fields = c("PX_LAST"),
    options = c("periodicitySelection" = "MONTHLY")
  )
  if (!is.null(reer)) {
    colnames(reer)[2] <- "REER"
    hist_prices <- left_join(hist_prices, reer, by = "date")
  }
  
  # Check final data validity
  if (is.null(hist_prices) || nrow(hist_prices) == 0) {
    warning("No valid data available for ", currency)
    return(NULL)
  }
  
  return(hist_prices)
}

factor_computation <- function(fx_data){
  #initialize new data frame to store results
  fx_factors <- fx_data %>%
    select(-c("PX_LAST","BID","ASK","Forward_Price", "Risk_Free_Rate_Currency", "REER"))
  
  fx_factors$log_return <- log(fx_data$PX_LAST) - log(lag(fx_data$PX_LAST, 1))
  
  fx_factors$Level <- fx_data$Risk_Free_Rate_Currency
  
  fx_factors$Carry <- fx_data$Risk_Free_Rate_Currency - Risk_Free_Rate_USD$PX_LAST
  
  #log-exp trick for computational stability
  fx_factors$Liquidity <- exp(log(fx_data$ASK - fx_data$BID) - log((fx_data$ASK + fx_data$BID)/2))
  
  fx_factors$Momentum <- log(lag(fx_data$PX_LAST,1)) - log(lag(fx_data$PX_LAST,12))
  
  fx_factors$REER <- fx_data$REER
  
  #we do not need the VIX for now
  
  return(fx_factors)
}

start_date <- Sys.Date() %m-% years(20)
end_date <- Sys.Date()
currencies <- c(  #define FX universe #CNY, TWD, IDR, BRL, INR, CLP, ARS, PEN 
                  #excluded for now bc error with forward spreads
  "EUR", "GBP", "JPY", "AUD", "CAD", "CHF", "HKD", "SGD", 
  "SEK", "KRW", "NOK", "NZD", "MXN", "ZAR", "SAR", "DKK",
  "PLN", "THB", "ILS", "AED", "HUF", "CZK", "TRY", "PHP",
  "MYR", "COP", "RUB", "RON", "BHD", "BGN"
)

fx_factors <- list()

USD_Risk_Free_Rate <- pull_data(
  security = deposit_rate_tickers[["USD"]],
  fields = "PX_LAST",
  c("periodicitySelection" = "MONTHLY"))

for (currency in currencies){
  fx_data <- fx_data_pull(currency = currency, carry_horizon = 6, start_date = start_date, end_date = end_date)
  fx_factors[[currency]] <- factor_computation(fx_data)
}
fx_factors <- bind_rows(fx_factors)

#export
save(fx_factors, file = "fx_factors.RData")
