# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# Load the data
fx_liquidity <- read_xlsx("./fx_liquidity.xlsx")

# Prepare
currencies <- unique(substr(colnames(fx_liquidity), 1, 3))
tmp_storage <- list()

# Loop through each currency and subset the corresponding columns
for (currency in currencies) {
  currency_df <- fx_liquidity %>%
    select(starts_with(currency))
  
  currency_df <- data.frame(currency = rep(currency, nrow(currency_df)), currency_df)
  
  colnames(currency_df) <- c("currency", "date", "liquidity")
  
  tmp_storage[[currency]] <- currency_df
}

# Combine all data frames in the list into one long data frame
fx_liquidity <- bind_rows(tmp_storage)

#Exclude USD as it is used as reference
fx_liquidity <- fx_liquidity %>%
  filter(currency != "USD")

# Omit NAs
fx_liquidity <- na.omit(fx_liquidity)

#Save
save(fx_liquidity, file = "./fx_liquidity.RData")

#Clean up
rm(fx_spots_carry_returns, currency_df, forwards, spots, tmp_storage,
   currencies, currency)
