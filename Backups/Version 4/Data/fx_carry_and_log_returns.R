# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# Load the data
fx_spots_carry_returns <- read_xlsx("./fx_spots_and_6m_forwards.xlsx")

# Transform forwards in bpts to forwards in absolute change (6m carry)
fx_spots_carry_returns <- fx_spots_carry_returns %>%
  mutate(across(ends_with(".forward_bpts_6m"), ~ . / 1000))

# Prepare
currencies <- unique(substr(colnames(fx_spots_carry_returns), 1, 3))
tmp_storage <- list()

# Loop through each currency and subset the corresponding columns
for (currency in currencies) {
  currency_df <- fx_spots_carry_returns %>%
    select(starts_with(currency))
  
  spots <- currency_df[c(1, 2)]
  spots <- na.omit(spots)
  forwards <- currency_df[c(3, 4)]
  forwards <- na.omit(forwards)
  
  colnames(spots) <- c("date", "spot")
  colnames(forwards) <- c("date", "carry")
  
  currency_df <- inner_join(x = spots, y = forwards, by = "date")
  currency_df <- data.frame(currency = rep(currency, nrow(currency_df)), currency_df)
  
  tmp_storage[[currency]] <- currency_df
}

# Combine all data frames in the list into one long data frame
fx_spots_carry_returns <- bind_rows(tmp_storage)

# Compute log returns (old way, calculates log returns for the wrong moment in time)
#fx_spots_carry_returns2 <- fx_spots_carry_returns %>%
  #group_by(currency) %>%
  #arrange(date) %>%
  #mutate(log_return = c(NA, diff(log(spot))))


#maybe adapt this part, it's not super smooth and might cause errors
fx_spots_carry_returns <- fx_spots_carry_returns %>%
  group_by(currency) %>%
  arrange(date) %>%
  mutate(log_return = log(lead(spot,1) / spot)) %>% # use lead(column, x) to shift column by x downwards
  filter(!is.na(log_return)) # remove rows where log_return could not be computed

# Select necessary columns only
fx_spots_carry_returns <- fx_spots_carry_returns %>%
  select(1,2,4,5)

#Exclude USD as it is used as reference
fx_spots_carry_returns <- fx_spots_carry_returns %>%
  filter(currency != "USD")

#Subset carry
fx_carry <- fx_spots_carry_returns %>%
  select(1,2,3)

#Subset log returns
fx_log_returns <- fx_spots_carry_returns %>%
  select(1,2,4)

#Save subsets
save(fx_carry, file = "./fx_carry.RData")
save(fx_log_returns, file = "./fx_log_returns.RData")

#Clean up
rm(fx_spots_carry_returns, currency_df, forwards, spots, tmp_storage,
   currencies, currency)
