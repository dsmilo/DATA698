# investigate missing reported values ##########################################
# set up variable to flag imputed & method
eo88_imp <- eo88 %>% mutate(Imputed = "")

# investigate distribution of Use values by SFY
ggplot(eo88_imp, aes(x = SFY, y = Use)) + geom_boxplot()

# investigate two extreme values (1.6e+9 & 1.6e-9)
extremes <- eo88_imp %>%
  filter(Use == max(Use, na.rm = TRUE) | Use == min(Use, na.rm = TRUE)) %>%
  select(ESPLocationID, Fuel, AccountNumber, Month, Use, Demand, Cost)

eo88_imp %>%
  filter(ESPLocationID %in% extremes$ESPLocationID, Fuel %in% extremes$Fuel) %>%
  ggplot(aes(x = Month, y = Use)) +
  geom_line() +
  facet_wrap(~ AccountNumber, scales = "free_y")

# sum two extreme values & split across two months (2011-01-01 & 2011-02-01)
## same account: ESP 324; Electricity, grid purchase; Electric-291010012800006

eo88_imp <- eo88_imp %>%
  mutate(Imputed = replace(Imputed, which(Use %in% extremes$Use), "manual"),
         Use = replace(Use, which(Use %in% extremes$Use), mean(extremes$Use)),
         Demand = replace(Demand, which(Demand %in% extremes$Demand), mean(extremes$Demand)),
         Cost = replace(Cost, which(Cost %in% extremes$Cost), mean(extremes$Cost)))


# explore missing value combinations ###########################################
# investigate missing values by reported measure
library(mice)
md.pattern(eo88_imp)

# viz missing
VIM::aggr(eo88_imp)
VIM::aggr(eo88_imp %>% select(Use, Demand, Cost), numbers = TRUE, prop = FALSE, bars = FALSE)


# missing use; reported demand & cost ##########################################
eo88_imp %>%
  filter(is.na(Use), !is.na(Demand), !is.na(Cost)) %>%
  select(ESPLocationID, AccountNumber, Month, Use, Demand, Cost)
### all same account (Electric-495520212030000); 2012-04-01 - 2012-10-01
### zero demand & cost --- account did not exist prior to 2012-11-01
### replace with zero
eo88_imp <- eo88_imp %>%
  mutate(Imputed = if_else(is.na(Use) & !is.na(Demand) & !is.na(Cost), "zero", Imputed),
         Use = if_else(is.na(Use) & !is.na(Demand) & !is.na(Cost), 0, Use))


# missing all 3 ################################################################
eo88_imp %>%
  filter(is.na(Use), is.na(Demand), is.na(Cost)) %>%
  select(ESPLocationID, Fuel, AccountNumber, Month) %>%
  View()

### many; may be similar to above (filling non-reported values)
### investigate NA & non-NA count for those with triple NAs
trip_na <- eo88_imp %>%
  filter(is.na(Use), is.na(Demand), is.na(Cost)) %>%
  select(AccountNumber, Month) %>%
  distinct()

eo88_imp %>%
  filter(AccountNumber %in% trip_na$AccountNumber) %>%
  group_by(AccountNumber) %>%
  summarize(allna = sum(is.na(Use) & is.na(Demand) & is.na(Cost)),
            notall = n() - allna, sharena = allna / n()) %>%
  ggplot(aes(x = sharena)) +
  geom_histogram(binwidth = 0.1, alpha = 0.5, col = "black") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent)

eo88_imp %>%
  filter(AccountNumber %in% trip_na$AccountNumber) %>%
  group_by(ESPLocationID, AccountNumber) %>%
  summarize(allna = sum(is.na(Use) & is.na(Demand) & is.na(Cost)),
            notall = n() - allna, sharena = allna / n()) %>%
  ungroup() %>%
  arrange(desc(sharena)) %>%
  top_n(10, sharena)

### those with 100% all na same facility (410); same facility above with backfilled
### both 2011-04-01 - 2012-04-01
### facility is excluded for SFY 2011-12 & 2012-13
### JOIN TO CHECK STATUS & number missing

eo88_imp %>%
  left_join(bldg_sfy %>% select(ESPLocationID, SFY, Status),
            by = c("ESPLocationID", "SFY")) %>%
  filter(Status == "included") %>%
  select(Use, Demand, Cost) %>%
  VIM::aggr(numbers = TRUE, prop = FALSE, bars = FALSE)

### still many missing
eo88_imp %>%
  filter(AccountNumber %in% trip_na$AccountNumber) %>%
  group_by(Fuel, AccountNumber) %>%
  summarize(allna = sum(is.na(Use) & is.na(Demand) & is.na(Cost)),
            notall = n() - allna, sharena = allna / n()) %>%
  arrange(Fuel, desc(sharena)) %>%
  print(n = nrow(.))

### investigate electrictity grid purchases
eo88_imp %>%
  filter(AccountNumber %in% trip_na$AccountNumber, Fuel == "Electricity, grid purchase") %>%
  select(AccountNumber, Month, Use, Demand, Cost) %>%
  group_by(AccountNumber) %>%
  mutate(na = is.na(Use) & is.na(Demand) & is.na(Use),
         Use = ifelse(na, 0, Use)) %>%
  ggplot(aes(Month, Use, col = na)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AccountNumber, scales = "free") +
  theme(legend.position = "bottom")

### many before or after reporting --- likely cancelled accts -- get six not
elec_trip <- c("Electric-1001-226-0906", "Electric-1001-6581-323", "Electric-41861-58005",
               "Electric-495301090000005", "Electric-695320202500015", "Electric-9564-007-100")

elec_cancelled <- eo88_imp %>%
  filter(Fuel == "Electricity, grid purchase",
         AccountNumber %in% trip_na$AccountNumber,
         !AccountNumber %in% elec_trip) %>%
  pull(AccountNumber) %>%
  unique()


### fill cancelled with zeroes
eo88_imp <- eo88_imp %>%
  mutate(Imputed = if_else(AccountNumber %in% elec_cancelled &
                             is.na(Use) & is.na(Demand) & is.na(Cost),
                           "zero", Imputed),
         Use = if_else(AccountNumber %in% elec_cancelled &
                         is.na(Use) & is.na(Demand) & is.na(Cost),
                       0, Use))

### flag others for imputation -- assume random forest due to unkown relationship with weather
eo88_imp <- eo88_imp %>%
  mutate(Imputed = if_else(AccountNumber %in% elec_trip &
                             is.na(Use) & is.na(Demand) & is.na(Cost),
                           "rf", Imputed))



### lot of thermals (fuel oil 2, natural gas, kerosene, propane, steam)
### periodic purchases -- makes sense as tank-based or seasonal
eo88_imp %>%
  filter(AccountNumber %in% trip_na$AccountNumber,
         Fuel != "Electricity, grid purchase") %>%
  select(ESPLocationID, AccountNumber, Month, Use, Demand, Cost) %>%
  View()

### FILL TRIPLE NULLS FOR THERMALS WITH USE = 0
eo88_imp <- eo88_imp %>%
  mutate(Imputed = if_else(is.na(Use) & is.na(Demand) & is.na(Cost) &
                         Fuel != "Electricity, grid purchase",
                       "zero", Imputed),
         Use = if_else(is.na(Use) & is.na(Demand) & is.na(Cost) &
                         Fuel != "Electricity, grid purchase",
                       0, Use))


# cost but no use or demand ####################################################
eo88_imp %>%
  select(Use, Demand, Cost) %>%
  VIM::aggr(numbers = TRUE, prop = FALSE, bars = FALSE)

### lot of natural gas & electricity grid purchase
eo88_imp %>%
  filter(is.na(Use), is.na(Demand), !is.na(Cost)) %>%
  group_by(Fuel) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

### investigate month difference
no_use <- eo88_imp %>%
  filter(is.na(Use), is.na(Demand), !is.na(Cost)) %>%
  arrange(ESPLocationID, AccountNumber, Fuel, Month) %>%
  group_by(ESPLocationID, AccountNumber, Fuel) %>%
  mutate(MonthDiff = interval(lag(Month), Month) %/% months(1)) %>%
  ungroup() %>%
  select(AccountNumber, ESPLocationID, Fuel, Month, MonthDiff)

### many with zero -- possibly cost only?
eo88_imp %>%
  filter(AccountNumber %in% unique(no_use$AccountNumber),
         Fuel == "Electricity, grid purchase") %>%
  select(ESPLocationID, Fuel, AccountNumber, Month, Use, Demand, Cost) %>%
  distinct() %>% # some due to split
  View()

### some never have any use (either all NA or all zero) -- find these
cost_only <- eo88_imp %>%
  filter(AccountNumber %in% unique(no_use$AccountNumber)) %>%
  group_by(AccountNumber) %>%
  filter(max(Use, na.rm = TRUE) %in% c(0, -Inf), is.na(Demand), is.na(Use)) %>%
  ungroup() %>%
  select(ESPLocationID, Fuel, AccountNumber, Month, Use, Demand, Cost) %>%
  distinct(AccountNumber)

### handles 419/503 accts (83%) & 8341/9302 (90%) of no_use -- REPLACE WITH ZERO
eo88_imp <- eo88_imp %>%
  mutate(Imputed = if_else(is.na(Use) & AccountNumber %in% cost_only$AccountNumber, "zero", Imputed),
         Use = if_else(AccountNumber %in% cost_only$AccountNumber, 0, Use))

### investigate remainings by fuel
no_use <- no_use %>%
  filter(!AccountNumber %in% cost_only$AccountNumber)

no_use %>%
  group_by(Fuel, AccountNumber) %>%
  summarize(n_missing = n()) %>%
  group_by(Fuel) %>%
  summarize(n_accounts = n(),
            n_missing = sum(n_missing)) %>%
  arrange(desc(n_accounts), desc(n_missing))

### check remaining numbers missing
table(no_use$MonthDiff)
no_use %>%
  group_by(MonthDiff, AccountNumber) %>%
  summarize(n_instances = n()) %>%
  group_by(MonthDiff) %>%
  summarize(n_accounts = n(),
            n_instances = sum(n_instances)) %>%
  arrange(desc(MonthDiff))


no_use %>%
  group_by(AccountNumber) %>%
  summarize(longest = max(MonthDiff, na.rm = TRUE),
            shortest = min(MonthDiff, na.rm = TRUE)) %>%
  select(shortest, longest) %>%
  table()

## many show monthdiff 0 -- possible from dupes??
no_use %>%
  filter(MonthDiff == 0) %>%
  distinct(AccountNumber, ESPLocationID, Fuel, Month) %>%
  nrow()

### check liquid propane -- easiest hit
eo88_imp %>%
  filter(AccountNumber %in% no_use$AccountNumber, Fuel == "Propane and Liquid Propane") %>%
  select(AccountNumber, Month, Use)
### single missing is 7 montHs after last actual & followed by 0 6 monts later -- ZERO
eo88_imp <- eo88_imp %>%
  mutate(Imputed = if_else(is.na(Use) & !is.na(Cost) & Fuel == "Propane and Liquid Propane",
                       "zero", Imputed),
         Use = if_else(is.na(Use) & !is.na(Cost) & Fuel == "Propane and Liquid Propane",
                       0, Use))

### electricity next
no_use_elec <- no_use %>%
  filter(Fuel == "Electricity, grid purchase") %>%
  select(AccountNumber, Month) %>%
  unique()

eo88_imp %>%
  filter(AccountNumber %in% no_use_elec$AccountNumber,
         !is.na(Cost)) %>%
  select(AccountNumber, Share, Month, Use, Cost) %>%
  mutate(Use = Use * Share, Cost = Cost * Share) %>%
  group_by(AccountNumber, Month) %>%
  summarize(Use = sum(Use), Cost = sum(Cost)) %>%
  gather(Measure, Value, -(AccountNumber:Month)) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Value, col = Measure)) +
  geom_point(aes(x = Month, y = Value, col = Measure)) +
  geom_point(data = no_use_elec, aes(x = Month, y = 1)) +
  facet_wrap(~ AccountNumber, scales = "free") +
  scale_y_continuous(trans = "log10") +
  theme(legend.position = c(0.85, 0.25))

### 43of 5 show NAs around zero or in last months
### likely cancelled accts with remaining charges -- zero out
### impute remaining with regression
no_use_elec <- no_use_elec %>%
  filter(!AccountNumber %in% c("Electric-00580-69006", "Electric-40-NORRWF-0126-E-001--INTEGRYS-N01000005531926"))

eo88_imp <- eo88_imp %>%
  mutate(Imputed = ifelse(is.na(Use) & !is.na(Cost) & AccountNumber %in% no_use_elec$AccountNumber,
                         "zero", Imputed),
         Use = ifelse(is.na(Use) & !is.na(Cost) & AccountNumber %in% no_use_elec$AccountNumber,
                          0, Use),
         Imputed = ifelse(is.na(Use) & !is.na(Cost) & Fuel == "Electricity, grid purchase",
                          "reg", Imputed))


### fuel oil
no_use_fuel <- no_use %>%
  filter(Fuel == "Fuel Oil #2")

### most either one off or all consecutive
no_use_fuel %>%
  group_by(AccountNumber) %>%
  summarize(n_missing = n(),
            shortest = min(MonthDiff, na.rm = TRUE),
            longest = max(MonthDiff, na.rm = TRUE)) %>%
  arrange(desc(n_missing))

eo88_imp %>%
  filter(AccountNumber %in% no_use_fuel$AccountNumber, !is.na(Cost)) %>%
  select(AccountNumber, Share, Month, Use, Cost) %>%
  mutate(Use = Use * Share, Cost = Cost * Share) %>%
  group_by(AccountNumber, Month) %>%
  summarize(Use = sum(Use), Cost = sum(Cost)) %>%
  gather(Measure, Value, -(AccountNumber:Month)) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Value + 1, col = Measure)) +
  geom_point(aes(x = Month, y = Value + 1, col = Measure)) +
  geom_point(data = no_use_fuel, aes(x = Month, y = 1)) +
  facet_wrap(~ AccountNumber, scales = "free") +
  scale_y_continuous("Value", trans = "log10") +
  theme(legend.position = "top")

## top 2 legit -- ZERO; reg for others
no_use_fuel <- no_use_fuel %>%
  filter(AccountNumber %in% c("FuelOil#2-26-PURCHASE-HIST-OIL2-001", "FuelOil#2-7502000"))

eo88_imp <- eo88_imp %>%
  mutate(Imputed = ifelse(is.na(Use) & !is.na(Cost) & AccountNumber %in% no_use_fuel$AccountNumber,
                          "zero", Imputed),
         Use = ifelse(is.na(Use) & !is.na(Cost) & AccountNumber %in% no_use_fuel$AccountNumber,
                      0, Use),
         Imputed = ifelse(is.na(Use) & !is.na(Cost) & Fuel == "Fuel Oil #2",
                          "reg", Imputed))

### nat gas left
no_use_gas <- no_use %>%
  filter(Fuel == "Natural Gas")

gas_durations <- no_use_gas %>%
  group_by(AccountNumber) %>%
  summarize(n_missing = n(),
            shortest = min(MonthDiff, na.rm = TRUE),
            longest = max(MonthDiff, na.rm = TRUE)) %>%
  arrange(desc(longest), desc(n_missing))


### some very long; investigate more than 4 month gaps
gas_investigate <- gas_durations %>%
  filter(longest > 4) %>%
  pull(AccountNumber)

eo88_imp %>%
  filter(AccountNumber %in% gas_investigate, !is.na(Cost)) %>%
  select(AccountNumber, Share, Month, Use, Cost) %>%
  mutate(Use = Use * Share, Cost = Cost * Share) %>%
  group_by(AccountNumber, Month) %>%
  summarize(Use = sum(Use), Cost = sum(Cost)) %>%
  gather(Measure, Value, -(AccountNumber:Month)) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Value + 1, col = Measure)) +
  geom_point(aes(x = Month, y = Value + 1, col = Measure)) +
  geom_point(data = filter(no_use, AccountNumber %in% gas_investigate), aes(x = Month, y = 1)) +
  facet_wrap(~ AccountNumber, scales = "free") +
  scale_y_continuous("Value", trans = "log10") +
  theme(legend.position = "top")

### some look to be min bill -- find; get 0 use == const (non-zero) cost
eo88_imp %>%
  filter(AccountNumber %in% no_use_gas$AccountNumber, !is.na(Cost)) %>%
  group_by(AccountNumber) %>%
  mutate(n_zero = sum(Use == 0, na.rm = TRUE),
         cost_zero = sum(if_else(Use == 0, Cost, 0), na.rm = TRUE) / n_zero,
         n_zero_cost = Cost > cost_zero * 0.9 & Cost < cost_zero * 1.1 & Use == 0) %>%
  filter(is.na(Use)) %>%
  select(AccountNumber, n_zero_cost) %>%
  table()

## doesn't check out -- manually inspect?
## if min bill, should be caught by imputation

p_gas <- lst()
for (i in 1:5) {
  gas_investigate <- gas_durations %>%
    slice(((i - 1) * 16 + 1):(i * 16)) %>%
    pull(AccountNumber)
  p_gas[[i]] <- eo88_imp %>%
    filter(AccountNumber %in% gas_investigate, !is.na(Cost)) %>%
    select(AccountNumber, Share, Month, Use, Cost) %>%
    mutate(Use = Use * Share, Cost = Cost * Share) %>%
    group_by(AccountNumber, Month) %>%
    summarize(Use = sum(Use), Cost = sum(Cost)) %>%
    gather(Measure, Value, -(AccountNumber:Month)) %>%
    ggplot() +
    geom_line(aes(x = Month, y = Value + 1, col = Measure)) +
    geom_point(aes(x = Month, y = Value + 1, col = Measure)) +
    geom_point(data = filter(no_use, AccountNumber %in% gas_investigate), aes(x = Month, y = 1)) +
    facet_wrap(~ AccountNumber, scales = "free") +
    scale_y_continuous("Value", trans = "log10") +
    theme(legend.position = "top")
}

## likely min bill or zero before/after meter reported -- zero
no_use_gas <- no_use_gas %>%
  filter(AccountNumber %in% c("Gas-0105156190", "Gas-2293459050", "Gas-398008033200000", "Gas-498010055100000",
                              "Gas-498010055200008", "Gas-03-BUFFALO-NC-NGAS-001--3758-874-05 & 873-07-SUPPLY",
                              "Gas-11-MAINCAMPUS-NG-001", "Gas-398010056700007", "Gas-55912-84451",
                              "Gas-6428680108", "Gas-65-SUPLZA-0005-NG-001--648337/648410",
                              "Gas-0892516570", "Gas-18-MAINCAMPUS-NG-001--T002306-FREDONIA",
                              "Gas-0672015270","Gas-0755040850", "Gas-0894540071", "Gas-0894540181",
                              "Gas-1274013891", "Gas-498011054700006", "Gas-498021020000001"))

eo88_imp <- eo88_imp %>%
  mutate(Imputed = ifelse(is.na(Use) & !is.na(Cost) & AccountNumber %in% no_use_gas$AccountNumber,
                          "zero", Imputed),
         Use = ifelse(is.na(Use) & !is.na(Cost) & AccountNumber %in% no_use_gas$AccountNumber,
                      0, Use),
         Imputed = ifelse(is.na(Use) & !is.na(Cost) & Fuel == "Natural Gas",
                          "reg", Imputed))


# impute with rf (no use, no demand, no cost) #################################
# read in NOAA weather data
weather <- read_csv("data/2017-11-14_NOAA_cdo-div-weather-indices.txt",
                    col_types = "iicnnnnnniinnnnnnnnn?")
## https://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp

# convert weather YearMonth to date; drop extra columns
weather <- weather %>%
  mutate(YearMonth = ymd(paste0(YearMonth, "01"))) %>%
  rename(Month = YearMonth) %>%
  select(-StateCode, -X21)

# get lookup values for climate regions
regions <- data_frame(
  Division = 1:10,
  ClimateRegion = c("Western Plateau", "Eastern Plateau", "Northern Plateau",
                    "Coastal", "Hudson Valley", "Mohawk Valley", "Champlain Valley",
                    "St. Lawrence Valley", "Great Lakes", "Central Lakes")
)

# find those needing rf
rf_accts <- eo88_imp %>%
  filter(Imputed == "rf") %>%
  pull(AccountNumber) %>%
  unique()

rf_data <- eo88_imp %>%
  filter(AccountNumber %in% rf_accts) %>%
  left_join(bldg_meta, by = "ESPLocationID") %>%
  left_join(regions, by = "ClimateRegion") %>%
  left_join(weather, by = c("Division", "Month")) %>%
  left_join(bldg_sfy %>% select(ESPLocationID, SFY, FloorArea, Type1),
            by = c("ESPLocationID", "SFY")) %>%
  # convert month to month number & year
  mutate(Year = year(Month), Month = month(Month)) %>%
  select(AccountNumber, Month, Year, Share, YearBuilt, FloorArea,
         Type = Type1, PCP:TMAX, Use)

# YearBuilt & Type don't change; FloorArea does
rf_data <- rf_data %>%
  select(-YearBuilt, -Type)

# for imputation, solve doesn't work due to highly correlated vars -- drop TMAX
# convert accountnumber to factor to fill by account
rf_data <- rf_data %>%
  arrange(AccountNumber, Year, Month) %>%
  mutate(AccountNumber = factor(AccountNumber)) %>%
  select(-TMAX)

# initate mice & extract predictors & methods
rf_init <- mice(rf_data, maxit = 0, seed = 100)
rf_meth <- rf_init$method
rf_pred <- rf_init$predictorMatrix

# set method to random forest for use
rf_meth["Use"] <- "rf"

# don't use share to impute
rf_pred["Share", "Use"] <- 0

#impute & retrieve
rf_imp <- mice(rf_data, method = rf_meth, predictorMatrix = rf_pred, print = FALSE, seed = 100)

rf_imp <- complete(rf_imp) %>%
  arrange(AccountNumber, Year, Month) %>%
  select(-(FloorArea:TMIN))

# check consistency -- all equal except nas
table(rf_data$Use == rf_imp$Use, useNA = "ifany")

# write two weather tables to db ###############################################
con <- dbConnect(MySQL(), default.file = paste0(getwd(), "/", ".my.cnf"))
dbSendQuery(con, "USE EO88;")
dbWriteTable(con, "noaa_regions", regions, row.names = FALSE)
dbWriteTable(con, "weather_monthly", weather, row.names = FALSE)
dbDisconnect(con)


# impute with regression (no use, has cost) ####################################
# find those needing regression
reg_accts <- eo88_imp %>%
  filter(Imputed == "reg") %>%
  pull(AccountNumber) %>%
  unique()

reg_data <- eo88_imp %>%
  filter(AccountNumber %in% reg_accts) %>%
  # convert month to month number & year
  mutate(Year = year(Month), Month = month(Month)) %>%
  select(AccountNumber, Month, Year, Share, Use, Cost) %>%
  arrange(AccountNumber, Year, Month, Share) %>%
  # convert accountnumber to factor to fill by account
  mutate(AccountNumber = factor(AccountNumber))

# initate mice & extract predictors & methods
reg_init <- mice(reg_data, maxit = 0, seed = 100)
reg_meth <- reg_init$method
reg_pred <- reg_init$predictorMatrix

# set method to regression for use; don't impute cost
reg_meth["Use"] <- "norm"
reg_meth["Cost"] <- ""

# don't use share to impute use; remove cost from matrix
reg_pred["Use", "Share"] <- 0
reg_pred["Cost", ] <- 0

#impute & retrieve
reg_imp <- mice(reg_data, method = reg_meth, predictorMatrix = reg_pred, print = FALSE, seed = 100)

reg_imp <- complete(reg_imp) %>%
  arrange(AccountNumber, Year, Month, Share)

# check consistency -- all equal except nas
table(reg_data$Use == reg_imp$Use, useNA = "ifany")


# gather imputed ###############################################################
rf_imp <- rf_data %>%
  left_join(rf_imp, by = c("AccountNumber", "Month", "Year", "Share"),
            suffix = c("_missing", "_imputed")) %>%
  filter(is.na(Use_missing)) %>%
  mutate(Month = ymd(paste(Year, Month, "1", sep = "-"))) %>%
  distinct(AccountNumber, Month, Year, Share, Cost, .keep_all = TRUE) %>%
  select(AccountNumber, Month, Share, Use = Use_imputed)

reg_imp <- reg_data %>%
  left_join(reg_imp, by = c("AccountNumber", "Month", "Year", "Share", "Cost"),
            suffix = c("_missing", "_imputed")) %>%
  filter(is.na(Use_missing), !is.na(Cost)) %>%
  mutate(Month = ymd(paste(Year, Month, "1", sep = "-"))) %>%
  distinct(AccountNumber, Month, Year, Share, Cost, .keep_all = TRUE) %>%
  select(AccountNumber, Month, Share, Use = Use_imputed, Cost)

# plot to check rf
rf_data %>%
  mutate(Month = ymd(paste(Year, Month, "1", sep = "-"))) %>%
  ggplot(aes(x = Month, y = Use)) +
  geom_point() +
  geom_line() +
  geom_point(data = rf_imp, aes(x = Month, y = Use), col = "red") +
  facet_wrap(~ AccountNumber, scales = "free")


# plot to check reg
reg_data %>%
  mutate(Month = ymd(paste(Year, Month, "1", sep = "-"))) %>%
  filter(AccountNumber %in% reg_accts[1:4]) %>%
  ggplot(aes(x = Month)) +
  geom_point(aes(y = Use + 1, col = "Use")) +
  geom_line(aes(y = Use + 1, col = "Use")) +
  geom_point(aes(y = Cost + 1, col = "Cost")) +
  geom_line(aes(y = Cost + 1, col = "Cost")) +
  geom_point(data = filter(reg_imp, AccountNumber %in% reg_accts[1:4]),
             aes(x = Month, y = Use + 1, col = "NA")) +
  scale_y_log10() +
  facet_wrap(~ AccountNumber, scales = "free")


# fill imputed values; write to db #############################################
# pull in imputed values from rf & reg to fill remaining NAs
eo88_imp <- eo88_imp %>%
  left_join(rf_imp, by = c("AccountNumber", "Month", "Share"),
            suffix = c("", "_rf")) %>%
  mutate(Use = if_else(Imputed == "rf", Use_rf, Use)) %>%
  left_join(reg_imp, by = c("AccountNumber", "Month", "Share", "Cost"),
            suffix = c("", "_reg")) %>%
  mutate(Use = if_else(Imputed == "reg", Use_reg, Use)) %>%
  select(-Use_rf, -Use_reg)

# write to db
con <- dbConnect(MySQL(), default.file = paste0(getwd(), "/", ".my.cnf"))
db <- dbSendQuery(con, "USE EO88;")
dbFetch(db)
dbWriteTable(con, "consumption_filingdata_imputed", eo88_imp, row.names = FALSE)

# aggregate totals by calendar month across different billing cycles
eo88_final <- eo88_imp %>%
  mutate(Use = Use * Share,
         Cost = Cost * Share) %>%
  select(-Share) %>%
  group_by(ESPLocationID, Utility, AccountNumber, Fuel, Units, Month, SFY) %>%
  summarize(Demand = max(Demand, na.rm = TRUE),
            Use = sum(Use, na.rm = TRUE),
            Cost = sum(Cost, na.rm = TRUE)) %>%
  ungroup()

# write to db
dbWriteTable(con, "consumption_filingdata_final", eo88_final, row.names = FALSE)

# write additional tables for non-reporting purposes
dbWriteTable(con, "consumption_filingdata_imputed_summed", eo88_final, row.names = FALSE)

# write summed non-imputed version
eo88_summed <- eo88 %>%
  mutate(Use = Use * Share,
         Cost = Cost * Share) %>%
  select(-Share) %>%
  group_by(ESPLocationID, Utility, AccountNumber, Fuel, Units, Month, SFY) %>%
  summarize(Demand = max(Demand, na.rm = TRUE),
            Use = sum(Use, na.rm = TRUE),
            Cost = sum(Cost, na.rm = TRUE)) %>%
  ungroup()

dbWriteTable(con, "consumption_filingdata_asis_summed", eo88_summed, row.names = FALSE)

# close connection
dbDisconnect(con)

# output results
save(eo88_imp, eo88_final, eo88_summed, file = "data/output/impute-missing.Rda")
