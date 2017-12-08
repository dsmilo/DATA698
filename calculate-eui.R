library(tidyverse)

# extract from db ##############################################################
library(dbplyr)

# connect to db
con <- dbConnect(MySQL(), default.file = paste0(getwd(), "/", ".my.cnf"))
db <- dbSendQuery(con, "USE EO88;")
dbClearResult(db)

# connect to relevant tables
meta <- tbl(con, "building_metadata")
sfy  <- tbl(con, "building_filingdata")
eofy <- tbl(con, "consumption_filingdata_summed")

# join tables
full_db <- eofy %>% 
  left_join(meta) %>% 
  left_join(sfy) %>% 
  filter(Status == "included")

# get eui by agency, facility, year
bsny_name <- full_db %>% 
  group_by(Agency, Name, SFY) %>% 
  summarize(EUI = sum(Use) / sum(FloorArea)) %>% 

# get eui by agency
bsny_agency <- full_db %>% 
  group_by(Agency, SFY) %>% 
  summarize(EUI = sum(Use) / sum(FloorArea))

# eui for all agencies
bsny_state <- full_db %>% 
  group_by(SFY) %>% 
  summarize(EUI = sum(Use) / sum(FloorArea))

# viz from db
library(dbplot)
theme_set(theme_light())
bsny_name %>%
  dbplot_bar(SFY)

# slow; check performance in memory ############################################
dbDisconnect(con)

bsny_data <- eo88 %>% 
  left_join(bldg_meta) %>% 
  left_join(bldg_sfy) %>% 
  # MTA has not reported for SFY 2016-17; only one overlapping month
  filter(Status == "included",
         Agency != "MTA" | SFY != "2016-17")

bsny_facility <- bsny_data %>% 
  group_by(Agency, Name, SFY) %>% 
  summarize(kBtu = sum(Use), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  group_by(Agency, Name) %>% 
  mutate(Reduction = 1 - EUI / first(EUI))

bsny_agency <- bsny_data %>% 
  filter(Status == "included",
         Agency != "MTA" | SFY != "2016-17") %>% 
  group_by(Agency, SFY) %>% 
  summarize(kBtu = sum(Use), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  group_by(Agency) %>% 
  mutate(Reduction = 1 - EUI / first(EUI))

bsny_state <- bsny_data %>% 
  # SUNY sf acting weird -- exclude
  filter(Agency != "SUNY") %>% 
  group_by(SFY) %>% 
  summarize(kBtu = sum(Use), sf = sum(FloorArea),
            EUI = kBtu / sf) %>%
  mutate(Reduction = 1 - EUI / first(EUI))


# viz agencies & total
library(scales)
bsny_agency %>% 
  # some agencies results off -- exclude
  filter(!Agency %in% c("AgricultureandMarkets", "DEC", "DMNA-Armories",
                        "DOH", "OPWDD", "SUNY")) %>% 
  ggplot() +
  geom_line(aes(x = SFY, y = Reduction, group = Agency), alpha = 0.25) +
  geom_point(data = bsny_state, aes(x = SFY, y = Reduction),
            size = 2, col = "black") +
  geom_line(data = bsny_state, aes(x = SFY, y = Reduction), group = 1,
            lwd = 1, col = "black") +
  geom_hline(yintercept = 0.2, lty = 2, lwd = 1, col = "#006ba6") +
  scale_y_continuous(labels = percent, limits = c(-0.5, 0.5), oob = squish) +
  labs(title = "State & agency progress towards EO88 target",
       subtitle = "Calculated from Talisen extracts & imputation for missing values",
       caption = "Imputation performed using linear regression & random trees",
       x = "State Fiscal Year", y = "EUI Reduction")

# viz facilities
bsny_facility %>% 
  ggplot() +
  geom_line(aes(x = SFY, y = Reduction, group = Name), alpha = 0.1) +
  geom_point(data = bsny_state, aes(x = SFY, y = Reduction),
             size = 2, col = "black") +
  geom_line(data = bsny_state, aes(x = SFY, y = Reduction), group = 1,
            lwd = 1, col = "black") +
  geom_hline(yintercept = 0.2, lty = 2, lwd = 1, col = "#006ba6") +
  scale_y_continuous(labels = percent, limits = c(-0.5, 0.5)) +
  labs(title = "State & facility progress towards EO88 target",
       subtitle = "Calculated from Talisen extracts & imputed missing values",
       caption = "Imputation performed using linear regression & random tree models",
       x = "State Fiscal Year", y = "EUI Reduction")
