library(tidyverse)

# extract from db ##############################################################
library(dbplyr)
library(odbc)
library(DBI)

# connect to db
con <- dbConnect(odbc(), Driver = "SQL Server", Server = getOption("ms-sql-server"), Database = "NYEMEO88")

# connect to relevant tables
meta <- tbl(con, in_schema("EO88", "building_metadata"))
sfy  <- tbl(con, in_schema("EO88", "building_filingdata"))
eofy <- tbl(con, in_schema("EO88", "consumption_filingdata_final"))

# join tables
full_db <- eofy %>% 
  left_join(meta) %>% 
  # MTA has not reported for SFY 2016-17; only one overlapping month
  filter(Agency != "MTA" | SFY != "2016-17") %>% 
  group_by(Agency, ESPLocationID, Name, SFY) %>% 
  summarize(kBtu = sum(Use)) %>% 
  left_join(sfy) %>% 
  filter(Status == "included")

# eui for all agencies
bsny_state_db <- full_db %>% 
  group_by(SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  ungroup() %>% 
  mutate(Reduction = EUI / first(EUI) - 1)

# get eui by agency
bsny_agency_db <- full_db %>% 
  group_by(Agency, SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  group_by(Agency) %>% 
  mutate(Reduction = EUI / first(EUI) - 1)

# get eui by agency, facility, year
bsny_name_db <- full_db %>% 
  group_by(Agency, ESPLocationID, Name, SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  group_by(Agency, ESPLocationID, Name) %>% 
  mutate(Reduction = EUI / first(EUI) - 1)

# viz from db
library(dbplot)
theme_set(theme_light())
bsny_name_db %>%
  dbplot_bar(SFY)

# slow; check performance in memory ############################################
dbDisconnect(con)

# square footage needs to be brought in after SFY roll-up due to multiple counting
bsny_data <- eo88_final %>%
  left_join(bldg_meta) %>% 
  # MTA has not reported for SFY 2016-17; only one overlapping month
  filter(Agency != "MTA" | SFY != "2016-17") %>% 
  group_by(Agency, ESPLocationID, Name, SFY) %>% 
  summarize(kBtu = sum(Use)) %>% 
  left_join(bldg_sfy) %>% 
  filter(Status == "included")

# sanity check non-imputed for DOCCS
eo88_summed %>% 
  left_join(bldg_meta) %>% 
  filter(Agency == "DOCCS",
         SFY %in% c("2010-11", "2016-17")) %>% 
  group_by(Agency, SubAgency, ESPLocationID, SFY) %>% 
  summarize(kBtu = sum(Use)) %>% 
  left_join(bldg_sfy) %>% 
  filter(Status == "included") %>% 
  group_by(Agency, SubAgency, SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>%
  group_by(Agency, SubAgency) %>% 
  mutate(Reduction = EUI / first(EUI) - 1) %>% 
  View()

# facility performance
bsny_facility <- bsny_data %>% 
  group_by(Agency, ESPLocationID, Name, SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  group_by(Agency, ESPLocationID, Name) %>% 
  mutate(Reduction = EUI / first(EUI) - 1)

# agency performance
bsny_agency <- bsny_data %>% 
  group_by(Agency, SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  group_by(Agency) %>% 
  mutate(Reduction = EUI / first(EUI) - 1)

# state performance
bsny_state <- bsny_data %>% 
  group_by(SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  ungroup() %>% 
  mutate(Reduction = EUI / first(EUI) - 1)

# viz agencies & total
bsny_agency %>% 
  ggplot() +
  geom_line(aes(x = SFY, y = Reduction, group = Agency), alpha = 0.25) +
  geom_point(data = bsny_state, aes(x = SFY, y = Reduction),
             size = 2, col = "black") +
  geom_line(data = bsny_state, aes(x = SFY, y = Reduction), group = 1,
            lwd = 1, col = "black") +
  geom_hline(yintercept = -0.2, lty = 2, lwd = 1, col = "#006ba6") +
  geom_hline(yintercept = 0, lty = 3, col = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(-0.4, 0.4), oob = scales::squish) +
  labs(title = "State & agency progress towards EO88 target",
       subtitle = "For all agencies reporting through SFY 2016-17",
       x = "State Fiscal Year", y = "Change in EUI")

# viz facilities & total
bsny_facility %>% 
  ggplot() +
  geom_line(aes(x = SFY, y = Reduction, group = ESPLocationID), alpha = 0.1) +
  geom_point(data = bsny_state, aes(x = SFY, y = Reduction),
             size = 2, col = "black") +
  geom_line(data = bsny_state, aes(x = SFY, y = Reduction), group = 1,
            lwd = 1, col = "black") +
  geom_hline(yintercept = -0.2, lty = 2, lwd = 1, col = "#006ba6") +
  geom_hline(yintercept = 0, lty = 3, col = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(-0.5, 0.5)) +
  labs(title = "State & facility progress towards EO88 target",
       subtitle = "For all agencies reporting through SFY 2016-17",
       x = "State Fiscal Year", y = "Change in EUI") +
  theme(panel.grid.minor = element_blank())

# viz by fuel & type
fuel_viz <- eo88_final %>%
  left_join(bldg_meta) %>% 
  filter(Agency != "MTA" | SFY != "2016-17") %>% 
  group_by(Agency, Fuel, ESPLocationID, Name, SFY) %>% 
  summarize(kBtu = sum(Use)) %>% 
  left_join(bldg_sfy) %>% 
  filter(Status == "included") %>% 
  group_by(Fuel, SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  group_by(Fuel) %>% 
  mutate(Reduction = EUI / first(EUI) - 1) %>% 
  filter(Fuel != "Fuel Oil # 1") %>% 
  ggplot(aes(x = SFY, y = Reduction, group = Fuel, col = Fuel)) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous(limits = c(NA, 1))

type_viz <- eo88_final %>%
  left_join(bldg_meta) %>% 
  filter(Agency != "MTA" | SFY != "2016-17") %>% 
  group_by(Agency, ESPLocationID, Name, SFY) %>% 
  summarize(kBtu = sum(Use)) %>% 
  left_join(bldg_sfy) %>% 
  filter(Status == "included") %>% 
  group_by(Type1, SFY) %>% 
  summarize(kBtu = sum(kBtu), sf = sum(FloorArea),
            EUI = kBtu / sf) %>% 
  group_by(Type1) %>% 
  mutate(Reduction = EUI / first(EUI) - 1) %>% 
  ggplot(aes(x = SFY, y = Reduction, group = Type1, col = Type1)) +
  geom_line(show.legend = FALSE)


# save output ##################################################################
save(bsny_data, bsny_state, bsny_agency, bsny_facility, file = "data/calculate-eui.Rda")
