
# load supplied building & consumption data ####################################
library(tidyverse)

bldg <- read_csv("data/2017-10-25_nyem-eo88_bldg-all-sfy.csv")
eo88 <- read_csv("data/2017-10-26_nyem-eo88_consumption-all-sfy.csv")


# transform building data ######################################################
library(stringr)

# tidy data that changes by SFY
bldg <- bldg %>%
  gather(Field, Value,
         `SFY2010-11 Gross Floor Area (ft2)`:
           `SFY2016-17 Property Type 5 Percent of Total Gross Floor Area`)
# separate FY from actual measure
bldg <- bldg %>% 
  mutate(SFY = str_sub(Field, end = 10),
         Field = str_sub(Field, start = 12))
# remove duplicate entries per FY & ESP ID (unique identifier)
bldg <- bldg %>% distinct(`ESP Location ID`, SFY, Field, .keep_all = TRUE)
# spread FY-based fields
bldg <- bldg %>% 
  spread(Field, Value)
# clean up bldg names
names(bldg) <- str_replace_all(names(bldg), " ", "")

# create two dfs -- one with fixed data & hierarchy; other with SFY data
bldg_meta <- bldg %>% select(ESPLocationID, NYEMSystemID, AgencyName:NYSNOAAClimateRegion)
bldg_sfy  <- bldg %>% select(ESPLocationID, Status, SFY:`WeeklyOperatingHours(Hours/Week)`)

# metadata cleanup: remove duplicates from SFY & rename fields
bldg_meta <- distinct(bldg_meta)
bldg_meta <- bldg_meta %>% 
  rename(Agency = AgencyName,
         ClimateRegion = NYSNOAAClimateRegion,
         Name = BuildingName,
         Address = BuildingAddress,
         City = BuildingCity,
         ZipCode = BuildingZipCode)

# sfy data cleanup: clean up names, drop unneeded columns, change types as needed
bldg_sfy$SFY <- str_sub(bldg_sfy$SFY, 4)  # remove "SFY" from SFY field
bldg_sfy <- bldg_sfy %>% # shorten long names, remove any parentheses
  rename(FloorArea = `GrossFloorArea(ft2)`,
         PeakOccupants = PeakTotalOccupants,
         CooledPct = PercentofGrossFloorAreaCooled,
         HeatedPct = PercentofGrossFloorAreaHeated,
         Type_BEDES = `PropertyType(BEDES)`,
         Type_ES = `PropertyType(ES)`,
         WeeklyHours = `WeeklyOperatingHours(Hours/Week)`)
names(bldg_sfy) <- str_replace_all(names(bldg_sfy), "Property", "") # shorten property & pct
names(bldg_sfy) <- str_replace_all(names(bldg_sfy), "PercentofTotalGrossFloorArea", "Pct")
bldg_sfy <- bldg_sfy %>% select(-Type) # drop empty "Type" field
bldg_sfy <- mutate_at(bldg_sfy,
  vars(FloorArea, PeakOccupants, CooledPct, HeatedPct, Type1Pct, Type2Pct,
       Type3Pct, Type4Pct, Type5Pct, WeeklyHours),
  parse_number)



# set up db & write building data ##############################################
library(DBI)
library(odbc)
# connect to local SQL Server instance
con <- dbConnect(odbc(), Driver = "{SQL Server}", Server = Sys.getenv("USERDOMAIN"))
# create EO88 db
dbSendQuery(con, "DROP DATABASE IF EXISTS EO88;")
dbSendQuery(con, "CREATE DATABASE EO88;")
dbSendQuery(con, "USE EO88;")
# write two building tables
dbWriteTable(con, "building_metadata", bldg_meta, row.names = FALSE)
dbWriteTable(con, "building_filingdata", bldg_sfy, row.names = FALSE)
# disconnect from server for sanitation while working on eo88 data
dbDisconnect(con)




# transform eo88 data ##########################################################

# clean up eo88 names
names(eo88) <- str_replace_all(names(eo88), " ", "")

# rename fields
eo88 <- eo88 %>% 
  rename(Parent = ParentESPLocationID,
         FuelUnits = `FuelType(units)`,
         Start = BillingPeriodStart,
         End = BillingPeriodEnd,
         Utility = UtilityProvider,
         Demand = `Demand(kW)`)
# split fuel type & units
eo88 <- eo88 %>%
  mutate(Fuel = str_split(FuelUnits, " \\(", simplify = TRUE)[, 1],
         Units = str_split(FuelUnits, " \\(", simplify = TRUE)[, 2],
         Units = str_replace_all(Units, "\\)", "")) %>% 
  select(-FuelUnits)
# remove duplicated bldg data (& empty rate/sc) -- retrieve using esp id
eo88 <- eo88 %>% select(ESPLocationID, Parent, Utility, AccountNumber,
                        Start, End, Fuel, Units, Use, Demand, Cost)
# remove missing usage rows
eo88 <- drop_na(eo88, Use)



