# detect missing or abberant values; prep for imputation  #####################

# create function to detect outliers
is.outlier <- function(out_var) {
  out_var < quantile(out_var, 0.25, na.rm = TRUE) - 1.5 * IQR(out_var, na.rm = TRUE) |
    out_var > quantile(out_var, 0.75, na.rm = TRUE) + 1.5 * IQR(out_var, na.rm = TRUE)
}

# flag values needing imputation (missing or outlier)
model_data <- eo88 %>%
  group_by(ESPLocationID, Fuel, Units) %>%
  mutate(Flagged = is.na(Use) | is.outlier(Use) | Use < 0) %>%
  ungroup()

# import weather data for imputation ###########################################
# get lookup values for climate regions
regions <- data_frame(
  Division = 1:10,
  ClimateRegion = c("Western Plateau", "Eastern Plateau", "Northern Plateau",
                    "Coastal", "Hudson Valley", "Mohawk Valley", "Champlain Valley",
                    "St. Lawrence Valley", "Great Lakes", "Central Lakes")
)

# read in NOAA weather data
weather <- read_csv("data/2017-11-14_NOAA_cdo-div-weather-indices.txt",
                    col_types = "iicnnnnnniinnnnnnnnn?")
## https://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp

# convert weather YearMonth to date; drop extra columns
weather <- weather %>%
  mutate(YearMonth = ymd(paste0(YearMonth, "01"))) %>%
  rename(Month = YearMonth) %>%
  select(-StateCode, -X21)

# prepare data for modeling ####################################################
# join reporting data with bldg metadata, weather, bldg sfy data
model_data <- model_data %>%
  left_join(bldg_meta, by = "ESPLocationID") %>%
  left_join(regions, by = "ClimateRegion") %>%
  left_join(weather, by = c("Division", "Month")) %>%
  left_join(bldg_sfy %>% select(ESPLocationID, SFY, FloorArea, Type1),
            by = c("ESPLocationID", "SFY"))

# filter for not Flagged; pull fields of interest & convert character to numeric
model_data_1 <- model_data %>%
  filter(Flagged == FALSE) %>%
  select(ESPLocationID, Fuel, Month, Agency, SFY, Agency, ZipCode,
         YearBuilt, Division:TMAX, FloorArea, Type1, Use) %>%
  mutate_if(is.character, function(x) as.numeric(as.factor(x)))

# separate predictors & response for model training
model_data_pred <- model_data_1 %>% select(-Use)
model_data_resp <- model_data_1 %>% pull(Use)

# split data into train & test set
library(caret)
set.seed(100)
model_split <- createDataPartition(model_data_resp, p = 0.75, list = FALSE)

pred_train <- model_data_pred[model_split, ]
pred_test <- model_data_pred[-model_split, ]

resp_train <- model_data_resp[model_split]
resp_test <- model_data_resp[-model_split]

# train & evaluate several models ##############################################
run_models <- function(mdl_set) {
  # create containers
  ##mdls <- vector(mode = "list")
  resampled <- numeric()
  test <- numeric()
  for (mdl in mdl_set) {
    # train model
    mdl_train <- try(train(pred_train, resp_train,
                           method = mdl))
    # predict on test set
    mdl_pred <- try(predict(mdl_train, pred_test))
    # get test set performance
    mdl_perf <- try(defaultSummary(data.frame(obs = resp_test, pred = mdl_pred)))
    # store results in container
    ##mdls[[mdl]] <<- mdl_train
    # mdl_results <- rbind(mdl_results,
    #                      data_frame(model = mdl,
    #                                 resampled = min(mdl_train$results$RMSE),
    #                                 test = mdl_perf[["RMSE"]]))
    resampled <- c(resampled, min(mdl_train$results$RMSE))
    test <- c(test, mdl_perf[["RMSE"]])
  }
  mdl_results <- data_frame(model = mdl_set, resampled, test)
  mdl_results
}

mdls <- vector(mode = "list")
my_models <- c(# linear
               "lm", "rlm", "ridge", "pls",
               # nonlinear
               "knn", "nnet", "earth", "svmRadial", "svmLinear", "svmPoly",
               # tree
               "rpart2", "M5Rules", "rf", "gbm", "Cubist")

for (m in my_models) {
  print(m)
  assign("last.warning", NULL, envir = baseenv())
  mdls[[m]] <- try(run_models(m))
  print(names(warnings())[which(names(warnings()) != "Setting row names on a tibble is deprecated.")])
}

linear_results <- run_models(c("lm", "ridge", "pls", "rlm"))
nonlinear_results <- run_models(c("knn", "nnet", "earth", "svmRadial", "svmLinear", "svmPoly"))
tree_results <- run_models(c("rpart2", "M5Rules", "rf", "gbm", "Cubist"))


# transform predictors #########################################################
# investigate weather variable distribution
weather %>%
  gather(Measure, Value, -(Division:Month)) %>%
  ggplot(aes(x = Value)) +
  geom_histogram(col = "grey50", fill = "white", bins = 15) +
  facet_grid(Division ~ Measure, scales = "free") +
  scale_x_continuous("Variable", NULL, NULL, position = "top") +
  scale_y_continuous("Region", NULL, NULL, position = "right") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# convert month to month of year
model_data_pred$Month <- month(model_data_pred$Month)
# transform raw predictors
model_preprocess <- preProcess(model_data_pred, method = c("center", "scale", "knnImpute", "corr", "zv"))
model_data_pred_trans <- predict(model_preprocess, model_data_pred)

# get data with character vectors
model_data_2 <- eo88 %>%
  left_join(bldg_meta, by = "ESPLocationID") %>%
  left_join(regions, by = "ClimateRegion") %>%
  left_join(weather, by = c("Division", "Month")) %>%
  left_join(bldg_sfy %>% select(ESPLocationID, SFY, FloorArea, Type1),
            by = c("ESPLocationID", "SFY")) %>%
  filter(Flagged == FALSE) %>%
  select(ESPLocationID, Fuel, Month, ZipCode,
         YearBuilt, Division:TMAX, FloorArea, Type1, Use)

# create dummy vars
model_data_pred_2 <- model_data_2 %>%
  mutate(Month = month(Month),
         ZipCode = parse_number(ZipCode)) %>%
  select(-Type1, -Use)

model_preprocess_2 <- preProcess(model_data_pred_2, method = c("center", "scale", "knnImpute", "corr", "zv"))
model_data_pred_trans_2 <- predict(model_preprocess_2, model_data_pred_2)

type_dummy <- dummies::dummy(model_data_2$Type1)
fuel_dummy <- dummies::dummy(model_data_pred_trans_2$Fuel)
model_data_pred_trans_2 <- model_data_pred_trans_2 %>%
  select(-Fuel, -Agency) %>%
  cbind(fuel_dummy, type_dummy)

# create both models (transfrormed + dummy/transformed)
# test & training set
pred_trans_train <- model_data_pred_trans[model_split, ]
pred_trans_test <- model_data_pred_trans[-model_split, ]

pred_trans_train_2 <- model_data_pred_trans_2[model_split, ]
pred_trans_test_2 <- model_data_pred_trans_2[-model_split, ]


# parallel if possible
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

set.seed(100)
knn_control <- trainControl(method = "cv",
                            number = 5,
                            allowParallel = TRUE)

set.seed(100)
knn_trans <- train(pred_trans_train, resp_train,
                   method="knn", trControl = knn_control)

set.seed(100)
knn_trans_2 <- train(pred_trans_train_2, resp_train,
                     method="knn", trControl = knn_control)

stopCluster(cluster)
registerDoSEQ()


# create knn model for each location ###########################################
library(dummies)

# get locations with missing data
esp_impute <- model_data %>% filter(Flagged == TRUE) %>% pull(ESPLocationID) %>% unique()

# initalize model container
esp_knn <- lst()
for (esp in esp_impute) {
  # join weatehr data
  loc_data <- eo88 %>%
    filter(ESPLocationID == esp, Flagged == FALSE) %>%
    left_join(bldg_meta, by = "ESPLocationID") %>%
    left_join(regions, by = "ClimateRegion") %>%
    left_join(weather, by = c("Division", "Month")) %>%
    left_join(bldg_sfy %>% select(ESPLocationID, SFY, FloorArea, Type1),
              by = c("ESPLocationID", "SFY"))
  # create dummies
  loc_fuel <- dummy(loc_data %>% filter(Flagged == FALSE) %>% .$Fuel)
  loc_type <- dummy(loc_data %>% filter(Flagged == FALSE) %>% .$Type1)
  # get non-categorical predictors & response
  loc_pred <- loc_data %>%
    filter(Flagged == FALSE) %>%
    select(Month, PCP:TMAX, FloorArea) %>%
    mutate(Month = month(Month))
  loc_resp <- loc_data %>% filter(Flagged == FALSE) %>% pull(Use)
  # transform predictors with centering/scaling/BoxCox, add dummy vars
  loc_preprocess <- preProcess(as.data.frame(loc_pred),
                               method = c("medianImpute", "zv", "center", "scale", "BoxCox"))
  loc_pred <- predict(loc_preprocess, as.data.frame(loc_pred)) %>%
    cbind(loc_fuel, loc_type)
  # get training & test
  set.seed(100)
  loc_train_rows <- sample(1:nrow(loc_pred), 0.75 * nrow(loc_pred))
  loc_resp_train <- loc_resp[loc_train_rows]
  loc_resp_test <- loc_resp[-loc_train_rows]
  loc_pred_train <- loc_pred[loc_train_rows, ]
  loc_pred_test <- loc_pred[-loc_train_rows, ]
  # train model
  loc_knn <- train(loc_pred_train, loc_resp_train, method = "knn")
  # get results
  resampled_rmse <- min(loc_knn$results$RMSE)
  test_rmse <- defaultSummary(data.frame(obs = loc_resp_test,
                                         pred = predict(loc_knn, loc_pred_test)))[["RMSE"]]
  esp_knn[[as.character(esp)]] <- list(model = loc_knn, perf = c(resampled_rmse, test_rmse))
}

# get results
loc  <- character()
resampled <- numeric()
test <- numeric()

for (l in 1:length(esp_knn)) {
  loc <- c(loc, names(esp_knn)[l])
  resampled <- c(resampled, esp_knn[[l]]$perf[1])
  test <- c(test, esp_knn[[l]]$perf[2])
}

esp_knn_results <- data_frame(loc, resampled, test)



# model for each fuel
esp_fuel_knn <- lst()
for (esp in esp_impute) {
  # join weatehr data
  loc_data <- eo88 %>%
    filter(ESPLocationID == esp, Flagged == FALSE) %>%
    left_join(bldg_meta, by = "ESPLocationID") %>%
    left_join(regions, by = "ClimateRegion") %>%
    left_join(weather, by = c("Division", "Month")) %>%
    left_join(bldg_sfy %>% select(ESPLocationID, SFY, FloorArea),
              by = c("ESPLocationID", "SFY"))
  # for each fuel
  fuel_knn <- lst()
  for (fuel in unique(loc_data$Fuel)) {
    # get predictors & response
    loc_pred <- loc_data %>%
      filter(Flagged == FALSE, Fuel == fuel) %>%
      select(Month, PCP:TMAX, FloorArea) %>%
      mutate(Month = month(Month))
    loc_resp <- loc_data %>% filter(Flagged == FALSE, Fuel == fuel) %>% pull(Use)
    # transform predictors with centering/scaling/BoxCox, add dummy vars
    loc_preprocess <- preProcess(as.data.frame(loc_pred),
                                 method = c("medianImpute", "zv", "center", "scale", "BoxCox"))
    loc_pred <- predict(loc_preprocess, as.data.frame(loc_pred))
    # get training & test
    set.seed(100)
    loc_train_rows <- sample(1:nrow(loc_pred), 0.75 * nrow(loc_pred))
    loc_resp_train <- loc_resp[loc_train_rows]
    loc_resp_test <- loc_resp[-loc_train_rows]
    loc_pred_train <- loc_pred[loc_train_rows, ]
    loc_pred_test <- loc_pred[-loc_train_rows, ]
    # train model
    loc_knn <- train(loc_pred_train, loc_resp_train, method = "knn")
    # get results
    resampled_rmse <- min(loc_knn$results$RMSE)
    test_rmse <- defaultSummary(data.frame(obs = loc_resp_test,
                                           pred = predict(loc_knn, loc_pred_test)))[["RMSE"]]
    fuel_knn[[fuel]] <- list(model = loc_knn, perf = c(resampled_rmse, test_rmse))
  }
  esp_fuel_knn[[as.character(esp)]] <- fuel_knn
}

# get results
loc  <- character()
fuel <- character()
resampled <- numeric()
test <- numeric()

for (l in 1:length(esp_fuel_knn)) {
  for (f in 1:length(esp_fuel_knn[[l]])) {
    loc <- c(loc, names(esp_fuel_knn)[l])
    fuel <- c(fuel, names(esp_fuel_knn[[l]][f]))
    resampled <- c(resampled, esp_fuel_knn[[l]][[f]]$perf[1])
    test <- c(test, esp_fuel_knn[[l]][[f]]$perf[2])
  }
}

esp_fuel_knn_results <- data_frame(loc, fuel, resampled, test)


# plot results for both
esp_knn_results %>%
  gather(Type, RMSE, -loc) %>%
  ggplot(aes(RMSE, ..count../sum(..count..))) +
  geom_histogram(bins = 50, alpha = 0.5, col = "black") +
  facet_wrap(~ Type) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Performance for k-nearest-neighbor models",
       subtitle = "Trained for each location needing imputation",
       y = "Percent")

# plot results for both
esp_fuel_knn_results %>%
  gather(Type, RMSE, -(loc:fuel)) %>%
  ggplot(aes(RMSE, ..count../sum(..count..))) +
  geom_histogram(bins = 50, alpha = 0.5, col = "black") +
  facet_wrap(~ Type) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Performance for k-nearest-neighbor models",
       subtitle = "Trained for each location & fuel needing imputation",
       y = "Percent")


# simple median of previous months (+-1) #######################################
# get data for testing; keep non-Flagged to test accuracy
med_data <- eo88 %>%
  filter(ESPLocationID %in% esp_impute) %>%
  select(ESPLocationID, Fuel, Month, Use, Flagged) %>%
  mutate(month_no = month(Month),
         use_Flagged = numeric(nrow(.)))

for (i in 1:nrow(med_data)) {
  months_include <- month(eo88$Month[i] + months(-1:1))
  med_data$use_Flagged[i] <- med_data %>%
    filter(ESPLocationID == ESPLocationID[i],
           Fuel == Fuel[i],
           month_no %in% months_include) %>%
    pull(Use) %>%
    median(na.rm = TRUE)
}

imp_med <- med_data %>% filter(Flagged == FALSE)
RMSE(imp_med$use_Flagged, obs = imp_med$Use, na.rm = TRUE)

med_data %>%
  filter(Flagged == FALSE) %>%
  group_by(ESPLocationID, Fuel) %>%
  summarize(RMSE = RMSE(use_Flagged, Use, na.rm = TRUE),
            RMSE_pct = RMSE / mean(Use, na.rm = TRUE)) %>%
  summary(RMSE_pct)

med_data %>%
  filter(Flagged == FALSE) %>%
  group_by(ESPLocationID, Fuel) %>%
  summarize(RMSE = RMSE(use_Flagged, Use, na.rm = TRUE)) %>%
  ggplot(aes(RMSE, ..count../sum(..count..))) +
  geom_histogram(bins = 50, alpha = 0.5, col = "black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Performance for median imputation",
       subtitle = "Flagged using median of similar months for each location & fuel needing imputation",
       y = "Percent")


# cubist models for each loc/fuel ##############################################
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# model for each fuel
esp_fuel_cubist <- lst()
for (esp in esp_impute) {
  # join weatehr data
  loc_data <- eo88 %>%
    filter(ESPLocationID == esp, Flagged == FALSE) %>%
    left_join(bldg_meta, by = "ESPLocationID") %>%
    left_join(regions, by = "ClimateRegion") %>%
    left_join(weather, by = c("Division", "Month")) %>%
    left_join(bldg_sfy %>% select(ESPLocationID, SFY, FloorArea),
              by = c("ESPLocationID", "SFY"))
  # set training control
  set.seed(100)
  cube_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

  # for each fuel
  fuel_cubist <- lst()
  for (fuel in unique(loc_data$Fuel)) {
    # get predictors & response
    loc_pred <- loc_data %>%
      filter(Flagged == FALSE, Fuel == fuel) %>%
      select(Month, PCP:TMAX, FloorArea) %>%
      mutate(Month = month(Month))
    loc_resp <- loc_data %>% filter(Flagged == FALSE, Fuel == fuel) %>% pull(Use)
    # transform predictors with centering/scaling/BoxCox, add dummy vars
    loc_preprocess <- preProcess(as.data.frame(loc_pred),
                                 method = c("medianImpute", "zv", "center", "scale", "BoxCox"))
    loc_pred <- predict(loc_preprocess, as.data.frame(loc_pred))
    # get training & test
    set.seed(100)
    loc_train_rows <- sample(1:nrow(loc_pred), 0.75 * nrow(loc_pred))
    loc_resp_train <- loc_resp[loc_train_rows]
    loc_resp_test <- loc_resp[-loc_train_rows]
    loc_pred_train <- loc_pred[loc_train_rows, ]
    loc_pred_test <- loc_pred[-loc_train_rows, ]
    # train model
    loc_cubist <- train(loc_pred_train, loc_resp_train, method = "cubist",
                        tuneGrid = expand.grid(committees = c(1, 25, 50, 75, 100),
                                               neighbors = c(0, 1, 3, 5, 7, 9)))
    # get results
    resampled_rmse <- min(loc_cubist$results$RMSE)
    test_rmse <- defaultSummary(data.frame(obs = loc_resp_test,
                                           pred = predict(loc_cubist, loc_pred_test)))[["RMSE"]]
    fuel_cubist[[fuel]] <- list(model = loc_cubist, perf = c(resampled_rmse, test_rmse))
  }
  esp_fuel_cubist[[as.character(esp)]] <- fuel_cubist
}

stopCluster(cluster)
registerDoSEQ()

# get results
loc  <- character()
fuel <- character()
resampled <- numeric()
test <- numeric()

for (l in 1:length(esp_fuel_cubist)) {
  for (f in 1:length(esp_fuel_cubist[[l]])) {
    loc <- c(loc, names(esp_fuel_cubist)[l])
    fuel <- c(fuel, names(esp_fuel_cubist[[l]][f]))
    resampled <- c(resampled, esp_fuel_cubist[[l]][[f]]$perf[1])
    test <- c(test, esp_fuel_cubist[[l]][[f]]$perf[2])
  }
}

esp_fuel_cubist_results <- data_frame(loc, fuel, resampled, test)



# troubleshoot errors ##########################################################
# summary statistics for Use by flagged & not flagged
model_data %>%
  filter(ESPLocationID == 92, Fuel == "Natural Gas") %>%
  select(Use, Flagged) %>%
  group_by(Flagged) %>%
  summarize(min = min(Use, na.rm = TRUE),
            max = max(Use, na.rm = TRUE),
            sd = sd(Use, na.rm = TRUE),
            mean = mean(Use, na.rm = TRUE),
            median = median(Use, na.rm = TRUE),
            n = n())

# histogram of natural gas Use colored by flagged
model_data %>%
  filter(ESPLocationID == 92, Fuel == "Natural Gas") %>%
  ggplot(aes(x = Use, y = ..count.. / sum(..count..), fill = Flagged)) +
  geom_histogram(bins = 25, col = "black") +
  scale_x_continuous(trans = "log10", breaks = 10^(2:8),
                     labels = c(100, "1k", "10k", "100k", "1M", "10M", "100M")) +
  scale_y_continuous("", labels = scales::percent) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

# boxplot of non-missing Use by fuel colored by flagged
model_data %>%
  filter(ESPLocationID == 92, !is.na(Use)) %>%
  ggplot(aes(x = Flagged, y = Use, fill = Flagged)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_continuous(trans = "log10", breaks = 10^(2:7),
                     labels = c(100, "1k", "10k", "100k", "1M", "10M")) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  facet_wrap(~ Fuel, scales = "free_y")

# violin plot of non-missing natural gas Use by SFY & month colored by flagged
model_data %>%
  filter(ESPLocationID == 92, !is.na(Use), Fuel == "Natural Gas",
         !SFY %in% c("2010-11", "2011-12", "2012-13")) %>%
  ggplot(aes(x = Flagged, y = Use, fill = Flagged)) +
  geom_violin() +
  scale_y_continuous(trans = "log10") +
  scale_x_discrete(NULL, NULL, NULL) +
  facet_grid(SFY ~ month(Month)) +
  theme(legend.position = "top")

# mosaic plot of non-missing natural gas Use by SFY & month colored by flagged
library(ggmosaic)
model_data %>%
  filter(ESPLocationID == 92, !is.na(Use), Fuel == "Natural Gas",
         !SFY %in% c("2010-11", "2011-12", "2012-13")) %>%
  group_by(SFY, Month, Flagged) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Month = factor(month(Month), levels = c(4:12, 1:3))) %>%
  ggplot() +
  geom_mosaic(aes(weight = n, x = product(1), fill = Flagged), alpha = 1, col = "black") +
  facet_grid(SFY ~ Month) +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())

# area plot of non-missing natural gas use by SFY & month colored by flagged
model_data %>%
  filter(ESPLocationID == 92, !is.na(Use), Fuel == "Natural Gas",
         !SFY %in% c("2010-11", "2011-12", "2012-13")) %>%
  mutate(Month = factor(month(Month), levels = c(4:12, 1:3))) %>%
  group_by(SFY, Month) %>%
  summarize(true = sum(Flagged == TRUE) / n(),
            false = 1 - true) %>%
  ungroup() %>%
  gather(Flagged, Share, -(SFY:Month)) %>%
  mutate(Flagged = if_else(Flagged == "true", TRUE, FALSE)) %>%
  ggplot() +
  geom_area(aes(Month, Share, fill = Flagged, group = interaction(SFY, Flagged)), col = "black") +
  facet_grid(SFY ~ .) +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent)

# area plot of non-missing natural gas use by month colored by flagged
model_data %>%
  filter(ESPLocationID == 92, !is.na(Use), Fuel == "Natural Gas",
         !SFY %in% c("2010-11", "2011-12", "2012-13")) %>%
  group_by(Month) %>%
  summarize(true = sum(Flagged == TRUE) / n(),
            false = 1 - true) %>%
  ungroup() %>%
  gather(Flagged, Share, -(Month)) %>%
  mutate(Flagged = if_else(Flagged == "true", TRUE, FALSE)) %>%
  ggplot(aes(Month, Share, fill = Flagged, col = Flagged)) +
  geom_area() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(NULL, date_breaks = "3 months", date_labels = "%Y\n%m")

# area plot of natural gas use by SFY & month colored by flagged
model_data %>%
  filter(ESPLocationID == 92, Fuel == "Natural Gas",
         !SFY %in% c("2010-11", "2011-12", "2012-13")) %>%
  group_by(Month) %>%
  summarize(true = sum(is.na(Use)) / n(),
            false = 1 - true) %>%
  ungroup() %>%
  gather(Missing, Share, -(Month)) %>%
  mutate(Missing = if_else(Missing == "true", TRUE, FALSE)) %>%
  ggplot(aes(Month, Share, fill = Missing, col = Missing)) +
  geom_area(alpha = 0.75) +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(NULL, date_breaks = "3 months", date_labels = "%Y\n%m")

# following scope adapation, see impute-missing.R ##############################
