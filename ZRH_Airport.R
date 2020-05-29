if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


# This is the location of the data, but for some reason, the download is not working
# url <- "https://github.com/tlorusso/twist_zrh/raw/master/twist_zrh.RDS"
# temp <- tempfile() # create a tempfile
# download.file(url, temp) # download to disk
# dat <- readRDS(temp) # read the tempfile
# unlink(temp) # Deletes tempfile

#So I have downloaded the data from Git and saved them in the project repository
twist_zrh_clean <- readRDS("./data/twist_zrh_cleaned.RDS")


###################### 
## DATA PREPARATION ##
###################### 


# Some data are as factors and not clean, so first step clean some data by:
# 1. converting factor to char and then to factor back
# 1. small exception on continent North America that has been counted has NA's...
# 2. create month and hour columns
# 3. create a new flight type more readable thatn start_landing variable
# 4. rename diffs variable
flightdata <- twist_zrh_clean %>%
  mutate(continent=as.character(continent),
         airline_name=as.character(airline_name),
         airplane_type=as.character(airplane_type),
         origin_destination_name=as.character(origin_destination_name),
         airport_type=as.character(airport_type),
         iso_country=as.character(iso_country),
         iso_region=as.character(iso_region),
         municipality=as.character(municipality),
         month = month(date),
         hour = hour(planed_time),
         flight_type=ifelse(start_landing=="L","Landing","Take-Off"))
flightdata <- flightdata %>%
  mutate(continent=replace_na(continent,"NA"))
flightdata <- flightdata %>%
  mutate(continent=as.factor(continent),
         airline_name=as.factor(airline_name),
         airplane_type=as.factor(airplane_type),
         airport_type=as.factor(airport_type),
         iso_country=as.factor(iso_country),
         iso_region=as.factor(iso_region),
         month = as.factor(month),
         hour = as.factor(hour),
         flight_type=as.factor(flight_type)) %>%
  rename(c("delay_sec"="diff_in_secs"))

rm(twist_zrh_clean)

#some variables are also not relevant (only one value or replaced by another one), so I remove them also
flightdata <- flightdata %>% select(-tde200h0) %>% select(-start_landing)

# weather data are not available for the 31.12, so I remove them to not have NA's
flightdata <- flightdata %>% filter(!is.na(temp_avg))

# Create a Delay Category to better understand data
flightdata <- flightdata %>% mutate(delay_cat = cut(as.numeric(delay_sec),breaks = c(-Inf,0,600,Inf),labels = c("Early","No Delay","Delay")))
flightdata %>% group_by(delay_cat) %>% summarise(n=n())

# Remove extreme values of the dataset and flights :
flightdata <- flightdata %>% filter(abs(delay_sec)<50000)
Landing <- Landing %>% filter(abs(delay_sec)<50000)
TakeOff <- TakeOff %>%  filter(abs(delay_sec)<50000)

# Select Airlines with more than 500 flights with a delay greater than 0
airline <- flightdata %>%
  group_by(airline_name) %>%
  summarize(NbFlights=n()) %>%
  filter(NbFlights>500) %>%
  select(airline_name)

flightdata <- inner_join(flightdata,airline,by="airline_name")
rm(airline)
flightdata$airline_name <- factor(flightdata$airline_name)
flightdata$airplane_type <- factor(flightdata$airplane_type)


###################### 
## DATA EXPLORATION ##
######################

# Summary of the variable we want to predict
summary(as.numeric(flightdata$delay_sec))

Landing <- flightdata %>% filter(flight_type=="Landing")
knitr::kable(data.frame(Min=as.numeric(summary(as.numeric(Landing$delay_sec))[1]),
                        'First Qu.'=as.numeric(summary(as.numeric(Landing$delay_sec))[2]),
                        Median=as.numeric(summary(as.numeric(Landing$delay_sec))[3]),
                        Mean=as.numeric(summary(as.numeric(Landing$delay_sec))[4]),
                        'Third Qu.'=as.numeric(summary(as.numeric(Landing$delay_sec))[5]),
                        Max=as.numeric(summary(as.numeric(Landing$delay_sec))[6])),caption="Landing Delay summary statistics")
TakeOff <- flightdata %>% filter(flight_type=="Take-Off")
knitr::kable(data.frame(Min=as.numeric(summary(as.numeric(TakeOff$delay_sec))[1]),
                        'First Qu.'=as.numeric(summary(as.numeric(TakeOff$delay_sec))[2]),
                        Median=as.numeric(summary(as.numeric(TakeOff$delay_sec))[3]),
                        Mean=as.numeric(summary(as.numeric(TakeOff$delay_sec))[4]),
                        'Third Qu.'=as.numeric(summary(as.numeric(TakeOff$delay_sec))[5]),
                        Max=as.numeric(summary(as.numeric(TakeOff$delay_sec))[6])),caption="Take-Off Delay summary statistics")

flightdata %>% group_by(flight_type) %>% summarise(m=mean(as.numeric(delay_sec)))

flightdata %>% arrange(desc(delay_sec))%>% select(date,airline_name, flight_type, distance_km, delay_sec) %>% head(15)

# BoxPlot to see patterns in delay by Type of Flight
flightdata %>%
  ggplot(aes(x=flight_type ,y=as.numeric(delay_sec))) +
  geom_boxplot() +
  xlab("Type of Flight") +
  ylab("Delay (sec)") +
  theme_economist()

# Focus Analysis on absolute delays < 2 hours
flightdata %>% ggplot(aes(x=flight_type ,y=as.numeric(delay_sec))) +
  geom_boxplot() +
  ylim(-7200,7200)+
  xlab("Type of Flight") +
  ylab("Delay (sec)") +
  theme_economist()

# Another view of data with an histogram
flightdata %>%
  filter(abs(delay_sec) < 7200) %>%
  ggplot(aes(x = delay_sec)) + 
  geom_histogram(bins = 60, col = 1) +
  labs(title = "Asolute delay < 2 hours") +
  ylab("Number of flights") + xlab("Delay (sec)") +
  facet_wrap(~ flight_type) +
  theme_economist()

# Analyze data by month
flightdata %>%
  filter(abs(delay_sec) < 5000) %>%
  ggplot(aes(x = month, y = as.numeric(delay_sec))) + 
  geom_boxplot() +
  ylab("Delay (sec)") +
  facet_wrap(~ flight_type)

flightdata %>%
  filter(abs(delay_sec) < 7200) %>%
  group_by(month) %>%
  summarize(n=n(),m=mean(as.numeric(delay_sec)))


#Analyze data by Hour
flightdata %>%
  filter(abs(delay_sec) < 7200) %>%
  ggplot(aes(x = hour, y = as.numeric(delay_sec))) + 
  geom_boxplot() +
  ylab("Delay (sec)") +
  facet_wrap(~ flight_type)

flightdata %>%
  filter(abs(delay_sec) < 7200) %>%
  group_by(month) %>%
  summarize(n=n(),m=mean(as.numeric(delay_sec)))

# Analyze data by Source/Destination of Flight
flightdata %>%
  filter(abs(delay_sec) < 7200) %>%
  ggplot(aes(x = continent, y = as.numeric(delay_sec))) +
  geom_boxplot()+
  ylab("Delay (sec)") +
  facet_wrap(~ flight_type)

# Analyze by Airline
flightdata %>% ggplot(aes(x=as.numeric(delay_sec),y=reorder(airline_name,-as.numeric(delay_sec)),col=airline_name)) +
  geom_point() + 
  geom_vline(xintercept = 900) +
  geom_jitter(alpha = 0.5, size = 0.3) + guides( color = FALSE) +
  facet_wrap(~ flight_type) + ylab(NULL) +
  xlab("Delay (sec)")


######################
### DATA PREDICTION ##
######################

rm(TakeOff,Landing)

# Create two datasets for Landing and Take-Off and select meaningfull variable
Landing <- flightdata %>% filter(flight_type=="Landing") %>%
  mutate(delay = as.numeric(delay_sec), delayYN=as.factor((delay>900))) %>%
  select(delayYN,airline_name, airplane_type,distance_km,windspeed_avg_h,precip,temp_avg,airpres,hour,month) 
TakeOff <- flightdata %>% filter(flight_type=="Take-Off") %>%
  mutate(delay = as.numeric(delay_sec), delayYN=as.factor((delay>900))) %>%
  select(delayYN,airline_name, airplane_type,distance_km,windspeed_avg_h,precip,temp_avg,airpres,hour,month) 


## Take-Off Part ##

rm(TakeOff_train_set,TakeOff_test_set, glm_TO_A,glm_TO_AHD,glm_TO_Full,delay_hat_glm_TO_A,
   randfor_TO_A,randfor_TO_AHD,randfor_TO_Full)

set.seed(2020)
TakeOff_index <- createDataPartition(TakeOff$delayYN, times = 1, p = 0.3, list = FALSE)
TakeOff_test_set <- TakeOff[TakeOff_index, ]
TakeOff_train_set <- TakeOff[-TakeOff_index, ]

temp <- TakeOff[TakeOff_index, ]
# Make sure Ariline in test set are also in training set
TakeOff_test_set <- temp %>% 
  semi_join(TakeOff_train_set, by = "airline_name") %>%
  semi_join(TakeOff_train_set, by = "airplane_type") %>%
  semi_join(TakeOff_train_set, by = "month") %>%
  semi_join(TakeOff_train_set, by = "hour")

# Add rows removed from test set back into training set
removed <- anti_join(temp, TakeOff_train_set)
TakeOff_train_set <- rbind(TakeOff_train_set, removed)

rm(TakeOff_index, temp, removed)


glm_TO_A <- train(delayYN ~ airline_name, method = "glm", data = TakeOff_train_set)
delay_hat_glm_TO_A <- predict(glm_TO_A, TakeOff_test_set)
results_TO <- data_frame(Method = "GLM",
                      Predictors = "Airline",
                      Accuracy = confusionMatrix(delay_hat_glm_TO_A,TakeOff_test_set$delayYN)$overall["Accuracy"],
                      Sensitivity = confusionMatrix(delay_hat_glm_TO_A,TakeOff_test_set$delayYN)$byClass["Sensitivity"],
                      Specifictiy = confusionMatrix(delay_hat_glm_TO_A,TakeOff_test_set$delayYN)$byClass["Specificity"])

glm_TO_AHD <- train(delayYN ~ airline_name+hour+distance_km, method = "glm", data = TakeOff_train_set)
delay_hat_glm_TO_AHD <- predict(glm_TO_AHD, TakeOff_test_set)
results_TO <- bind_rows(results_TO,
                          data_frame(Method = "GLM",
                                     Predictors = "Airline+Hour+Distance",
                                     Accuracy = confusionMatrix(delay_hat_glm_TO_AHD,TakeOff_test_set$delayYN)$overall["Accuracy"],
                                     Sensitivity = confusionMatrix(delay_hat_glm_TO_AHD,TakeOff_test_set$delayYN)$byClass["Sensitivity"],
                                     Specifictiy = confusionMatrix(delay_hat_glm_TO_AHD,TakeOff_test_set$delayYN)$byClass["Specificity"]))

glm_TO_Full <- train(delayYN ~ airline_name+airplane_type+distance_km+windspeed_avg_h+precip+temp_avg+airpres+hour+month, method = "glm", data = TakeOff_train_set)
delay_hat_glm_TO_Full <- predict(glm_TO_Full, TakeOff_test_set)
results_TO <- bind_rows(results_TO,
                     data_frame(Method = "GLM",
                                Predictors = "All",
                                Accuracy = confusionMatrix(delay_hat_glm_TO_Full,TakeOff_test_set$delayYN)$overall["Accuracy"],
                                Sensitivity = confusionMatrix(delay_hat_glm_TO_Full,TakeOff_test_set$delayYN)$byClass["Sensitivity"],
                                Specifictiy = confusionMatrix(delay_hat_glm_TO_Full,TakeOff_test_set$delayYN)$byClass["Specificity"]))


randfor_TO_A <- randomForest(delayYN ~ airline_name, data = TakeOff_train_set)
delay_hat_randfor_TO_A <- predict(randfor_TO_A, TakeOff_test_set)
results_TO <- bind_rows(results_TO,
                     data_frame(Method = "Random Forest",
                                Predictors = "Airline",
                                Accuracy = confusionMatrix(delay_hat_randfor_TO_A, TakeOff_test_set$delayYN)$overall["Accuracy"],
                                Sensitivity = confusionMatrix(delay_hat_randfor_TO_A, TakeOff_test_set$delayYN)$byClass["Sensitivity"],
                                Specifictiy = confusionMatrix(delay_hat_randfor_TO_A, TakeOff_test_set$delayYN)$byClass["Specificity"]))

randfor_TO_AHD <- randomForest(delayYN ~ airline_name+hour+distance_km, data = TakeOff_train_set)
delay_hat_randfor_TO_AHD <- predict(randfor_TO_AHD, TakeOff_test_set)
results_TO <- bind_rows(results_TO,
                     data_frame(Method = "Random Forest",
                                Predictors = "Airline+Hour+Distance",
                                Accuracy = confusionMatrix(delay_hat_randfor_TO_AHD, TakeOff_test_set$delayYN)$overall["Accuracy"],
                                Sensitivity = confusionMatrix(delay_hat_randfor_TO_AHD, TakeOff_test_set$delayYN)$byClass["Sensitivity"],
                                Specifictiy = confusionMatrix(delay_hat_randfor_TO_AHD, TakeOff_test_set$delayYN)$byClass["Specificity"]))


randfor_TO_Full <- randomForest(delayYN ~ airline_name+airplane_type+distance_km+windspeed_avg_h+precip+temp_avg+airpres+hour+month, data = TakeOff_train_set)
delay_hat_randfor_TO_Full <- predict(randfor_TO_Full, TakeOff_test_set)
results_TO <- bind_rows(results_TO,
                     data_frame(Method = "Random Forest",
                                Predictors = "All",
                                Accuracy = confusionMatrix(delay_hat_randfor_TO_Full, TakeOff_test_set$delayYN)$overall["Accuracy"],
                                Sensitivity = confusionMatrix(delay_hat_randfor_TO_Full, TakeOff_test_set$delayYN)$byClass["Sensitivity"],
                                Specifictiy = confusionMatrix(delay_hat_randfor_TO_Full, TakeOff_test_set$delayYN)$byClass["Specificity"]))

results_TO
confusionMatrix(delay_hat_randfor_TO_Full, TakeOff_test_set$delayYN)

## Landing part ##

rm(Landing_train_set,Landing_test_set,glm_LA_A,glm_LA_AHD,glm_LA_Full,
   randfor_LA_A,randfor_LA_AHD,randfor_LA_Full)

# Split Landing dataset for training and testing
set.seed(2020)
Landing_index <- createDataPartition(Landing$delayYN, times = 1, p = 0.3, list = FALSE)
Landing_train_set <- Landing[-Landing_index, ]

temp <- Landing[Landing_index, ]
# Make sure Ariline in test set are also in training set
Landing_test_set <- temp %>% 
  semi_join(Landing_train_set, by = "airline_name") %>%
  semi_join(Landing_train_set, by = "airplane_type") %>%
  semi_join(Landing_train_set, by = "month") %>%
  semi_join(Landing_train_set, by = "hour")

# Add rows removed from test set back into training set
removed <- anti_join(temp, Landing_train_set)
Landing_train_set <- rbind(Landing_train_set, removed)

rm(Landing_index, temp, removed)


glm_LA_A <- train(delayYN ~ airline_name, method = "glm", data = Landing_train_set)
delay_hat_glm_LA_A <- predict(glm_LA_A, Landing_test_set)
results_LA <- data_frame(Method = "GLM",
                         Predictors = "Airline",
                         Accuracy = confusionMatrix(delay_hat_glm_LA_A,Landing_test_set$delayYN)$overall["Accuracy"],
                         Sensitivity = confusionMatrix(delay_hat_glm_LA_A,Landing_test_set$delayYN)$byClass["Sensitivity"],
                         Specifictiy = confusionMatrix(delay_hat_glm_LA_A,Landing_test_set$delayYN)$byClass["Specificity"])

glm_LA_AHD <- train(delayYN ~ airline_name+hour+distance_km, method = "glm", data = Landing_train_set)
delay_hat_glm_LA_AHD <- predict(glm_LA_AHD, Landing_test_set)
results_LA <- bind_rows(results_LA,
                        data_frame(Method = "GLM",
                                   Predictors = "Airline+Hour+Distance",
                                   Accuracy = confusionMatrix(delay_hat_glm_LA_AHD,Landing_test_set$delayYN)$overall["Accuracy"],
                                   Sensitivity = confusionMatrix(delay_hat_glm_LA_AHD,Landing_test_set$delayYN)$byClass["Sensitivity"],
                                   Specifictiy = confusionMatrix(delay_hat_glm_LA_AHD,Landing_test_set$delayYN)$byClass["Specificity"]))

glm_LA_Full <- train(delayYN ~ airline_name+airplane_type+distance_km+windspeed_avg_h+precip+temp_avg+airpres+hour+month, method = "glm", data = Landing_train_set)
delay_hat_glm_LA_Full <- predict(glm_LA_Full, Landing_test_set)
results_LA <- bind_rows(results_LA,
                        data_frame(Method = "GLM",
                                   Predictors = "All",
                                   Accuracy = confusionMatrix(delay_hat_glm_LA_Full,Landing_test_set$delayYN)$overall["Accuracy"],
                                   Sensitivity = confusionMatrix(delay_hat_glm_LA_Full,Landing_test_set$delayYN)$byClass["Sensitivity"],
                                   Specifictiy = confusionMatrix(delay_hat_glm_LA_Full,Landing_test_set$delayYN)$byClass["Specificity"]))


randfor_LA_A <- randomForest(delayYN ~ airline_name, data = Landing_train_set)
delay_hat_randfor_LA_A <- predict(randfor_LA_A, Landing_test_set)
results_LA <- bind_rows(results_LA,
                        data_frame(Method = "Random Forest",
                                   Predictors = "Airline",
                                   Accuracy = confusionMatrix(delay_hat_randfor_LA_A, Landing_test_set$delayYN)$overall["Accuracy"],
                                   Sensitivity = confusionMatrix(delay_hat_randfor_LA_A, Landing_test_set$delayYN)$byClass["Sensitivity"],
                                   Specifictiy = confusionMatrix(delay_hat_randfor_LA_A, Landing_test_set$delayYN)$byClass["Specificity"]))

randfor_LA_AHD <- randomForest(delayYN ~ airline_name+hour+distance_km, data = Landing_train_set)
delay_hat_randfor_LA_AHD <- predict(randfor_LA_AHD, Landing_test_set)
results_LA <- bind_rows(results_LA,
                        data_frame(Method = "Random Forest",
                                   Predictors = "Airline+Hour+Distance",
                                   Accuracy = confusionMatrix(delay_hat_randfor_LA_AHD, Landing_test_set$delayYN)$overall["Accuracy"],
                                   Sensitivity = confusionMatrix(delay_hat_randfor_LA_AHD, Landing_test_set$delayYN)$byClass["Sensitivity"],
                                   Specifictiy = confusionMatrix(delay_hat_randfor_LA_AHD, Landing_test_set$delayYN)$byClass["Specificity"]))


randfor_LA_Full <- randomForest(delayYN ~ airline_name+airplane_type+distance_km+windspeed_avg_h+precip+temp_avg+airpres+hour+month, data = Landing_train_set)
delay_hat_randfor_LA_Full <- predict(randfor_LA_Full, Landing_test_set)
results_LA <- bind_rows(results_LA,
                        data_frame(Method = "Random Forest",
                                   Predictors = "All",
                                   Accuracy = confusionMatrix(delay_hat_randfor_LA_Full, Landing_test_set$delayYN)$overall["Accuracy"],
                                   Sensitivity = confusionMatrix(delay_hat_randfor_LA_Full, Landing_test_set$delayYN)$byClass["Sensitivity"],
                                   Specifictiy = confusionMatrix(delay_hat_randfor_LA_Full, Landing_test_set$delayYN)$byClass["Specificity"]))

results_LA
confusionMatrix(delay_hat_randfor_LA_Full, Landing_test_set$delayYN)
