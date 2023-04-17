library(readr)
library(tidyverse)

investigations_data = read_csv("investigations.csv")
crimes_data = read_csv("crimes.csv")
crashroadway = read_csv("crash_info_roadway.csv")
crash_general = read_csv("crash_info_general.csv")
flag_variables = read_csv("crash_info_flag_variables.csv")
crash_info_people = read.csv("Crashes/crash_info_people.csv")

colnames(investigations_data)

# crimes_data_2022_09_08 <- crimes_data %>% filter(dispatch_date == "2022-09-08" | dispatch_date == "2022-09-09")
# crimes_data_2022_09_08

crash_general %>% filter(ILLUMINATION == 1) %>% pull(PERSON_COUNT) %>% mean()
crash_general %>% filter(ILLUMINATION == 1) %>% pull(FATAL_COUNT) %>% mean()

crash_general %>% filter(ILLUMINATION != 1) %>% pull(PERSON_COUNT) %>% mean()
crash_general %>% filter(ILLUMINATION != 1) %>% pull(FATAL_COUNT) %>% mean()

crash_general %>% filter(ROAD_CONDITION == 1) %>% pull(PERSON_COUNT) %>% mean()
crash_general %>% filter(ROAD_CONDITION == 1) %>% pull(FATAL_COUNT) %>% mean()

crash_general %>% filter(ROAD_CONDITION != 1) %>% pull(PERSON_COUNT) %>% mean()
crash_general %>% filter(ROAD_CONDITION != 1) %>% pull(FATAL_COUNT) %>% mean()

investigations_data %>% drop_na() %>% ggplot(aes(y=lat)) + geom_histogram()

library(xgboost)
library(caret)

colnames(crash_general)

data = as.data.frame(crash_general)
ohe_feats = c('COLLISION_TYPE', 'DISTRICT', 'HOUR_OF_DAY', 'ILLUMINATION', 'INTERSECT_TYPE', 'LOCATION_TYPE','ROAD_CONDITION', 'TCD_FUNC_CD', "TCD_TYPE", "WEATHER1", "WEATHER2")
dummies = dummyVars(~ COLLISION_TYPE + DISTRICT + HOUR_OF_DAY + ILLUMINATION + INTERSECT_TYPE + LOCATION_TYPE + ROAD_CONDITION + TCD_FUNC_CD + TCD_TYPE + WEATHER1 + WEATHER2, data = crash_general)
df_all_ohe <- as.data.frame(predict(dummies, newdata = crash_general))
df_all_combined <- cbind(data[,-c(which(colnames(data) %in% ohe_feats))],df_all_ohe)

###

crashroadway = read_csv("crash_info_roadway.csv")
crash_general = read_csv("crash_info_general.csv")


# omitting null values
crashroadwayspeed = crashroadway[complete.cases(crashroadway$SPEED_LIMIT), ]

# plotting kde for car crashes across different types of roadways
library(ks) #library for kernel density estimation 
plot(kde(crashroadwayspeed$ROAD_OWNER, h = 0.2), main = "Roadway maintained by state, local or private jurisdiction", xlab = "Crashes")

# plotting piechart for % of crashes which took place at an Unsignalized Intersection
library(scales)
pie(c(sum(flag_variables$UNSIGNALIZED_INT == 0), sum(flag_variables$UNSIGNALIZED_INT == 1))
    , labels = paste0(c("No","Yes"), "\n", percent(c(sum(flag_variables$UNSIGNALIZED_INT == 0), sum(flag_variables$UNSIGNALIZED_INT == 1))/sum(c(sum(flag_variables$UNSIGNALIZED_INT == 0), sum(flag_variables$UNSIGNALIZED_INT == 1)))))
    , col = c("red", "green"), 
    main = "The crash took place at an Unsignalized Intersection")

###

hr <- crash_general %>% filter (INTERSECT_TYPE< 14)
# 99 is unknown, so we cant focus on that
hist(hr$INTERSECT_TYPE, main="Intersect type for crashes",xlab="Intersection Type",ylab="No. of Crashes", col=c("darkorange"),labels=TRUE, ylim = c(0,120000), breaks=13)
plot(density(hr$INTERSECT_TYPE))
percent_value_00 <- mean(hr$INTERSECT_TYPE == "0") * 100
percent_value_00
# Create a pie chart of the frequency of each collision type
pie_data <- flag_variables %>%
  group_by(INTERSECTION) %>%
  summarise(count = n()) %>%
  mutate(label = ifelse(INTERSECTION == 1, "Yes", "No"),
         percentage = count / sum(count) * 100)
ggplot(pie_data, aes(x = "", y = count, fill = label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Yes" = "steelblue", "No" = "darkorange")) +
  labs(title = "Pie Chart of Accidents at Intersection",
       x = NULL, y = NULL,
       fill = "Intersection") +
  geom_text(aes(y = cumsum(count) - count/2, label = paste0(round(percentage, 1), "%")), size = 4) +
  theme_void()
