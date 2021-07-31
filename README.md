# AnalyticPortfolio

# **Bellabeat Marketing Stragey/Analysis**

## Project Introduction

  This project contains one of two case studies of Google’s Data Analytics Professional Certification provided by Google. The requirements for the case study are for the analyst to do data analysis using FitBit Fitness Tracker data to provide high-level marketing strategy recommendations for Bellabeat through the process of Ask, Prepare, Process, Analyst, Share, and Act process.
  
  Bellabeat is a high-tech company that manufactures health-focused innovative products like an app, Leaf (bracelet), Time (watch), and Spring (water bottle). The company also provides subscription services or membership programs for users to have 24/7 access to their fully personalized guidance on nutrition, activity, sleep, health and beauty, and mindfulness-based on their lifestyle and goals. 
  
### *Objective*

* What are some trends in smart device usage?
* How could these trends apply to Bellabeat customers?
* How could these trends help influence Baellabeat marketing strategy?

Import library needed: 

```{r Import library needed, message=FALSE}
#install.packages("tidyverse")
#install.packages("ids")
#install.packages("gridExtra")
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(ids)
library(gridExtra)
```

Importing/Read .csv datasets into dataframes:

```{r Importing/Read .csv datasets into dataframes, warning=FALSE}
daily_activity = read.csv("~/Case_Study_bellabeat/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
heart_rate_seconds = read.csv("~/Case_Study_bellabeat/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
sleep_day = read.csv("~/Case_Study_bellabeat/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight_log = read.csv("~/Case_Study_bellabeat/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
fitness_trend = read.csv("~/Case_Study_bellabeat/25.csv")
activity_log = read.csv("~/Case_Study_bellabeat/ACTIVITY/ACTIVITY_1599810432505.csv")
sleep_log = read.csv("~/Case_Study_bellabeat/SLEEP/SLEEP_1599810433552.csv")
heartrate_auto_log = read.csv("~/Case_Study_bellabeat/HEARTRATE_AUTO/HEARTRATE_AUTO_1599810433761.csv")
```

## Prepare and Process 

### **Heart Rate Dataset**

```{r process/checking heartrate dataset, warning=FALSE}
#checking data quality of the original
head(heart_rate_seconds)
skim_without_charts(heart_rate_seconds)
anyNA(heart_rate_seconds)
anyDuplicated(heart_rate_seconds)

#cleaning/confirming data for quality to be place in a dataset that will be used for analysis
heart_rate_sec = heart_rate_seconds %>%
  clean_names() %>%
  distinct() %>%
  mutate(time = parse_date_time(time, "%m/%d/%Y %I:%M:%S %p")) %>%
  separate(col = time, 
           into = c("date", "time"), sep = " ") %>%
  mutate(date = as.Date(date), 
         time = format(strptime(time, "%H:%M:%S"), "%H")) %>%
  rename(heart_rate = value) %>%
  group_by(id, date, time) %>%
  summarize(heart_rate = mean(heart_rate)) %>%
  arrange(id, date, time)

#final quality check
head(heart_rate_sec)
anyNA(heart_rate_sec)
anyDuplicated(heart_rate_sec)
```



```{r warning=FALSE}
#checking the second heart rate data for quality of the original
head(heartrate_auto_log)
skim_without_charts(heartrate_auto_log)
anyNA(heartrate_auto_log)
anyDuplicated(heartrate_auto_log)

#random id generator
set.seed(2430)
activity_id = sort(sample.int(2430,2430))

#cleaning/confirming data for quality to be place in a dataset that will be used for analysis
heartrate_auto = heartrate_auto_log %>%
  clean_names() %>%
  distinct() %>%
  rename(date = i_date) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"), 
         time = substr(time, 1, 5),
         id = activity_id) %>%
  arrange(id, date, time)

#final quality check
head(heartrate_auto)
anyNA(heartrate_auto)
anyDuplicated(heartrate_auto)
```

### **Sleep Dataset**

```{r process/checking sleep dataset, warning=FALSE}
#checking data quality of the original
head(sleep_day) 
skim_without_charts(sleep_day)
anyNA(sleep_day)
anyDuplicated(sleep_day)

#cleaning/confirming data for quality to be place in a dataset that will be used for analysis
sleep_d = sleep_day %>%
  distinct() %>%
  clean_names() %>%
  separate(col = sleep_day, 
           into = c("sleep_day", "sleep_time"), sep = " ") %>%
  mutate(sleep_day = as.Date(sleep_day,"%m/%d/%Y")) %>%
  rename(date = sleep_day) %>% 
  mutate(time_awake = total_time_in_bed - total_minutes_asleep) %>% 
  arrange(id,date) %>%
  select(-total_sleep_records, -sleep_time)

#final quality check
head(sleep_d)
anyNA(sleep_d)
anyDuplicated(sleep_d)

#checking the second sleep data for quality of the original
head(sleep_log)
skim_without_charts(sleep_log)
anyNA(sleep_log)
anyDuplicated(sleep_log)

#random id generator
set.seed(269)
sleep_id = sort(sample.int(269,269))

#cleaning/confirming data for quality to be place in a dataset that will be used for analysis
sleep_l = sleep_log %>%
  distinct() %>%
  clean_names() %>%
  rename(date = i_date, 
         time_awake = wake_time) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"), id = sleep_id,
         total_minutes_asleep = deep_sleep_time + shallow_sleep_time,
         total_time_in_bed = total_minutes_asleep + time_awake) %>%
  select(id, date, total_minutes_asleep, 
         total_time_in_bed, time_awake) %>%
  arrange(id, date)

#final quality check
head(sleep_l)
anyNA(sleep_l)
anyDuplicated(sleep_l)
```

### **Weight Log Dataset**

```{r warning=FALSE}
#checking data quality of the original
head(weight_log)
anyNA(weight_log)
anyDuplicated(weight_log)

#cleaning/confirming data for quality to be place in a dataset that will be used for analysis
weight_l = weight_log %>%
  distinct() %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%m/%d/%Y"), height = sqrt(weight_kg/bmi)*100) %>%
  select(-weight_pounds, -log_id, -is_manual_report, -fat) %>%
  arrange(id, date)

#final quality check
head(weight_l)
anyNA(weight_l)
anyDuplicated(weight_l)
```

### **Fitness Trend Dataset**

```{r warning=FALSE}
#checking data quality of the original
head(fitness_trend)
anyNA(fitness_trend)
anyDuplicated(fitness_trend)

#random id generator
set.seed(96)
fitness_id = sort(sample.int(96,96))
    
#cleaning/confirming data for quality to be place in a dataset that will be used for analysis
fitness_t = fitness_trend %>%
  distinct() %>%
  clean_names() %>%
  rename(total_minutes_asleep = hours_of_sleep, total_steps = step_count) %>%
  mutate(id = fitness_id,
         date = as.Date(date, "%Y-%m-%d"),
         total_minutes_asleep = total_minutes_asleep * 60) %>%
  select(-mood, -bool_of_active) %>%
  arrange(id, date)

fitness_sleep_trend = fitness_t %>%
  select(id, date, total_minutes_asleep)

fitness_weight_trend = fitness_t %>%
  select(id, date, weight_kg)

fitness_activity_trend = fitness_t %>%
  select(id, date, total_steps, calories_burned)

#final quality check
head(fitness_t)
head(fitness_activity_trend)
head(fitness_sleep_trend)
head(fitness_weight_trend)
anyNA(fitness_t)
anyNA(fitness_activity_trend)
anyNA(fitness_sleep_trend)
anyNA(fitness_weight_trend)
anyDuplicated(fitness_t)
anyDuplicated(fitness_activity_trend)
anyDuplicated(fitness_sleep_trend)
anyDuplicated(fitness_weight_trend)
```

### **Activities Dataset**

```{r warning=FALSE}
#checking data quality of the original
head(daily_activity)
anyNA(daily_activity)
anyDuplicated(daily_activity)

#cleaning/confirming data for quality to be place in a dataset that will be used for analysis
activity_d = daily_activity %>%
  distinct() %>%
  clean_names() %>%
  rename(date = activity_date, calories_burned = calories) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  #mutate(avg_active_min = fairly_active_minutes + lightly_active_minutes + sedentary_minutes +
                                 #very_active_minutes) %>%
  select(-tracker_distance, -logged_activities_distance, -moderately_active_distance,
         -light_active_distance, -sedentary_active_distance,
         -fairly_active_minutes, -lightly_active_minutes, -sedentary_minutes, -very_active_minutes) %>%
  arrange(id, date)

#final quality check
head(activity_d)
anyNA(activity_d)
anyDuplicated(activity_d)

#checking the second Activity data for quality of the original
head(activity_log)
anyNA(activity_log)
anyDuplicated(activity_log)

#generating random id number
set.seed(269)
activity_id = sort(sample.int(269, 269))

#cleaning/confirming data for quality to be place in a dataset that will be used for analysis
activity_l = activity_log %>%
  distinct() %>%
  clean_names() %>%
  rename(date = i_date, calories_burned = calories, very_active_distance = run_distance, 
         total_steps = steps, total_distance = distance) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"),
         total_distance = total_distance / 1000,
         very_active_distance = very_active_distance / 1000,
         id = c(activity_id)) %>%
  select(-last_sync_time) %>%
  arrange(id, date)

#final quality check
head(activity_l)
anyNA(activity_l)
anyDuplicated(activity_l)
```

## Analyze & Share

```{r warning=FALSE}
#binding all the activity dataset
activity = bind_rows(activity_d, activity_l, fitness_activity_trend)
sleep = bind_rows(sleep_d, sleep_l, fitness_sleep_trend)
heartrate = rbind(heart_rate_sec, heartrate_auto)
weight = bind_rows(fitness_weight_trend, weight_l)
```

```{r warning=FALSE}
paste("The number of unique IDs in Activity dataset =",n_unique(activity$id))
paste("The number of unique IDs in Sleep dataset =", n_unique(sleep$id))
paste("The number of unique IDs in Heartrate dataset =", n_unique(heartrate$id))
paste("The number of unique IDs in Weight dataset =", n_unique(weight$id))
```
### **Visualization**

```{r distance vs steps with calories burned, warning=FALSE}
ggplot(activity, aes(total_steps, total_distance))+
  geom_jitter() +
  geom_point(aes(color = calories_burned)) +
  scale_color_viridis_b(name = "Calories Burned") + 
  stat_smooth(method = lm) +
  labs(title = "Calories Burned by Total Number of Steps and Total Distance",
       subtitle = "Done by Users", x = "Total Steps", y = " Total Distance")

ggsave("calories_burned_by_total_steps.png")
```
**Finding:** Strong corrilation of total steps taken with the total distance taken in terms of data relation. Along with calories burned the more steps and distance taken. 

```{r total distance to calories burned, warning=FALSE}
ggplot(data = activity, aes(x = total_distance, y = calories_burned)) + 
  geom_point(aes(color = calories_burned)) +
  scale_color_viridis_b(name = "Calories Burned") +
  labs(title = "Calories Burned by Total Distance", subtitle = "Done by Users", 
       x = "Total Distance", y = "Calories Burned")

ggsave("calories_burned_by_total_distance.png")

```
**Finding:** 
Strong indication of two different user base.
1. This user base are power user who burn calories as intended by the amount of distance taken.
2. This user base take the most minimal distance to burn calories.
However, these two segments is quite disproportionate. 

```{r warning=FALSE}
heartrate %>%
  summarise(avg_heart_rate = mean(heart_rate)) %>%
  merge(activity, all = TRUE) %>%
  drop_na() %>%
  ggplot(aes(x = total_steps, y = avg_heart_rate)) +
  geom_point(aes(color = calories_burned)) +
  geom_hline(aes(yintercept = 90), color = "red") +
  scale_color_viridis_b(name = "Calories Burned") +
  labs(title = "Average Heart Rate by Total Number of Steps and Calories Burned", 
       subtitle = "Done by Users", x = "Total Steps", y = "Avg Heart Rate")

ggsave("avg_hear_rate_by_total_steps.png")
```
**Findings:** 
On average, most users' heart rate are not intensly high (below 90) to burn large amount of calories with a lot of amount of steps. 

```{r warning=FALSE}
heartrate %>%
  #summarise(avg_heart_rate = mean(heart_rate)) %>%   
  # The above line could be added to look at the average heart rate
  merge(sleep, all = TRUE) %>%
  drop_na() %>%
  ggplot(aes(x = total_minutes_asleep, y = heart_rate)) +
  geom_point(aes(color = heart_rate)) +
  geom_hline(aes(yintercept = 90), color = "red") +
  scale_color_viridis_b(name = "Heart Rate") +
  labs(title = "Heart Rate of Users by the amout of sleep", 
       x = "Total Amount of Sleep (in Minutes)", y = "Heart Rate")

ggsave("heart_rate_by_sleep_amount.png")
```
**Finding:**
Majority of people who tracks their sleep data generally have low heart rate as expected, but have some outlier that have heart rate unusually high which could be false reading when a tracker is not equipped properly. 
```{r warning=FALSE}
ggplot(data = sleep, aes(x = total_minutes_asleep, y = time_awake)) +
  geom_point(aes(color = time_awake)) +
  stat_smooth(method = lm, size = 1) +
  scale_color_viridis_b(name = "Time Awake") +
  labs(title = "Calories Burned during sleep", 
       x = "Total Amount of Sleep (in Minutes)", y = " Time Awake")

ggsave("calories_burned_during_sleep.png")
```
**Finding:**
Good indication that people generally stay asleep and do not wake up very much. 

```{r warning=FALSE}
heartrate %>%
  merge(activity, all = TRUE) %>%
  drop_na() %>%
  ggplot(aes(x = time, y = heart_rate)) +
  geom_col(fill = "darkblue") +
  labs(title = "Heart Rate throughout the day", 
       x = "Time", y = "Heart Rate")

ggsave("heart_rate_in_a_day.png")
```
**Finding:**
People are generally active throughout the day, but one discrepancy the graph inputted a time of 15:39.

```{r warning=FALSE}
heartrate %>%
  #summarise(avg_heart_rate = mean(heart_rate)) %>%
  merge(activity, all = TRUE) %>%
  drop_na() %>%
  ggplot(aes(x = heart_rate, y = calories_burned)) +
  geom_point() +
  geom_smooth(size = 2) +
  geom_vline(aes(xintercept = 90), color = "red", size = 2, linetype = "dashed") +
  labs(title = "Calories Burned vs Heart Rate",
       x = "Heart Rate", y = "Calories Burned") +
  theme(legend.position = "none")

ggsave("calories_burned_vs_heart_rate.png")
```
**Finding:**
There is indication that shows that more calories are burned with increase heart rate. However, this also shows that most users have a heart rate of less than 100 that burns between 2000 to 3000 calories. So it seems like users have moderate activities throught out the day as previously shown as well. 

## Act

### **Limitation**

As previously mentioned: 

* *Reliability:* **Low** - The datasets collected consisted of only 30 individuals who are anonymous with only the assumption that the majority of data collected are of the female gender. 

* *Originality:* **Low** - The datasets were generated by respondents to a distributed survey via Amazon Mechanical Turk. 

* *Comprehensive:* **High** - The datasets contain daily, hourly, and minutes of calories burned, activity intensity, number of steps, sleep duration, and weight information. 

* *Current:* **Medium** - The datasets are 5 years old, but significant changes in a person’s life may vary depending on a person’s life events, habits, or routines. Data are recorded from 2016, March 12th to 2016, May 13th (3 months period).

* *Cited:* **Medium** - The data collection and source were well documented. 

* The data collected do not indicate the user’s age in order to indicate what is the appropriate heart rate for the individual as well. 

### **Recommendation**

* Bellabeat can include functions in the Bellabeat app to alert users of their in-activity or unusual readings as timely notifications. Even a notification to indicate users to stretch their legs a little for in-activity to maintain wellness goals. 

* Offer more or improved customization for users that regularly ask for the user’s age, weight, height if the user chooses to input them to recommend users personalized tips to help the user achieve their wellness goals. 

* Do a more targeted marketing campaign toward people who are more active, and health-conscious by showing the uniqueness of Bellabeat’s products like the Spring (water bottle).
For a broader marketing campaign, advertise the connection of each Bellabeat’s products with getting enough activities every day, maintaining proper health and hydration, and wellness goals that other competitors can’t. 

* Points or rewards programs that both subscription and non-subscription-based users can earn for their activity to encourage users to continuously use Bellabeat products that they own while keeping users continue using the product to minimize forgetfulness which could potentially create brand loyalty.



