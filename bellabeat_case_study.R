# About the Company- 
##Bellabeat, a hightech company that manufactures health-focused smart products. since founded in 2013, Bellabeat
##has grown rapidly and quickly positioned itself as a tech-driven wellness company for women. By 2016, Bellabeat had
## opened offices around the world and launched multiple products. Srsen, president, knows that an analysis of 
##Bellabeats available consumer data would reveal more opportunities for growth and needs our help to find patterns
## to make recommendations and inform the marketing team.


# ASK
## Questions we will be tasked with during our analysis. 1)What are some trends in smart device usage?
## 2) How could these trends apply to Bellabeat customers?
## 3) How could these trends help influence Bellabeat marketing strategy?

# Business Task
### Identify potential opportunities for business growth based on customer interaction with products. Giving 
###recommendations of where to focus our efforts in reaching new customers and challenging current customers. 

# Loading packages needed for Analysis
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(hms)

# Downloaded all google sheets and converted them into CSV files. Now ready to download, assinging new data
###frame names.

activity <- read_csv("C:/Users/jmcla/OneDrive/Documents/DATA Analytics/Portfolio_CaseStudy/Case_study_samples/Fit Bit case study2/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv", col_types=cols(.default="c"))

calories <- read_csv("C:/Users/jmcla/OneDrive/Documents/DATA Analytics/Portfolio_CaseStudy/Case_study_samples/Fit Bit case study2/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv", col_types=cols(.default="c"))

sleep <- read_csv("C:/Users/jmcla/OneDrive/Documents/DATA Analytics/Portfolio_CaseStudy/Case_study_samples/Fit Bit case study2/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv",col_types=cols(.default="c"))

weight <- read_csv("C:/Users/jmcla/OneDrive/Documents/DATA Analytics/Portfolio_CaseStudy/Case_study_samples/Fit Bit case study2/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv", col_types=cols(.default="c"))

intensities <- read_csv ("C:/Users/jmcla/OneDrive/Documents/DATA Analytics/Portfolio_CaseStudy/Case_study_samples/Fit Bit case study2/Fitabase Data 3.12.16-4.11.16/hourlyIntensities_merged.csv", col_types=cols(.default="c"))

steps <- read_csv ("C:/Users/jmcla/OneDrive/Documents/DATA Analytics/Portfolio_CaseStudy/Case_study_samples/Fit Bit case study2/Fitabase Data 3.12.16-4.11.16/hourlySteps_merged.csv", col_types=cols(.default="c"))

heartRate <- read_csv ("C:/Users/jmcla/OneDrive/Documents/DATA Analytics/Portfolio_CaseStudy/Case_study_samples/Fit Bit case study2/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv", col_types=cols(.default="c"))

# checking the column and table layout to verify our data and format.
head(activity)
colnames(activity)

head(sleep)
colnames(sleep)
# the ActivityHour columns in multiple sheets share date & Time together in the same
# column. we will need to separate using the lubridate() within Tidyverse package.
# First, will run a class function to recognize that each of our data frames are recognized and not considered
# vector's, which would produce errors in our lubridate ().

class(activity)
class(sleep)
class(intensities)
class(calories)
class(weight)

# now that class verified our data frames, we can use lubridate to separate our columns
# we need the library(hms)function to properly separate our columns as characters and recognized as DateTime
# pay close attention, as some sheets have different column titles listed.


#following code will separate columns and format data correctly. a pipe makes this easy.
# converts string to datetime (04/12/2016 09:14:00), extracts date, Extracts just the time
#Error message on failed to parse Calories data. to fix, we changed our import data code to "read_csv" and added " col_types=cols(.default="c"))
#adding this will convert all columns to a character, then we can properly run our formula to split our data columns.
#run a Head code to verify format of calories->ActivityHour column to give us proper MDY reading.

head(calories$ActivityHour,5)

calories <- calories %>%
  mutate(ActivityHour = parse_date_time(ActivityHour, orders="mdy IMS p"),
          date=date(ActivityHour),
          time=as_hms(ActivityHour),
          ActivityDate=as.Date(ActivityHour))


str(activity$ActiveyHour)
str(calories$ActivityHour)

# Now are date frame should have 3 separate columns.
print(calories)
#Repeat for the following sheets that need conversion for both date/time and making values numeric to allow our plot to be readable.

activity<- activity %>%
  mutate(SedentaryMinutes=as.numeric(SedentaryMinutes),
         TotalSteps=as.numeric(TotalSteps),
         Calories=as.numeric(Calories), 
         LightlyActiveMinutes= as.numeric(LightlyActiveMinutes))

sleep <- sleep %>%
  mutate(ActivityTime=mdy_hms(date), date=date(ActivityTime), time=as_hms(ActivityTime))
View(sleep)

intensities<-intensities%>%
  mutate(ActivityHour = mdy_hms(ActivityHour),
         date=date(ActivityHour),
         time=as_hms(ActivityHour))
View(intensities)

weight<-weight %>%
  mutate(ActivityTime=mdy_hms(Date)
         ) %>% #create datetime column to have R recognize ActivityTime before using it.
  mutate(date=date(ActivityTime), time=as_hms(ActivityTime))

steps<-steps %>%
  mutate(ActivityHour=mdy_hms(ActivityHour), date=date(ActivityHour), time=as_hms(ActivityHour))
heartRate<-heartRate %>%
  mutate(Time=mdy_hms(Time), date=date(Time), time=as_hms(Time))

# How many unique participants are there in each dataframe
n_distinct(activity$Id)
n_distinct(steps$Id)
n_distinct(sleep$Id)
n_distinct(calories$Id)
n_distinct(weight$Id)
n_distinct(intensities$Id)
n_distinct(heartRate$Id)
# Results indicate that 11ppl participated in weights data frame and 14ppl in the heartRate, which could result in survey bias.
#How many observations are in each data frame.
nrow(activity)
nrow(sleep)
nrow(weight)
nrow(steps)
nrow(calories)
nrow(intensities)
nrow(heartRate)
#Again weight data frame has a significant difference in entries that will conclude this data to be irrelevant. The Activity dataframe
#also has low amount of entries compared to the others but lets keep it in mind for our analysis as it does have 35 participants.
# lets run a summary table of our data frames
activity%>%
  select(TotalSteps,TotalDistance,Calories,SedentaryMinutes)%>%
  summary()
activity%>%
  select(VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes) %>%
  summary()
intensities%>%
  select(ActivityHour, TotalIntensity, AverageIntensity)%>%
  summary()
calories%>%
  select(ActivityHour,Calories)%>%
  summary()
steps%>%
  select(ActivityHour, StepTotal)%>%
  summary()
heartRate%>%
  select(Time,Value)%>%
  summary()
activity %>%
  summarize(avg_calories = mean(Calories, na.rm=TRUE),
            avg_steps = mean(TotalSteps, na.rm=TRUE))
# With our summary results we can conclude
## LightAcivity was highly popular with a average of 170 minutes
## Most fit bit users who were active burned calories averaging 2189 with 6547 steps. 
## Average workouts were less than 20minutes and most preferred early morning workouts between 5-6am.
## Data also shows a high amount of SedentaryMinutes with avg. 995 which means most participants were active only 32% of their day.
## 24hrs a day x 60minutes in an hour = 1440 minutes. 991min/1440Min = .68. 1-.68=.32
## Although participants were less than 15ppl in the heartRate dataframe, we can conclude that there was an average increase in heart rate when active 79.76


activity_new2<- activity%>%
  group_by(Calories)%>%
  drop_na()%>%
  summarize(mean_total_Activity=mean(LightlyActiveMinutes))

ggplot(data=activity, aes(x= Calories, y= LightlyActiveMinutes))+
  geom_point(color='darkorange')+ geom_smooth()+
  labs(title='LightlyActiveMinutes Vs. Calories')

# Nothing surprising here as the plots tell us the more active we are produces more steps and calories burned
# Next, we will use joins to get a more accurate picture. wanting to show activity increases health.

# We will reformat both the activity and calorie dataframe for consistency. 
activity<-activity%>%
  mutate(ActivityDate=as.Date(ActivityDate, format= "%m/%d/%Y"))%>%
  mutate(Id=as.character(Id))

# Need to convert the 'ActivityHour' column (date and time) into a proper Date object, and assign
# it to the new column name 'ActivityDate'. First coerce column to character, then POSIXct (date/time),
# and finally truncate the standard Date class. 
calories<-calories %>%
 mutate(ActivityDate=as.Date(as.POSIXct(as.character(ActivityHour), format = "%m/%d/%Y %I:%M:%S %p"))) %>%
  mutate(Id=as.character(Id))

activity_calories<- activity %>%
    mutate(ActivityDate= as.Date(ActivityDate, format= "%m/%d/%Y")) # fixes datetime mismatch of column names
  activity_calories <- activity %>%
    left_join(calories, by= c("Id", "ActivityDate")) #performs a clean join

  
ggplot(activity, aes(x=Calories, y=LightlyActiveMinutes)) +
  geom_point(color='darkorange', alpha=.6)+
  geom_smooth(method="lm", color="steelblue", se = FALSE)+
  labs( title= "Relationship Between LightlyActiveMinutes and Calories Burned",
        x="Calories", Y="Lightly Active Minutes")+
  theme_minimal()
# this looks great. Now we will separate by each activity type.

ggplot(activity, aes(x= VeryActiveMinutes, y=Calories))+
  geom_point(color="firebrick")+
  geom_smooth(method= "lm", se= FALSE)+
  labs(title= "Very Active Minutes vs Calories Burned")

ggplot(activity, aes(x=FairlyActiveMinutes, y=Calories))+
  geom_point(color="purple")+
  geom_smooth(method="lm", se=FALSE)+
  labs(title="Fairly Active Minutes vs. Calories Burned")

ggplot(activity_calories, aes(x=LightlyActiveMinutes, y=Calories.x))+
         geom_point(color="darkgreen")+
         geom_smooth(method= "lm", se= FALSE)+
         labs(title="Lightly Active Minutes vs. Calories Burned")
 # Finally, lets compare each activity type side-by-side. we will use the Pivot Long function to support this data frame.

activity_calories<- activity_calories %>%
  mutate(LightlyActiveMinutes= as.numeric(LightlyActiveMinutes),
         FairlyActiveMinutes = as.numeric(FairlyActiveMinutes),
         VeryActiveMinutes = as.numeric(VeryActiveMinutes))
      

activity_long<- activity_calories%>%
  pivot_longer(cols=c(LightlyActiveMinutes, FairlyActiveMinutes,VeryActiveMinutes),
               names_to= "ActivityType",
               values_to="Minutes")
ggplot(activity_long, aes(x= Minutes, y=Calories.x))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", se= FALSE, color="limegreen")+
  facet_wrap(~ActivityType)+
  labs(title="Calories Burned vs. Different Types of Activity")

glimpse(activity_calories)

#Lets track to see where most activity Levels are according to the days of the week

activity <- read_csv("C:/Users/jmcla/OneDrive/Documents/DATA Analytics/Portfolio_CaseStudy/Case_study_samples/Fit Bit case study2/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv", col_types=cols(.default="c"))

cleaned_dates <- gsub(pattern = "[^0-9/]", replacement = "", x = activity$ActivityDate)
activity$ActivityDate <- mdy(cleaned_dates)

print(paste("TotalSteps class before conversion:", class(activity$TotalSteps)))
activity$TotalSteps <- as.numeric(activity$TotalSteps)
print(paste("TotalSteps class after conversion:", class(activity$TotalSteps)))

activity$weekday <- wday(activity$ActivityDate, label = TRUE)

print(paste("Number of unique weekdays found:", length(unique(activity$weekday))))

ggplot(activity, aes(x=weekday, y=TotalSteps)) +
  geom_boxplot(fill="yellow") +
  labs(title="Steps by Day of the Week")


# Lets List the top 5 users from our analysis and show total steps and calories.
activity %>% mutate(Calories=as.numeric(Calories))%>%
  group_by(Id) %>%
  summarize(TotalSteps = sum(TotalSteps, na.rm=TRUE),
            TotalCalories = sum(Calories, na.rm=TRUE)) %>%
  arrange(desc(TotalSteps)) %>%
  head(5)




# Conclusions
###The following data tells us that women who are active are more healthy than those who are Sedentary. We also see a increase in calories burned
###in conjunction with activity. Bellabeat has incurred a fast growth rate and has appealed to several women across the globe.
###the following are insights on what we would recommend moving forward into next year
###Bella users are more active on Saturday and Wednesday with Saturday showing the highest amount of activity. We also see most participants were involved in 
###LightActivity, this could be due to time constraints or personal schedules. Would be beneficial to raise awarness on app of lightactvity related compettitions
###during the week and have a heavier activity challenge on Saturday which is the busiest day. Each graph on calories and actvity shows us the more active users are,
###the more calories they burn, which has been know to support good health. Launching a campagin for users to join together to win "belladollars", prizes or even
###trips; to help raise awareness and increase enrolled users under the bellabeat platform. Earlier we found that, on average, users are only active for under 35%
###of their time in a 24h period. Note we would expect this to increase a little as 8hrs (30%) of their time is reserved for sleeping- therefore there is room for
###increased activity for all bella users. There is several opportunities for both bella and its users to improve as the market and usage is strong. The fit bit is
###population and users are warning them even if they are not active, maybe have links to life coaches, workout plans, health/eating plans and competitions to help
###raise awareness and increase activity which in turn produces healtheir clients. Start thinking of quarterly competitions and maybe to an anual retreat for your 
###top performers per region as a reward for their usage which can be tracked by a point based system over a one year time frame. Eitherway, Bella has a strong 
###infulence among women and opportunity to grow even more within the active health industry. 









