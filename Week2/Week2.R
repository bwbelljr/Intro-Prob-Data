# Load packages

library(statsr)
library(dplyr)
library(ggplot2)

# We begin by loading the nycflights data frame. 
# Type the following in your console to load the data:

data(nycflights)

# To view the names of the variables, type the command

names(nycflights)

# Found out information on the data frame
?nycflights

# A very useful function for taking a quick peek at your data frame, 
# and viewing its dimensions and data types is str, which stands 
# for structure.

str(nycflights)

# We can examine the distribution of departure delays of all 
# flights with a histogram.

ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram()

## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

# Histograms are generally a very good way to see the shape of a 
# single distribution, but that shape can change depending on how 
# the data is split between the different bins. You can easily 
# define the binwidth you want to use:
  
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)

# Here is another binwidth

ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 150)

# If we want to focus on departure delays of flights headed to RDU 
# only, we need to first filter the data for flights headed to RDU 
# (dest == "RDU") and then make a histogram of only departure delays
# of only those flights.

# Line 1: Take the nycflights data frame, filter for flights headed 
# to RDU, and save the result as a new data frame called rdu_flights.
# == means “if it’s equal to”.
# RDU is in quotation marks since it is a character string.

rdu_flights <- nycflights %>%
  filter(dest == "RDU")

# Line 2: Basically the same ggplot call from earlier for making a 
# histogram, except that it uses the data frame for flights headed 
# to RDU instead of all flights.
ggplot(data = rdu_flights, aes(x = dep_delay)) +
  geom_histogram()

## `stat_bin()` using `bins = 30`. 
# Pick better value with `binwidth`.

# We can also obtain numerical summaries for these flights:

rdu_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

# Note that in the summarise function we created a list of two 
# elements. The names of these elements are user defined, like 
# mean_dd, sd_dd, n, and you could customize these names as you 
# like (just don’t use spaces in your names). Calculating these 
# summary statistics also require that you know the function calls. 
# Note that n() reports the sample size.

# Summary statistics: Some useful function calls for summary statistics for a single numerical variable are as follows:

# mean
# median
# sd
# var
# IQR
# range
# min
# max

# We can also filter based on multiple criteria. Suppose we are 
# interested in flights headed to San Francisco (SFO) in February:

sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO", month == 2)

nrow(sfo_feb_flights) # 68
str(sfo_feb_flights)
summary(sfo_feb_flights)

# Question 2
ggplot(data = sfo_feb_flights, aes(x = arr_delay)) +
  geom_histogram()

# Another useful functionality is being able to quickly calculate 
# summary statistics for various groups in your data frame. For 
# example, we can modify the above command using the group_by 
# function to get the same summary stats for each origin airport:

rdu_flights %>%
  group_by(origin) %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

# Question 3: Calculate the median and interquartile range for 
# arr_delays of flights in the sfo_feb_flights data frame, grouped 
# by carrier. Which carrier is the has the hights IQR of arrival 
# delays?

sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_dd = median(arr_delay), IQR_dd = IQR(arr_delay), n = n())

# Which month would you expect to have the highest average delay 
# departing from an NYC airport?

# Let’s think about how we would answer this question:
  
# First, calculate monthly averages for departure delays. With the 
# new language we are learning, we need to
# group_by months, then
# summarise mean departure delays.
# Then, we need to arrange these average delays in descending order

nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))

# Question 5
nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))

# We can also visualize the distributions of departure delays 
# across months using side-by-side box plots:

ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()
# There is some new syntax here: We want departure delays on the 
# y-axis and the months on the x-axis to produce side-by-side box 
# plots. Side-by-side box plots require a categorical variable on 
# the x-axis, however in the data frame month is stored as a 
# numerical variable (numbers 1 - 12). Therefore we can force R to 
# treat this variable as categorical, what R calls a factor, 
# variable with factor(month).

# On time departure rate for NYC airports

# Suppose you will be flying out of NYC and want to know which of 
# the three major NYC airports has the best on time departure rate 
# of departing flights. Suppose also that for you a flight that is 
# delayed for less than 5 minutes is basically “on time”. You 
# consider any flight delayed for 5 minutes of more to be “delayed”.

# In order to determine which airport has the best on time 
# departure rate, we need to

# first classify each flight as “on time” or “delayed”,
# then group flights by origin airport,
# then calculate on time departure rates for each origin airport,
# and finally arrange the airports in descending order for on 
# time departure percentage.

# Let’s start with classifying each flight as “on time” or “delayed” by creating a new variable with the mutate function.

nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

# The first argument in the mutate function is the name of the new 
# variable we want to create, in this case dep_type. Then if 
# dep_delay < 5 we classify the flight as "on time" and "delayed" 
# if not, i.e. if the flight is delayed for 5 or more minutes.

# Note that we are also overwriting the nycflights data frame with 
# the new version of this data frame that includes the new 
# dep_type variable.

# We can handle all the remaining steps in one code chunk:

nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))

# We can also visualize the distribution of on time departure 
# rate across the three airports using a segmented bar plot.

ggplot(data = nycflights, aes(x = origin, fill = dep_type)) +
  geom_bar()

# Question 8

# Mutate the data frame so that it includes a new variable that 
# contains the average speed, avg_speed traveled by the plane for 
# each flight (in mph). What is the tail number of the plane with 
# the fastest avg_speed? Hint: Average speed can be calculated as 
# distance divided by number of hours of travel, and note that 
# air_time is given in minutes. If you just want to show the 
# avg_speed and tailnum and none of the other variables, use the 
# select function at the end of your pipe to select just these two 
# variables with select(avg_speed, tailnum). You can Google this 
# tail number to find out more about the aircraft.

nycflights <- nycflights %>%
  mutate(avg_speed = distance/(air_time/60))

nycflights %>%
#  group_by(tailnum) %>%
  arrange(desc(avg_speed)) %>%
  select(avg_speed, tailnum)

# Question 9
ggplot(nycflights, aes(x = distance, y = avg_speed)) +
  geom_point()

# Question 10

nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on time", "delayed"))

nycflights %>%
  group_by(dep_type) %>%
  summarise(ot_arr_rate = sum(arr_type == "on time") / n()) %>%
  arrange(desc(ot_arr_rate))


