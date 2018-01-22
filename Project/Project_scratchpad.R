# Question 1: Which U.S. State had most people overweight or obese.

# Verify that the proportions in dataset match with the codebook
table(brfss2013$X_rfbmi5)

# mostly, it does.

# Now let's add obesity rates for each state and order them,
# first, what is the variable for state?

# brfss2013$X_state is how we find the state... there are 55 levels.

# let's create a smaller dataframe with only the 

state_weight_df <- subset(brfss2013, select = c(X_rfbmi5, X_state))

# complete cases?
# complete_state_weight_df <- complete.cases(state_weight_df)
# complete cases does not work...

# state_weight_df$bmi_numeric <- as.numeric(state_weight_df$X_rfbmi5) - 1

complete_state_weight_df <- na.omit(state_weight_df)

# Since we do not assume the numebr sampled is the same for each "state", we will
# calculate the percentage of obesity per state and then sort in descending order.

complete_state_weight_df %>%
  group_by(X_state) %>%
  summarise(obesity_rate = sum( X_rfbmi5 == "Yes"  ) / n()) %>%
  arrange(desc(obesity_rate))

# The issue was JUST complete cases. R did not know how to add NA's.

--
# In California, how does sleep time vary per month?
  
# Create smaller dataframe with only California
brfss2013_CA <- subset(brfss2013, X_state == "California")

# How many observations?
nrow(brfss2013_CA) # 11518

brfss2013_CA_smalldf <- subset(brfss2013_CA, select=c(fmonth, sleptim1))

complete_brfss2013_CA_smalldf <- na.omit(brfss2013_CA_smalldf)

# sleeptime is brfss2013_CA$sleptim1

complete_brfss2013_CA_smalldf %>%
  group_by(fmonth) %>%
  summarise(mean_sleep = mean(sleptim1)) %>%
  arrange(desc(mean_sleep))

# We can visualize the distributions of departure delays across months using 
# side-by-side box plots:
  
ggplot(complete_brfss2013_CA_smalldf, aes(x = factor(fmonth), y = sleptim1)) +
  geom_boxplot()

# Research Question #3: Correlation based question?
# Is there a relationship between X and Y?
# number of adults in household? numadult
# total number of people = numadult + children
# number of days depressed in past 30 days? qlmentl2

# Is there some association between number of people in a household and
# self-reported depression over the past 30 days?

small_brfss2013_df <- subset(brfss2013, select=c(numadult, children, qlmentl2))

complete_small_brfss2013_df <- na.omit(small_brfss2013_df)

complete_small_brfss2013_df$num_hh <- as.numeric(complete_small_brfss2013_df$numadult) + complete_small_brfss2013_df$children

ggplot(data = complete_small_brfss2013_df, aes(x = num_hh, y = qlmentl2)) +
  geom_point()

# There does NOT seem to be any relationship
