# Load packages into memory

library(dplyr)
library(ggplot2)
library(statsr)

data(arbuthnot)

arbuthnot

dim(arbuthnot)

names(arbuthnot)

arbuthnot$boys
arbuthnot$girls[1]

ggplot(data = arbuthnot, aes(x = year, y = girls)) +
  geom_point()

?ggplot

5218+4683

arbuthnot$boys + arbuthnot$girls

arbuthnot <- arbuthnot %>%
  mutate(total=boys+girls)

# line plot with total, year
ggplot(data = arbuthnot, aes(x = year, y = total)) +
  geom_line()

# line plot + scatter plot
ggplot(data = arbuthnot, aes(x = year, y = total)) +
  geom_line() +
  geom_point()


# line plot + scatter plot with boys vs time
ggplot(data = arbuthnot, aes(x = year, y = boys)) +
  geom_line() +
  geom_point()

arbuthnot <- arbuthnot %>%
  mutate(more_boys = boys > girls)

data(present)
summary(present)

range(present$year)

present <- present %>%
  mutate(total=boys+girls)

present <- present %>%
  mutate(prop_boys=boys/total)

# line plot + scatter plot with proportion of boys vs time
ggplot(data = present, aes(x = year, y = prop_boys)) +
  geom_line() +
  geom_point()

present <- present %>%
  mutate(more_boys = boys > girls)

summary(present$more_boys)
table(present$more_boys)
# there are more boys every year than girls

present <- present %>%
  mutate(prop_boy_girl = boys/girls)

# line plot + scatter plot with ratio of boys to girl over time
ggplot(data = present, aes(x = year, y = prop_boy_girl)) +
  geom_line() +
  geom_point()

max(present$total)
present[max(present$total) == present$total]

