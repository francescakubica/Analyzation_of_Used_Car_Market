library(tidyverse)
library(moderndive)
library(skimr)

## EVERYTHING THAT WENT INTO THE ACTUAL PROJECT ##

###### EXPLORING THE DATA ###### 


## Looking at the data ##
sapply(cars, class)
# Most variables seem appropriate, but recommend changing the 0's and 1's for personal use only,
# one owner, accident or damage, to No's and Yes. 

# There appears to be a lot of NA underneath: price drop, seller rating, and driver rating.
# Removing these NA points would get rid of a lot of data. If I attempt to make a model that involves
# these variables, then I have to remove the NA, otherwise I can keep them. There could be missing data
# because customers and producers simply didn't want to give a rating and or there could be no price drop offer
# from the previous sell (price).


# Looking at drivetrain specifically: Some points say “Front-wheel Drive” others say “FWD”, similar concept for “RWD” 
# and “Rear-wheel Drive”, meaning we have to take this into consideration if we want to explore drivetrain 
# variable [ex. Filtering for only “Rear-wheel Drive” doesn’t account for “RWD".
# Making all of the data points have the same name "RWD" and "FWD" and "4WD" would clean up the data.

# Looking at the summary for outliers:
summary(cars)

# Price drop has an extreme outlier of 79909 compared to 3rd Quartile 1000 
Outlier_pricedop <- cars %>% 
  filter(price_drop == 79909)
# It is 3 porches, which skews the data. If we are attempting to find correlations using price drop,
# we can consider removing this. 

# Should remove the cars that have zero mileage because the one's with no mileage likely aren't used and
# could be skewing the data as an outlier. Because the mean mileage is 57808 and the 1st Quartile is 26899.

# Cars with extremely high prices and low prices should be removed so that the data is more normally distributed.
# There are billion dollar cars, a reasonable price could be below 3 million for a filter.
# Looking at the visualization of cars and price, the outliers are noticeable:
cars %>% 
  skim()
# The histogram for price skews heavily to the right.
# The high and low prices could simply be mistakes or jokes on the seller's end on the website. They could be fake entries.


# Considering how many data points are being rid of if we filter for low and high end prices:
countofprice <- cars %>%
  filter(price > 1) %>% 
  filter(price < 30000000)
551381 - 551375 # Removes 6 data points 
# Consider adding log price because of excessive skew.

# Will not continue to remove price outliers because luxury cars could be 3 million dollars.
# Because of this, consider: separating into subgroups to find correlations and make recommendations because of the price differences.

# Looking at mileage outliers
cars %>% 
  ggplot(aes(x = mileage, y = price, color = manufacturer)) +
  geom_point()
# Consider adding log mileage because of excessive skew. 

# Counting how many are above 200,000 mileage 
countofmileage <- cars %>% 
  filter(mileage > 200000) # There are only 3486

# Cars that have excessive mileage are skewing the data, but could have different affects on each subgroup.
# Won't filter for the main data but for each subgroup of economy, luxury, etc.

# There are police interceptors in the data. But potentially these could be sold and exchanged so there's no reason to 
# get rid of them from the data. 
police <- cars %>% 
  filter(model == "Tahoe Police") %>%  # 32 Tahoe Police 
# There are also Ford Crown Victoria police cars (very few)


## Looking at the data ##

### Making Changes & Creating a New Dataset ###

# Ivana:
cars1 <- cars %>% 
  mutate(drivetrain = 
           ifelse(drivetrain == "All-wheel Drive", "AWD",
                  ifelse(drivetrain == "Front-wheel Drive", "FWD",
                         ifelse(drivetrain == "Rear-wheel Drive", "RWD",
                                ifelse(drivetrain == "Four-wheel Drive", "4WD",
                                       drivetrain)))))

# Francesca:
cars1 <- cars %>%
  filter(price > 1)
cars1 <- cars %>% 
  filter(price < 30000000)

# Francesca:
cars1 <- cars %>% 
  mutate(accidents_or_damage = ifelse(accidents_or_damage == 0, "No", "Yes")) %>% 
  mutate(one_owner = ifelse(one_owner == 0, "No", "Yes")) %>% 
  mutate(personal_use_only = ifelse(personal_use_only == 0, "No", "Yes"))

# Victor:
cars1 <- filter(cars, year >= 2010)

# Riley / Finny:
cars1 <- cars1 %>%
  mutate(fuel_type = 
           ifelse(fuel_type == "E85 Flex Fuel", "Flex",
                  ifelse(fuel_type == "Flexible Fuel", "Flex",
                         fuel_type)))

#Thomaz:
cars1 <- cars1 %>% 
  mutate(subgroup = ifelse(manufacturer == "Dodge" |
                             manufacturer == "Ford" |
                             manufacturer == "Chevrolet" |
                             manufacturer == "Jaguar" |
                             manufacturer == "Acura" |
                             manufacturer == "Toyota" |
                             manufacturer == "Jeep" |
                             manufacturer == "INFINITI", "mid_range",
                           ifelse(manufacturer == "Mitsubishi" |
                                    manufacturer == "Hyundai" |
                                    manufacturer == "Chrysler" |
                                    manufacturer == "Kia" |
                                    manufacturer == "Nissan" |
                                    manufacturer == "Mazda" |
                                    manufacturer == "Buick" |
                                    manufacturer == "Volkswagen" |
                                    manufacturer == "Honda" |
                                    manufacturer == "Subaru", "economy", 
                                  ifelse(manufacturer == "Volvo" |
                                           manufacturer == "Lincoln" |
                                           manufacturer == "Lexus" |
                                           manufacturer == "RAM", "premium",
                                         ifelse(manufacturer == "Audi" |
                                                  manufacturer == "Audi" |
                                                  manufacturer == "Cadillac" |
                                                  manufacturer == "BMW" |
                                                  manufacturer == "GMC", "luxury",
                                                "outlier")))))

# Thomaz/Francesca:
## Adding them all together so they don't overwrite each other:

cars1 <- cars %>% 
  filter(price > 1) %>% 
  filter(price < 30000000) %>% 
  filter(year >= 2010) %>% 
  mutate(accidents_or_damage = ifelse(accidents_or_damage == 0, "No", "Yes")) %>% 
  mutate(one_owner = ifelse(one_owner == 0, "No", "Yes")) %>% 
  mutate(personal_use_only = ifelse(personal_use_only == 0, "No", "Yes")) %>% 
  mutate(drivetrain = 
           ifelse(drivetrain == "All-wheel Drive", "AWD",
                  ifelse(drivetrain == "Front-wheel Drive", "FWD",
                         ifelse(drivetrain == "Rear-wheel Drive", "RWD",
                                ifelse(drivetrain == "Four-wheel Drive", "4WD",
                                       drivetrain))))) %>% 
  mutate(fuel_type = ifelse(fuel_type == "E85 Flex Fuel", "Flex",
                  ifelse(fuel_type == "Flexible Fuel", "Flex",
                         fuel_type))) %>% 
  mutate(log_price = log10(price)) %>%        # Extra we added in as a team 
  mutate(log_mileage = log10(mileage)) %>%    # Same **
  filter(mileage > 0) %>% 
  mutate(subgroup = ifelse(manufacturer == "Dodge" |
                             manufacturer == "Ford" |
                             manufacturer == "Chevrolet" |
                             manufacturer == "Jaguar" |
                             manufacturer == "Acura" |
                             manufacturer == "Toyota" |
                             manufacturer == "Jeep" |
                             manufacturer == "INFINITI", "mid_range",
                           ifelse(manufacturer == "Mitsubishi" |
                                    manufacturer == "Hyundai" |
                                    manufacturer == "Chrysler" |
                                    manufacturer == "Kia" |
                                    manufacturer == "Nissan" |
                                    manufacturer == "Mazda" |
                                    manufacturer == "Buick" |
                                    manufacturer == "Volkswagen" |
                                    manufacturer == "Honda" |
                                    manufacturer == "Subaru", "economy", 
                                  ifelse(manufacturer == "Volvo" |
                                           manufacturer == "Lincoln" |
                                           manufacturer == "Lexus" |
                                           manufacturer == "RAM", "premium",
                                         ifelse(manufacturer == "Audi" |
                                                  manufacturer == "Audi" |
                                                  manufacturer == "Cadillac" |
                                                  manufacturer == "BMW" |
                                                  manufacturer == "GMC", "luxury",
                                                "outlier")))))

# Save the data as a file
save(cars1, file = "cars1.RData")

# Project goals are explained in presentation, alongside approach / effectiveness.

### Making Changes & Creating a New Dataset ###

###### EXPLORING THE DATA ###### 

## CREATION OF SUBGROUPS ## 


# Economy Group: Mitsubishi, Hyundai, Chrysler, Kia, Nissan, Mazda, Buick, Volkswagen, Honda, Subaru   - Francesca and Thomas 

# Mid-range Group: Dodge, Ford, Chevrolet, Jaguar, Acura, Toyota, Jeep, INFINITI. - Finny and Riley

# Luxury: Audi, Cadillac, BMW, GMC.      - Victor & Ivana

# Assigned based off of amount of data per subset. 


# Got rid of: Premium and Ulta-Luxury brands. Ulta Luxury was removed because it included 
# extreme outliers that could not be predicted well.
# Premium models wouldn't make actionable / differentiable results.


## CREATION OF SUBGROUPS ## 

#### SEPERATE BY SUB-GROUPS (THOMAS + FRANCESCA) ECONOMY CARS #### 
economy <- cars1 %>% 
  filter(subgroup == "economy")

summary(economy)

# Looking for correlations and skew
economy %>% 
  skim()
# Right skew is shown in: mileage, engine, mpg_c, price drop, and price.
# Left skew is shown in: year, seller rating, driver rating, and log mileage
# from the economy group. The mpg_h and log price is roughly normally distributed.
# These means that the mileage should be filtered of outliers and year, in order to make
# an accurate model.

# Found price should equal 43000 before becoming an outlier by doing a rough IQR rule. This should make price
# more normally distributed. 

# Clarification of log: Logging the mileage and price decreases overall skew (proved by trial and error of visualizations)
# When assessing correlation, I refer to mileage, and only use log when making models and visualizations because the correlations are similar.
economy <- cars1 %>% 
  filter(subgroup == "economy") %>% 
  filter(year > 2010) %>% 
  filter(log_price > 4.0) %>% 
  filter(log_price < 4.602) %>% 
  filter(log_mileage > 3.7125) %>% 
  filter(log_mileage < 5.5885) %>% # Chose these roughly based off of visualization clarity (seen later) & ultimately removed skew
  filter(price < 43000)  # Removing price outliers because they skew the data to the right 

# Filter for year has to be greater than 2010 to see if it'll get rid of the left skew. Conclusion: worked, and filter
# for cars older than 2010 has been applied.
    
economylmpoints <- economylmpoints %>% 
  arrange(desc(stdres))
# Looks like above 4.6 price, it's skewing
# Approach: Looking at the QQ plot, seeing where the standard residual is starting to skew from the normal line,
# looking at irregularities within those points (usually due to low or high prices). Then double checking by using skim
# to see if the histograms are affected.

# QQ plot shows a strong tail at 3 standardized residual, looking at the points to identify outliers
# Making the price ceiling higher ( price < X), makes the QQ plot worse. 
# Making the price bottom lower ( price > X), makes the QQ plot a lot worse and less normally distributed.


# Reviewing the same economy group but after changes:
economy %>% 
  skim()
# Both log price and log mileage are more normally distributed now. Year is more normally distributed when the car has
# to be older than 2010. Price is also more normally distributed (practially no more right skew), meaning the filter
# for less than 43000 was roughly correct.


# Looking for correlation
economy_m <- select_if(economy, is.numeric)
economy_m %>% 
  cor()

# Because there are a lot of NA, to compare seller rating and driver rating, we have to remove a lot of rows
# Besides that: it seems that year and price have a positive moderately strong correlation of 0.67.
# Mileage and year have a negative strong correlation of -0.68, which could have a connection to price. 
# Mileage and price have a strong negative correlation of -0.63. Meaning as mileage increases, price decreases.
# Mpg_c and Mpg_h have the strongest correlations out of all the variables, with a positive 0.87.
# The weaker correlations for price include a positive correlation with engine, 0.26, and a negative
# correlation with mpg_c and mpg_h of -0.17 and 0.28 respectively. This makes sense because a better engine
# is more likely to be sold for more. Though if the number of miles a car can travel in a city or highway increases,
# it doesn't make practical sense that the price should decrease because that is a valuable trait.

# Findings:
# Year and price are closely related and should be looked into more closely.
# Mileage and year also have a strong negative correlation and should be examined.
# Unique: As mpg_c and mpg_h increase, price decrease? Coud look closer into it with visualizations.

#### Heat Map to Visualize Correlations (Without NA) ####

economy_NA <- economy %>% 
  na.omit()

economy.numeric <- select_if(economy_NA, is.numeric)

## Generate correlation matrix, round result to two decimal places
economy.cor=round(cor(economy.numeric),2)
## Melt correlation matrix
economy.cor.melt=melt(economy.cor)

## Heat Map
economy.cor.melt %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="red", high="light blue", mid="white", midpoint=0)+
  geom_text(aes(x=Var1, y=Var2, label=value), size=3)+
  theme(axis.text.x=element_text(size=8, angle=30, hjust=0.8)) +
  ggtitle("Heat Map for Economy Cars") +
  labs(y = "Variables", x = "Variables")

## Removing the NA does not affect correlations greatly because it alters it by a mere 0.03 for each relationship of variables. Meaning
# it is okay to make conclusions based off of this heat map for other variables.

# FINDINGS:
# Given driver rating, seller rating, and price drop, there aren't any significant
# correlations. All conclusions should be met with other numeric variables such as mileage
# price, and year, because those have more strong relationships to show depreciation.
# Factors that go into depreciation could be mileage and year.
# I am looking for depreciation aspects because that will determine what kind of car is the
# best to buy over time. 

#### Heat Map to Visualize Correlations (Without NA) ####

## Proving why we're using price for depreciation is shown in Appendix ## 

#### CREATING VISUALIZATIONS ####

## Connecting mileage with depreciation ##

# Visualization to show: price going down as mileage increases over year
# The price is logged because it gets rid of the skew.

# Not shown in Appendix but this makes the conclusion that year and mileage are closely related to depreciation.
# Since my model (introduced later) does not show possible depreciation factors, I consider mileage a depreciation factor
# given this visualization.
economy %>%
  ggplot(aes(y = log_price, x = log_mileage, color = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Log Mileage", y = "Price of Car") +
  ggtitle("Mileage Over Time") +
  scale_y_continuous(breaks = seq(0,4.8, by = 0.1))
## FINDINGS:
# As mileage increases, the price decreases. Also, newer cars usually have lower mileage and older cars have
# higher mileage. This makes sense because newer cars haven't had time to be driven as much. There could be a correlation
# between having high mileage and being an older car. Once can be affecting the other, because the older the car,
# the more increase in mileage, which decreases the price.
# Can put this next to the model to say that a possible reason for depreciation over time is increasing mileage.

## ANSWERING QUESTION 1 ##
# Which manufacturer is the best to buy a used car from with respect to depreciation? (Price over year)

# All manufacturers depreciation 
economy %>% 
  ggplot(aes(x = year, y = log_price, color = manufacturer)) +
  geom_point(alpha=0.05) +
  geom_smooth(method = "lm", se = F) + 
  labs(y = "Log Price", x = "Year") +
  theme(legend.position = c(0.9,0.22)) +
  ggtitle("Depreciation for Manufacturers")
# FINDINGS:
# Comparing all of the manufacturers with their respective depreciation. Looking at that, Mitsubishi has
# the least depreciation over time because their slope is the flattest. Meaning that as year increases,
# price increases steadily, and in reverse, price gradually decreases and doesn't have a stark change in depreciation.
# Honda has the most depreciation over time alongside Subaru.
# So, someone attempting to purchase a car, with depreciation in mind, should buy a Mitsubishi if they don't
# want their car to depreciate as fast.

# We can look at the differences more closely (clearer visualization)
economy %>% 
  filter(manufacturer == "Subaru" |
           manufacturer == "Mitsubishi") %>% 
  ggplot(aes(x = year, y = log_price, color = manufacturer)) +
  geom_point(alpha=0.05) +
  geom_smooth(method = "lm", se = F) + 
  labs(y = "Log Price", x = "Year") +
  theme(legend.position = c(0.1,0.9)) +
  ggtitle("Depreciation for Manufacturers")

# Given this, we can see the true difference between Mitsubishi and Subaru. 
# Subaru's price decreases drastically given the year and Mitsubishi doesn't decrease as drastically.


#### CREATING VISUALIZATIONS #### 

#### CREATING A MODEL BASED OFF VISUALIZATIONS #### 


# The variables are logged so the QQ plot can show a more normally distribution. 

economylm <- lm(log_price~year*manufacturer, economy)
summary(economylm)
#R^2 is 0.5777 #Adjusted R^2 is 0.5776
get_regression_summaries(economylm)
# RMSE: 0.0848

economylmpoints %>% 
  summarize(rmse = sqrt(mean( ( 10^log_price - 10^log_price_hat )^2 )))
# RMSE = 4599 Dollars 
# So based off of the error, the average dollar error is 4599. Our prediction will be
# on average 4599 dollars off for each car. 

### REASONS FOR MODEL:
# Using an interaction model because the RMSE is lower for interaction than parallel and the year
# depreciation of price depends on the manufacturer as seen in the previous visuals that 
# show the slopes of all manufacturers over time.

# LIMITATION OF MODEL: Cannot predict what factors lead to depreciation. Simply shows depreciation
# between all manufacturers, comparing them for each year. 

# This model only shows the estimates of depreciation over time for manufacturers.
# Could use this to find brands that are better than others over certain periods of time (as 
# I do an example of later).

economylmpoints <- get_regression_points(economylm)
# Getting the rstandard
economylmpoints <- economylmpoints %>% 
  mutate(stdres = rstandard(economylm))

# Residual plot for model (Not represented in project)
economylmpoints %>% 
  ggplot(aes(y = residual, x = year)) +
  geom_jitter(color = "darkblue", alpha = 0.05) + 
  ggtitle("Residual Plot of Car Depreciation") +
  labs(x = "Year", y = "Residual") +
  geom_smooth(method = lm, se = FALSE)
# Independence is not violated because the linear model shows 0 correlation.

# QQ Plot for model (One presented in the project)
economylmpoints %>% 
  ggplot(aes(sample=stdres))+
  geom_qq(color="light blue") +
  geom_qq_line(color="dark red") +
  labs(y = "Standardized Residuals", x = "Theortical") +
  ggtitle("QQ Plot for Car Depreciation")

## Linear Model Assumptions ##
# 1. Mean of zero error
# 2. Constant variance
# 3. Normal Distribution
# 4. Independence of errors

# If the model is roughly normally distributed, then the mean of error will be zero over time and
# the variance will be constant. Also, given the QQ plot, these assumptions are not violated because the
# distribution follows the QQ line roughly close meaning it's roughly normally distributed.
# As for the independence of errors, because the residual plot does not have a pattern and has a correlation of 0,
# independence is not violated. 


#### CREATING A MODEL BASED OFF VISUALIZATIONS #### 

#### MAKING ESTIMATES #### 

# Plugging in numbers to showcase the price depreciation between Mitsubishi and Subaru 

# Mitsubishi
# (2.206e+01 -7.862e+01) + (4.111e-02 -1.097e-02 )*year
# Subaru 
# (3.169e+00 -7.862e+01) + (4.111e-02 -1.560e-03)*year

# 3 Years Depreciation

# 2022 Mitsubishi 

10^ (
  (2.206e+01 - 7.862e+01) + (4.111e-02 -1.097e-02 ) * 2022
)
# 24159.06

# 2019 Mitsubishi
10^ (
  (2.206e+01 - 7.862e+01) + (4.111e-02 -1.097e-02 ) * 2019
)
# 19618.24

# Calculate depreciation: $4540.82
24159.06 - 19618.24 # 4540.82

# Over 3 years - Mitsubishi: Depreciated 18.7%
1 - (19618.24 / 24159.06)


# 2022 Subaru 

10^ (
  (3.169e+00 -7.862e+01) + (4.111e-02 -1.560e-03)*2022
)
# 33044.56

# 2019 Subaru

10^ (
  (3.169e+00 -7.862e+01) + (4.111e-02 -1.560e-03)*2019
)
# 25144.91

# Calculate depreciation: $7899.65
33044.56 - 25144.91

# Over 3 Years - Subaru: Depreciated 23.9%
1 - (25144.91 / 33044.56)


# These findings correlate with the visualizations whereas Subaru depreciated 
# more than Mitsubishi by more than 5%.
# Over a 3 year period, Mitsubishi only depreciated by 18.7%. 
# Over a 3 year period, Subaru depreciated by 23.9%. 


#### MAKING ESTIMATES #### 


#### MAKING RECOMMENDATIONS ####

# Businesses, looking to rent out economy cars, should invest in certain manufacturers if they want to have cars that don't depreciate as quick.
# The Mitsubishi manufacturer in the used-car market depreciates the slowest and least out of all manufacturers
# given, so businesses should invest in Mitsubishi if depreciation is a strong decision factor. 

# Businesses, looking to rent out economy cars, should not invest in a Subaru if depreciation is a large decision factor for buying into
# the used car market, given that it depreciates the most.

# Suggestion: Investors should also do regular upkeep on the cars so they do not depreciate as fast.

# Further recommendations / business perspectives / migitating risk is showcased on presentation.

#### MAKING RECOMMENDATIONS ####


#### SEPERATE BY SUB-GROUPS (THOMAS + FRANCESCA) ECONOMY CARS #### 


## EVERYTHING THAT WENT INTO THE ACTUAL PROJECT ##






################## ALL THE TRIAL AND ERROR (Not Sorted) ################## 

# Do facet wrap first then create this to say that's why we chose these 

economy %>% 
  ggplot(aes(y = log_price, x = year, color = manufacturer)) +
  geom_point() +
  facet_wrap(~manufacturer) +
  geom_smooth(method = "lm", se = FALSE)
# FINDINGS:
# Comparing all of the manufacturers with their respective depreciation. Looking at that, Mitsubishi has
# the least depreciation over time because their slope is the flattest. Meaning that as year increases,
# price increases steadily, and in reverse, price gradually decreases and doesn't have a stark change in depreciation.


economy %>% 
  filter(manufacturer == "Chrysler" |
           manufacturer == "Subaru" |
           manufacturer == "Mitsubishi") %>% 
  ggplot(aes(x = year, y = log_price, color = manufacturer)) +
  geom_point(alpha=0.05) +
  geom_smooth(method = "lm", se = F) + 
  labs(y = "Price", x = "Year") +
  theme(legend.position = c(0.1,0.9)) +
  ggtitle("Depreciation for Manufacturers")

# Findings:

# We can assume that usually the depreciation happens due to mileage 

# Making a visualization for price and mileage for each manufacturer 
# The price and mileage is logged to get rid of the skew.
economy %>% 
  ggplot(aes(y = price, x = mileage, color = year)) +
  geom_point() +
  facet_wrap(~manufacturer) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Log10 Mileage of Car", y = "Log10 Price of Car") +
  theme(legend.position = c(0.65,0.15)) +
  ggtitle("Comparing Price and Mileage Over Time for Car Manufacturers")
## FINDINGS:
# Appears that Chrysler is the worst for depreciation. As their slope is significantly more steep, meaning
# as the mileage increases and year increases, their price goes down the fastest.
# Mitsubishi is the best economy car for depreciation because over time, as mileage and year increases,
# their price does not drastically decrease. There is one outlier with a old car and little mileage that is
# less expensive than the rest. 




## ATEMPTING OTHER KINDS OF MODELS / VISUALIZATIONS FROM HERE ONWARDS ##

## Analyze price and mileage

# Interpretation: When mileage increases, price decreases. 
# When it's a a city

# See trends: is the mileage of used-cars increasing over time?

economy %>% 
  ggplot(aes(y = mileage, x = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10()
# As the cars become newer, mileage decreases
# The older the car, the higher the mileage
# Only the really new cars have a low mileage, even at 2020 the cars have the same mileage as they would in 2018



cars1 %>% 
  ggplot(aes(y = mileage, x = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10()


### Make a category saying is this higher than 2020 or not ? "Yes" or "No",
# dependent on that we can make a correlation 

# This doesn't have NA's so we're just using NA
oldeconomy <- economy %>% 
  filter(year < 2020)
neweconomy <- economy %>% 
  filter(year > 2020)

## Facet-wrap the Older and Newer -- Showing the mileage and year 

economy <- economy %>% 
  mutate(Age = ifelse(year >= 2020, "Newer", "Older"))


economy %>%
  ggplot(aes(y = mileage, x = year)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE)

## Trying out a histogram for skew and 
economy %>% 
  ggplot(aes(x = mileage, fill = Age)) +
  geom_histogram() +
  facet_wrap(~year)

lmtests <- lm(price~mileage*Age + year, economy)
summary(lmtests)
get_regression_summaries(lmtests)
get_regression_summaries(lm(price~mileage*year + Age, economy))

economy %>% 
  ggplot(aes(y = price, x = year)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~Age) +
  geom_smooth(method = "lm", se = FALSE)


# How many old cars are there in the market? Or any in the future?

oldcars <- economy_NA %>% 
  filter(year < 2020)
# There are 37231 slightly older cars compared to the 78960 total
# Meaning 47% of the data has high mileage (all of which are older than 2020)
# There's a lot more newer cars in the market than older cars and they're depreciating faster




# We know that mileage decrease price, but does mileage increase depreciation?
# Mileage and price have a moderately strong negative correlation of 0.63 which is shown in this visualization.


# Split the data from 2020 greater than and less than 



lm1 <- lm(price~log10_mileage, economy_NA)
summary(lm1)

# R^2 = 32.2%, significant
get_regression_summaries(lm1)


## Comparing how price goes up when engine goes up 
lm2 <- lm(price~year + engine, economy)
summary(lm2)
# R^2 = 54.8%, significant

# Doing an interactive model doesn't make a difference really
lm2 <- lm(price~year*engine, economy_NA)
summary(lm2)


## Why does price decrease when mpg_c or mpg_h increase?
lm3 <- lm(price~mpg_c + mpg_h, economy_NA)
summary(lm3)
## R^2 : 13.8%, significant
# Interactive model makes it worse 

economy_NA %>% 
  ggplot(aes(y = price, x = mpg_h, color = manufacturer)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# WHY IS PRICE GOING DOWN FOR BETTER TRAITS??



# How many cars 


## Same cars over time 
# Car that depreciates the least. 

economy %>% 
  ggplot(aes(x = price, fill = manufacturer)) +
  geom_histogram(position = "dodge", color = "white", binwidth = 400) +
  facet_wrap(~year) +
  scale_x_log10()


### POTENTIAL DATA TO INCLUDE ###
# Initial price of the car (not just the price now)
# The market for the new cars [ sold cars ]


#### CREATING VISUALIZATIONS WITH CATEGRORICAL VARIABLES ###

#### SEPERATE BY SUB-GROUPS (THOMAS + FRANCESCA) ECONOMY CARS #### 

#### ANALYZING THE FUEL TYPE ####


  # 1. Have a comparison between fuel types within your subgroup. Which fuel type depreciates the least with time (price over year, color by fuel type)? Which fuel type depreciates the most?
  #  Any trends/outliers?
  #  - investors should prefer the ones that depreciate the least
  
  economy %>% 
  ggplot(aes(y = price, x = year, color = fuel_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Hard to see the others so lets do jitter b/c gasoline takes up a lot of it

economy %>% 
  ggplot(aes(y = price, x = year, color = fuel_type)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

# Let's look at them individually with facet wrap

economy %>% 
  ggplot(aes(y = price, x = year, color = fuel_type)) +
  geom_point() +
  facet_wrap(~ fuel_type) +
  geom_smooth(method = "lm", se = FALSE) 

# There are simply a lot more cars in gasoline which can explain the more likelihood
# of the car having a high price. 
# Looks like hybrid cars don't have an extreme depreciation compared to gasoline
# Flex Fuel also doesn't have as high depreciation 
# Gasoline cars look like they depreciate the most 
# Flexible Fuel depreciates the least?
# They all look very similar

# HAVE TO MAKE SURE THAT THESE ARE SIGNIFICANT FINDINGS 


#2. Have a comparison between brands within your subgroup. 
#Which brand depreciates the least? Which depreciates the most? Any trends/outliers?

economy %>% 
  ggplot(aes(y = price, x = year, color = manufacturer)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~manufacturer)

# Can't really tell what's going on in the visualization 
# Looks like Volkswagen has the highest depreciation 
# An outlier for Volkswagen at 2020, one for Hyundai at 2020, lots of data points for Nissan

economyprice$year <-as.numeric(economyprice$year)
# Made year into a character so we can visualize it 
economy %>% 
  ggplot(aes(y = price, x = fuel_type, color = year)) +
  geom_point() +
  geom_smooth(method = "lm")# +
#facet_wrap(~manufacturer)
# Looking at how fuel type affects price in each year
# The hybrid cars are affected mostly


# Each year how does fuel type affect price for manufactureres.





#3. Build a linear model that predicts price within your subgroup. Explain that 
#if a car is beneath this model, then the car is a good purchase. 


economylm <- lm(price~fuel_type + manufacturer, economy)
# This shows how the price affects the fuel type but not the depreciation aspect
# A consumer wants to know that their product won't depreciate fast. 
summary(economylm)

# The fuel types are not significant in predicting depreciation 
# They are significant in predicting price

fueltypepoints <- get_regression_points(economylm)
fueltypepoints %>% 
  ggplot(aes(y = residual, x = fuel_type)) +
  geom_point() +
  scale_y_continuous(breaks = seq(-7,7, by = 1))
# I'm not sure what this is showing me.
# Residual of predicted price to fuel type. 

## Make a QQ plot anyway
fueltypepoints <- fueltypepoints %>% 
  mutate(stdres=rstandard(economylm))

fueltypepoints %>% 
  ggplot(aes(sample=stdres))+
  geom_qq(color="light blue") +
  geom_qq_line(color="dark red") 

# Shows a U shape. Meaning we can log it to make it less skew.
# Tails are vering off. 


# If the p hat over predicts, saying the price is higher. and its lower, (negative residual), then that
# is good because its overvalued 

####









### My own exploration at this point #####

## Looking at how mileage affects depreciation and new and old cars
# When mileage increases, it depreciates


# My questions:

# 1. Have a comparison between fuel mileage and year within your subgroup. 
#Which manufacturers depreciates the least with time (price over year, color by manufacturer)? Which manufacturer depreciates the most?
#  Any trends/outliers?
# - investors should prefer the ones that depreciate the least

## Comparing year
economy %>% 
  ggplot(aes(y = price, x = year, color = manufacturer)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~manufacturer)

# To mileage
economy %>% 
  ggplot(aes(y = price, x = mileage, color = manufacturer)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~manufacturer) +
  scale_x_log10()

# Mitsubishi stays relatively the same. Increased mileage does not really decrease the price.
# For all other cars, increasing mileage decreases the price drastically.
# Chrysler is the most affected and depreciates the fastest.
# Mitsubishi does not depreciate the most. 


#3. Build a linear model that predicts price within your subgroup. Explain that 
#if a car is beneath this model, then the car is a good purchase. 

# For this model: would have to prove that mileage increases depreciation
# Consumers would want to pick a manufacturer that when mileage increases, depreciation isn't
# extreme
mileagelm <- lm(price~mileage + manufacturer, economy)
summary(mileagelm)
# R^2: 46.12% - only variable that is not significant is Nissan? 
# Why??

mileagelmpoints <- mileagelmpoints %>% 
  mutate(log10mileage = log10(mileage))

mileagelmpoints <- get_regression_points(mileagelm)
mileagelmpoints %>% 
  ggplot(aes(y = residual, x = log10mileage)) +
  geom_point() +
  scale_y_continuous(breaks = seq(-7,7, by = 1))
# This can't work because there's an obvious pattern 

fueltypepoints <- fueltypepoints %>% 
  mutate(stdres=rstandard(economylm))


fueltypepoints %>% 
  ggplot(aes(sample=stdres))+
  geom_qq(color="light blue") +
  geom_qq_line(color="dark red") 


## Removing the Nissan might make this better? ?!?

#### EVERYTHING HERE FORWARD ACTUALLY MATTERS #####
### Filter by price < 44014

economyprice <- economy %>% 
  filter(price <44014)

mileagelm2 <- lm(price~mileage + manufacturer, economyprice)
summary(mileagelm2)
# Increases R^2 - 0.5188 

# Volkswagen is similar to Buick # explain IT

lm2testpoints <- get_regression_points(mileagelm2)
lm2testpoints %>% 
  ggplot(aes(y = stdres, x = mileage)) +
  geom_point(alpha=0.05) +
  scale_y_continuous(breaks = seq(-3,3, by = 1))

## Potentially do the same filtering but with mileage
# 150994 - top of the data 

## QQ plot
lm2testpoints <- lm2testpoints %>% 
  mutate(stdres=rstandard(mileagelm2))


lm2testpoints %>% 
  ggplot(aes(sample=stdres))+
  geom_qq(color="light blue") +
  geom_qq_line(color="dark red") 


### Let's look for the points that we want to get ###
## Those where the predicted is overvalued, and the residual is negative

# Filter for negative residuals and find the primary manufacturer

count <- lm2testpoints %>% 
  filter(residual < 0)
# There are 99090

lm2testpoints %>% 
  filter(residual > 0)
# There are 82978 

## Reason why: more expensive cars 

### It's a little odd that there are more overvalued 
# Looking for manufacturer
# There's probably an easier way to do this 
manufacturers <- lm2testpoints %>% 
  filter(residual < 0) %>% 
  filter(manufacturer == "Subaru")
# 15303 - Kia
# 2124 - Mitsubishi
# 9442 - Hyundai
# 4357 - Chrysler
# 5801 - Mazda
# 6453 - Buick
# 10433 - Volkswagen
# 15866 - Honda
# 10089 - Subaru


# The most is Honda and Kia. 
# The ones that 


# We can look at the manufacturers that are the most volatile to depreciation with mileage

economyprice <- economyprice %>% 
  mutate(Age = ifelse(year >= 2020, "Newer", "Older"))


economyprice %>% 
  ggplot(aes(y = price, x = mileage, color = Age)) +
  geom_point() +
  facet_wrap(~Age) +
  geom_smooth(method = "lm", se = FALSE, color = "black")
# The exact same...
# How are the new cars affected by depreciation? - Looks like they're about the same
# We need to find: Do new cars price decrease FASTER when mileage increases??
# It doesn't??


# Looking at a histogram of mileage and age
economyprice %>% 
  ggplot(aes(x = mileage, fill = Age)) +
  geom_histogram(position = "dodge", color = "white")

# The older cars are more normally distributed 
# The newer cars are roughly right skew



# Let's look at the manufacturers that are most volatile to mileage increase and price

economyprice %>% 
  ggplot(aes(y = price, x = mileage, color = manufacturer)) +
  geom_point() +
  facet_wrap(~manufacturer) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_log10() +
  scale_y_log10()

# IMPORTANT FINDINGS!:
# Chrysler is HEAVILY affected by the mileage and price decrease
# Mitsubishi is the LEAST affected by the mileage increase and price decrease
# Things to consider: the original price point 



## How did you filter by price < 44014 ?? 
# - Prove by IQR (max before its an outlier) and visualizations 
# forcing it to be roughly normally distributed 
# We need to make sure that it's clear that price depreciates because of increasing mileage


### The data points themselves could have less mileage 
# Have to check if Mitsubchi simply has less cars (check the mean - Thomaz)
# Chrysler simply has higher mileage which means the findings aren't as significant 




# WHERE ARE THE PATTERNS?? #


## Engine a possible factor?


# How the model can tell us good aspects of cars to buy
lmtest3 <- lm(price~mileage + year*manufacturer + engine, economyprice)
get_regression_summaries(lmtest3)
summary(lmtest3)
## Everything is significant

## Finding the good aspects of a car
# See how price is affected by the engine, manufacturer 

economyprice %>% 
  ggplot(aes(y = price, x = engine)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", se = F)
# Chrysler - Smaller cars with better engines are good
# Mazada - Larger cars with better engines are worse because you need to simply buy more gas



## It makes a better buy when you consider the year, manufacturer, and mileage

# Price increases when engines increase 
# Except for Mazda ???

## FIND THE KINDS OF CARS THAT YOU WANT TO BUY!
## IF YOU CARE ABOUT DEPRECIATION - MITSUBSCHI
# IF YOU SIMPLY WANT A CAR THAT IS OVERVALUED BUT YOU CAN BUY FOR LESS - THESE ASPECTS

# Explain how the visualizations and models are the connected ?
# - We are doing visualizations 


# NOT DIRECT DEPRECIATION (TALK ABOUT THAT)


#### ANSWERING WHAT CAR DEPRECIATES THE LEAST / MOST ####


### Filter by price < 44014

economyprice <- economy %>% 
  filter(price < 44014) 

mileagelm2 <- lm(price~mileage * manufacturer, economyprice)
summary(mileagelm2)
# Increases R^2 - 0.5188 
#R^2 - 40.6% with log
# Nissan wasnt good

mileagelmtest <- lm(price~year * manufacturer, economyprice)


mileagelmtestpoints <- get_regression_points(mileagelmtest)
mileagelmtestpoints <- mileagelmtestpoints %>% 
  mutate(stdres=rstandard(mileagelmtest))
mileagelmtestpoints %>% 
  ggplot(aes(sample=stdres))+
  geom_qq(color="light blue") +
  geom_qq_line(color="dark red") 



# Volkswagen is similar to Buick # explain IT


lm2testpoints <- get_regression_points(mileagelm2)

lm2testpoints <- lm2testpoints %>% 
  mutate(stdres=rstandard(mileagelm2))

lm2testpoints %>% 
  ggplot(aes(y = stdres, x = mileage)) +
  geom_point(alpha=0.05) + 
  scale_y_continuous(breaks = seq(-3,3, by = 1))
# What is skewing our data in mileage?

## Why is it good that its roughly normally distributed 
# Linear assumptions 



economyprice <- economy %>% 
  filter(price < 44014)

mileagelm2 <- lm(price~mileage + manufacturer, economyprice)
summary(mileagelm2)

get_regression_summaries(mileagelm2)

# Showing how we can predict the price, given the mileage and the manufacturer
# Meaning mileage affects manufacturers differently - as shown how the Mitsubishi 
# depreciates the least

# Residual plot
lm2testpoints %>% 
  ggplot(aes(y = stdres, x = mileage)) +
  geom_point(alpha=0.05) + 
  scale_y_continuous(breaks = seq(-3,3, by = 1))


## QQ plot
lm2testpoints <- lm2testpoints %>% 
  mutate(stdres=rstandard(mileagelm2))


lm2testpoints %>% 
  ggplot(aes(sample=stdres))+
  geom_qq(color="light blue") +
  geom_qq_line(color="dark red") +
  labs(y = "Standardized Residuals", x = "Theorical") +
  ggtitle("QQ Plot for Car Depreciation with Mileage")

## added this to the slides
economyprice %>% 
  ggplot(aes(y = price, x = mileage, color = year)) +
  geom_point() +
  facet_wrap(~manufacturer) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Log10 Mileage of Car", y = "Log10 Price of Car") +
  theme(legend.position = c(0.65,0.15)) +
  ggtitle("Comparing Price and Mileage Over Time for Car Manufacturers")

# Because we know, when mileage increases, price decreases
# Mileage increases over time as you use the car
# So the cars that are not affected by mileage as much, will have the best depreciation (least)






# Proving that over time, mileage increases
economyprice %>%
  ggplot(aes(y = mileage, x = year)) +
  geom_point(color = "brown") +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Year", y = "Log10 Mileage of the Car") +
  ggtitle("Mileage Over Time")


################## ALL THE TRIAL AND ERROR ################## 


