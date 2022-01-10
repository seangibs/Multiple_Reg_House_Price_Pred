# make sure that R Studio has working directory set to folder/directory where data is
#install.packages("mass")

library(dplyr)
library(readr)
HouseDetails <- read_delim("HouseDetails.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)

# Q. The above did not work so had to use the below.
#HouseDetails <- read.delim("HouseDetails.txt")
## check for missing data and strange data Note, some data are numeric that should be factors

# Q. Why error on this line?
attach(HouseDetails)

# Q. What is this telling us apart from the stats on each item?
print( HouseDetails %>% count(bedrooms))
print( HouseDetails %>% count(fireplaces))
print( HouseDetails %>% count(heating))
print( HouseDetails %>% count(fuel))
print( HouseDetails %>% count(sewer))
print( HouseDetails %>% count(waterfront))
print( HouseDetails %>% count(newConstruction))
print( HouseDetails %>% count(age))

#reload the data with  newConstruction, centralAir, and waterfront, as logical
# code as factors the following   heating, fuel, sewer

# Q. Explanation on this for full clarity.
HouseDetails <- read_delim("HouseDetails.txt", 
                           "\t", escape_double = FALSE, col_types = cols(fireplaces = col_number(), 
                                                                         heating = col_factor(levels = c("electric", 
                                                                                                         "hot water/steam", "hot air")), 
                                                                         fuel = col_factor(levels = c("electric", 
                                                                                                      "gas", "oil")), sewer = col_factor(levels = c("septic", 
                                                                                                                                                    "public/commercial", "none")), 
                                                                         waterfront = col_character()), na = "NA", 
                           trim_ws = TRUE)

save(HouseDetails, file = "HouseDetails.RData")

# read HouseDetails.RData for next time

#examine some of  the variables using histograms

hist(livingArea);hist(age);hist(lotSize, breaks = 5)

# apply some basic models to start testing elements for regression


########################################################
###############Data Description#########################
########################################################

############Overall###############

str(HouseDetails)


############Price###############
HouseDetails <- HouseDetails[HouseDetails<45000]
#Positively skewed - number of outliers. These outliers are all for high prices. 
#Q. Should these outliers be removed?
ggplot(data=HouseDetails, aes(x = price/1000)) + geom_histogram(bins=50) + xlab("Price (x10^3)") + ylab("# House")

ggplot(data=HouseDetails, aes(y = price/1000)) + ylab("Price (x10^3)") + geom_boxplot(, outlier.shape=1,
                                                              outlier.size=3)


summary(price)

#Histogram of age. Added log 
#Q. What do we do with the NULL values? - na.omit = true
#Q. Reconfirmation that it is ok to use log
ggplot(data=HouseDetails, aes(x = age, na.omit = TRUE)) + geom_histogram(bins=50) + scale_x_log10() + xlab("Age") + ylab("# House")


#Histogram of lot size. We see here that lot size is fairly evenly distributed.
#Q. What do we do with outliers.
#Q. What does the non finite error message mean? - found NA and / = infinite
ggplot(data=HouseDetails, aes(x = lotSize)) + geom_histogram(bins=50) + scale_x_log10() + xlab("lot size (ac)") + ylab("# House")

#Histogram of land value
#Negatively skewed with outliers in the lower value of land. This tells us that most land value is more commonly expensive over the norm.
#Q. What do we do with outliers
ggplot(data=HouseDetails, aes(x = landValue)) + geom_histogram(bins=50) + scale_x_log10() + xlab("Land Value (x100)") + ylab("# House")

#Histogram of bathrooms
#Is a histogram the best way to display this data in describing it?
ggplot(data=HouseDetails, aes(x = bathrooms)) + geom_histogram(bins = 7) + xlab("# Bathrooms") + ylab("# House")
summary(bathrooms)

#Histogram of rooms
#Q. Is a histogram the best way to display this data in describing it?
#Q. Is it ok to call this a normal distribution given the number of outcomes - No because too total number of values are too small. Ordinal and not normal
ggplot(data=HouseDetails, aes(x = bathrooms)) + geom_histogram(bins = 5) + xlab("# Rooms") + ylab("# House")
summary(bathrooms)


#Histogram of bedrooms
#Q. Is a histogram the best way to display this data in describing it?
#Q. Is it ok to call this a normal distribution given the number of outcomes?
ggplot(data=HouseDetails, aes(x = bedrooms)) + geom_histogram(bins = 7) + xlab("# Bedrooms") + ylab("# House")
summary(bedrooms)



##############################################
################Comparing Data################
##############################################


# Visualization:
# Scatter plot between house price and lot Size: positive correlation
library(ggplot2)
ggplot(data=HouseDetails, aes(x = lotSize, y = price)) + geom_point() + geom_smooth(method="lm")

# Scatter plot between house price and bedrooms: positive correlation
ggplot(data=HouseDetails, aes(x = bedrooms, y = price)) + geom_point() + geom_smooth(method="lm")


# Scatter plot between house price and rooms: positive correlation
ggplot(data=HouseDetails, aes(x = rooms, y = price)) + geom_point() + geom_smooth(method="lm")

# Scatter plot between house price and bathrooms: positive correlation
ggplot(data=HouseDetails, aes(x = bathrooms, y = price)) + geom_point() + geom_smooth(method="lm")

# Scatter plot between house price and age: negative correlation
ggplot(data=HouseDetails, aes(x = age, y = price)) + geom_point() + geom_smooth(method="lm")

# Scatter plot between house price and landvalue: positive correlation
ggplot(data=HouseDetails, aes(x = landValue, y = price)) + geom_point() + geom_smooth(method="lm")

# Scatter plot between house price and percent of people in college: positive correlation
ggplot(data=HouseDetails, aes(x = pctCollege, y = price)) + geom_point() + geom_smooth(method="lm")


# Scatter plot between house price and number of fireplaces: positive correlation
# This is likely since the bigger the house the more fireplaces
ggplot(data=HouseDetails, aes(x = fireplaces, y = price)) + geom_point() + geom_smooth(method="lm")


# Scatter plot between house price and living area: positive correlation
ggplot(data=HouseDetails, aes(x = livingArea, y = price)) + geom_point() + geom_smooth(method="lm")

## Boxplot for categorical variable
ggplot(data=HouseDetails, aes(factor(heating), price)) + geom_boxplot() + ggtitle("House price by heating system") + xlab("Heating system") + ylab("Price")

ggplot(data=HouseDetails, aes(factor(fuel), price)) + geom_boxplot() + ggtitle("House price by fuel system") + xlab("Fuel system") + ylab("Price")

#different plot is required
ggplot(data=HouseDetails, aes(factor(sewer), price)) + geom_boxplot() + ggtitle("House price by sewer system") + xlab("Sewer system") + ylab("Price")

######################################################
################Develop a linear model################
######################################################

####################################
################Sean################
####################################

mod1 <- lm(formula = price ~., data=HouseDetails)
summary(mod1)

mod2 <- lm(formula = price ~ lotSize + landValue + livingArea + bathrooms + rooms + bedrooms + age, data=HouseDetails)
summary(mod2)
## Interpret model 2
#As number of rooms increases by 1, then the house price increases by $22572.9
# One more rooms in the house is associated with an increase of $22572.9 of house price, on average, holding other things constant. 
# p-value =0.000...02 < 0.05 -> the coefficient is statistically significant at 5% level.

mod3 <- lm(formula = price ~ livingArea, data=HouseDetails)
summary(mod3)

mod4 <- lm(formula = price ~ rooms + livingArea, data=HouseDetails)
summary(mod4)

#rooms + living area
mod5 <- lm(formula = price ~ lotSize + landValue + age + bathrooms, data=HouseDetails)
summary(mod5)

##############
#####0.72#####
#####BEST#####
##############
mod6 <- lm(formula = price ~ lotSize + landValue + age + bedrooms, data=HouseDetails)
summary(mod6)

#adding living area makes it worse
mod8 <- lm(formula = price ~ lotSize + landValue + age + bedrooms + livingArea, data=HouseDetails)
summary(mod8)

cor(lotSize,landValue,age,bedrooms)

#trial and error of others
summary(mod1) #for reference
#pctrCollege makes it worse
mod9 <- lm(formula = price ~ lotSize + landValue + livingArea + age + bathrooms + pctCollege, data=HouseDetails)
summary(mod9)

#fireplaces makes worse
mod10 <- lm(formula = price ~ lotSize + landValue + livingArea + age + bathrooms + fireplaces, data=HouseDetails)
summary(mod10)

#pctrCollege makes it worse
mod9 <- lm(formula = price ~ livingArea + pctCollege + bathrooms + fireplaces + landValue, data=HouseDetails)
summary(mod9)

#Q. In correlation, why was 0.7 our cutoff point?
cor(HouseDetails[1:10])

####################################
################David###############
####################################

mod1 <- lm(formula = price ~., data = HouseDetails)
summary(mod1)
house.reg <-lm(price~lotSize, data = HouseDetails)
summary(house.reg)
mod2 <- lm(formula = price ~ rooms, data=HouseDetails)
summary(mod2)
## Interpret model 2
#As number of rooms increases by 1, then the house price increases by $22572.9
# One more rooms in the house is associated with an increase of $22572.9 of house price, on average, holding other things constant. 
# p-value =0.000...02 < 0.05 -> the coefficient is statistically significant at 5% level.

mod3 <- lm(formula = price ~ livingArea, data=house)
summary(mod3)

mod4 <- lm(formula = price ~ rooms + livingArea, data=house)
summary(mod4)

#Q. What does the esitmated column indicate? - estimate as lotsize increases by 1 then the x value increases by 7.312e+03
mod5.reg <-lm(price~lotSize+ age+livingArea+landValue+bedrooms+rooms+bathrooms, data=HouseDetails)
summary(mod5.reg)


# multicollinearity issue in model 5 because the two variables are highly correlated and we need to use one of them
# VIF test
library(car)
vif(mod5.reg)

#Do some diagnostics on the model
shapiro.test(price)

# the results of the Shapiro-Wilks test indicates that Price is NOT normally distributed
# we might consider doing a log of the price.
# We will generate the residuals for the last model (using regression object)
# then plot the results
mod5.res<- resid(mod5.reg)
qqnorm(mod5.res); qqline(mod5.res)

# the whole model looks fair, but some elements may be confounding. 
# let us do a Variance Inflation Factor test. Predictors with an excesively high 
# factor (greater than 1/(1-R^2)  or 2.58 for this model, are suspect)
# Q. Where did we get the R for the above R^2?

vif(mod5.reg)

# well it seems that livingArea may be confounded with rooms

# let us do a multiple correlation of the first 9 predictors to check
cor(HouseDetails[c(2:10)], method=c("pearson"))

# there seems to be a high degree of correlation between living area and rooms
# also living area and bathrooms and bedrooms. All expected and all predicted
# by the vif test
library(MASS)
#OK, we try some more models, eliminating rooms, or bedrooms, or living area. 
# using log(price) and such
# Q. Was log(price) a trial and error or did we do it just to have normality?
# Q. I got a higher R^2 value on line 182.
# The best seems to be
mod6.reg <-lm(price~lotSize+age+landValue+livingArea+bathrooms, data=HouseDetails)
mod6.res <-studres(mod6.reg)
summary(mod6.reg)
qqnorm(mod6.res); qqline(mod6.res)

##NEW STUFF***   adding categorical predictors that have been declared as 
## factors in the import (can be done after import using as.factor())

mod7.reg <-lm(price~lotSize+age+landValue+livingArea+bathrooms
              +fuel+centralAir+sewer+waterfront, data=HouseDetails)
summary(mod7.reg)

# Getting close. fuel does not seem important, nor sewer. Last try
mod8.reg <-lm(price~lotSize+age+landValue+livingArea+bathrooms
              +centralAir+waterfront, data=HouseDetails)
summary(mod8.reg)
mod8.res <-studres(mod8.reg)
qqnorm(mod8.res); qqline(mod8.res)

#additional tests of the regression show that for the main data, the model is ok
# Q. What are these tests?

# transformation
mod9.reg <-lm(log(price)~lotSize+age+landValue+livingArea+bathrooms
              +centralAir+waterfront, data=HouseDetails)
summary(mod9.reg)
mod9.res <-studres(mod9.reg)
qqnorm(mod9.res); qqline(mod9.res)
