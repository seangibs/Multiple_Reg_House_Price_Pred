# everything
mod1 <- lm(formula = price ~ 
             lotSize +
             age +
             landValue +
             livingArea +
             pctCollege +
             bedrooms +
             fireplaces +
             bathrooms +
             rooms +
             heating +
             fuel +
             sewer +
             waterfront +
             newConstruction +
             centralAir
           
           , data=HouseDetails)

summary(mod1)

HouseDetails$pred = predict(mod9.reg, newdata = HouseDetails)

shapiro.test(HouseDetails$price - HouseDetails$pred)

plot(HouseDetails$price,HouseDetails$pred)

# remove heating
mod2 <- lm(formula = price ~ 
             lotSize +
             age +
             landValue +
             livingArea +
             pctCollege +
             bedrooms +
             fireplaces +
             bathrooms +
             rooms +
             fuel +
             sewer +
             waterfront +
             newConstruction +
             centralAir
           
           , data=HouseDetails)

summary(mod2)

# remove sewer
mod3 <- lm(formula = price ~ 
             lotSize +
             age +
             landValue +
             livingArea +
             pctCollege +
             bedrooms +
             fireplaces +
             bathrooms +
             rooms +
             fuel +
             waterfront +
             newConstruction +
             centralAir
           
           , data=HouseDetails)

summary(mod3)

# remove fuel
mod4 <- lm(formula = price ~ 
             lotSize +
             age +
             landValue +
             livingArea +
             pctCollege +
             bedrooms +
             fireplaces +
             bathrooms +
             rooms +
             waterfront +
             newConstruction +
             centralAir
           
           , data=HouseDetails)

summary(mod4)

# remove fireplace
mod5 <- lm(formula = price ~ 
             lotSize +
             age +
             landValue +
             livingArea +
             pctCollege +
             bedrooms +
             bathrooms +
             rooms +
             waterfront +
             newConstruction +
             centralAir
           
           , data=HouseDetails)

summary(mod5)


# remove pctCollege
mod5 <- lm(formula = price ~ 
             lotSize +
             age +
             landValue +
             livingArea +
             bedrooms +
             bathrooms +
             rooms +
             waterfront +
             newConstruction +
             centralAir
           
           , data=HouseDetails)

summary(mod5)

# check correlation
cor(HouseDetails[c(2:10)], method=c("pearson"))


# categorical correlation covered in desc analysis

library(ggplot2)
library(ggcorrplot)

r <- cor(HouseDetails[c(2:10)], method=c("pearson"))

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


# remove confounding variables (rooms, bedrooms)
mod5 <- lm(formula = price ~ 
             lotSize +
             age +
             landValue +
             livingArea +
             waterfront +
             newConstruction +
             centralAir
           
           , data=HouseDetails)

summary(mod5)

# multipling age * -1
mod6 <- lm(formula = price ~ 
             lotSize +
             age*-1 +
             landValue +
             livingArea +
             waterfront +
             newConstruction +
             centralAir
           
           , data=HouseDetails)

summary(mod6)

# applying log to price
mod7 <-lm(log(price)~
                lotSize+
                age*-1 +
                landValue +
                livingArea+
                bedrooms+
                centralAir+
                waterfront
              , data=HouseDetails_Data)

summary(mod7)

# removing outliers
HouseDetails_Data1 <- HouseDetails_Data[HouseDetails_Data$price < 456999, ]
HouseDetails_Data1 <- HouseDetails_Data[HouseDetails_Data$price > 5000, ]

mod8 <-lm(log(price)~
            lotSize+
            age*-1 +
            landValue +
            livingArea+
            bedrooms+
            centralAir+
            waterfront
          , data=HouseDetails_Data1)

summary(mod8)

# newConstruction & Age

HouseDetails_Data2 <- HouseDetails_Data[HouseDetails_Data$age < 2, "newConstruction"] <- "Yes"

HouseDetails_Data2 <- HouseDetails_Data[HouseDetails_Data$age > 2, "newConstruction"] <- "No"


mod9 <-lm(log(price)~
            lotSize+
            age*-1 +
            landValue +
            livingArea+
            bedrooms+
            centralAir+
            waterfront
          , data=HouseDetails_Data1)

summary(mod9)


