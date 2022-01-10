library(ggfortify)
library(ggplot2)
library(car)
library(graphics)
library(ggplot2)

# shuffling data order due to bug in R
set.seed(1234)
HouseDetails <- HouseDetails[order(runif(nrow(HouseDetails))), ]

HouseDetails_Data <- HouseDetails

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

# studentizing residuals
mod7.res <-studres(mod7)

summary(mod7.res)

# linearity

qqnorm(mod7); qqline(mod7)


# no autocorrelation of variables / independance of redisuals

durbinWatsonTest(mod7)


# the errors are independent


## Normality of residuals.
hist(mod7$residual)

# check for multicollinearity

vif(mod7)

# homoscedasticity 

HouseDetails_Data$pred = predict(mod9.reg, newdata = HouseDetails_Data)

shapiro.test(HouseDetails_Data$price - HouseDetails_Data$pred)

plot(HouseDetails_Data$price,HouseDetails_Data$pred)







