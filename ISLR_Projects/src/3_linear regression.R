library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

lm.fit <- lm(medv~lstat, data= Boston)

summary(lm.fit)

coef(lm.fit)
confint(lm.fit)

#THE MEAN LIES WITHIN THE CONFIDENCE 
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = 'confidence')

#ANY DATA POINT (95%) LIE WITHIN THE PREDICTION INTERVAL
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = 'prediction')

attach(Boston)
plot ( lstat , medv)
abline(lm.fit)

abline ( lm.fit , lwd =3)
abline ( lm.fit , lwd =3 , col =" red ")
plot ( lstat , medv , col =" red ")
plot ( lstat , medv , pch =2)
plot ( lstat , medv , pch ="+")
plot (1:20 ,1:20 , pch =1:20)

par ( mfrow = c (2 ,2) )
plot ( lm.fit )

plot ( predict ( lm.fit ) , residuals ( lm.fit ) )

plot ( predict ( lm.fit ) , rstudent ( lm.fit ) )

#leverage stats
plot ( hatvalues ( lm.fit ) )
which.max ( hatvalues ( lm.fit ) ) #index of largest leverage

#multiple regression
mlm.fit <- lm(medv~lstat+age, data = Boston[,1:14])
summary(mlm.fit)

lm.fit  <- lm(medv ~ . , data= Boston[,1:14])
summary(lm.fit)

#variance inflation (in car package)
library ( car )
?vif(lm.fit)

#interaction terms
summary ( lm ( medv~lstat * age , data = Boston [,1:14]) )
 # I() is used to escape the squared term
summary(lm(medv~lstat+ I(lstat^2), data= Boston))

anova(lm.fit, lm(medv~lstat+ I(lstat^2), data= Boston))


par(mfrow = c(2,2))
plot(lm(medv~lstat+ I(lstat^2), data= Boston))


#try all polys up to 5 
lm.fit5 <- lm(medv ~ poly(lstat,5), data = Boston)
summary(lm.fit5)

summary ( lm ( medv ~log ( rm ) , data = Boston ) )
