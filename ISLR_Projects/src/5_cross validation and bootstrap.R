#validation set 
library(ISLR)
set.seed(1)

train <- sample(392,196)

lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict (lm.fit, Auto))[-train]^2)

#poly models
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict (lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict (lm.fit3, Auto))[-train]^2)

#choose different training set
set.seed(2)
train <- sample(392,196)


#LEAVE ONE OUT CROSS-VALIDATION
#library: boot
lm.fit <- glm(mpg~horsepower,data = Auto)

cv.err <- boot::cv.glm(Auto, glm.fit)

cv.error <- rep(0,5)

for( i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower,i),data = Auto)
  cv.error[i] <- boot::cv.glm(Auto, glm.fit)$delta[1]
}


# k-Fold Cross-Validation
#boot library
set.seed(17)
cv.error.10 <- rep(0,10)
for( i in 1:10)
{
  glm.fit <- glm(mpg~poly(horsepower,i), data = Auto)
  cv.error.10[i] <- boot::cv.glm(Auto, glm.fit, K=10)$delta[1]
}


#bootstrap 
