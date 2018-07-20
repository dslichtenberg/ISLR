library(ISLR)

names(Smarket)

dim(Smarket)
summary(Smarket)

cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

glm.fit <-  glm(Direction~ Lag1 + Lag2+Lag3+Lag4 + Lag5 + Volume, data =Smarket, family = binomial)
summary(glm.fit)

coef(glm.fit)

glm.probs <- predict(glm.fit, type = 'response')

glm.probs[1:10]

contrasts ( Direction )

glm.pred <- rep('Down',1250)
glm.pred[glm.probs > .5 ] <- 'Up'

table(glm.pred, Direction)

(507+145)/1250 == mean(glm.pred == Direction)

#Linear Discriminant Analysis

library(MASS)

lda.fit <- lda(Direction ~ Lag1  + Lag2 , data = Smarket, subset = Year < 2005)
summary(lda.fit)
lda.pred <- predict(lda.fit, Smarket[Year <2005,])

names(lda.pred)
lda.pred

table( lda.pred$class, Smarket[Year <2005,]$Direction)

#quadratic discriminant 

qda.fit <- qda(Direction ~ Lag1 + Lag2 , data = Smarket , subset= (Year <2005))
qda.class <- predict(qda.fit, Smarket[Smarket$Year ==2005,])$class
table(qda.class, Direction[Year ==2005])

mean(qda.class == Direction[Year ==2005])

#KNN
#class::knn()
#takes  4 args:
#1) A matrix containing the predictors associated with the training data 
#2) A matrix containing the predictors associated with the data for which
#       we wish to make predictions
#3) A vector containing the class labels for the training observations
#4) a value for K (number of neighbors)

train.X <- cbind(Lag1[Year < 2005],Lag2[Year < 2005])
test.X <- cbind(Lag1[!(Year < 2005)],Lag2[! (Year < 2005)])
train.Y <- Direction[(Year < 2005 )]

?set.seed(NULL)
knn.pred <- class::knn(train.X  , test.X, train.Y, k = 3)

table(knn.pred, Direction[!(Year < 2005)])  
mean(knn.pred == Direction[!(Year < 2005)])
