# Classification Tree
library(rpart)

View(kyphosis)

###################################################################
# formula	is in the format 
# outcome ~ predictor1+predictor2+predictor3+ect.

# data=	specifies the data frame

# method=	"class" for a classification tree 
# "anova" for a regression tree

# control=	optional parameters for controlling tree growth. 
# For example, control=rpart.control(minsplit=30, cp=0.001) 
# requires that the minimum number of observations in a node be 30 
# before attempting a split and that a split must decrease the 
# overall lack of fit by a factor of 0.001 (cost complexity factor) 
# before being attempted.
###################################################################

# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)


###################################################################
# printcp(fit)	display cp table

# rsq.rpart(fit)	plot approximate R-squared and relative error for 
# different splits (2 plots). labels are only appropriate for the "anova" method.

# print(fit)	print results

# summary(fit)	detailed results including surrogate splits

# plot(fit)	plot decision tree

# text(fit)	label the decision tree plot
###################################################################
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)



# Regression Tree Example
library(rpart)

View(cu.summary)

# grow tree 
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
             method="anova", data=cu.summary)

summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


# Random Forest prediction of Kyphosis data
library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fit) # view results 
importance(fit) # importance of each predictor
