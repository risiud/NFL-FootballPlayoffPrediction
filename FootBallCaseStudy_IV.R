## Case Study 1 NFL Playoff Score Predictor - Spring 2013 
## David Risius
## This project explores multiple models to predict the scores of post-season NFL games given
##  historical information
## Read in the csv file 
Playoffs <- read.csv("NFL_Playoff_Data11.csv", header = T)
#Load packages
library(car)
library(leaps)
library(MPV)
library(boot)

## Part 1: Plots and Legends (Exploratory Analysis) 
names(Playoffs)
range(Playoffs$PtsScored)
mean(Playoffs$PtsScored)
median(Playoffs$PtsScored)

pairs(Playoffs[,c(1,2,3,4,5,6,7,8)])
pairs(Playoffs[,c(1,9,10,11,12,13,14,15,16)])

#Make a boxplot for points scored in home and away games.
par(mfrow=c(1,1))
plot(Playoffs$HomeAway, Playoffs$PtsScored, ylab = "Points Scored")
title(main="Playoff Points Scored for Home and Away Games")

#One-way ANOVA table for Home or Away games.
HomeAway.lm <- lm(Playoffs$PtsScored~HomeAway, data = Playoffs)
anova(HomeAway.lm)

plot(Playoffs$Avg_Pts_G, Playoffs$PtsScored, xlab = "Average Points Per Game (Reg Season)", ylab = "Points Scored")
title(main = "Playoff Points Scored v. Average Points per Game in Regular Season")

Playoffs$HomeAway
class(Playoffs$HomeAway)
as.numeric(Playoffs$HomeAway)

mycols <- c("red", "steelblue1")
plot(Playoffs$Avg_Pts_G, Playoffs$PtsScored, 
     xlab = "Average Points per Game (Regular Season)", ylab = "Points Scored in Playoffs", 
     pch = 19, col = mycols[as.numeric(Playoffs$HomeAway)]) #assigns colors to the different cities
title(main = "Points Scored in Playoffs v. Avg Points per Game")

legend (locator(1), legend=levels(Playoffs$HomeAway), col = mycols, pch = 19) #adds a legend to the current plot

## Part 2: Re-order the Regression Model with Categorical Variables 

PlayoffReorder <- Playoffs[order(Playoffs$Avg_Pts_G),]
PlayoffReorder
#Original Model from Case Study I
Playoff.lm.orig <- lm(PlayoffReorder$PtsScored~Avg_Pts_G+Rsh_Yds_Gm+Rec_Yds_Gm+QB_RTG+Def_Pts_G+Def_Yards_Play+HomeAway,data = PlayoffReorder)
summary(Playoff.lm.orig)
pairs(Playoffs[c(1,3,7,9,2,11,13,16)])
#lets try some crazy stuff on the original model
#make a couple new combined variables
y <- Playoffs$PtsScored
x1 <- Playoffs$Rec_Yds_Gm*.01*Playoffs$QB_RTG/Playoffs$Avg_Pts_G
x1
x2 <- Playoffs$Def_Pts_G/Playoffs$Def_Yards_Play
x3 <- Playoffs$Rsh_Yds_Gm
x4 <- Playoffs$HomeAway
PlayoffsCombined <- data.frame(y,x1, x2, x3, x4)
PlayoffsCombined
Playoff.lm.combined <- lm(y~x1+x2+x3+x4, data = PlayoffsCombined)
summary(Playoff.lm.combined)
pairs(PlayoffsCombined)
Playoff.lm.orig1 <- lm(PlayoffReorder$PtsScored~(Rec_Yds_Gm*QB_RTG/Avg_Pts_G)+Rsh_Yds_Gm+(Def_Pts_G/Def_Yards_Play)+HomeAway, data = PlayoffReorder)
summary(Playoff.lm.orig1)

#New Model are going to try with updated data(use NFL_Playoff_Data5.csv)
#Playoff.lm1 <- lm(PtsScored~Avg_Pts_G+Def_Pts_Gm+QB_RTG+Pass_Yds_G+Rsh_Yds_Gm+ Rsh_FUM+Rec_Yds_Gm+Rec_FUM+Def_Yds_Gm+Def_FUM+HomeAway,data = PlayoffReorder) 

#First lets try to fit a new model taking out som of the old variables(Avg_Pts_GM, Rec_TD,Rsh_TD) uses version 2 of csv.

#Playoff.lm1 <- lm(PlayoffReorder$PtsScored~., data = PlayoffReorder) #uses all the terms in csv version 6
Playoff.lm1 <- lm(Playoffs$PtsScored~Rsh_Yds_Gm+Rec_Yds_Gm+QB_RTG+Def_Yards_Play+HomeAway,data = Playoffs)
summary(Playoff.lm1)
Playoff.lm1.2F <- lm(Playoffs$PtsScored~(Rsh_Yds_Gm+Rec_Yds_Gm+QB_RTG+Def_Yards_Play+HomeAway)^2,data = Playoffs)
summary(Playoff.lm1.2F)
Playoff.lm1a <-lm(Playoffs$PtsScored~.-(Rsh_Yds_Gm+Rec_Yds_Gm), data = Playoffs)
summary(Playoff.lm1a)

#Like lm1 except combined Rsh_Yds_gm and Rec_Yds_gm into total yards_gm
Playoff.lm1b <- lm(Playoffs$PtsScored~Tot_Yards_Gm+QB_RTG+Def_Yards_Play+HomeAway,data = Playoffs)
summary(Playoff.lm1b)

#Try to fit as many regressors and 2 factor interactions as possible
Playoff.lm1c <- lm(Playoffs$PtsScored~(Avg_Pts_G+QB_RTG+Pass_Int+Rec_Avg+Tot_FUM+Def_Pts_G+Def_Plys+Def_FUM+Tot_Yards_Gm+HomeAway)^2, data = Playoffs)
summary(Playoff.lm1c)

#check out the residuals.
plot(Playoff.lm1, add.smooth = F)
plot(Playoff.lm1b, add.smooth=F)
#seems to be a terrible model.  Lets try a new one
Playoff.lm2 <- lm(PlayoffReorder$PtsScored~Avg_Pts_G+Rec_Yds_Gm+QB_RTG+HomeAway,data = PlayoffReorder)
summary(Playoff.lm2)
#still shitty
#Playoff.lm3 <- lm(PlayoffReorder$PtsScored~Avg_Pts_G,data = PlayoffReorder)
#summary(Playoff.lm3)
#shitty
#Playoff.lm4 <- lm(PlayoffReorder$PtsScored~Rsh_Yds_Gm+Rec_Yds_Gm+QB_RTG+Def_Yards_Play+HomeAway,data = PlayoffReorder)
#summary(Playoff.lm4)
#shitty too
#Playoff.lm5 <- lm(PlayoffReorder$PtsScored~Avg_Pts_G+QB_RTG+HomeAway,data = PlayoffReorder)
#summary(Playoff.lm5)

#Lets drop each variable one by one to see if our model gets better.
drop1(Playoff.lm1, test = "F")
anova(Playoff.lm1, test = "F")
#compare models Playoff.lm1 and Playoff.lm2
anova(Playoff.lm2, Playoff.lm1)

#let's create a vector of the predicted scores 
pdt <- predict(Playoff.lm1)
pdt
#now we will add our predictions to the graph 

## Part 3: Confidence Intervals and Prediction Intervals 

#Here is the function to get this without having to do it manually! 
confint(Playoff.lm1)

#Let's create confidence intervals on our y hats 
pdt <- predict(Playoff.lm1, interval = "confidence")
pdt

## Case Study commands based on lab 4
## check the case study to see if we have violated any of the assumptions.
## Also make sure to load the car package prior to running these commands 

## Part 1 of Lab 4 - Residual Diagnostics 


#Now we will ask R for the residual plots to test for adequacy 
plot(Playoff.lm1, add.smooth = F)
#Notice on Residuals vs fitted plot.  Variance appears to cone out (violation of constant var assumption)
#Normal Q-Q plot seems to look okay.
#Go back to residuals vs. fitted if we connect the dots, would the lines apear random? maybe


#We could also check out the diagnostic tests for normality and correlated residuals 
shapiro.test(residuals(Playoff.lm1))
#Notice after Shapiro test that our p-value is very high which shows that we fail to reject that our residuals are normally distributed.
durbinWatsonTest(Playoff.lm1)

#In addition to the residual plots from the plot() function we can get the 
#partial residuals by using the termplot() function 
#changed residual color to blue to make it easier to see
termplot(Playoff.lm1, partial.resid = T, col.res='blue')

## Part 2 - Lab 4 - Cook's Distance 


#now let's check out the residual plots 
plot(Playoff.lm1, add.smooth = F)
#Looks like in Residual vs. Fitted Plot, we have some bad data (a curve)
#Leverage points are those which exceed high leverage is h> 2*p/n
2*(10/35) #basically all the points to the right of this are high leverage points.
#point 39 has a cookes value above 1.  Influences the slope of the regression line, 

#We can learn info from the residual plots, but also ask for leverage/outlier info
cooks.distance(Playoff.lm1)[1:5]
max(cooks.distance(Playoff.lm1)) #can see from plot is associated with point 35
which(cooks.distance(Playoff.lm1) == max(cooks.distance(Playoff.lm1))) #tells us that the point is 8 and 27

Playoff.influence <- influence.measures(Playoff.lm1) #
Playoff.influence #Gives a bunch of rows of data and puts asterisks by the points considered high leverage.
#conclusion for this part looking for high leverage and influential points.

## Part 3 - Lab 4 - Multicollinearity 

#We can look at the correlation between pairs of variable with this command 
cor(model.matrix(Playoff.lm1)[,-1]) #contains all the variables we've already included in model.  Leaves out the first column of ones.
#we can look down columns to see how each value correlates.  Looks like receiving TDs, Rec_yds_game are very highly correlated.
#As is QB Rating and AVG_pts per game and receiving TDs
#We can ask for the variance inflation factors using this comand 
vif(Playoff.lm1) #looking to see if any of the variance inflation values are above 10
#can see that we have multiple variable above 10 which shows we have multicolinearity.

#how do we do this manually?  Run a new linear model below.
Playoffs.QBRTG <- lm(QB_RTG~.-PtsScored, data = Playoffs)
summary(Playoffs.QBRTG)
names(summary(Playoffs.QBRTG))

#Let's look at the variance inflation factors to see if we have multicollinearity
#I will also show how this calculation is done without the vif() function 
vif(Playoff.lm1)
QB_RTG.vif <- 1 / (1 - summary(Playoffs.QBRTG)$r.squared)
QB_RTG.vif #so inf is the vif for QB_Rating.  Important because if variance is inflated,
#we might fail to reject the null which is a type II error.

#Homework 6.  Principle Component Analysis
Playoff.pc <-prcomp(Playoffs[,c(2:15)])
#Playoff.pc <- prcomp(Playoffs[,c(7,9,2,13)]) #running principal components on only continuous variables in Playoff.lm1 model
Playoff.pc

Playoff.pc$rotation #gets only the loadings
Playoff.pc$x[c(1:5),] ## shows only the first 5 rows of the z values

Playoff.pc.Data <- data.frame(PtsScored = Playoffs$PtsScored, Playoff.pc$x)
Playoff.pc.Data[c(1:5),]

colnames(Playoff.pc.Data) <- c("PtsScored", "z1", "z2", "z3", "z4","z5","z6","z7","z8","z9","z10","z11","z12","z13","z14") #Change the column names
Playoff.pc.Data[c(1:5),]

Playoff.pc.lm <- lm(PtsScored~., data = Playoff.pc.Data) #using all variables
#Playoff.pc.lm <- lm(PtsScored~z1 + z2 + z3 + z4, data = Playoff.pc.Data) #using the new model with z1 z2,z3 z4, while forcing HomeAway in Model
summary(Playoff.pc.lm) ## why so bad? need other variables!!! 
summary(Playoff.lm1) #compare to original
vif(Playoff.pc.lm) #should all be one and they are

## Part 2 - Variable Selection 
#First fit the model with all variables 
# Regession subsets with regsubsets() 
Playoff.Subset <- regsubsets(PtsScored~.-HomeAway, data = Playoffs, nbest = 5) #selects the five best models from each subset
summary(Playoff.Subset)
#two different models to compare
Playoff.Subset.lm1 <- lm(PtsScored~QB_RTG+Pass_Yds+Def_Yards_Play+HomeAway, data = Playoffs)
Playoff.Subset.lm2 <- lm(PtsScored~Avg_Pts_G+Def_Yards_Play+HomeAway, data = Playoffs)
summary(Playoff.Subset.lm1)
summary(Playoff.Subset.lm2)
plot(Playoff.Subset, scale = "adjr2")
## Backwards elimination with stepAIC() Will run this for case study for backwards main effects only and me with interaction
#Playoff.lm2 <- lm(PlayoffReorder$PtsScored~(Tot_Yards_Gm+QB_RTG+Def_Yards_Play+HomeAway)^2,data = PlayoffReorder)
#summary(Playoff.lm2) #doesn't look too good

Playoff.backward1 <- step(Playoff.lm1c, scope=~., direction = "backward", trace = F) #all main effects
Playoff.backward2 <- step(Playoff.lm1b, scope=~.^2, direction = "backward", trace = F) #all main effects plus main effects with two factor interaction
Playoff.backward3 <- step(Playoff.lm1.2F, scope=~., direction= 'backward', trace = F)
Playoff.backward1a <-lm(Playoffs$PtsScored ~ QB_RTG + Pass_Int + 
                          Def_Plys + Def_FUM + HomeAway + QB_RTG:Rec_Avg + Pass_Int:Rec_Avg  + 
                          Rec_Avg:Def_Plys + Rec_Avg:Def_FUM + 
                          Def_Plys:HomeAway, data = Playoffs) #used this one as a base in the final model candidates
summary(Playoff.backward1t)
drop1(Playoff.backward1t, test = "F")
summary(Playoff.backward1)
summary(Playoff.backward1a)
summary(Playoff.backward2)
summary(Playoff.backward3) #lets fit this one except taking out HomeAwayHome (P-val =.3)
Playoff.backward3a <- lm(Playoffs$PtsScored~Rsh_Yds_Gm+Rec_Yds_Gm+QB_RTG+Def_Yards_Play+Rsh_Yds_Gm:QB_RTG+Rec_Yds_Gm:HomeAway+QB_RTG:HomeAway, data = Playoffs)
summary(Playoff.backward3a)
##Forwards regression with stepAIC() 
Playoff.null <- lm(PtsScored~1, data = Playoffs)
Playoff.null
Playoff.forward <- step(Playoff.null, scope=~(Tot_Yards_Gm+QB_RTG+Avg_Pts_G+ Pass_Yds+Pass_Int+Rsh_Yds_Gm+Rec_Avg+Rec_Yds_Gm+Tot_FUM
   +Def_Pts_G+Def_Plys+Def_Yards_Play+Def_Pen_Yds+Def_FUM+HomeAway)^2, direction = "forward", trace = T )
summary(Playoff.forward)
drop1(Playoff.forward, test = "F") #That that the only variable which seems to have a whole lot of significance is recieving yards per game

##Mixed stepwise regression with stepAIC()
Playoff.mixed <- step(Playoff.lm1, direction = "both", scope=~(QB_RTG+Avg_Pts_G+Rsh_Yds_Gm+Rec_Yds_Gm+Def_Yards_Play+HomeAway+Def_Plys+Def_FUM+Tot_Yards_Gm)^2, trace = T)
summary(Playoff.mixed)

##Lab 8 using case study data
args(aov) #gives the arguments for the functions of aov
CS.OW.ANOVA <- aov(PtsScored~HomeAway, data = Playoffs)
CS.OW.ANOVA 
summary(CS.OW.ANOVA)
#Reject the null hypothesis that there are no differences between home and away.
##Comparison of All models fit so far
summary(Playoff.lm.orig)
drop1(Playoff.lm.orig, test = "F")
summary(Playoff.lm1)
summary(Playoff.lm1a)
summary(Playoff.lm1b)
summary(Playoff.lm1c)
summary(Playoff.Subset.lm1) #obvious overfit (see adjusted R-Squared)
summary(Playoff.Subset.lm2) #Candidate
summary(Playoff.backward1)
summary(Playoff.backward1a) #Candidate
summary(Playoff.backward2) #Candidate
summary(Playoff.forward)
summary(Playoff.mixed) #Candidate
###########################################Model Assumption Testing for Final Three Models##################################3
##########################################Lets try using backward1a (Model3)###############################
pairs(Playoffs[,c(1,2,6,12,15)])
summary(Playoff.backward1a)
plot(Playoff.backward1a, add.smooth=F) #Everything seems to be okay here.
#We could also check out the diagnostic tests for normality and correlated residuals 
shapiro.test(residuals(Playoff.backward1a))
#Notice after Shapiro test that our p-value is very high which shows that we fail to reject that our residuals are normally distributed.
durbinWatsonTest(Playoff.backward1a)
#We can look at the correlation between pairs of variable with this command 
cor(model.matrix(Playoff.backward1a)[,-1]) #contains all the variables we've already included in model.  Leaves out the first column of ones.
#We can ask for the variance inflation factors using this comand 
vif(Playoff.backward1a) #looking to see if any of the variance inflation values are above 10
#Leverage points are those which exceed high leverage is h> 2*p/n
2*(11/95) #basically all the points to the right of this are high leverage points.

#We can learn info from the residual plots, but also ask for leverage/outlier info
cooks.distance(Playoff.backward2)[1:5]
max(cooks.distance(Playoff.backward2)) #can see from plot is associated with point 35
which(cooks.distance(Playoff.backward2) == max(cooks.distance(Playoff.backward2))) #tells us that the point is 8 and 27

###############################For backward 2 assumptions (Model 1)#######################################
pairs(Playoffs[,c(1,2,13)])
plot(Playoff.backward2, add.smooth=F) #Everything seems to be okay here.
#We could also check out the diagnostic tests for normality and correlated residuals 
shapiro.test(residuals(Playoff.backward2))
#Notice after Shapiro test that our p-value is very high which shows that we fail to reject that our residuals are normally distributed.
durbinWatsonTest(Playoff.backward2)
#We can look at the correlation between pairs of variable with this command 
cor(model.matrix(Playoff.backward2)[,-1]) #contains all the variables we've already included in model.  Leaves out the first column of ones.
#We can ask for the variance inflation factors using this comand 
vif(Playoff.backward2) #looking to see if any of the variance inflation values are above 10
#Leverage points are those which exceed high leverage is h> 2*p/n
2*(3/96) #basically all the points to the right of this are high leverage points.

#We can learn info from the residual plots, but also ask for leverage/outlier info
cooks.distance(Playoff.backward2)[1:5]
max(cooks.distance(Playoff.backward2)) #can see from plot is associated with point 35
which(cooks.distance(Playoff.backward2) == max(cooks.distance(Playoff.backward2))) #tells us that the point is 8 and 27

###############For forward model assumptions(Model 2)############################################
pairs(Playoffs[,c(1,9,13,17)])
plot(Playoff.forward, add.smooth=F) #Everything seems to be okay here.
#We could also check out the diagnostic tests for normality and correlated residuals 
shapiro.test(residuals(Playoff.forward))
#Notice after Shapiro test that our p-value is very high which shows that we fail to reject that our residuals are normally distributed.
durbinWatsonTest(Playoff.forward)
#We can look at the correlation between pairs of variable with this command 
cor(model.matrix(Playoff.forward)[,-1]) #contains all the variables we've already included in model.  Leaves out the first column of ones.
#We can ask for the variance inflation factors using this comand 
vif(Playoff.forward) #looking to see if any of the variance inflation values are above 10
#Leverage points are those which exceed high leverage is h> 2*p/n
2*(4/97) #basically all the points to the right of this are high leverage points.

#We can learn info from the residual plots, but also ask for leverage/outlier info
cooks.distance(Playoff.forward)[1:5]
max(cooks.distance(Playoff.forward)) #can see from plot is associated with point 35
which(cooks.distance(Playoff.forward) == max(cooks.distance(Playoff.backward2))) #tells us that the point is 8 and 27


####################Validation#######################################################
##Validation step (Lets validate our model)  How well will this model do at predicting new observations
#we will do cross validation (withhold some data and run the predictions on this)
test.rows <- sample(1:95, 15,replace = F) # Taking out a sample about 10% of n. comment out so we don't keep recalculating every time we run the script
test.rows #vector of the 10 sample points taken from n.  Replace = F means we don't take the same numbers twice.
Playoffs.main <- data.frame(Playoffs[-test.rows,]) #remember want about 10-25 percent for testing.  New data frame minus the testing rows
Playoffs.test <- data.frame(Playoffs[test.rows,]) # New data frame from the original data using only the testing rows.
Playoffs.test
Playoffs.main

#Rewrite int the correct format.  For example using final.plasma.1 stepwise regression, we have to fit the linear model

#Backwards2 Model
FinalPlayoff.1 <- lm(Playoffs$PtsScored~QB_RTG + Def_Yards_Play, data = Playoffs)
summary(FinalPlayoff.1)

#########################Mixed and Forward Model (Same)#################################

FinalPlayoff.2 <- lm(Playoffs$PtsScored~Rec_Yds_Gm+Def_Yards_Play+HomeAway, data = Playoffs)
summary(FinalPlayoff.2)
##Assumption 1 Error term has zero mean
mean(FinalPlayoff.2$residuals) #check
par(mfrow = c(1,1))
pairs(Playoffs[,c(1,9,13,17)])

plot(FinalPlayoff.2, add.smooth=F) #Everything seems to be okay here

par(mfrow=c(1,3))
termplot(FinalPlayoff.2, partial.resid=TRUE, col.res = "blue")
#We could also check out the diagnostic tests for normality and correlated residuals 
shapiro.test(residuals(FinalPlayoff.2))
#Notice after Shapiro test that our p-value is very high which shows that we fail to reject that our residuals are normally distributed.
durbinWatsonTest(FinalPlayoff.2)
#We can look at the correlation between pairs of variable with this command 
cor(model.matrix(FinalPlayoff.2)[,-1]) #contains all the variables we've already included in model.  Leaves out the first column of ones.
#We can ask for the variance inflation factors using this comand 
vif(FinalPlayoff.2)
#See if drop1 and anova are different for the HomeAway variable
drop1(FinalPlayoff.2, test = "F")
anova(FinalPlayoff.2)
#Leverage points are those which exceed high leverage is h> 2*p/n
2*(4/97) #basically all the points to the right of this are high leverage points.

#We can learn info from the residual plots, but also ask for leverage/outlier info
cooks.distance(FinalPlayoff.2)[1:5]
max(cooks.distance(FinalPlayoff.2)) #can see from plot is associated with point 35
which(cooks.distance(FinalPlayoff.2) == max(cooks.distance(FinalPlayoff.2))) #tells us that the point is 8 and 27


#Backwards1a Model
FinalPlayoff.3 <- Playoff.backward1a
FinalPlayoff.3 <- lm(PtsScored ~ QB_RTG + Pass_Int + Def_Plys + Def_FUM + HomeAway + QB_RTG:Rec_Avg + Pass_Int:Rec_Avg  + 
                           Rec_Avg:Def_Plys + Rec_Avg:Def_FUM + Def_Plys:HomeAway, data = Playoffs)
summary(FinalPlayoff.3)
mean(FinalPlayoff.3$residuals) #check
par(mfrow = c(1,1))
pairs(Playoffs[,c(1,2,6,12,15)])

plot(FinalPlayoff.3, add.smooth=F) #Everything seems to be okay here

par(mfrow=c(1,3))
termplot(FinalPlayoff.3, partial.resid=TRUE, col.res = "blue")
#We could also check out the diagnostic tests for normality and correlated residuals 
shapiro.test(residuals(FinalPlayoff.3))
#Notice after Shapiro test that our p-value is very high which shows that we fail to reject that our residuals are normally distributed.
durbinWatsonTest(FinalPlayoff.3)
#We can look at the correlation between pairs of variable with this command 
cor(model.matrix(FinalPlayoff.3)[,-1]) #contains all the variables we've already included in model.  Leaves out the first column of ones.
#We can ask for the variance inflation factors using this comand 
vif(FinalPlayoff.3)
drop1(FinalPlayoff.3, test = "F")
anova(FinalPlayoff.3)

#Leverage points are those which exceed high leverage is h> 2*p/n
2*(12/97) #basically all the points to the right of this are high leverage points.

#We can learn info from the residual plots, but also ask for leverage/outlier info
cooks.distance(FinalPlayoff.3)[1:5]
max(cooks.distance(FinalPlayoff.3)) #can see from plot is associated with point 35
which(cooks.distance(FinalPlayoff.3) == max(cooks.distance(FinalPlayoff.3))) #tells us that the point is 8 and 27
###########Manual Cross Validation############################

#Fit new model with new data frame minus the test data.  Will attempt to predict the scores for the ten test observations we took out
FinalPlayoff.1Main <- lm(Playoffs.main$PtsScored~QB_RTG + Def_Yards_Play, data = Playoffs.main)
summary(FinalPlayoff.1Main)
predict.test1 <- predict(FinalPlayoff.1Main, newdata = Playoffs.test) #making predictions on the test data that we withheld
RSPE1 = sqrt(sum((Playoffs.test$PtsScored-predict.test1)^2) / 10) #Predicting the RSPE.  This should be close to the RSE on our original model.
RSPE1
#Calculate the difference between the predicted and actual data.
abs(RSPE1-10.16)/10.16*100 #about 6 percent off.
#Get Press Statistic
PRESS.final.P1 <- PRESS(FinalPlayoff.1)
PRESS.final.P1 #This is basically the SSres which will will use to calculate our R squared (R2)

##R2=1-PRESS/SStotal
#R2 = 1 - (SS res / SS total)
#SS total = ((RSE^2)*(n-p))/(1-R2)
summary(FinalPlayoff.1)
SSTotal.P1 <- ((10.16^2)*92)/(1-.1227) #Used RSE, R2, and n-p in equation above from summary statistics.
R2.P1.Pred <- 1- (PRESS.final.P1)/(SSTotal.P1)
R2.P1.Pred #says .071, which says how much variability in the response is explained by the model in the presence of new data.
#Would like as high as possible and between R2 and R2 adjusted and this is pretty low

###Model 2 #############################3
#we will do cross validation (withhold some data and run the predictions on this)

#Rewrite int the correct format.  For example using final.plasma.1 stepwise regression, we have to fit the linear model
FinalPlayoff.2a <- glm(PtsScored~Rec_Yds_Gm+Def_Yards_Play+HomeAway,family = gaussian, data = Playoffs)
summary(FinalPlayoff.2a)
cv.glm(data=Playoffs, FinalPlayoff.2a, K=6)$delta
sqrt(101.64)
sqrt(101.04)

FinalPlayoff.2Main <- lm(Playoffs.main$PtsScored~Rec_Yds_Gm+Def_Yards_Play+HomeAway, data = Playoffs.main)
summary(FinalPlayoff.2Main)
predict.test2 <- predict(FinalPlayoff.2Main, newdata = Playoffs.test) #making predictions on the test data that we withheld
RSPE2 = sqrt(sum((Playoffs.test$PtsScored-predict.test2)^2) / 15)
RSPE2
#Calculate the percentate difference between RSE and RSPE.
abs(10.24-RSPE2)/10.24*100 #This is about 21 percent better on the predicted model than the actual fit.

#Get Press Statistic
PRESS.final.P2 <- PRESS(FinalPlayoff.2)
PRESS.final.P2


summary(FinalPlayoff.2)
##R2=1-PRESS/SStotal
#R2 = 1 - (SS res / SS total)
#SS total = ((RSE^2)*(n-p))/(1-R2)
SSTotal.P2 <- ((9.965^2)*91)/(1-.1656)
R2.P2.Pred <- 1- (PRESS.final.P2)/(SSTotal.P2)
R2.P2.Pred #says .0984, which says how much variability in the response is explained by the model in the presence of new data.
#Would like as high as possible and between R2 and R2 adjusted and this is pretty low

###Model 3 #############################
FinalPlayoff.3a <- glm(PtsScored ~ QB_RTG + Pass_Int + Def_Plys + Def_FUM + HomeAway + QB_RTG:Rec_Avg + Pass_Int:Rec_Avg  + 
                         Rec_Avg:Def_Plys + Rec_Avg:Def_FUM + Def_Plys:HomeAway, family = gaussian, data = Playoffs)

summary(FinalPlayoff.3a)
cv.glm(data=Playoffs, FinalPlayoff.3a, K=6)$delta
sqrt(98.28)
sqrt(95.71)

#Manual Method
FinalPlayoff.3Main <- lm(Playoffs.main$PtsScored ~ QB_RTG + Pass_Int + Def_Plys + Def_FUM + HomeAway + QB_RTG:Rec_Avg + Pass_Int:Rec_Avg  + 
    Rec_Avg:Def_Plys + Rec_Avg:Def_FUM + Def_Plys:HomeAway, data = Playoffs.main)
summary(FinalPlayoff.3Main)
predict.test3 <- predict(FinalPlayoff.3Main, newdata = Playoffs.test) #making predictions on the test data that we withheld
RSPE3 = sqrt(sum((Playoffs.test$PtsScored-predict.test3)^2) / 15)
RSPE3
#Calculate how far the predicted value is off from the model
abs(8.988-RSPE3)/8.988*100 #about 2.8 percent off

#Get Press Statistic
PRESS.final.P3 <- PRESS(FinalPlayoff.3)
PRESS.final.P3
summary(FinalPlayoff.3)
SSTotal.P3 <- ((9.119^2)*84)/(1-.3549)
R2.P3.Pred <- 1- (PRESS.final.P3)/(SSTotal.P3)
R2.P3.Pred #says .0984, which says how much variability in the response is explained by the model in the presence of new data.
#Would like as high as possible and between R2 and R2 adjusted and this is pretty low. Want at about .2  Fucking sucks.

##Model 4 Hugh model with lots of terms but all p-values under .05 lets just try it (backward1o).

FinalPlayoff.4 <-lm(Playoffs$PtsScored ~ QB_RTG + Pass_Int + 
                          Def_Pts_G + Def_Plys + Def_FUM + Tot_Yards_Gm + 
                          HomeAway + QB_RTG:Rec_Avg + QB_RTG:Tot_Yards_Gm + 
                          Pass_Int:Rec_Avg  + Rec_Avg:Def_Pts_G + Rec_Avg:Def_Plys + Rec_Avg:Def_FUM + 
                          Rec_Avg:Tot_Yards_Gm + 
                          Def_Plys:HomeAway, data = Playoffs)
summary(FinalPlayoff.4)

plot(FinalPlayoff.4, add.smooth=F) #Everything seems to be okay here.
#We could also check out the diagnostic tests for normality and correlated residuals 
shapiro.test(residuals(FinalPlayoff.4))
#Notice after Shapiro test that our p-value is very high which shows that we fail to reject that our residuals are normally distributed.
durbinWatsonTest(FinalPlayoff.4)
#We can look at the correlation between pairs of variable with this command 
cor(model.matrix(FinalPlayoff.4)[,-1]) #contains all the variables we've already included in model.  Leaves out the first column of ones.
#We can ask for the variance inflation factors using this comand 
vif(FinalPlayoff.4) #looking to see if any of the variance inflation values are above 10
#Leverage points are those which exceed high leverage is h> 2*p/n
2*(16/97) #basically all the points to the right of this are high leverage points.

FinalPlayoff.4Main <-lm(Playoffs.main$PtsScored ~ QB_RTG + Pass_Int + Def_Pts_G + Def_Plys + Def_FUM + Tot_Yards_Gm + 
                      HomeAway + QB_RTG:Rec_Avg + QB_RTG:Tot_Yards_Gm + 
                      Pass_Int:Rec_Avg  + Rec_Avg:Def_Pts_G + Rec_Avg:Def_Plys + Rec_Avg:Def_FUM + 
                      Rec_Avg:Tot_Yards_Gm +  Def_Plys:HomeAway, data = Playoffs.main)

summary(FinalPlayoff.4Main)

predict.test4 <- predict(FinalPlayoff.4Main, newdata = Playoffs.test) #making predictions on the test data that we withheld
RSPE4 = sqrt(sum((Playoffs.test$PtsScored-predict.test4)^2) / 10)
RSPE4

#Get Press Statistic
PRESS.final.P4 <- PRESS(FinalPlayoff.4)
PRESS.final.P4

##R2=1-PRESS/SStotal
#R2 = 1 - (SS res / SS total)
#SS total = ((RSE^2)*(n-p))/(1-R2)
summary(FinalPlayoff.4)
SSTotal.P4 <- ((8.777^2)*79)/(1-.4381)
R2.P4.Pred <- 1- (PRESS.final.P4)/(SSTotal.P4)
R2.P4.Pred #says .0984, which says how much variability in the response is explained by the model in the presence of new data.
#Would like as high as possible and between R2 and R2 adjusted and this is pretty low. Want at about .2  Fucking sucks.

##########################################Lets try using backward1aa (Model5)###############################

FinalPlayoff.5 <-lm(Playoffs$PtsScored ~ QB_RTG + Pass_Int + 
                           Def_Plys + Def_FUM + QB_RTG:Rec_Avg + Pass_Int:Rec_Avg  + 
                           Rec_Avg:Def_Plys + Rec_Avg:Def_FUM, data = Playoffs)
summary(FinalPlayoff.5)
drop1(FinalPlayoff.5, test = "F")
vif(FinalPlayoff.5)
pairs(Playoffs[,c(1,2,6,12,15)])
plot(FinalPlayoff.5, add.smooth=F) #Everything seems to be okay here.
#We could also check out the diagnostic tests for normality and correlated residuals 
shapiro.test(residuals(FinalPlayoff.5))
#Notice after Shapiro test that our p-value is very high which shows that we fail to reject that our residuals are normally distributed.
durbinWatsonTest(FinalPlayoff.5)
#We can look at the correlation between pairs of variable with this command 
cor(model.matrix(FinalPlayoff.5)[,-1]) #contains all the variables we've already included in model.  Leaves out the first column of ones.
#We can ask for the variance inflation factors using this comand 
vif(FinalPlayoff.5) #looking to see if any of the variance inflation values are above 10
#Leverage points are those which exceed high leverage is h> 2*p/n
2*(9/97) #basically all the points to the right of this are high leverage points.

FinalPlayoff.5Main <-lm(Playoffs.main$PtsScored ~ QB_RTG + Pass_Int + 
                          Def_Plys + Def_FUM + QB_RTG:Rec_Avg + Pass_Int:Rec_Avg  + 
                          Rec_Avg:Def_Plys + Rec_Avg:Def_FUM, data = Playoffs.main)
summary(FinalPlayoff.5Main)

predict.test5 <- predict(FinalPlayoff.5Main, newdata = Playoffs.test) #making predictions on the test data that we withheld
RSPE5 = sqrt(sum((Playoffs.test$PtsScored-predict.test5)^2) / 10)
RSPE5

#Get Press Statistic
PRESS.final.P5 <- PRESS(FinalPlayoff.5)
PRESS.final.P5

##R2=1-PRESS/SStotal
#R2 = 1 - (SS res / SS total)
#SS total = ((RSE^2)*(n-p))/(1-R2)
summary(FinalPlayoff.5)
SSTotal.P5 <- ((9.463^2)*86)/(1-.2888)
R2.P5.Pred <- 1- (PRESS.final.P5)/(SSTotal.P5)
R2.P5.Pred #says .0984, which says how much variability in the response is explained by the model in the presence of new data.
#Would like as high as possible and between R2 and R2 adjusted and this is pretty low. Want at about .2  Fucking sucks.

###Compare the final 5 models and then I am done with this case study.
summary(FinalPlayoff.1) 
summary(FinalPlayoff.2) #USE This one for the first
summary(FinalPlayoff.3) #USE This one for the second
summary(FinalPlayoff.4)
summary(FinalPlayoff.5)