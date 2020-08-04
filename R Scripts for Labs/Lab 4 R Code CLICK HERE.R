# First we need to load the packages
library(behaviouR)

## Part 1: Barnacle goose vigilance
# Then we load in our goose data
data('BarnacleGooseData')

# Note you will want to collect your own data and load into R using read.csv 
# BarnacleGooseData <- read.csv('FULL FILE PATH TO YOUR DATASHEET.csv')

### Part 1a: Surveillance behavior
# Scatterplot of total number of 'head up' in our data
ggscatter(data=BarnacleGooseData,
          x='FlockSize',y='TotalHeadsUp')+ylab('Surveillance rate')

# Now let's add a trend line to see if there is a relationship between flock size and the total number of 'head up' behaviors
ggscatter(data=BarnacleGooseData,
          x='FlockSize',y='TotalHeadsUp',add='reg.line')+ylab('Surveillance rate')

# Let's see if there were any differences between you and your partner
ggscatter(data=BarnacleGooseData,
          x='FlockSize',y='TotalHeadsUp',add='reg.line', facet.by = 'Partner',
          cor.coef = T)+ylab('Surveillance rate')

# Let's plot you and your partner's data in different colors. 
ggscatter(data=BarnacleGooseData,
          x='FlockSize',y='TotalHeadsUp',add='reg.line', facet.by = 'Partner',
          color = 'Partner', palette =c('black','blue'),
          cor.coef = T)+ylab('Surveillance rate')

#*Question 1*: Were there any major differences between you and your partner?

# Now we will do model selection using Akaike information criterion (AIC). First we create a null model
# and then we create a model with flock size as a predictor of total number of heads up
SurveillanceNullModel <- glm(TotalHeadsUp ~ (1/Partner),family=poisson , data=BarnacleGooseData)
SurveillanceModel <- glm(TotalHeadsUp ~ FlockSize + (1/Partner) ,family=poisson, data=BarnacleGooseData)

# Then we compare the models using AIC
AICtab(SurveillanceNullModel,SurveillanceModel)

#*Question 2*: If the null model is ranked higher than the model with flock size as a predictor. How do we interpret this finding?

### Part 1b: Time vigilant (sec/min)
# Now we will look at the relationship between the duration (calculated as seconds per minute)
# that the geese were vigilant as a function of flock size.

# Scatterplot of time vigilant (sec/min) as a function of group size
ggscatter(data=BarnacleGooseData,
          x='FlockSize',y='TimeSecHeadUp')+ylab('time vigilant (sec/min)')

# Scatterplot of duration of vigilance behavior in our data with a trend line.
ggscatter(data=BarnacleGooseData,x='FlockSize',y='TimeSecHeadUp',add='reg.line')+
  ylab('time vigilant (sec/min)')


# Let's see if there were any differences between you and your partner. 
# We will also add the command 'cor.coef = T' which will give us the correlation coefficient (R) 
# along with an associated p-value.

# NOTE: The data here are simulated so your plots should look different
ggscatter(data=BarnacleGooseData,x='FlockSize',y='TimeSecHeadUp',add='reg.line',
          facet.by = 'Partner',cor.coef = T)+
  ylab('time vigilant (sec/min)')


# Let's plot you and your partner's data in different colors. 
ggscatter(data=BarnacleGooseData,x='FlockSize',y='TimeSecHeadUp',add='reg.line',
          facet.by = 'Partner', color='Partner', cor.coef = T,
          palette =c('black','blue'))+
  ylab('time vigilant (sec/min)')

# As before we will create a null model and then a model with flock size as a predictor and compare them
# using AIC. We will not use a Poisson distribution here because our outcome variable is continuous.

# This is our null model
VigilanceNullModel <- lmer(TimeSecHeadUp ~ (1|Partner), data=BarnacleGooseData)

# This is our model with flock size as a predictor duration of vigilance
VigilanceModel <- lmer(TimeSecHeadUp ~ FlockSize + (1|Partner) ,data=BarnacleGooseData)

# Now we compare the models using AIC
AICtab(VigilanceNullModel,VigilanceModel)

#*Question 3.* How do you interpret the results of your model selection? 

## Part 2: Meerkat data revisted
# Please upload your meerkat scan data to this project and delete the existing datasheet.

data('MeerkatScanData')

# Note you will want to collect your own data and load into R using read.csv 
# MeerkatScanData <- read.csv('FULL FILE PATH TO YOUR DATASHEET.csv')

# As before we will turn our NA values to zero
MeerkatScanData[is.na(MeerkatScanData)] <- '0'

# We will remove the time and out of sight columns as we do not need them
MeerkatScanData <- select(MeerkatScanData,-c(Time,OutOfSight))

# We need to reformat our data so that we can plot it
MeerkatScanDataSummaryLong <- melt(MeerkatScanData, id.vars=c("Treatment", "Partner"))

# Here we add more informative column names
colnames(MeerkatScanDataSummaryLong) <- c('Treatment','Partner',
                                          'BehavioralState','InstancesOfBehavior')

# We need to tell R that our outcome variable is not categorical but numeric
MeerkatScanDataSummaryLong$InstancesOfBehavior <- 
  as.numeric(MeerkatScanDataSummaryLong$InstancesOfBehavior)

# Now we plot our data
ggboxplot(MeerkatScanDataSummaryLong,x='Treatment',
          y='InstancesOfBehavior', fill = 'BehavioralState')

#**Question 4.** Based on your inspection of the boxplot, are there any major differnces between treatment groups?

# Now we will test to see if there were differences in vigilance behaviors across treatments?

# First we subset our data so that it only includes the vigilant category
MeerkatScanDataVigilantOnly <- subset(MeerkatScanDataSummaryLong,
                                      BehavioralState=='Vigilant' )

# R can be picky about the format of data, so we use this command to tell R that treatment group is a factor
MeerkatScanDataVigilantOnly$Treatment <- 
  as.factor(MeerkatScanDataVigilantOnly$Treatment)

# Here we are reordering the levels of the factors. For our model selection we are interested
# in whether we see differences from the control (no predator) and the predator treatments,
# so here we are setting the no predator group as our reference group.
MeerkatScanDataVigilantOnly$Treatment <- 
  factor(MeerkatScanDataVigilantOnly$Treatment, levels = c("NoPredator", "AerialPredator", 
                                                           "TerrestrialPredator"))

# Now as before we will do model selection. Note that because our outcome variable (instances of behavior) 
# is in the form of count data we use a poisson distribution.
MeerkatVigilanceNullModel <- glm(InstancesOfBehavior ~ 1, family=poisson, data=MeerkatScanDataVigilantOnly)
MeerkatVigilanceModel <- glm(InstancesOfBehavior ~ Treatment, family=poisson,data=MeerkatScanDataVigilantOnly)

# Now we compare the models using AIC
AICtab(MeerkatVigilanceNullModel,MeerkatVigilanceModel)

# Here we will use the summary function to look at the estimates. There is a lot of information here
# but we want to focus on the 'Estimate'. In particular we are interested in the estimates
# for 'TreatmentAerialPredator' and 'TreatmentTerrestrialPredator'. The estimate is showing the effect
# that these variables have on our outcome (instances of behavior), relative to our control (no predator).
# Therefore positive estimates indicate that there were more vigilance behaviors in aerial 
# and terrestrial predator treatments.
summary(MeerkatVigilanceModel)

# A common way to visulize results such as these are coefficient plots. Here we are looking at the
# effect of 'TreatmentAerialPredator' and 'TreatmentTerrestrialPredator' relative to our control group.
# The reference or group is indicated by the vertical dashed line. So, we can interpret that because the 
# coefficients are positive (and the confidence intervals don't overlap zero) that both terrestrial and
# aerial treatments lead to an increase in vigilant behaviors.
coefplot(MeerkatVigilanceModel,intercept=F)


# For reasons that will not go into here, I am not a fan of p-values or null hypothesis significance
# testing. There is a nice overview if you want to learn more here: https://doi.org/10.1098/rsbl.2019.0174.But,
# the model selection approach will lead to the same inference as the use of a one-way anova, an approach that 
# you may be more familiar with.

# Compute the analysis of variance
MeerkatAOV <- aov(InstancesOfBehavior ~ Treatment, data = MeerkatScanDataVigilantOnly)

# Here this will tell us if there are differences between groups
summary(MeerkatAOV)

# Since the ANOVA test is significant, we can compute Tukey Honest Significant Differences test.
TukeyHSD(MeerkatAOV)

#**Question 5.** Based on your interpretation of the model selection and the coefficient plots were there differences between treatment groups?
