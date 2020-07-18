##### Assessing /t/ variables

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(car)
library(sjPlot)
library(stargazer)

# setwd("C:/Users/Roy/Box Sync/PhD/Elan") # edit as needed
setwd("C:/Users/phoen/Dropbox/Box Sync - Feb 2020/PhD/Elan")

master <- read.csv("AllTMaster.csv")

## exclude the unreleased and other tokens for the statistical modelling
## old version of good.data, which includes pre-C tokens
# good.data <- master %>% filter(!grepl('imitat|cuts|laugh|count', notes)) %>% 
#  filter(!grepl('obstruent|laughter|other', followingContextSO)) %>%
#  filter(precedingSound != 's' & precedingSound != 'k', speaker != 'Jay',
#        tProduction != 'unreleased' & tProduction != 'other') %>%
# droplevels
# View(good.data)

##### new version of good.data that excludes the pre-C tokens!
good.data <- master %>% filter(!grepl('imitat|cuts|laugh|count', notes)) %>% 
  filter(!grepl('obstruent|laughter|other', followingContextSO)) %>%
  filter(precedingSound != 's' & precedingSound != 'k', speaker != 'Jay',
         tProduction != 'unreleased' & tProduction != 'other') %>%
  filter(!grepl('C|other', overallContextMedium)) %>%
  droplevels

levels(good.data$overallContextMedium)
levels(good.data$followingSound)

# get log of word frequency
good.data <- mutate(good.data, logFreq = log10(wordFrequencyPlus1))

# rename columns (early models below aren't adjusted to these names)
good.data <- good.data %>% rename(context = overallContextMedium, prevSch = previousSchoolType)

# might want to change some of the baseline levels
good.data$prevSch <- relevel(good.data$prevSch, 'state')

####
# how many tokens does each speaker have?
spk <- good.data %>%
  group_by(speaker) %>%
  count() # could add tBinary inside the brackets to get each type of /t/
spk
View(spk)

# how many tokens does each context have?
cntxt <- good.data %>%
  group_by(overallContextMedium) %>%
  count() # could add tBinary inside the brackets to get each type of /t/
cntxt
# _#V        2305
# _#zero     1698
# _V          941
# _syllabic   369

###################
# Let's make a maximal model with scaled continuous variables and then check the collinearity.
# First, let's scale the continuous predictors into a new version of the data set.

# I updated all the models below, which originally used the previous version of this, which was just good.data, though I haven't re-tested them all yet.
# This scaling process needs to be done separately to the main good.data so that the sub-sets don't have messed-up scaled variables (errors occur if you try).
good.data_z <- good.data %>%
  mutate(housePrice_z = scale(housePriceVsMean),
         age_z = scale(age),
         class9_z = scale(ClassScore9),
         groupSize_z = scale(groupSize),
         syllables_z = scale(syllables),
         logFreq_z = scale(logFreq))

# When I say maximal, I mean the whole data set, with random slopes for the most theoretically interesting predictors (gender, school, prevSchool, context)
# Note: this model took 30+ minutes to run on the office computer
# actually also needs NSSEC5
model.max <- glmer(tBinary ~ (1 + overallContextBroad | speaker) + (1 + gender + school + previousSchoolType | word) + 
                     gender + school + age_z + settlement + previousSchoolType + parentsHome + parentsUni + parentsLocal + 
                     parentsOrigin + housePrice_z + ClassGroup3 + class9_z + deprivationLocalQuartile + overallContextBroad + 
                     wordClassBinary + taskTypeBroad + groupGender + groupGenderRelative + groupSize_z + precedingContext + followingContextVC + syllables_z + logFreq_z, 
                   data = good.data_z, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa"))
summary(model.max)
plot(allEffects(model.max))

# Predictably, there are lots of errors (6!), including convergence errors. We'll deal with them later.
# We could in theory add a random slope for EVERY fixed effect but since this model is already too complex, there's no point (and it will take hours!). It's going to have to get LESS complex, not more.
# For now, let's just look at the collinearity.

# before doing this, run vif.mer() code from Florian Jaeger's blog
vif.mer(model.max)

# alternative method - fit normal glm without random effects and use vif()
# had to remove parentsOrigin, groupGenderRelative and fcVC as they produced NAs in the glm, possibly due to some cells having 0
model.max.glm <- glm(tBinary ~  
                      gender + school + age_z + settlement + 
                       previousSchoolType + parentsHome + parentsUni + 
                       parentsLocal + housePrice_z + NSSEC5 + ClassGroup3 + 
                       class9_z + deprivationLocalQuartile + overallContextMedium + 
                       wordClassBinary + taskTypeBroad + groupGender + groupSize_z + 
                       precedingContext + syllables_z + logFreq_z, 
                     data = good.data_z, family = binomial)
summary(model.max.glm)
# plot(allEffects(model.max.glm))

vif(model.max.glm)

# The results from both methods are pretty similar
# The really bad variables (10 or higher) are: school, parentsUni, ClassGroup3, class9, deprivation, NSSEC5 (better than NSSEC8, worse than NSSEC3), groupGender, followingContextVC
# The somewhat bad variables (4 or higher) are: age, previousSchoolType, parentsHome, parentsLocal, parentsOrigin, housePrice, groupSize, groupGenderRelative

# Most of this is unsurprising. These social variables are unevenly split across the two schools or just completely imbalanced (eg parentsHome, groupSize)
# At this point, it's probably worth dumping some of the worst variables
# school - not getting rid - it's too theoretically important, even though it is correlated with some of the social variables.
scho <- good.data %>%
  group_by(speaker) %>%
  count(school, gender) %>%
  group_by(school, gender) %>% # add / remove grouping variables as needed
  count(school)
scho
# Alton     female    18
# Alton     male       8
# Churchers female     8
# Churchers male      11
# One option is removing outlying or under-contributing participants (e.g. some quiet Alton girls), though let's deal with the other variables first.
# parentsUni
uni <- good.data %>%
  group_by(speaker) %>%
  count(school, gender, parentsUni) %>%
  group_by(gender, parentsUni) %>% # add / remove grouping variables as needed
  count(parentsUni)
uni
# This perfectly sums up the balance issues across school and gender
# both        26 (9 AF; 4 AM; 4 CF; 9 CM)
# one          7 (1 AF; 2 AM; 2 CF; 2 CM)
# no          12 (8 AF; 2 AM; 2 CF; 0 CM)
# This variable should probably be removed as there's a 0 cell. Getting rid of quiet Alton girls may help fix this. 
# classgroup3
class3 <- good.data %>%
  group_by(speaker) %>%
  count(school, gender, ClassGroup3) %>%
  group_by(ClassGroup3, school, gender) %>% # add / remove grouping variables as needed
  count(ClassGroup3)
class3
# high          20 (5 AF; 4 AM; 3 CF; 8 CM)
# medium        20 (10 AF; 2 AM; 5 CF; 3 CM)
# low            5 (3 AF; 2 AM; 0 CF, 0 CM)
# 'low' is only repped by Alton. AF dominate medium. Probably scrap.
class9 <- good.data %>%
  group_by(speaker) %>%
  count(school, gender, ClassScore9) %>%
  group_by(ClassScore9, school) %>% # add / remove grouping variables as needed
  count(ClassScore9)
class9
#  ClassScore     Count
#           3     3 (all AF)
#           4     2 (all AM)
#           5     6 (5 Alton)
#           6     6 (no AM)
#           7     8 (4 AF)
#           8    13 (5 AF)
#           9     7 (5 CM, no AF)
# AF and Alton generally clustered at bottom; vice-versa for CM and Churcher's generally
depr <- good.data %>%
  group_by(speaker) %>%
  count(school, gender, deprivationLocalQuartile) %>%
  group_by(deprivationLocalQuartile, school, gender) %>% # add / remove grouping variables as needed
  count(deprivationLocalQuartile)
depr
# q1     6 (3 AF)
# q2     8 (2 each!)
# q3    18 (7 AF; 6 CM)
# q4    13 (6 AF; 4 AM)
# This demonstrates how Alton students are actually from less deprived locations thanks to where they live
# An option here might be getting relative (within-sample) deprivation quartiles
# A possible advantage (?) here though is that deprivation is probably not too badly correlated with parental occupation, etc (I forgot to include NSSEC options in the max glmer model :())
occ5 <- good.data %>%
  group_by(speaker) %>%
  count(school, gender, NSSEC5) %>%
  group_by(NSSEC5) %>% # add / remove grouping variables as needed
  count(NSSEC5)
occ5
# managerial        33 (11 AF, 10 CM)
# intermediate       5 (no CM)
# smallEmployers     4 (no CF)
# semiAndRoutine     3 (all AF)
# NSSEC3 just merges int. + smallEmpl together. NSSEC8 has (high)22/11/5/4/2/1(low).
# The social class variables are all quite bad but at least one should really be kept. 
gr.gen <- good.data %>%
  group_by(speaker) %>%
  count(school, gender, groupGender) %>%
  group_by(groupGender, school) %>% # add / remove grouping variables as needed
  count(groupGender)
gr.gen
# all female      8 (4 each, including Holly)
# all male        6 (all Churcher's)
# mixed           31 (22 Alton)
gr.gen.rel <- good.data %>%
  group_by(speaker) %>%
  count(school, gender, groupGenderRelative) %>%
  group_by(groupGenderRelative, school, gender) %>% # add / remove grouping variables as needed
  count(groupGenderRelative)
gr.gen.rel
# all the same        14 (no AM)
# equal                7
# in majority         17 (9 AF)
# in minority          7

# the variables with the worst VIFs are classGroup3 and ClassScore9. This is no surprise as they're composed of three others. Let's remove them and see what happens.

model.max.glm2 <- glm(tBinary ~  
                       gender + school + age_z + settlement + 
                       previousSchoolType + parentsHome + parentsUni + 
                       parentsLocal + housePrice_z + NSSEC5 + 
                       deprivationLocalQuartile + overallContextMedium + 
                       wordClassBinary + taskTypeBroad + groupGender + groupSize_z + 
                       precedingContext + syllables_z + logFreq_z, 
                     data = good.data_z, family = binomial)
summary(model.max.glm2)
vif(model.max.glm2)

# This looks much better already. The 3 main class indices are now all less than 10.
# School, deprivation and groupGender are the only effects with >10 VIF.

# The alternative option is to remove the 3 indices and keep one of group3 or score9:

model.max.glm3 <- glm(tBinary ~  
                        gender + school + age_z + settlement + 
                        previousSchoolType + parentsHome + 
                        parentsLocal + class9_z + 
                        deprivationLocalQuartile + overallContextMedium + 
                        wordClassBinary + taskTypeBroad + groupGender + groupSize_z + 
                        precedingContext + syllables_z + logFreq_z, 
                      data = good.data_z, family = binomial)
summary(model.max.glm3)
vif(model.max.glm3)

# The best VIFs are obtained using class9_z, then ClassGroup3, then the three indices separately
# House price on its own is the best of all
# The only question is whether class score is actually a better way of theorising class than the three separate indices (or deprivation)

# At this point, let's get rid of groupGender as it has a high VIF and isn't all that interesting

model.max.glm4 <- glm(tBinary ~  
                        gender + school + age_z + settlement + 
                        previousSchoolType + parentsHome + 
                        parentsLocal + class9_z + 
                        deprivationLocalQuartile + overallContextMedium + 
                        wordClassBinary + taskTypeBroad + groupSize_z + 
                        precedingContext + syllables_z + logFreq_z, 
                      data = good.data_z, family = binomial)
summary(model.max.glm4)
vif(model.max.glm4)

# This helps a lot. School and deprivation now only have VIFs of 7-8.
# Deprivation is a strange variable - it has the same theoretical purpose as class but is (a bit) less balanced.
# Could deprivation really just be an indicator of geography? Let's try removing it and see what happens.
# (Doing the above model with deprivation but not class9 shows that depr is still quite high, so it's not correlated with class9 but something else)

model.max.glm5 <- glm(tBinary ~  
                        gender + school + age_z + settlement + 
                        previousSchoolType + parentsHome + 
                        parentsLocal + class9_z + 
                        overallContextMedium + 
                        wordClassBinary + taskTypeBroad + groupSize_z + 
                        precedingContext + syllables_z + logFreq_z, 
                      data = good.data_z, family = binomial)
summary(model.max.glm5)
vif(model.max.glm5)

# looks good! The only VIF higher than 4 now is school (5.6)
# let's try deprivation terciles and quintiles to see if they improve things

# tercile
model.max.glm4a <- glm(tBinary ~  
                        gender + school + age_z + settlement + 
                        previousSchoolType + parentsHome + 
                        parentsLocal + class9_z + 
                        deprivationLocalTercile + overallContextMedium + 
                        wordClassBinary + taskTypeBroad + groupSize_z + 
                        precedingContext + syllables_z + logFreq_z, 
                      data = good.data_z, family = binomial)
summary(model.max.glm4a)
vif(model.max.glm4a)

# tercile works quite well here (VIF = 2) but school VIF is 6 and it captures less of the deprivation variation

# quintile
model.max.glm4b <- glm(tBinary ~  
                         gender + school + age_z + settlement + 
                         previousSchoolType + parentsHome + 
                         parentsLocal + class9_z + 
                         deprivationLocalQuintile + overallContextMedium + 
                         wordClassBinary + taskTypeBroad + groupSize_z + 
                         precedingContext + syllables_z + logFreq_z, 
                       data = good.data_z, family = binomial)
summary(model.max.glm4b)
vif(model.max.glm4b)
# quintile is less good - 10 VIF for both school and deprivation

# OK, so according to the VIFs, class9_z is the best overall class index and housePrice_z is the best individual one
# deprivaton and class9 don't seem to be correlated but cover the same theoretical grounds
# deprivation3 has the lowest VIF out of 3/4/5 but takes less of the variation into account

###### Let's see what a maximal glmer looks like with this fixed-effects structure
# copy of model.max.glm5 above but in glmer form with random slopes

model.max2 <- glmer(tBinary ~ (1 + overallContextMedium | speaker) + 
                        (1 + gender + school + previousSchoolType | word) +
                        gender + school + age_z + settlement + 
                        previousSchoolType + parentsHome + 
                        parentsLocal + class9_z + 
                        overallContextMedium + 
                        wordClassBinary + taskTypeBroad + groupSize_z + 
                        precedingContext + syllables_z + logFreq_z, 
                      data = good.data_z, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))
summary(model.max2)
vif.mer(model.max2)

# this model fails to converge - 3 error messages + singular fit - so some random slopes must be scrapped
# VIFs - school, overallContextB >4; parentsLocal >3

# let's try removing the correlation parameters just to see if that improves things

model.max2a <- glmer(tBinary ~ (0 + overallContextMedium | speaker) + 
                      (0 + gender + school + previousSchoolType | word) +
                      (1 | speaker) + (1 | word) +
                      gender + school + age_z + settlement + 
                      previousSchoolType + parentsHome + 
                      parentsLocal + class9_z + 
                      overallContextMedium + 
                      wordClassBinary + taskTypeBroad + groupSize_z + 
                      precedingContext + syllables_z + logFreq_z, 
                    data = good.data_z, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(model.max2a)
vif.mer(model.max2a)

# this model fails to converge - 3 error messages + singular fit - so some random slopes must be scrapped
# VIFs - school >4; overallContextB >5; parentsLocal >3

# At this point, I could try only removing the corr. param.s from one random slope, but it's quite clear by now that it's unlikely to help as the model is too complex
# What we really need is a simpler random-effects structure, particularly given that interactions still need to be added
# Following context is probably the least important random slope since we know that it is almost guaranteed to vary from previous studies - we don't need to be so conservative with this variable
# If doing separate models for sub-sections, it would also not be included as an effect

model.max3 <- glmer(tBinary ~ (1 + gender + school + previousSchoolType | word) +
                       (1 | speaker) + 
                       gender + school + age_z + settlement + 
                       previousSchoolType + parentsHome + 
                       parentsLocal + class9_z + 
                       overallContextMedium + 
                       wordClassBinary + taskTypeBroad + groupSize_z + 
                       precedingContext + syllables_z + logFreq_z, 
                     data = good.data_z, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))
summary(model.max3)
vif.mer(model.max3)

# this works! Time to start cutting pointless variables and adding interactions.
# some of the VIFs are still a bit high (>5)

# get rid of parentsHome/Local, add * context interactions
good.data <- good.data %>% rename(context = overallContextMedium, prevSch = previousSchoolType)

new.model <- glmer(tBinary ~ (0 + gender + school + prevSch | word) +
                      (1 | speaker) + (1 | word) +
                      gender + school + age_z + settlement + 
                      prevSch +
                      class9_z + 
                      school : context +
                      gender : context +
                      prevSch : context +
                      context + 
                      wordClassBinary + taskTypeBroad + groupSize_z + 
                      precedingContext + syllables_z + logFreq_z, 
                    data = good.data_z, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(new.model)
vif.mer(new.model)
plot(allEffects(new.model))
# made a few similar models. 
# The VIFs increase massively when adding interactions.
# Adding more interactions causes problems for the model, so another random slope needs to go (removing the CPs didn't help).

# Here's quite a good model for the whole data set
# PrevSchool was removed as a random slope in order to help the model converge
# PrecedingContext was also removed as it was very insignificant and removing it helped the model converge
# Features interactions for context * gen/sch/prevSch, but could add e.g. gen*sch ones still (see below)
# (Did another version of this model with housePrice_z instead of class9_z and there's no real difference)

new.model0 <- glmer(tBinary ~ (1 + gender + school | word) +
                     (1 | speaker) +
                     gender + school + age_z + settlement + 
                     prevSch +
                     class9_z + 
                     context : school +
                     context : gender +
                     context : prevSch +
                     context + 
                     wordClassBinary + taskTypeBroad + groupSize_z + 
                     syllables_z + logFreq_z, 
                   data = good.data_z, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
summary(new.model0)
vif.mer(new.model0)
plot(allEffects(new.model0))

# Now let's try removing non-sig variables and comparing models with ANOVAs
# removed least sig. var. - settlement. model with it included is not better
# removed least sig. var. - age. model with it included is not better
# removed least sig. var. - word class. model with it included is not better
# (class9 is next least sig var but I'm not removing it yet as it's more theoretically important)
# removed least sig. var. - group size. model with it included is not better
# removed least sig. var. - syllables. model with it included is not better
# removed final insig. var. - class9. model with it included is not better

# model with only significant variables:
new.model6 <- glmer(tBinary ~ (1 + gender + school | word) +
                      (1 | speaker) +
                      gender + school + 
                      prevSch +
                      context : school +
                      context : gender +
                      context : prevSch +
                      context + 
                      taskTypeBroad + 
                      logFreq_z, 
                    data = good.data_z, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(new.model6)
vif.mer(new.model6)
plot(allEffects(new.model6))

# now let's see if we can re-add prevSch as a random slope
# nope - causes singular fit. Same with version without corr. params.
# I added more interactions to new.model6 (sch*gen, sch*prevSch, gen*prevSch) but none were significant
# For now, then, new.model6 above is the best model for the whole data set. :)

# I then tried swapping the random slopes around
# (1 | school + prevSch) is actually the best combo (sig better than gen + sch in a model comaprison; gen + prevSch is worse than both)
# the thing is, leaving out gender as a random slope is arguably theoretically worse than the new option
# The decision is then to base the choice of random slopes on a) theory (gen + sch) or b) the model fit
# The p-values in the better-fitting model are a fair bit more conservative
# I'm leaning towards theory over model fit at the moment.

######
# After this, I made models for each of the sub-sets (see 'New models for each T context.R')
# It became clear that a) random slopes could only be supported by one big model as three of the sub-sets couldn't handle them
# b) The only significant effects in at least one context were: gender, school, precedingContext, taskType, logFreq_z, wordClass (almost), previousSchool, followingTorN (pre-syllabic), syllables
 
# Gender, school and prevSch were already included as interactions with context in new.model6 above, so I then tried fitting the model with context*other interactions:
# Note that including 5+ *context interactions in the model produces a warning: 'maxfun < 10 * length(par)^2 is not recommended'

# Class9*context is not significant at all - only for syllabic is it almost (0.06)
# housePrice*context is sig for syllabic and almost sig (0.08) for WM. 
# logFreq*context is sig for all apart from the baseline fixed effect (#_V), but is arguably less theoretically interesting and so could be replaced with a more socially interesting interaction
# syllables*context is not sig - only for PP is it almost (0.08). Neither is it very theoretically interesting. Also maybe correlated with word frequency.
# preceding context*(following) context is sig for all contexts except PP, which is very close (0.06)
# How many tokens are in each preceding context per following context?
precCon <- good.data_z %>%
  group_by(context) %>%
  count(precedingContext)
precCon
# post-vocalic /t/ makes up 93% (_#V), 81% (PP), 82% (syllabic) and 86% (_V) of the totals in each context
# taskType*context is only sig for WF and PP, though this is probably because of the small numbers and imbalance in the other two contexts
# How many tokens are in each task type per following context?
tt <- good.data_z %>%
  group_by(context) %>%
  count(taskTypeBroad)
tt
# convo tokens make up 85% (_#V), 96% (PP), 74% (syllabic) and 82% (_V) of the totals in each context
# wordClassBinary is only sig for PP and WM
# How many tokens are in each word class type per following context?
wc <- good.data_z %>%
  group_by(context) %>%
  count(wordClassBinary)
wc
# function tokens make up 70% (_#V), 67% (PP), 97% (syllabic) and 96% (_V) of the totals in each context
# Note that mean word frequency for each context varies dramatically: mean WF+1 = 8009 (_#V), 9385 (PP), 723 (syllabic) and 77.3 (_V) (high = more frequent)
frq <- good.data_z %>%
  group_by(context) %>%
  summarise(mean = mean(wordFrequencyPlus1, na.rm=TRUE))
frq
# This would potentially support the inclusion of wordFreq as an interaction over the other internal factors above, as they have more similar proportions

## I then tried interactions between the social variables.
# gender*school, gender*prevSch and school*prevSch are not significant

# best so far - school*class9 and gender*class9 are almost significant
new.model7 <- glmer(tBinary ~ (1 + gender + school | word) +
                      (1 | speaker) +
                      gender + school + 
                      prevSch + class9_z +
                      context : school +
                      context : gender +
                      context : prevSch +
                      context : logFreq_z +
                      school : class9_z +
                      gender : class9_z +
                      context + 
                      taskTypeBroad +
                      logFreq_z, 
                    data = good.data_z, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(new.model7)
vif.mer(new.model7)
plot(allEffects(new.model7))

# if you change the random slope, gender*class becomes significant and the model fit improves... but is this theoretically the best?
# if you remove all the interactions, the only sig fixed effects are school, context, taskType and logFreq_z
new.model8 <- glmer(tBinary ~ (1 + prevSch + school | word) +
                      (1 | speaker) +
                      gender + school + 
                      prevSch + class9_z +
                      context : school +
                      context : gender +
                      context : prevSch +
                      context : logFreq_z +
                      school : class9_z +
                      gender : class9_z +
                      context + 
                      taskTypeBroad +
                      logFreq_z, 
                    data = good.data_z, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(new.model8)
vif.mer(new.model8)
plot(allEffects(new.model8))

# note that changing the internal interaction (i.e. wordFreq to something else like precContext) doesn't improve things

# get ANOVA table for thesis corrections
# see https://stats.stackexchange.com/questions/101566/anova-type-iii-test-for-a-glmm for advice on getting ANOVA tables from glmers (from Ben Bolker of lme4 fame)
Anova(new.model8, type = 'III')

# get pairwise comparisons for phonological contexts for thesis corrections
emmeans(new.model8, list(pairwise ~ context), adjust = 'Bonferroni')

### export the model output
# both of these methods are annoyingly imperfect

# odds ratios instead of estimates. CIs instead of z-value.
tab_model(new.model8, show.adj.icc = TRUE, show.est = TRUE, show.se = TRUE)

# gets all info in one column
stargazer(new.model8, type = "html", style = 'default', out = 'model8.html', 
          intercept.bottom = FALSE, intercept.top = TRUE, report = 'vcstp*')

# only gets estimate - could combine this with tab_model code above
stargazer(new.model8, type = "html", style = 'default', out = 'model8b.html', 
          intercept.bottom = FALSE, intercept.top = TRUE, report = 'vc')

# only gets z-value - could combine this with tab_model code above
stargazer(new.model8, type = "html", style = 'default', out = 'model8c.html', 
          intercept.bottom = FALSE, intercept.top = TRUE, report = 'vt')

# For tab_model, open the HTML in a new window (using button in the Viewer), then copy and paste into Excel
# For stargazer models, get rid of the annoying extra rows between each predictor:
# Excel > paste output > Find & Select > Go To Special > Blanks > Delete Sheet Rows

########### Private school-only model
# Let's see if room is significant at the private school
# A plot (see T plots.R) suggests not much of a room difference in any of the contexts

ch.data <- good.data %>% filter(school == 'private') %>% droplevels

ch.data_z <- ch.data %>%
  mutate(housePrice_z = scale(housePriceVsMean),
         age_z = scale(age),
         class9_z = scale(ClassScore9),
         groupSize_z = scale(groupSize),
         syllables_z = scale(syllables),
         logFreq_z = scale(logFreq))

# model based on new.model7 above but with room instead of school
ch.model <- glmer(tBinary ~ (1 + gender + room | word) +
                      (1 | speaker) +
                      gender + room + 
                      prevSch + class9_z +
                      context : room +
                      context : gender +
                      context : prevSch +
                      context : logFreq_z +
                      room : class9_z +
                      gender : class9_z +
                      context + 
                      taskTypeBroad +
                      logFreq_z, 
                    data = ch.data_z, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(ch.model)
vif.mer(ch.model)
plot(allEffects(ch.model))

# the private school-only model data set can only handle room or prevSch (not gender) as a random slope
# an ANOVA shows that having prevSch as the random slope is sig better than with room as the random slope instead
# I then tested the model with room* other social factors and removed non-sig effects and interactions as per usual

# best model - room*class is almost significant (0.064)
# removing all the interactions means no social factors are significant
ch.model2 <- glmer(tBinary ~ (1 + prevSch | word) +
                      (1 | speaker) +
                      room + 
                      class9_z +
                      context : class9_z +
                      context : logFreq_z +
                      context + 
                      taskTypeBroad, 
                    data = ch.data_z, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(ch.model2)
vif.mer(ch.model2)
plot(allEffects(ch.model2))
# Is room*class even balanced?
# room     ClassScore9    n
# outgoing           5     1
# outgoing           6     2
# outgoing           8     2
# outgoing           9     4
# reserved           6     2
# reserved           7     3
# reserved           8     3
# reserved           9     2
# Answer: sort of, but note 4/9 outgoing people have a score of 9, and no speakers in res7 and outg5.
# The interaction suggests that 'lower'-class outgoing kids use a lot more glottals than higher-class outgoing kids, whereas the effect is reversed (but less strong) for the reserved room
# I'd say this might be possibly worthy of discussion if relevant but not a super-robust result.


#####
# consider for the future:
# cutting some of the participants (e.g. the 5 quietest state girls) and doing all this again for the new data set;
# try and work out why school has a high VIF - could just add school into a model then keep adding stuff until the VIF jumps
# (high school VIF might be something to do with the _#zero context)


# next time, maybe relevel the context factors so that pre-pausal is the baseline


