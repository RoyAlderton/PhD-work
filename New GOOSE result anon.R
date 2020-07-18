######## GOOSE RESULTS
## Uses the second run of the Lobanov normalisation, involving all 11 vowels

library(plyr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(car)
library(sjPlot)
library(stargazer)

setwd("C:/Users/Roy/Box Sync/PhD/Production Data/GOOSE Praat Files")
setwd("C:/Users/phoen/Dropbox/Box Sync - Feb 2020/PhD/Production Data/GOOSE Praat Files")

goose <- read.csv("All GOOSE & hVd.csv") # 2796 tokens (including Alex)
View(goose)

# get log of word frequency; get rid of unneeded tokens
goose <- mutate(goose, logFreq = log10(wordFrequencyPlus1)) %>% filter(vowel == 'GOOSE') %>%
  filter(speaker != 'Alex') %>% filter(followingContext != 'other') %>%
  filter(!grepl('imitat|cuts|laugh|exagg|example|funny', notes)) %>%
  filter(!grepl('used0|who0', fileName)) %>% # including these tokens completely changes the direction of the task type effect, showing that they have a massive influence on the result
  rename(prevSch = previousSchoolType,
         context = overallContextWithinGOOSE,
         preceding = precedingContext,
         final.l = followingFinal.l) %>% droplevels # 2143 tokens

goose.only <- goose %>% filter(vowel == 'GOOSE') %>% droplevels # 2330 tokens
alex.only <-  goose.only %>% filter(speaker == 'Alex') %>% droplevels # 24 tokens

# change some of the baseline levels
goose$prevSch <- relevel(goose$prevSch, 'state')
goose$preceding <- relevel(goose$preceding, 'nonCoronal')

# how many tokens does each speaker have?
spk <- goose %>%
  group_by(speaker) %>%
  count()
View(spk)

####
# make big model

# standardise continuous independent variables
goose_z <- goose %>%
  mutate(housePrice_z = scale(housePriceVsMean),
         age_z = scale(age),
         class9_z = scale(ClassScore9),
         groupSize_z = scale(groupSize),
         syllables_z = scale(syllables),
         logFreq_z = scale(logFreq),
         duration_z = scale(duration))

# similar full (error-ful) model to /t/ - can only handle one random slope
new.model0 <- lmer(norm_euclid_i_u ~ (1 + gender + school | word) +
                     (1 | speaker) +
                     gender + school + age_z + settlement + 
                     prevSch +
                     class9_z +
                     preceding + 
                     final.l +
                     FLEECEcompetitor +
                     duration_z +
                     wordClassBinary + taskType + groupSize_z + 
                     syllables_z + logFreq_z, 
                   data = goose_z)
summary(new.model0)
vif.mer(new.model0)
plot(allEffects(new.model0))

# version with all interactions, including preceding*social
# seems to only be able to handle one by-word random slope
new.model2 <- lmer(norm_euclid_i_u ~ (1 + school | word) +
                     (1 + duration_z | speaker) +
                     gender + school + age_z + prevSch + settlement + 
                     preceding : duration_z +
                     preceding : taskType +
                     school : age_z +
                     preceding : gender +
                     preceding : prevSch +
                     preceding : settlement +
                     preceding + 
                     final.l +
                     taskType +
                     duration_z, 
                   data = goose_z)
summary(new.model2)
vif.mer(new.model2)
plot(allEffects(new.model2))

# edit: school*age is a bad interaction as state has 16/17/18/19 but private only has 16/17
# prevSch*age is also bad
# try again from the top
new.model0e <- lmer(norm_euclid_i_u ~ (1 + school | word) +
                      (1 | speaker) +
                      school + age_z + gender + settlement + prevSch + class9_z +
                      preceding : duration_z +
                      gender : school +
                      gender : prevSch +
                      school : settlement +
                      gender : age_z +
                      gender : class9_z +
                      school : class9_z +
                      settlement : class9_z +
                      prevSch : class9_z +
                      preceding : school +
                      preceding : prevSch +
                      preceding : gender +
                      preceding : settlement +
                      preceding : taskType +
                      preceding +
                      FLEECEcompetitor +
                      taskType +
                      final.l +
                      duration_z, 
                    data = goose_z)
summary(new.model0e)
vif.mer(new.model0e)
plot(allEffects(new.model0e))

# good model
# can only handle prevSch or duration_z as a random slope (by word)
# if you remove the interactions, none of the social variables are significant
new.model4 <- lmer(norm_euclid_i_u ~ (1 + prevSch | word) +
                     (1 | speaker) +
                     gender + prevSch + settlement +
                     preceding : duration_z +
                     preceding : prevSch +
                     preceding : gender +
                     preceding : settlement +
                     preceding : taskType +
                     preceding +
                     final.l + 
                     taskType +
                     duration_z, 
                   data = goose_z)
summary(new.model4)
vif.mer(new.model4)
plot(allEffects(new.model4))

# ANOVA table for thesis corrections
anova(new.model4)

# get pairwise comparisons for phonological contexts for thesis corrections
emmeans(new.model4, list(pairwise ~ preceding), adjust = 'Bonferroni')

## export the model output
# both of these methods are annoyingly imperfect

# CIs instead of t-value
tab_model(new.model4, show.adj.icc = TRUE, show.est = TRUE, show.se = TRUE)

# gets all info in one column
stargazer(new.model4, type = "html", style = 'default', out = 'model4.html', 
          intercept.bottom = FALSE, intercept.top = TRUE, report = 'vcstp*')

# only gets estimate - could combine this with tab_model code above
stargazer(new.model4, type = "html", style = 'default', out = 'model4b.html', 
          intercept.bottom = FALSE, intercept.top = TRUE, report = 'vc')

# only gets z-value - could combine this with tab_model code above
stargazer(new.model4, type = "html", style = 'default', out = 'model4c.html', 
          intercept.bottom = FALSE, intercept.top = TRUE, report = 'vt')

# For stargazer models, get rid of the annoying extra rows between each predictor:
# Excel > paste output > Find & Select > Go To Special > Blanks > Delete Sheet Rows
# stargazer doesn't seem to work anymore
 

### now let's try it with the pre-/l/ tokens removed

goose.no.l <- goose %>% filter(final.l == 'no') %>% droplevels
goose.no.l.z <- goose.no.l %>%
  mutate(housePrice_z = scale(housePriceVsMean),
         age_z = scale(age),
         class9_z = scale(ClassScore9),
         groupSize_z = scale(groupSize),
         syllables_z = scale(syllables),
         logFreq_z = scale(logFreq),
         duration_z = scale(duration))

# full preceding*social no-POOL model
new.model0c <- lmer(norm_euclid_i_u ~ (1 + school | word) +
                      (1 | speaker) +
                      school + age_z + gender + settlement + prevSch + class9_z +
                      preceding : duration_z +
                      preceding : taskType +
                      preceding : school +
                      preceding : prevSch +
                      preceding : gender +
                      preceding : settlement +
                      preceding : age_z +
                      preceding : class9_z +
                      preceding + 
                      duration_z +
                      FLEECEcompetitor +
                      taskType +
                      logFreq_z, 
                    data = goose.no.l.z)
summary(new.model0c)
vif.mer(new.model0c)
plot(allEffects(new.model0c))

# full social*social model with the sig preceding*social interactions from the one above
new.model0d <- lmer(norm_euclid_i_u ~ (1 + school | word) +
                      (1 | speaker) +
                      school + age_z + gender + settlement + prevSch + class9_z +
                      preceding : duration_z +
                      gender : school +
                      gender : prevSch +
                      school : settlement +
                      gender : age_z +
                      gender : class9_z +
                      school : class9_z +
                      settlement : class9_z +
                      prevSch : class9_z +
                      preceding : prevSch +
                      preceding : gender +
                      preceding : settlement +
                      preceding +
                      FLEECEcompetitor +
                      duration_z, 
                    data = goose.no.l.z)
summary(new.model0d)
vif.mer(new.model0d)
plot(allEffects(new.model0d))




# no-POOL model without social*social interactions
# still need to test random slopes
new.model3 <- lmer(norm_euclid_i_u ~ (1 + school | word) +
                     (1 | speaker) +
                     school + age_z + gender + settlement + prevSch +
                     preceding : duration_z +
                     school : age_z +
                     preceding : prevSch +
                     preceding : gender +
                     preceding : settlement +
                     preceding + 
                     duration_z, 
                   data = goose.no.l.z)
summary(new.model3)
vif.mer(new.model3)
plot(allEffects(new.model3))

# no-POOL model with social*social interactions (none were sig.)
# school*age is removed early on but adding it back in at the end puts it at almost significant
# removing the interactions means none of the social variables are significant as fixed effects
new.model3a <- lmer(norm_euclid_i_u ~ (1 + gender | word) +
                      (1 | speaker) +
                      gender + settlement +
                      preceding : duration_z +
                      preceding : gender +
                      preceding : settlement +
                      preceding + 
                      duration_z, 
                    data = goose.no.l.z)
summary(new.model3a)
vif.mer(new.model3a)
plot(allEffects(new.model3a))

# the results between the /l/ and non-/l/ models are mostly pretty similar
# we'll keep the /l/ tokens in for now but at the moment it's not too bad if they need to be removed later



### Private school-only models

c.goose <- goose %>% filter(school == 'private') %>% droplevels
c.goose.z <- c.goose %>%
  mutate(housePrice_z = scale(housePriceVsMean),
         age_z = scale(age),
         class9_z = scale(ClassScore9),
         groupSize_z = scale(groupSize),
         syllables_z = scale(syllables),
         logFreq_z = scale(logFreq),
         duration_z = scale(duration))



# full social*social model with most of the sig prec*social interactions from models above
c.model0 <- lmer(norm_euclid_i_u ~ (1 + room | word) +
                      (1 | speaker) +
                      room + age_z + gender + settlement + prevSch + class9_z +
                      preceding : duration_z +
                      room : age_z +
                      gender : room +
                      gender : prevSch +
                      room : settlement +
                      gender : age_z +
                      gender : class9_z +
                      room : class9_z +
                      settlement : class9_z +
                      prevSch : class9_z +
                      preceding : prevSch +
                      preceding : gender +
                      preceding : settlement +
                      preceding : room +
                      preceding +
                      final.l +
                      FLEECEcompetitor +
                      duration_z, 
                    data = c.goose.z)
summary(c.model0)
vif.mer(c.model0)
plot(allEffects(c.model0))

# only sig or non-sig variables left
# can only handle age as a random slope
c.model0a <- lmer(norm_euclid_i_u ~ (1 + age_z | word) +
                    (1 | speaker) +
                    room + age_z + prevSch + class9_z +
                    preceding : duration_z +
                    room : age_z +
                    room : class9_z +
                    preceding : prevSch +
                    preceding : room +
                    final.l +
                    preceding + 
                    duration_z, 
                  data = c.goose.z)
summary(c.model0a)
vif.mer(c.model0a)
plot(allEffects(c.model0a))

# let's have a look at the sig interactions to check if they really mean much
# thinking about it, maybe prevSch isn't such a great variable for the individual school models, as it's very imbalanced
p.scho <- goose %>%
  group_by(speaker) %>%
  count(school, prevSch) %>%
  group_by(school, prevSch) %>% # add / remove grouping variables as needed
  count(school)
p.scho

room.age <- c.goose %>%
  group_by(speaker) %>%
  count(room, age) %>%
  group_by(room, age) %>% # add / remove grouping variables as needed
  count(room)
room.age
# the room/age balance isn't too bad but age itself is bad at the private school - 15x 16yos, 4x 17yos

room.class <- c.goose %>%
  group_by(speaker) %>%
  count(room, ClassScore9) %>%
  group_by(room, ClassScore9) %>% # add / remove grouping variables as needed
  count(room)
room.class
# class*room isn't too bad given the class homogeneity at the private school but all 3 of the 7s are in the reserved room and the 9s are in a 4/2 outg/res split, hence a slight corr between higher class and outg

# conclusion: scrap prevSch and age; keep class
c.model0b <- lmer(norm_euclid_i_u ~ (1 + room | word) +
                    (1 | speaker) +
                    room + class9_z +
                    preceding : duration_z +
                    room : class9_z +
                    preceding : room +
                    final.l +
                    preceding + 
                    duration_z, 
                  data = c.goose.z)
summary(c.model0b)
vif.mer(c.model0b)
plot(allEffects(c.model0b))

# OK PRIVATE-SCHOOL MODEL
# sig effects only
# can't handle duration_z as a random slope
c.model1 <- lmer(norm_euclid_i_u ~ (1 + room | word) +
                    (1 | speaker) +
                    room +
                    final.l +
                    preceding + 
                    duration_z +
                    preceding : duration_z, 
                  data = c.goose.z)
summary(c.model1)
vif.mer(c.model1)
plot(allEffects(c.model1))

# from the top again - this time without age and prevSch
# settlement is kept, though it's a bit limited - 12/7 rural/town
c.model0c <- lmer(norm_euclid_i_u ~ (1 + room | word) +
                   (1 | speaker) +
                   room + gender + settlement + class9_z +
                   preceding : duration_z +
                   gender : room +
                   room : settlement +
                   gender : class9_z +
                   room : class9_z +
                   settlement : class9_z +
                   preceding : gender +
                   preceding : settlement +
                   preceding : room +
                   preceding +
                   final.l +
                   FLEECEcompetitor +
                   duration_z, 
                 data = c.goose.z)
summary(c.model0c)
vif.mer(c.model0c)
plot(allEffects(c.model0c))

# GOOD PRIVATE-SCHOOL-ONLY MODEL
# can only handle room as a random slope
# removing the interactions means settlement is no longer a sig main effect but room is! :)
c.model2 <- lmer(norm_euclid_i_u ~ (1 + room | word) +
                   (1 | speaker) +
                   room + settlement +
                   preceding : duration_z +
                   preceding : settlement +
                   preceding +
                   final.l +
                   duration_z, 
                 data = c.goose.z)
summary(c.model2)
vif.mer(c.model2)
plot(allEffects(c.model2))

# ANOVA table for thesis corrections
anova(c.model2)

# get pairwise comparisons for phonological contexts for thesis corrections
emmeans(c.model2, list(pairwise ~ preceding), adjust = 'Bonferroni')

# CIs instead of t-value
tab_model(c.model2, show.adj.icc = TRUE, show.est = TRUE, show.se = TRUE)
