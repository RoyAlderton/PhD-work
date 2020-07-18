### GOOSE VISUALISATION

library(plyr)
library(tidyverse)

setwd("C:/Users/alderton/Box Sync/PhD/Production Data/GOOSE Praat Files")

goose.hVd <- read.csv("All GOOSE & hVd.csv")
View(goose.hVd)

# get log of word frequency; remove unneeded tokens
goose.hVd <- mutate(goose.hVd, logFreq = log10(wordFrequencyPlus1)) %>% 
  filter(speaker != 'Alex') %>% filter(followingContext != 'other') %>%
  filter(!grepl('imitat|cuts|laugh|exagg|example|funny', notes)) %>%
  filter(!grepl('used0|who0', fileName)) %>% # including these tokens completely changes the direction of the task type effect, showing that they have a massive influence on the result
  rename(prevSch = previousSchoolType,
         contextYOUPOOL = overallContextYOUPOOL,
         contextPOOL = overallContextPOOL,
         contextGOOSE = overallContextWithinGOOSE,
         preceding = precedingContext,
         final.l = followingFinal.l) %>% droplevels

# change some of the baseline levels
goose.hVd$prevSch <- relevel(goose.hVd$prevSch, 'state')
goose.hVd$preceding <- relevel(goose.hVd$preceding, 'nonCoronal')

# (remove non-GOOSE vowels)
goose <- goose.hVd %>% filter(vowel == 'GOOSE') %>% droplevels
  
# these plots might look better if pre-/l/ tokens are removed
goose.hVd.no.l <- goose.hVd %>% filter(final.l == 'no') %>% droplevels
  

#### Plotting raw F1/F2 data
# super-basic raw F1/F2 gender plot
ggplot(data = goose.hVd, aes(x = F2, y = F1, colour = vowel)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~ gender) +
  scale_y_reverse() +
  scale_x_reverse() +
  theme_bw()

# get vowel means for each speaker
v.means <- goose.hVd %>% 
  select(vowel, speaker, gender, school, prevSch, room, ClassScore9, F1, F2, F1norm, F2norm, norm_euclid_i_u) %>%
  group_by(vowel, speaker, gender, school, prevSch, room, ClassScore9) %>%
  summarise(mean(F1), mean(F2)) %>%
  rename('meanF1' = `mean(F1)`, 'meanF2' = `mean(F2)`)
View(v.means)

# find envelope of vowel means
find_hull = function(v.means) {
v.means[chull(v.means$meanF2, v.means$meanF1), ]
}

# apply this function to find the envelope for each speaker
hulls <- ddply(v.means, .(speaker), find_hull)

# ellipses for each vowel mean
ggplot(data = v.means, aes(x = meanF2, y = meanF1, colour = vowel)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_reverse() +
  stat_ellipse() +
  facet_wrap(~ gender) +
  theme_bw()

# vowel mean labels, coloured by vowel, no envelopes
# change the gender sub-set as necessary - this makes it easier to read
ggplot(data = v.means [v.means$gender == "male",], aes(x = meanF2, y = meanF1, colour = vowel, label = vowel)) +
  geom_text(size = 2.5) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ speaker) +
  theme_bw()

# version with envelopes
# change the gender sub-set as necessary - this makes it easier to read
ggplot(data = v.means [v.means$gender == "male",], aes(x = meanF2, y = meanF1, colour = speaker, fill = speaker, label = vowel)) +
  geom_text(size = 2.5) +
  geom_polygon(data = hulls[hulls$gender == "male",], alpha = .2) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ speaker) +
  theme_bw()


#### Plotting normalised F1/F2 data
# Assumes that vowels have already been normalised
# super-basic normalised F1/F2 gender plot
ggplot(data = goose.hVd, aes(x = F2norm, y = F1norm, colour = vowel)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~ gender) +
  scale_y_reverse() +
  scale_x_reverse() +
  theme_bw()

# get normalised vowel means for each speaker
v.norm.means <- goose.hVd %>% 
  select(vowel, speaker, gender, school, prevSch, room, ClassScore9, F1, F2, F1norm, F2norm, norm_euclid_i_u) %>%
  group_by(vowel, speaker, gender, school, prevSch, room, ClassScore9) %>%
  summarise(mean(F1norm), mean(F2norm)) %>%
  rename('meanF1norm' = `mean(F1norm)`, 'meanF2norm' = `mean(F2norm)`)
View(v.norm.means)

# find envelope of normalised vowel means
find_hull = function(v.norm.means) {
  v.norm.means[chull(v.norm.means$meanF2norm, v.norm.means$meanF1norm), ]
}

# apply this function to find the envelope for each speaker
# old version using plyr
#   hulls.norm <- ddply(v.norm.means, .(speaker), find_hull)
# new version using dplyr
# new version seems to cause an issue with making the envelopes
hulls.norm <- v.norm.means %>% group_by(speaker) %>% find_hull()

# ellipses for each normalised vowel mean
# this doesn't work very well - the GOOSE norm means are much more clustered than the others
ggplot(data = v.norm.means, aes(x = meanF2norm, y = meanF1norm, colour = vowel)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_reverse() +
  stat_ellipse() +
  facet_wrap(~ gender) +
  theme_bw()

# normalised vowel mean labels, coloured by vowel, no envelopes
# change the gender sub-set as necessary - this makes it easier to read
ggplot(data = v.norm.means [v.norm.means$gender == "male",], aes(x = meanF2norm, y = meanF1norm, colour = vowel, label = vowel)) +
  geom_text(size = 2.5) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ speaker) +
  theme_bw()

# version with envelopes
# change the gender sub-set as necessary - this makes it easier to read
ggplot(data = v.norm.means [v.norm.means$gender == "male",], aes(x = meanF2norm, y = meanF1norm, colour = school, fill = school, label = vowel)) +
  geom_text(size = 2.5) +
  geom_polygon(data = hulls.norm[hulls.norm$gender == "male",], alpha = .2) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ speaker) +
  theme_bw()

# Private school-only version by room
ggplot(data = v.norm.means [v.norm.means$school == "private",], aes(x = meanF2norm, y = meanF1norm, colour = room, fill = room, label = vowel)) +
  geom_text(size = 2.5) +
  geom_polygon(data = hulls.norm[hulls.norm$school == "private",], alpha = .2) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ speaker) +
  theme_bw()

###### gender comparison
# speaker envelopes by gender
# clearly a mess to look at but shows that the physiological differences have been removed
ggplot(data = v.norm.means, aes(x = meanF2norm, y = meanF1norm, colour = speaker, fill = speaker, label = vowel)) +
  geom_text(size = 2.5) +
  geom_polygon(data = hulls.norm, alpha = .2) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ gender) +
  theme_bw()

# apply the hulls function to find the envelope for each gender
hulls.g.norm <- ddply(v.norm.means, .(gender), find_hull)

hulls.g.norm <- v.norm.means %>% group_by(gender) %>% find_hull()

# shows envelope for each gender but vowel means for each speaker
# would be good if the GOOSE ones weren't much more clustered than the others
ggplot(data = v.norm.means, aes(x = meanF2norm, y = meanF1norm, colour = gender, fill = gender, label = vowel)) +
  geom_text(size = 2.5) +
  geom_polygon(data = hulls.g.norm, alpha = .2) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ gender) +
  theme_bw()

# in this case, we need to get normalised vowel means for each gender
v.gen.norm.means <- goose.hVd %>% 
  select(vowel, speaker, gender, school, prevSch, room, ClassScore9, F1, F2, F1norm, F2norm, norm_euclid_i_u) %>%
  group_by(vowel, gender) %>%
  summarise(mean(F1norm), mean(F2norm)) %>%
  rename('meanF1norm' = `mean(F1norm)`, 'meanF2norm' = `mean(F2norm)`)
View(v.gen.norm.means)

# find envelope of normalised gender vowel means
find_gen_hull = function(v.gen.norm.means) {
  v.gen.norm.means[chull(v.gen.norm.means$meanF2norm, v.gen.norm.means$meanF1norm), ]
}

# apply this function to find the envelope for each gender
hulls.gen.norm <- ddply(v.gen.norm.means, .(gender), find_gen_hull)

hulls.gen.norm <- v.gen.norm.means %>% group_by(gender) %>% find_gen_hull()

# shows envelopes and vowel means for each gender
# very comparable - no physiological differences
ggplot(data = v.gen.norm.means, aes(x = meanF2norm, y = meanF1norm, colour = gender, fill = gender, label = vowel)) +
  geom_text(size = 2.5) +
  geom_polygon(data = hulls.gen.norm, alpha = .2) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ gender) +
  theme_bw()

## repeat as needed for school, prevSch, room, etc

# filter private school only to get rooms
c.v.norm.means <- v.norm.means %>% filter(school == 'private') %>% droplevels

# apply the hulls function to find the envelope for each room
hulls.r.norm <- ddply(c.v.norm.means, .(room), find_hull)

# shows envelope for each room but vowel means for each speaker
# would be good if the GOOSE ones weren't much more clustered than the others
ggplot(data = c.v.norm.means, aes(x = meanF2norm, y = meanF1norm, colour = room, fill = room, label = vowel)) +
  geom_text(size = 2.5) +
  geom_polygon(data = hulls.r.norm, alpha = .2) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~ room) +
  theme_bw()

# continue as for gender above


#################
# old visualisations from private-school GOOSE result R code
# some of these will be good to use, especially the box plots
# needs streamlining for the newly-normalised data

fleece <- goose.hVd %>% filter(vowel == 'FLEECE') %>% droplevels

# fleece <- read.csv("C:/Users/alderton/Box Sync/PhD/Production Data/GOOSE Praat Files/FLEECE vowels.csv")
View(fleece)

c.goose <- goose %>% filter(school == 'private') %>% droplevels
c.fleece <- fleece %>% filter(school == 'private') %>% droplevels
a.goose <- goose %>% filter(school == 'state') %>% droplevels
a.fleece <- fleece %>% filter(school == 'state') %>% droplevels

# each private-school speaker's vowel plot
ggplot(data = c.goose, aes(x = f2norm, y = f1norm, label = word, colour = room)) + 
  geom_text(size = 2) + 
  scale_y_reverse() + 
  scale_x_reverse() + 
  facet_wrap(~ speaker) + 
  theme_bw() +
  stat_ellipse()

# facetted by room
ggplot(data = c.goose, aes(x = f2norm, y = f1norm, colour = room)) + 
  geom_point(size = 1.5) + 
  scale_y_reverse() + 
  scale_x_reverse() + 
  facet_wrap(~ room) + 
  theme_bw() +
  stat_ellipse()

# each private-school speaker's vowel plot with FLEECE - can't change the colour of FLEECE without messing it up but it's OK for now
# could be solved by adding FLEECE to the main data set but then you can't use colour to differentiate groups

ggplot(data = c.goose, aes(x = F2norm, y = F1norm)) + 
  geom_text(aes(label = word, colour = room), size = 2) +
  scale_x_reverse() + 
  scale_y_reverse() +
  facet_wrap(~ speaker) + 
  theme_bw() +
  stat_ellipse(aes(colour = room)) +
  geom_text(data = c.fleece, aes(x = F2norm, y = F1norm, label = vowel), size = 2, show.legend = FALSE, inherit.aes = FALSE)

# version facetted by room
ggplot(data = c.goose, aes(x = f2norm, y = f1norm)) + 
  geom_text(aes(label = word, colour = room), size = 2) +
  scale_x_reverse() + 
  scale_y_reverse() +
  facet_wrap(~ room) + 
  theme_bw() +
  stat_ellipse(aes(colour = room)) +
  geom_text(data = c.fleece, aes(x = f2norm, y = f1norm, label = vowel), size = 2, show.legend = FALSE, inherit.aes = FALSE)

# overall big version
ggplot(data = goose, aes(x = F2norm, y = F1norm)) + 
  geom_text(aes(label = word, colour = vowel), size = 2) +
  scale_x_reverse() + 
  scale_y_reverse() +
  theme_bw() +
  stat_ellipse(aes(colour = vowel)) +
  geom_text(data = fleece, aes(x = F2norm, y = F1norm, label = vowel), size = 2, show.legend = FALSE, inherit.aes = FALSE)



## better version with newly-normalised data
c.goose.fleece <- goose.hVd %>% filter(vowel == 'GOOSE' | vowel == 'FLEECE', school == 'private') %>% droplevels
c.goose.fleece.trap.start <- goose.hVd %>% filter(vowel == 'GOOSE' | vowel == 'FLEECE' | vowel == 'TRAP' | vowel == 'START', school == 'private') %>% droplevels

# consider using 'colour = context' to show how YOU / POOL are different
ggplot(data = c.goose.fleece.trap.start, 
       aes(x = F2norm, y = F1norm, colour = vowel)) + 
  geom_point() +
  scale_x_reverse() + 
  scale_y_reverse() +
  facet_wrap(~ room) + 
  theme_bw() +
  stat_ellipse()

## good version for whole data set
# can use 'colour = vowel' to just show hVd vowel differences

goose.fleece <- goose.hVd %>% filter(vowel == 'GOOSE' | vowel == 'FLEECE') %>% droplevels
goose.fleece.trap.start <- goose.hVd %>% filter(vowel == 'GOOSE' | vowel == 'FLEECE' | vowel == 'TRAP' | vowel == 'START') %>% droplevels

ggplot(data = goose.fleece.trap.start, 
       aes(x = F2norm, y = F1norm, colour = contextPOOL, shape = contextPOOL)) + 
  geom_point(alpha = 0.5) +
  scale_x_reverse() + 
  scale_y_reverse() +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  xlab("\nNormalised F2") +
  ylab("Normalised F1\n") + 
  stat_ellipse() +
  scale_colour_discrete(name = 'Vowel') +
  scale_shape_discrete(name = 'Vowel')

## version for preceding context differences
# it's quite messy to read, so maybe means would work better

# re-order the levels
goose.fleece$contextGOOSE <- factor(goose.fleece$contextGOOSE, c('FLEECE', 'GHOUL', 'Palatal', 'Coronal', 'Non-coronal', 'Liquid'))

ggplot(data = goose.fleece, 
       aes(x = F2norm, y = F1norm, colour = contextGOOSE, shape = contextGOOSE)) + 
  geom_point(alpha = 0.5) +
  scale_x_reverse() + 
  scale_y_reverse() +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  xlab("\nNormalised F2") +
  ylab("Normalised F1\n") + 
  stat_ellipse() +
  scale_colour_discrete(name = 'Vowel context') +
  scale_shape_discrete(name = 'Vowel context')

####### box plots
c.goose.no.l <- c.goose %>% filter(final.l == 'no') %>% droplevels

# room.labels <- c('outgoing' = "Outgoing", 'reserved' = "Reserved")
prec.labels <- c('nonCoronal' = "Non-coronal", 'coronal' = "Coronal",
                 'j' = 'Palatal', 'liquid' = 'Liquid')

# /l/ boxplot
ggplot(goose, aes(x = final.l, y = norm_euclid_i_u, fill = final.l)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  xlab("\nContext") +
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") + 
  guides(fill = FALSE) +
  scale_x_discrete(breaks=c("no", "yes"),
                   labels=c("GOOSE", "GHOUL"))

# gender*preceding boxplot
ggplot(goose.no.l, aes(x = gender, y = norm_euclid_i_u, fill = gender)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  xlab("\nGender") +
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") + 
  guides(fill = FALSE) +
  scale_x_discrete(breaks=c("female", "male"),
                   labels=c("Girls", "Boys")) +
  facet_wrap(~ preceding, labeller = labeller(preceding = prec.labels))

# prevSch*preceding boxplot
ggplot(goose.no.l, aes(x = prevSch, y = norm_euclid_i_u, fill = prevSch)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  xlab("\nPrevious school type") +
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") + 
  guides(fill = FALSE) +
  scale_x_discrete(breaks=c("state", "private"),
                   labels=c("State school", "Private school")) +
  facet_wrap(~ preceding, labeller = labeller(preceding = prec.labels))

# settlement*preceding boxplot
ggplot(goose.no.l, aes(x = settlement, y = norm_euclid_i_u, fill = settlement)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  xlab("\nSettlement type") +
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") + 
  guides(fill = FALSE) +
  scale_x_discrete(breaks=c("rural", "town"),
                   labels=c("Rural", "Town")) +
  facet_wrap(~ preceding, labeller = labeller(preceding = prec.labels))

# task type*preceding boxplot
# there are only 2 post-liquid tokens in the reading task, so liquid should be removed from this one
goose.no.liquid <- goose.no.l %>% filter(preceding != 'liquid') %>% droplevels

ggplot(goose.no.liquid, aes(x = taskType, y = norm_euclid_i_u, fill = taskType)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  xlab("\nTask type") +
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") + 
  guides(fill = FALSE) +
  scale_x_discrete(breaks=c("conversation", "reading"),
                   labels=c("Conversation\ntask", "Reading\ntask")) +
  facet_wrap(~ preceding, labeller = labeller(preceding = prec.labels))

# duration*preceding scatterplot
ggplot(goose.no.l, aes(x = duration, y = norm_euclid_i_u, colour = preceding)) + 
  geom_point(shape = 16, size = 2, alpha = 0.4) +
  geom_smooth(method = lm,
              se = FALSE,
              fullrange = TRUE) +
  xlab("\nDuration (ms)") + 
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
  facet_wrap(~ preceding, labeller = labeller(preceding = prec.labels)) +
  guides(colour = FALSE)


# room boxplot
ggplot(c.goose.no.l, aes(x = room, y = norm_euclid_i_u, fill = room)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  xlab("\nRoom") +
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") + 
  guides(fill = FALSE) +
  scale_x_discrete(breaks=c("outgoing", "reserved"),
                   labels=c("Outgoing", "Reserved"))

# school
ggplot(goose, aes(x = school, y = norm_euclid_i_u, fill = school)) + 
  geom_boxplot() + 
  theme_bw() + 
  xlab("\nSchool") +
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") + 
  guides(fill = FALSE)

# gender
ggplot(goose, aes(x = gender, y = norm_euclid_i_u, fill = gender)) + 
  geom_boxplot() + 
  theme_bw() + 
  xlab("\nGender") +
  ylab("Euclidean distance between\nnormalised FLEECE and GOOSE\n(F2~F1 space in z-scores)\n") + 
  guides(fill = FALSE)

##
# get means for individuals
priv.means <- c.goose.no.l %>% 
  group_by(speaker, gender, room) %>% # could add more social variables here, or just copy and paste them from another spreadsheet
  summarise(mean(norm_euclid_i_u))

write.csv(priv.means, "Private school GOOSE means.csv")

# each private-school speaker's FLEECE and GOOSE vowels
ggplot(data = goose.fleece [goose.fleece$school == 'private',], aes(x = F2norm, y = F1norm, colour = vowel)) +
  geom_point() +
  facet_wrap(~ speaker) +
  scale_y_reverse() +
  scale_x_reverse() +
  theme_bw()

