############# PLOTS FOR /t/ DATA

### GUIDE FOR PUTTING HIGH-QUALITY PICTURES IN MICROSOFT WORD
# If using Office 2010/2016, save the plot in R as an EPS file
# Convert the EPS file to an EMF file using software such as Adobe Illustrator, GIMP or InkScape, or use a website such as:
# https://cloudconvert.com/
# https://convertio.co/emf-converter/
# If using Office 365/2019, save the plot in R as an SVG file and insert it.
# Sometimes EMF files look blurry in Word. This may be to do with resizing, etc: 
# https://superuser.com/questions/973500/how-to-convert-eps-file-to-emf-in-windows-using-a-free-software
#
# Problem: the EPS file format is not compatible with transparent / translucent points (eg 'alpha = 0.3')
# Partial solution: save the file as a PDF or SVG, then convert it to EMF. This produces opaque points.
# It is possible to open PDFs in Word but the axis labels don't sit with the plot properly
# Another option is to use ggsave("plot.eps", device = cairo_ps, fallback_resolution = 600)
# However, the translucent points look blurry if you do this
# Could try ggsaving as an SVG then converting to EMF
###

### See 'Get glottal percentages... (etc)' R code for some preliminary steps to get relevant % data

library(tidyverse)
library(ggbeeswarm)

setwd("C:/Users/alderton/Box Sync/PhD/Elan")

master <- read.csv("AllTMaster.csv")

# load row form of percentages data
pcts <- read.csv("Glottal percentages for each context for each speaker.csv")

# exclude only the most rubbish tokens for the initial graph
broad.data <- master %>% filter(!grepl('imitat|cuts|laugh|count', notes)) %>% 
  filter(!grepl('obstruent|laughter|other', followingContextSO)) %>%
  filter(precedingSound != 's' & precedingSound != 'k', speaker != 'Alex') %>%
  filter(!grepl('other', overallContextMedium)) %>%
  droplevels

# exclude the pre-C, unreleased and other tokens for the statistical modelling
good.data <- master %>% filter(!grepl('imitat|cuts|laugh|count', notes)) %>% 
  filter(!grepl('obstruent|laughter|other', followingContextSO)) %>%
  filter(precedingSound != 's' & precedingSound != 'k', speaker != 'Alex',
         tProduction != 'unreleased' & tProduction != 'other') %>%
  filter(!grepl('C|other', overallContextMedium)) %>%
  droplevels

# filter to remove the pre-C rows and Jay and select only the most useful columns
pcts <- pcts %>% filter(overallContextMedium != '_#C' & overallContextMedium != '_C') %>%
  filter(speaker != 'Alex') %>%
  select(speaker, gender, school, previousSchoolType, ClassScore9, overallContextMedium, totalTokens, glottalPercent) %>%
  rename('context' = overallContextMedium, 'class' = ClassScore9, 'prevSch' = previousSchoolType) %>%
  droplevels()
View(pcts)

# get mean percentage of glottalling for a particular context (useful to comapre individuals to)
pcts %>% filter(context == '_V') %>% summarise(mean(glottalPercent))

# re-order the prevSch types to make it consistent with school
pcts$prevSch <- factor(pcts$prevSch, c("state", "private"))


## set facet labels for the contexts
# phon. description version
# context.labels <- c('_#zero' = "Pre-pausal", '_#V' = "Word-final pre-vocalic", 
#                    '_syllabic' = "Pre-syllabic", '_V' = "Word-medial pre-vocalic")
# lexical set version
context.labels <- c('_#zero' = "WHAT", '_#V' = "SORT OF", 
                    '_syllabic' = "LITTLE", '_V' = "BUTTER")


###### Initial /t/ production by following context plot (FIGURE 1  of results)
# If using 'lexical sets' for the contexts, they might need renaming

# re-order the /t/ realisations and following contexts
broad.data$tProduction2 <- factor(broad.data$tProduction, c("alveolar stop", "glottal stop", "tap", "unreleased", "other"))
broad.data$overallContextNarrow2 <- reorder(broad.data$overallContextNarrow, broad.data$tProduction2 == 'alveolar stop')


# make plot
ggplot(data = broad.data, aes(x = overallContextNarrow2)) + 
  geom_bar(aes(fill = tProduction2), position = "fill") + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black")) +
  xlab("\nFollowing phonological context") + 
  ylab("Ratio of realisations of /t/\n") + 
  scale_fill_brewer(name = "Realisation of /t/", palette = "Set1", 
                    breaks = c("alveolar stop","glottal stop","tap", "unreleased", "other"),
                    labels = c("Alveolar stop","Glottal stop","Tap", "Unreleased", "Other")) +
  scale_x_discrete(limits = c("_#C", "_C", "_#zero", "_#V", "_syllabic-l", "_syllabic-n", "_V"), 
                 labels = c("Word-initial\nconsonant", "Word-medial\nconsonant", "Pause", "Word-initial\nvowel", "Syllabic\n/l/", "Syllabic\nnasal", "Word-medial\nvowel"))


###### Private-school room beeswarm plot for each context
# this needs editing a bit because pcts already has pre-C rows removed and doesn't have 'room' column

# filter data for private school only and to remove the pre-C rows
ch.pcts <- pcts %>% filter(school == 'private') %>% 
#                           , overallContextMedium != '_#C' & overallContextMedium != '_C') %>% 
  droplevels()

# select only the most useful columns
room.pcts <- ch.pcts %>% select(speaker, gender, room, overallContextMedium, totalTokens, glottalPercent) # easier to read
View(room.pcts)

# get room means regardless of context
# room.means <- room.pcts %>%
#   group_by(room) %>%
#   summarise(mean_p = mean(glottalPercent, na.rm = T))

# get room means for each context
room.context.means <- room.pcts %>%
  group_by(room, overallContextMedium) %>%
  summarise(mean_p = mean(glottalPercent, na.rm = T))

# re-order the contexts to make the facets look nice
ch.pcts$overallContextMedium2 <- 
  factor(ch.pcts$overallContextMedium, c("_#zero", "_#V", "_syllabic", "_V"))
room.context.means$overallContextMedium2 <- 
  factor(room.context.means$overallContextMedium, c("_#zero", "_#V", "_syllabic", "_V"))

# make plot
ggplot(data = room.context.means, aes(x = room, y = mean_p)) + # add 'colour = room' if so desired
  geom_line(data = room.context.means, aes(x = room, y = mean_p, group = 1), 
            alpha = 0.3, colour = "grey", size = 2) + # 'group = 1' is needed when the line connects only one point per group
  geom_beeswarm(data = ch.pcts, aes(x = room, y = glottalPercent, colour = room, group = speaker), 
                alpha = 0.3, size = 3, priority = 'random', cex = 4.2, groupOnX = TRUE) + # adjust 'cex' for appropriate level of jitter
  geom_point(size = 3, shape = 3, alpha = 0.8) +
  facet_wrap(~overallContextMedium2, labeller = labeller(overallContextMedium2 = context.labels)) +
  xlab("\nRoom") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  scale_x_discrete(breaks=c("outgoing", "reserved"),
                   labels=c("Outgoing", "Reserved"))


###### gender beeswarm plot for each context

# get gender means for each context
gender.context.means <- pcts %>%
  group_by(gender, context) %>%
  summarise(mean_p = mean(glottalPercent, na.rm = T))

# re-order the contexts to make the facets look nice
pcts$context2 <- 
  factor(pcts$context, c("_#zero", "_#V", "_syllabic", "_V"))
gender.context.means$context2 <- 
  factor(gender.context.means$context, c("_#zero", "_#V", "_syllabic", "_V"))

# optional: cut off participants with small token totals for certain contexts
pcts.cutoff <- pcts %>% filter(totalTokens >= 8) %>% droplevels
gender.context.means.cutoff <- pcts.cutoff %>%
  group_by(gender, context2) %>%
  summarise(mean_p = mean(glottalPercent, na.rm = T))

# make plot
ggplot(data = gender.context.means, aes(x = gender, y = mean_p)) + # add 'colour = room' if so desired
  geom_line(data = gender.context.means, aes(x = gender, y = mean_p, group = 1), 
            alpha = 0.3, colour = "grey", size = 2) + # 'group = 1' is needed when the line connects only one point per group
  geom_beeswarm(data = pcts, aes(x = gender, y = glottalPercent, colour = gender, group = speaker), 
                alpha = 0.3, size = 3, priority = 'random', cex = 4, groupOnX = T) + # adjust 'cex' for appropriate level of jitter
  geom_point(size = 3, shape = 3, alpha = 0.8) +
  facet_wrap(~context2, labeller = labeller(context2 = context.labels)) +
  xlab("\nGender") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  scale_x_discrete(breaks=c("female", "male"),
                   labels=c("Girls", "Boys"))

# cut-off version of plot
ggplot(data = gender.context.means.cutoff, aes(x = gender, y = mean_p)) + # add 'colour = room' if so desired
  geom_line(data = gender.context.means.cutoff, aes(x = gender, y = mean_p, group = 1), 
            alpha = 0.3, colour = "grey", size = 2) + # 'group = 1' is needed when the line connects only one point per group
  geom_beeswarm(data = pcts.cutoff, aes(x = gender, y = glottalPercent, colour = gender, group = speaker), 
                alpha = 0.3, size = 3, priority = 'random', cex = 4, groupOnX = T) + # adjust 'cex' for appropriate level of jitter
  geom_point(size = 3, shape = 3, alpha = 0.8) +
  facet_wrap(~context2, labeller = labeller(context2 = context.labels)) +
  xlab("\nGender") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  scale_x_discrete(breaks=c("female", "male"),
                   labels=c("Girls", "Boys"))

#### school beeswarm plot for each context

# get school means for each context
school.context.means <- pcts %>%
  group_by(school, context) %>%
  summarise(mean_p = mean(glottalPercent, na.rm = T))

# re-order the contexts to make the facets look nice
school.context.means$context2 <- 
  factor(school.context.means$context, c("_#zero", "_#V", "_syllabic", "_V"))

# optional: cut off participants with small token totals for certain contexts
pcts.cutoff <- pcts %>% filter(totalTokens >= 8) %>% droplevels
school.context.means.cutoff <- pcts.cutoff %>%
  group_by(school, context2) %>%
  summarise(mean_p = mean(glottalPercent, na.rm = T))

# make plot
ggplot(data = school.context.means, aes(x = school, y = mean_p)) + # add 'colour = room' if so desired
  geom_line(data = school.context.means, aes(x = school, y = mean_p, group = 1), 
            alpha = 0.3, colour = "grey", size = 2) + # 'group = 1' is needed when the line connects only one point per group
  geom_beeswarm(data = pcts, aes(x = school, y = glottalPercent, colour = school, group = speaker), 
                alpha = 0.3, size = 3, priority = 'random', cex = 4, groupOnX = T) + # adjust 'cex' for appropriate level of jitter
  geom_point(size = 3, shape = 3, alpha = 0.8) +
  facet_wrap(~context2, labeller = labeller(context2 = context.labels)) +
  xlab("\nSchool") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  scale_x_discrete(breaks=c("private", "state"),
                   labels=c("Private school", "State school"))

# cut-off version of plot
ggplot(data = school.context.means.cutoff, aes(x = school, y = mean_p)) + # add 'colour = room' if so desired
  geom_line(data = school.context.means.cutoff, aes(x = school, y = mean_p, group = 1), 
            alpha = 0.3, colour = "grey", size = 2) + # 'group = 1' is needed when the line connects only one point per group
  geom_beeswarm(data = pcts.cutoff, aes(x = school, y = glottalPercent, colour = school, group = speaker), 
                alpha = 0.3, size = 3, priority = 'random', cex = 4, groupOnX = T) + # adjust 'cex' for appropriate level of jitter
  geom_point(size = 3, shape = 3, alpha = 0.8) +
  facet_wrap(~context2, labeller = labeller(context2 = context.labels)) +
  xlab("\nSchool") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  scale_x_discrete(breaks=c("private", "state"),
                   labels=c("Private school", "State school"))


#### previous school beeswarm plot for each context

# get previous school means for each context
prevSch.context.means <- pcts %>%
  group_by(prevSch, context) %>%
  summarise(mean_p = mean(glottalPercent, na.rm = T))

# re-order the contexts to make the facets look nice
prevSch.context.means$context2 <- 
  factor(prevSch.context.means$context, c("_#zero", "_#V", "_syllabic", "_V"))

# re-order the prevSch types to make it consistent with school
prevSch.context.means$prevSch <- factor(prevSch.context.means$prevSch, c("state", "private"))

# optional: cut off participants with small token totals for certain contexts
pcts.cutoff <- pcts %>% filter(totalTokens >= 8) %>% droplevels
prevSch.context.means.cutoff <- pcts.cutoff %>%
  group_by(prevSch, context2) %>%
  summarise(mean_p = mean(glottalPercent, na.rm = T))

# make plot
ggplot(data = prevSch.context.means, aes(x = prevSch, y = mean_p)) + # add 'colour = room' if so desired
  geom_line(data = prevSch.context.means, 
            aes(x = prevSch, y = mean_p, group = 1), 
            alpha = 0.3, colour = "grey", size = 2) + # 'group = 1' is needed when the line connects only one point per group
  geom_beeswarm(data = pcts, aes(x = prevSch, y = glottalPercent, 
                                 colour = prevSch, group = speaker), 
                alpha = 0.3, size = 3, priority = 'random', cex = 4, groupOnX = T) + # adjust 'cex' for appropriate level of jitter
  geom_point(size = 3, shape = 3, alpha = 0.8) +
  facet_wrap(~context2, labeller = labeller(context2 = context.labels)) +
  xlab("\nPrevious school") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  scale_x_discrete(breaks=c("state", "private"),
                   labels=c("State school", "Private school"))

# cut-off version of plot
ggplot(data = prevSch.context.means.cutoff, aes(x = prevSch, y = mean_p)) + # add 'colour = room' if so desired
  geom_line(data = prevSch.context.means.cutoff, 
            aes(x = prevSch, y = mean_p, group = 1), 
            alpha = 0.3, colour = "grey", size = 2) + # 'group = 1' is needed when the line connects only one point per group
  geom_beeswarm(data = pcts.cutoff, aes(x = prevSch, y = glottalPercent, 
                                        colour = prevSch, group = speaker), 
                alpha = 0.3, size = 3, priority = 'random', cex = 4, groupOnX = T) + # adjust 'cex' for appropriate level of jitter
  geom_point(size = 3, shape = 3, alpha = 0.8) +
  facet_wrap(~context2, labeller = labeller(context2 = context.labels)) +
  xlab("\nPrevious school") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black")) +
  scale_x_discrete(breaks=c("private", "state"),
                   labels=c("Private school", "State school"))


#### Attempt at class * gender plot

# load speaker percentages data
pcts.spk <- read.csv("Glottal percentages for each speaker.csv")

# filter to remove the pre-C rows and Jay and select only the most useful columns
pcts.spk <- pcts.spk %>%
  filter(speaker != 'Jay') %>%
  select(speaker, gender, school, previousSchoolType, ClassScore9, totalTokens, glottalPercent) %>%
  rename('class' = ClassScore9, 'prevSch' = previousSchoolType) %>%
  droplevels()
View(pcts.spk)

# re-order the prevSch types to make it consistent with school
pcts.spk$prevSch <- factor(pcts.spk$prevSch, c("state", "private"))

# get class means for each gender
class.gender.means <- pcts.spk %>%
  group_by(class, gender) %>%
  summarise(mean_p = mean(glottalPercent, na.rm = T))

# optional: cut off participants with small token totals for certain contexts
# pcts.cutoff <- pcts %>% filter(totalTokens >= 8) %>% droplevels
# prevSch.context.means.cutoff <- pcts.cutoff %>%
#  group_by(prevSch, context2) %>%
#  summarise(mean_p = mean(glottalPercent, na.rm = T))

# change class to a factor - only do this for the beeswarm plot
class.gender.means$class <- as.factor(class.gender.means$class)
pcts.spk$class <- as.factor(pcts.spk$class)

# make beeswarm plot with means - doesn't really work
ggplot(data = class.gender.means, aes(x = class, y = mean_p)) + # add 'colour = room' if so desired
  geom_line(data = class.gender.means, 
            aes(x = class, y = mean_p), 
            alpha = 0.3, colour = "grey", size = 2, group = 1) + # 'group = 1' is needed when the line connects only one point per group
  geom_beeswarm(data = pcts.spk, aes(x = class, y = glottalPercent, 
                                 colour = class, group = speaker), 
                alpha = 0.3, size = 3, priority = 'random', cex = 4, groupOnX = T) + # adjust 'cex' for appropriate level of jitter
  geom_point(size = 3, shape = 3, alpha = 0.8) +
  facet_wrap(~gender) +
  xlab("\nClass score") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour="black"))

# new attempt with normal geom_point
# note this requires class to be numeric, not a factor
ggplot(pcts.spk, aes(x = class, y = glottalPercent, colour = gender)) + 
  geom_point(shape = 16, size = 3, alpha = 0.5, position = position_jitter(width = 0.3)) +
  geom_smooth(method = lm,
              se = FALSE,
              fullrange = TRUE) +
  xlab("\nClass score") + 
  ylab("Percentage of glottal /t/\n") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(3, 9, 1)) +
  scale_colour_hue(l = 50,
                      name="Gender",
                      breaks=c("female", "male"),
                      labels=c("Girls", "Boys"))

ggsave("class-gender plot.eps", device=cairo_ps, fallback_resolution = 600)

# consider saving as svg and converting to eps if word can't load it

###
# Might need to go back to the 'Get percentages' code and edit that a bit to get the internal factor %s for those plots
