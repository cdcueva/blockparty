##################################
# Libraries

library(tidyverse)
library(car)
library(gtable)
library(gridExtra)

##################################

party <- read_csv("Downloads/MSIT 423/Block Party Project/party.csv")

# Warning message:
# Missing column names filled in: 'X1' [1] 

##################################
glimpse(party)

# Some cleaning

# X1 looks like an ID or index, change column name
colnames(party)[colnames(party)=="X1"] <- "id"

# "particpate" is misspelled, correct column name
colnames(party)[colnames(party)=="particpate"] <- "participate"

##################################

# Prep data

# Sum mile1:mile4 to new column "postmile"
party <- party %>%
  mutate(postmile = mile1 + mile2 + mile3 + mile4)

# Convert "participate" to factor
party <- party %>%
  mutate(participate = factor(participate))

class(party$participate)

# Tukey first aid: add 1 because we can't log(0) transform
party <- party %>%
  mutate(elaboration = elaboration + 1
         ,prefood = prefood + 1
         ,pregas = pregas + 1
         ,prebank = prebank +1
         ,preretail = preretail + 1
         ,preother = preother + 1
         ,Rfreq = Rfreq + 1
         ,mile1 = mile1 + 1
         ,mile2 = mile2 + 1
         ,mile3 = mile3 + 1
         ,mile4 = mile4 + 1
         ,basemile = basemile + 1
         ,postmile = postmile + 1)

# Make some plots
pre_boxplot1 <- ggplot(party) +
  geom_boxplot(aes(participate, basemile, color = participate)) +
  labs(x = "Loyalty Program Participants", y = "Total Base Miles", color = "Participate") +
  ggtitle("Pre-Period") +
  scale_x_discrete(breaks = c("1", "0"), labels = c("Yes", "No")) +
  scale_color_hue(l=50) +
  coord_flip()

post_boxplot1 <- ggplot(party) +
  geom_boxplot(aes(participate, postmile, color = participate)) +
  labs(x = "Loyalty Program Participants", y = "Total Post Miles", color = "Participate") +
  ggtitle("Post-Period") +
  scale_x_discrete(breaks = c("1", "0"), labels = c("Yes", "No")) +
  scale_color_hue(l=50) +
  coord_flip()


# Boxplot comparison shows a couple of "post" outliers, find & remove them

filter(party, postmile > 4000)
filter(party, postmile > 3000)

party <- filter(party, postmile < 3000)

# Compare plots again

#pre_boxplot2 <- ggplot(party) +
#  geom_boxplot(aes(participate, basemile, color = participate)) +
#  labs(x = "Loyalty Program Participants", y = "Total Base Miles", color = "Participate") +
#  ggtitle("Pre-Period") +
#  scale_x_discrete(breaks = c("1", "0"), labels = c("Yes", "No")) +
#  scale_color_hue(l=50) +
#  coord_flip()

post_boxplot2 <- ggplot(party) +
  geom_boxplot(aes(participate, postmile, color = participate)) +
  labs(x = "Loyalty Program Participants", y = "Total Post Miles", color = "Participate") +
  ggtitle("Post-Period Minus Outliers") +
  scale_x_discrete(breaks = c("1", "0"), labels = c("Yes", "No")) +
  scale_color_hue(l=50) +
  coord_flip()

##################################
# Does participation in the Block Party promotion increase subsequent purchases?
# Q1 Model
q1_lm <- lm(log(postmile) ~ participate + log(elaboration) + log(basemile) + log(Rfreq), party)

# Multicollinearity check
vif(q1_lm)
plot(q1_lm)

# vif shows multicollinearity between participate and log(elaboration), remove log(elaboration)

q1_lm <- lm(log(postmile) ~ participate + log(basemile) + log(Rfreq), party)
vif(q1_lm)
summary(q1_lm)


##################################
# Does the amount of elaboration (number of words written) affect subsequent purchases?
# Q2 Models by week
q2.1_lm <- lm(log(mile1) ~ log(elaboration) + log(basemile) + log(Rfreq), party)
q2.2_lm <- lm(log(mile2) ~ log(elaboration) + log(basemile) + log(Rfreq), party)
q2.3_lm <- lm(log(mile3) ~ log(elaboration) + log(basemile) + log(Rfreq), party)
q2.4_lm <- lm(log(mile4) ~ log(elaboration) + log(basemile) + log(Rfreq), party)

vif(q2.1_lm)
vif(q2.2_lm)
vif(q2.3_lm)
vif(q2.4_lm)

summary(q2.1_lm)
summary(q2.2_lm)
summary(q2.3_lm)
summary(q2.4_lm)

plot(q2.1_lm)
plot(q2.2_lm)
plot(q2.3_lm)
plot(q2.4_lm)

##################################
# For how long does the participation / elaboration affect persist?
# Q3 Models by week with filter on participation
q3.1_lm <- lm(log(mile1) ~ log(elaboration) + log(basemile) + log(Rfreq), party, subset=(participation==1))
q3.2_lm <- lm(log(mile2) ~ log(elaboration) + log(basemile) + log(Rfreq), party, subset=(participation==1))
q3.3_lm <- lm(log(mile3) ~ log(elaboration) + log(basemile) + log(Rfreq), party, subset=(participation==1))
q3.4_lm <- lm(log(mile4) ~ log(elaboration) + log(basemile) + log(Rfreq), party, subset=(participation==1))

vif(q3.1_lm)
vif(q3.2_lm)
vif(q3.3_lm)
vif(q3.4_lm)

summary(q3.1_lm)
summary(q3.2_lm)
summary(q3.3_lm)
summary(q3.4_lm)


plot(q3.1_lm)
plot(q3.2_lm)
plot(q3.3_lm)
plot(q3.4_lm)

q3.1 <- ggplot(party, aes(elaboration-1, mile1-1, color=participate)) +
  geom_point(shape = 1, show.legend=FALSE) +
  scale_color_hue(l=50) +
  labs(x = "", y = "Week 1")
  
q3.2 <- ggplot(party, aes(elaboration-1, mile2-1, color=participate)) +
  geom_point(shape = 1, show.legend=FALSE) +
  scale_color_hue(l=50) +
  labs(x = "", y = "Week 2")

q3.3 <- ggplot(party, aes(elaboration-1, mile3-1, color=participate)) +
  geom_point(shape = 1, show.legend=FALSE) +
  scale_color_hue(l=50) +
  labs(x = "", y = "Week 3")

q3.4 <- ggplot(party, aes(elaboration-1, mile4-1, color=participate)) +
  geom_point(shape = 1, show.legend = FALSE) +
  scale_color_hue(l=50) +
  labs(x = "", y = "Week 4", color = "Participate")

# Compare Q3 plots
grid.arrange(q3.1
             ,q3.2
             ,q3.3
             ,q3.4
             ,nrow = 4
             ,ncol = 1
             ,left = "Miles"
             ,bottom = "Elaboration"
             ,top = "Weekly Miles Against Elaboration"
             ,right = legend)