#
# Shawn Schwartz
# EEB 109L - Summer 2018
#
# Email: shawnschwartz@ucla.edu
#

# Microplastics vs. Sand Crabs - Final Project Paper Analyses of Data

#set working directory
setwd("~/OneDrive/UCLA Undergraduate Degree/Summer 2018/Summer Session C - 2018/EE BIOL 109L/R-Projects/EEB109L-Microplastics")

# load tidyverse library
library(tidyverse)

# read-in csv file data
Zuma <- read.csv(file = "ZumaBeachData.csv", header = TRUE)
Dume <- read.csv(file = "PointDumeData.csv", header = TRUE)

# make scatterplot of all crab counts and microplastic proportions for Zuma Beach

plot(Zuma$Total.Sand.Crab.Count, Zuma$Microplastic.Proportion, xlab = "Sand Crab count", ylab = "Proportion of Microplastics", main = "Sand crabs and microplastics - Zuma Beach")

# fit linear regression model
ZumaBeachCrabsFit <- lm(Total.Sand.Crab.Count ~ Microplastic.Proportion, data = Zuma)

summary(ZumaBeachCrabsFit)

# probability distribution of the residuals
plot(density(resid(ZumaBeachCrabsFit)), xlab = "Residuals", ylab = "Probability density", main = "Distribution of residuals")

plot(ZumaBeachCrabsFit, which = 2)

plot(Zuma$CrabCounts, Zuma$Microplastic.Proportion, xlab = "Sand Crab Count", ylab = "Proportion of Microplastics", main = "Sand crabs and microplastics - Zuma Beach")

abline(ZumaBeachCrabsFit)

legend("bottomright", bty = "n", legend = paste("R-squared=", format(summary(ZumaBeachCrabsFit)$r.squared, digits = 4)))