#
# Shawn Schwartz
# EEB 109L - Summer 2018
#
# Email: shawnschwartz@ucla.edu
#

# Microplastics vs. Sand Crabs - Final Project Paper Analyses of Data

# set working directory
setwd("~/OneDrive/UCLA Undergraduate Degree/Summer 2018/Summer Session C - 2018/EE BIOL 109L/R-Projects/EEB109L-Microplastics")

# load tidyverse library
library(tidyverse)

# read-in csv file data
# Zuma Beach 
ZumaCounts <- read.csv(file = "ZumaCounts.csv", header = TRUE)
ZumaPlastics <- read.csv(file = "ZumaPlastics.csv", header = TRUE)

# Point Dume State Beach
DumeCounts <- read.csv(file = "DumeCounts.csv", header = TRUE)
DumePlastics <- read.csv(file = "DumePlastics.csv", header = TRUE)
  

# make scatterplot of zuma beach and dume sand crab counts
# Zuma Beach Crab Counts
TotalZumaCrabs <- ZumaCounts$Small + ZumaCounts$Medium + ZumaCounts$Large
ZumaCounts <- cbind(ZumaCounts, TotalZumaCrabs)

# Point Dume State Beach Crab Counts
TotalDumeCrabs <- DumeCounts$Small + DumeCounts$Medium + DumeCounts$Large
DumeCounts <- cbind(DumeCounts, TotalDumeCrabs)

TotalCounts <- numeric(20)
TotalCounts <- cbind(TotalCounts, TotalZumaCrabs, TotalDumeCrabs)

#### Zuma Beach Plastic and Crabs Correlations #####
# Zuma Beach Plastic
TotalZumaPlastic <- ZumaPlastics$Microplastic.percentage
ZumaPlastics <- cbind(ZumaPlastics, TotalZumaPlastic)

plot(ZumaPlastics$TotalZumaPlastic, ZumaCounts$TotalZumaCrabs, xlab = "Microplastic Proportion",
     ylab = "Crab Counts", main = "Microplastics v. Sand Crab Counts - Zuma Beach, CA")

# fit linear regression model
ZumaCrabPlasticFit <- lm(TotalZumaPlastic ~ TotalZumaCrabs)

summary(ZumaCrabPlasticFit)

# probability distribution of the residuals
plot(density(resid(ZumaCrabPlasticFit)), xlab = "Residuals", 
     ylab = "Probability density", main = "Distribution of residuals")

plot(ZumaCrabPlasticFit, which = 2)

plot(ZumaPlastics$TotalZumaPlastic, ZumaCounts$TotalZumaCrabs, xlab = "Microplastics Proportion", 
     ylab = "Sand Crab Count", main = "Microplastics Proportion v. Sand Crab Counts - Zuma Beach, CA")

abline(ZumaCrabPlasticFit)

legend("bottomright", bty = "n", legend = paste("R-squared=", format(summary(ZumaCrabPlasticFit)$r.squared, digits = 4)))

# enhanced plotted figure with 95% confidence regions around regression line
ggplot(data = ZumaPlastics) + 
  geom_point(mapping = aes(x = TotalZumaPlastic, y = ZumaCounts$TotalZumaCrabs)) + 
  geom_smooth(mapping = aes(x = TotalZumaPlastic, y = ZumaCounts$TotalZumaCrabs), method = lm) + 
  labs(x = "Microplastic Proportion", y = "Crab Abundance", title = "Microplastics v. Sand Crabs - Zuma Beach, CA", caption = "Figure 1: Significant positive relationship between adult and recruit sand crab counts at Dockweiler State 
       Beach (p-value: 0.02989, R-squared: 0.07563). Linear regression line includes 95% confidence regions.")

ggsave("ZumaBeach_Plastic-v-Crabs.png")

#### Point Dume State Beach Plastic and Crabs Correlations #####
# Point Dume State Beach Plastic
TotalDumePlastic <- DumePlastics$Microplastics.percentage
DumePlastics <- cbind(DumePlastics, TotalDumePlastic)

plot(DumePlastics$TotalDumePlastic, DumeCounts$TotalDumeCrabs, xlab = "Microplastic Proportion",
     ylab = "Crab Counts", main = "Microplastics v. Sand Crab Counts - Point Dume State Beach, CA")

# fit linear regression model
DumeCrabPlasticFit <- lm(TotalDumePlastic ~ TotalDumeCrabs)

summary(DumeCrabPlasticFit)

# probability distribution of the residuals
plot(density(resid(DumeCrabPlasticFit)), xlab = "Residuals", 
     ylab = "Probability density", main = "Distribution of residuals")

plot(DumeCrabPlasticFit, which = 2)

plot(DumePlastics$TotalDumePlastic, DumeCounts$TotalDumeCrabs, xlab = "Microplastics Proportion", 
     ylab = "Sand Crab Count", main = "Microplastics Proportion v. Sand Crab Counts - Point Dume State Beach, CA")

abline(DumeCrabPlasticFit)

legend("bottomright", bty = "n", legend = paste("R-squared=", format(summary(DumeCrabPlasticFit)$r.squared, digits = 4)))

# enhanced plotted figure with 95% confidence regions around regression line
ggplot(data = DumePlastics) + 
  geom_point(mapping = aes(x = TotalDumePlastic, y = DumeCounts$TotalDumeCrabs)) + 
  geom_smooth(mapping = aes(x = TotalDumePlastic, y = DumeCounts$TotalDumeCrabs), method = lm) + 
  labs(x = "Microplastic Proportion", y = "Crab Abundance", title = "Microplastics v. Sand Crabs - Point Dume State Beach, CA", caption = "Figure 1: Significant positive relationship between adult and recruit sand crab counts at Dockweiler State 
       Beach (p-value: 0.02989, R-squared: 0.07563). Linear regression line includes 95% confidence regions.")

ggsave("PointDumeStateBeach_Plastic-v-Crabs.png")

# create and plot two size frequency histograms
ggplot(data = ZumaPlastics, aes(TotalZumaPlastic, fill = ZumaCounts$TotalZumaCrabs)) + 
  geom_histogram(position = "dodge") + 
  labs(x = "Microplastic proportion", y = "Sand crab count", title = "Sand crabs and microplastics", caption = "Figure 2: Size frequency histograms of sand crabs per microplastic proportion at Zuma Beach, CA.")

ggsave("ZumaBeach_Plastic-Crab-Histogram.png")


ggplot(data = DumePlastics, aes(TotalDumePlastic, fill = DumeCounts$TotalDumeCrabs)) + 
  geom_histogram(position = "dodge") + 
  labs(x = "Microplastic proportion", y = "Sand crab count", title = "Sand crabs and microplastics", caption = "Figure 3: Size frequency histograms of sand crabs per microplastic proportion at Point Dume State Beach, CA.")

ggsave("PointDumeStateBeach_Plastic-Crab-Histogram.png")

# estimate summary stats for crab presence and plastics
zuma.crabs <- filter(ZumaPlastics, Crabs == "Present")
zuma.no.crabs <- filter(ZumaPlastics, Crabs =="Absent")

xbarP.zuma.crabs <- mean(zuma.crabs$Microplastic.percentage)
xbarP.zuma.no.crabs <- mean(zuma.no.crabs$Microplastic.percentage)

sdP.zuma.crabs <- sd(zuma.crabs$Microplastic.percentage)
sdP.zuma.no.crabs <- sd(zuma.no.crabs$Microplastic.percentage)

n.zuma.crabs <- nrow(zuma.crabs)
n.zuma.no.crabs <- nrow(zuma.no.crabs)

seP.zuma.crabs <- sdP.zuma.crabs / sqrt(n.zuma.crabs)
seP.zuma.no.crabs <- sdP.zuma.no.crabs / sqrt(n.zuma.no.crabs)

boundP.zuma.crabs <- 1.96 * seP.zuma.crabs
boundP.zuma.no.crabs <- 1.96 * seP.zuma.no.crabs

lowP.zuma.crabs <- xbarP.zuma.crabs - boundP.zuma.crabs
lowP.zuma.no.crabs <- xbarP.zuma.no.crabs - boundP.zuma.no.crabs

upP.zuma.crabs <- xbarP.zuma.crabs + boundP.zuma.crabs
upP.zuma.no.crabs <- xbarP.zuma.no.crabs + boundP.zuma.no.crabs

zuma.crabs.stats <- c(lowP.zuma.crabs, xbarP.zuma.crabs, upP.zuma.crabs)
zuma.no.crabs.stats <- c(lowP.zuma.no.crabs, xbarP.zuma.no.crabs, upP.zuma.no.crabs)

zumastats <- rbind(zuma.crabs.stats, zuma.no.crabs.stats)

colnames(zumastats) <- c("2.5% plastic", "Mean plastic (%)", "97.5% plastic")

rownames(zumastats) <- c("Crabs", "No crabs")

zumastats