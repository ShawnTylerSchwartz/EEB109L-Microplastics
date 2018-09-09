#
# Shawn Schwartz
# EEB 109L - Summer 2018
#
# Email: shawnschwartz@ucla.edu
#

# Microplastics vs. Sand Crabs - Final Project Paper Analyses of Data

#### set working directory ####
setwd("~/OneDrive/UCLA Undergraduate Degree/Summer 2018/Summer Session C - 2018/EE BIOL 109L/R-Projects/EEB109L-Microplastics")

#### load tidyverse library ####
library(tidyverse)

#### read-in csv file data ####
## Zuma Beach
ZumaCounts <- read.csv(file = "ZumaCounts.csv", header = TRUE)
ZumaPlastics <- read.csv(file = "ZumaPlastics.csv", header = TRUE)

## Point Dume State Beach
DumeCounts <- read.csv(file = "DumeCounts.csv", header = TRUE)
DumePlastics <- read.csv(file = "DumePlastics.csv", header = TRUE)
  
#### make scatterplot of zuma beach and dume sand crab counts ####
## Zuma Beach Crab Counts
TotalZumaCrabs <- ZumaCounts$Small + ZumaCounts$Medium + ZumaCounts$Large
ZumaCounts <- cbind(ZumaCounts, TotalZumaCrabs)

## Point Dume State Beach Crab Counts
TotalDumeCrabs <- DumeCounts$Small + DumeCounts$Medium + DumeCounts$Large
DumeCounts <- cbind(DumeCounts, TotalDumeCrabs)

TotalCounts <- numeric(20)
TotalCounts <- cbind(TotalCounts, TotalZumaCrabs, TotalDumeCrabs)

#### Zuma Beach Plastic and Crabs Correlations #####
## Zuma Beach Plastic
TotalZumaPlastic <- ZumaPlastics$Microplastic.percentage
ZumaPlastics <- cbind(ZumaPlastics, TotalZumaPlastic)

plot(ZumaPlastics$TotalZumaPlastic, ZumaCounts$TotalZumaCrabs, xlab = "Microplastic Proportion",
     ylab = "Crab Counts", main = "Microplastics v. Sand Crab Counts - Zuma Beach, CA")

### fit linear regression model
ZumaCrabPlasticFit <- lm(TotalZumaPlastic ~ TotalZumaCrabs)

summary(ZumaCrabPlasticFit)

### probability distribution of the residuals
plot(density(resid(ZumaCrabPlasticFit)), xlab = "Residuals", 
     ylab = "Probability density", main = "Distribution of residuals")

plot(ZumaCrabPlasticFit, which = 2)

plot(ZumaPlastics$TotalZumaPlastic, ZumaCounts$TotalZumaCrabs, xlab = "Microplastics Proportion", 
     ylab = "Sand Crab Count", main = "Microplastics Proportion v. Sand Crab Counts - Zuma Beach, CA")

abline(ZumaCrabPlasticFit)

legend("bottomright", bty = "n", legend = paste("R-squared=", format(summary(ZumaCrabPlasticFit)$r.squared, digits = 4)))

### enhanced plotted figure with 95% confidence regions around regression line
ggplot(data = ZumaPlastics) + 
  geom_point(mapping = aes(x = TotalZumaPlastic, y = ZumaCounts$TotalZumaCrabs)) + 
  geom_smooth(mapping = aes(x = TotalZumaPlastic, y = ZumaCounts$TotalZumaCrabs), method = lm) + 
  labs(x = "Proportion of Microplastics", y = "Sand Crab Counts", title = "Microplastics v. Sand Crabs - Zuma Beach, CA", caption = "Figure 1: Non-significant negative relationship between proportion of microplastics and sand crab counts at Zuma Beach, CA 
      (p-value: 0.4354, R-squared: -0.01951). Linear regression line includes 95% confidence regions.")

ggsave("figures_output/Fig1-ZumaBeach_Plastic-v-Crabs.jpg")

#### Point Dume State Beach Plastic and Crabs Correlations #####
## Point Dume State Beach Plastic
TotalDumePlastic <- DumePlastics$Microplastics.percentage
DumePlastics <- cbind(DumePlastics, TotalDumePlastic)

plot(DumePlastics$TotalDumePlastic, DumeCounts$TotalDumeCrabs, xlab = "Microplastic Proportion",
     ylab = "Crab Counts", main = "Microplastics v. Sand Crab Counts - Point Dume State Beach, CA")

### fit linear regression model
DumeCrabPlasticFit <- lm(TotalDumePlastic ~ TotalDumeCrabs)

summary(DumeCrabPlasticFit)

### probability distribution of the residuals
plot(density(resid(DumeCrabPlasticFit)), xlab = "Residuals", 
     ylab = "Probability density", main = "Distribution of residuals")

plot(DumeCrabPlasticFit, which = 2)

plot(DumePlastics$TotalDumePlastic, DumeCounts$TotalDumeCrabs, xlab = "Microplastics Proportion", 
     ylab = "Sand Crab Count", main = "Microplastics Proportion v. Sand Crab Counts - Point Dume State Beach, CA")

abline(DumeCrabPlasticFit)

legend("bottomright", bty = "n", legend = paste("R-squared=", format(summary(DumeCrabPlasticFit)$r.squared, digits = 4)))

### enhanced plotted figure with 95% confidence regions around regression line
ggplot(data = DumePlastics) + 
  geom_point(mapping = aes(x = TotalDumePlastic, y = DumeCounts$TotalDumeCrabs)) + 
  geom_smooth(mapping = aes(x = TotalDumePlastic, y = DumeCounts$TotalDumeCrabs), method = lm) + 
  labs(x = "Proportion of Microplastics", y = "Sand Crab Counts", title = "Microplastics v. Sand Crabs - Point Dume State Beach, CA", caption = "Figure 2: Marginally significant negative relationship between proportion of microplastics and sand crab counts at Point Dume State 
       Beach, CA (p-value: 0.06209, R-squared: 0.1347). Linear regression line includes 95% confidence regions.")

ggsave("figures_output/Fig2-PointDumeStateBeach_Plastic-v-Crabs.jpg")

#### create and plot two size frequency histograms ####
ggplot(data = ZumaPlastics, aes(TotalZumaPlastic, fill = ZumaCounts$TotalZumaCrabs)) + 
  geom_histogram(position = "dodge") + 
  labs(x = "Proportion of Microplastics", y = "Sand Crab Counts", title = "Sand Crabs and Microplastics - Zuma Beach, CA", caption = "Figure 3: Size frequency histograms of sand crabs per microplastic proportion at Zuma Beach, CA.")

ggsave("figures_output/Fig3-ZumaBeach_Plastic-Crab-Histogram.jpg")

ggplot(data = DumePlastics, aes(TotalDumePlastic, fill = DumeCounts$TotalDumeCrabs)) + 
  geom_histogram(position = "dodge") + 
  labs(x = "Proportion of Microplastics", y = "Sand Crab Counts", title = "Sand Crabs and Microplastics - Point Dume State Beach, CA", caption = "Figure 4: Size frequency histograms of sand crabs per microplastic proportion at Point Dume State Beach, CA.")

ggsave("figures_output/Fig4-PointDumeStateBeach_Plastic-Crab-Histogram.jpg")

#### estimate summary stats for crab presence and plastics ####
## Zuma Beach
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
write.csv(zumastats, "stats_output/zumabeach_stats.csv")

## Point Dume State Beach
dume.crabs <- filter(DumePlastics, Crabs == "Present")
dume.no.crabs <- filter(DumePlastics, Crabs =="Absent")

xbarP.dume.crabs <- mean(dume.crabs$Microplastics.percentage)
xbarP.dume.no.crabs <- mean(dume.no.crabs$Microplastics.percentage)

sdP.dume.crabs <- sd(dume.crabs$Microplastics.percentage)
sdP.dume.no.crabs <- sd(dume.no.crabs$Microplastics.percentage)

n.dume.crabs <- nrow(dume.crabs)
n.dume.no.crabs <- nrow(dume.no.crabs)

seP.dume.crabs <- sdP.dume.crabs / sqrt(n.dume.crabs)
seP.dume.no.crabs <- sdP.dume.no.crabs / sqrt(n.dume.no.crabs)

boundP.dume.crabs <- 1.96 * seP.dume.crabs
boundP.dume.no.crabs <- 1.96 * seP.dume.no.crabs

lowP.dume.crabs <- xbarP.dume.crabs - boundP.dume.crabs
lowP.dume.no.crabs <- xbarP.dume.no.crabs - boundP.dume.no.crabs

upP.dume.crabs <- xbarP.dume.crabs + boundP.dume.crabs
upP.dume.no.crabs <- xbarP.dume.no.crabs + boundP.dume.no.crabs

dume.crabs.stats <- c(lowP.dume.crabs, xbarP.dume.crabs, upP.dume.crabs)
dume.no.crabs.stats <- c(lowP.dume.no.crabs, xbarP.dume.no.crabs, upP.dume.no.crabs)

dumestats <- rbind(dume.crabs.stats, dume.no.crabs.stats)

colnames(dumestats) <- c("2.5% plastic", "Mean plastic (%)", "97.5% plastic")

rownames(dumestats) <- c("Crabs", "No crabs")

dumestats
write.csv(dumestats, "stats_output/pointdume_stats.csv")

#### t-tests #### 
## paired samples (plastics (Zuma v. Dume))
zuma.dume.plastics.t.test <- t.test(ZumaPlastics$TotalZumaPlastic, DumePlastics$TotalDumePlastic, paired = TRUE)
zuma.dume.plastics.t.test
zumadumeplasticschars <- capture.output(print(zuma.dume.plastics.t.test))
writeLines(zumadumeplasticschars, con = file("stats_output/zuma_dume_plastics_t-test.txt"))

## paired samples (crabs (Zuma v. Dume))
zuma.dume.crabs.t.test <- t.test(ZumaCounts$TotalZumaCrabs, DumeCounts$TotalDumeCrabs, paired = TRUE)
zuma.dume.crabs.t.test
zumadumecrabschars <- capture.output(print(zuma.dume.crabs.t.test))
writeLines(zumadumecrabschars, con = file("stats_output/zuma_dume_crabs_t-test.txt"))

#### Combined Beaches Plastic and Crabs Correlations #####

### enhanced plotted figure with 95% confidence regions around regression line
CombinedCounts <- read.csv(file = "CombinedCounts.csv", header = TRUE)
CombinedPlastics <- read.csv(file = "CombinedPlastics.csv", header = TRUE)

TotalCombinedCrabs <- CombinedCounts$Small + CombinedCounts$Medium + CombinedCounts$Large
CombinedCounts <- cbind(CombinedCounts, TotalCombinedCrabs)

TotalCombinedPlastic <- CombinedPlastics$Microplastic.percentage
CombinedPlastics <- cbind(CombinedPlastics, TotalCombinedPlastic)

plot(CombinedPlastics$TotalCombinedPlastic, CombinedCounts$TotalCombinedCrabs, xlab = "Microplastic Proportion",
     ylab = "Crab Counts", main = "Microplastics v. Sand Crab Counts - COMBINED")

### fit linear regression model
CombinedCrabPlasticFit <- lm(TotalCombinedPlastic ~ TotalCombinedCrabs)

summary(CombinedCrabPlasticFit)

### probability distribution of the residuals
plot(density(resid(CombinedCrabPlasticFit)), xlab = "Residuals", 
     ylab = "Probability density", main = "Distribution of residuals")

plot(CombinedCrabPlasticFit, which = 2)

plot(CombinedPlastics$TotalCombinedPlastic, CombinedCounts$TotalCombinedCrabs, xlab = "Microplastics Proportion", 
     ylab = "Sand Crab Count", main = "Microplastics Proportion v. Sand Crab Counts - COMBINED")

abline(CombinedCrabPlasticFit)

legend("bottomright", bty = "n", legend = paste("R-squared=", format(summary(CombinedCrabPlasticFit)$r.squared, digits = 4)))

### enhanced plotted figure with 95% confidence regions around regression line
ggplot(data = CombinedPlastics) + 
  geom_point(mapping = aes(x = TotalCombinedPlastic, y = CombinedCounts$TotalCombinedCrabs, colour = factor(Beach))) + 
  geom_smooth(mapping = aes(x = TotalCombinedPlastic, y = CombinedCounts$TotalCombinedCrabs), method = lm) + 
  labs(x = "Proportion of Microplastics", y = "Sand Crab Counts", title = "Microplastics v. Sand Crabs", caption = "Figure 5: Combined significantly negative relationship between proportion of microplastics and sand crab counts at Zuma 
       Beach, CA and Point Dume State Beach, CA (p-value: 0.05541, R-squared: 0.06935). Linear regression line includes 95% confidence regions.")

ggsave("figures_output/Fig5-CombinedBeaches-Correlation_Plastic-v-Crabs.jpg")
