library(tidyverse)
library(car)

options(scipen=999)

# Get openpowerlifting.csv from Kaggle
# https://www.kaggle.com/datasets/open-powerlifting/powerlifting-database

data <- read.csv("openpowerlifting.csv")
sg <- read.csv("sg_lift_percentages.csv")

tested <- subset(data, Tested == "Yes")
tested <- subset(tested, Equipment == "Raw")
tested <- subset(tested, Event == "SBD")
tested <- subset(tested, Division == "Open")

# Get percentage of total
tested$squatpct <- (tested$Best3SquatKg / tested$TotalKg)*100
tested$benchpct <- (tested$Best3BenchKg / tested$TotalKg)*100
tested$dlpct <- (tested$Best3DeadliftKg / tested$TotalKg)*100

tested <- subset(tested, benchpct > 0)

opl_squat <- tested$squatpct
opl_bench <- tested$benchpct
opl_dead <- tested$dlpct
sg_squat <- sg$squat
sg_bench <- sg$bench
sg_dl <- sg$deadlift

# Check normality
qqPlot(opl_bench)
shapiro.test(sample(opl_bench, size=5000))  # shapiro.test won't allow
                                            # samples with n > 5000.
qqPlot(sg_bench, line = "quartile")
shapiro.test(sg_bench)

# SG data is normal, OPL data is not. Must use nonparametric test (MWU)

#Perform the Mann-Whitney U test
wc_test_result <- wilcox.test(sg_bench, opl_bench, alternative = "greater",
                              paired = F, conf.level = 0.95, conf.int = T)
print(wc_test_result)

# OPL Bench Median: 22.6%
# SG Bench Median: 24.00%
