library(tidyverse)
library(readxl)

# Repeats
dd <- read_excel("../data/ICM_C9_expanded_rawRepeats_withClinical_2025-08-29.xlsx")
# MetaData
dc <- read_excel("../data/ICM_C9_expanded_summary_stats_withClinical_2025-08-29.xlsx")

dc <- dc %>% filter(Condition %in% c("ALS","FTD"))
dd <- dd %>% filter(Sample %in% dc$Sample)

dc$AO <- as.numeric(dc$AO)
dd$repeats <- as.numeric(dd$repeats)

# Legacy
tmp <- dd
dim(tmp)
tmp$Age10 <- tmp$AP/10
reso <- glm(formula = repeats ~  Age10 + Gender + Condition, family="gaussian", data=tmp)
reso


library(lme4)
library(sjPlot)

tmp$Age10 <- tmp$AP/10
tmp$sample <- paste0("p",tmp$Sample)

model <- lmer(repeats ~ Age10 +  Gender + Condition + (1|sample), data = tmp)


sjPlot::plot_model(model, show.values=TRUE, show.p=TRUE)





model <- lmer(repeats ~ Age10 +  Gender + Condition + (1|sample), data = mydata)

