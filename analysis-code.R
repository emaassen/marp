# This is the final analysis code for the Many Analysts Religion Project 
# Team 041: E. Maassen & M.B. Nuijten

# Clean workspace
rm(list=ls()) 


### Load packages
# library(lme4) # to estimate models
# library(sjPlot) # to check assumptions of the model
# library(glmmTMB) # we need this package for the sjPlot package
# library(lattice) # check if still needed
# library(mlmRev) # check if still needed
# library(tidyverse) # for data wrangling

### Load data
dat_raw <- read.csv("../MARP_data.csv", sep = ",", header = TRUE)

# inspect data
head(dat_raw)
nrow(dat_raw)

### Clean data 

# only include data when participants passed the attention check
dat <- dat_raw[dat_raw$attention_check == 1, ]

# inspect
nrow(dat)

# center wellbeing items
wb_items <- dat %>%
  dplyr::select(starts_with("wb") & !ends_with("mean"))

wb_items_centered <- apply(wb_items, 2, function(x) x - mean(x, na.rm = TRUE))
colnames(wb_items_centered) <- paste0(colnames(wb_items), "_cent")

# check; means should be 0
summary(wb_items_centered)

# dichotomize rel_3 item
dat$rel_3[dat$rel_3 != 1] <- 0

# center religiosity items
rel_items <- dat %>%
  dplyr::select(starts_with("rel"))

rel_items_centered <- apply(rel_items, 2, function(x) x - mean(x, na.rm = TRUE))
colnames(rel_items_centered) <- paste0(colnames(rel_items), "_cent")

# check; means should be 0
summary(rel_items_centered)

# center cultural norms items
cnorm_items <- dat %>%
  dplyr::select(starts_with("cnorm"))

cnorm_items_centered <- apply(cnorm_items, 2, function(x) x - mean(x, na.rm = TRUE))
colnames(cnorm_items_centered) <- paste0(colnames(cnorm_items), "_cent")

# check; means should be 0
summary(cnorm_items_centered)

# center control variables ses and education
ses_cent <- dat$ses - mean(dat$ses, na.rm = TRUE)
edu_cent <- dat$edu - mean(dat$edu, na.rm = TRUE)

# check; means should be 0
summary(ses_cent)
summary(edu_cent)

### Factor analysis - wellbeing



### Factor analysis - religiosity


### Construct cultural norms variable

# calculate average
cnorm_mean <- rowMeans(cnorm_items_centered, na.rm = TRUE)


### Specify and estimate models

# Null model
mod0 <- lme4::lmer(normexam ~ 1 + (1 | school), data=Exam)
mod0 <- lme4::lmer(wellbeing ~ 1 + (1 | country), REML=TRUE, data=dat) 


### 1st random intercept model (RQ1, without interaction)
mod1 <- lme4::lmer(normexam ~ standLRT + (1 | school), data=Exam)
mod1 <- lme4::lmer(wellbeing ~ religosity + ses + edu + (1 | country), REML=TRUE, data=dat)


### 2nd random intercept model (RQ2, with interaction)
mod2 <- lme4::lmer(wellbeing ~ religosity + cultural + religiosity*cultural + ses + edu + (1 | country), REML=TRUE, data=dat)


### Assumptions check

# Normality of residuals 
# Dots should be plotted along the line

# model 0
lattice::qqmath(mod0)
sjPlot::plot_model(mod0, type = 'diag')[[1]]

# model 1
qqmath(mod1)
sjPlot::plot_model(mod1, type = 'diag')[[1]]

# model 2
qqmath(mod2)
sjPlot::plot_model(mod1, type = 'diag')[[1]]


# Normality of random effects 
# Dots should be plotted along the line

# model 0
sjPlot::plot_model(mod0, type = 'diag')[[2]]

# model 1
sjPlot::plot_model(mod1, type = 'diag')[[2]]

# model 2
sjPlot::plot_model(mod2, type = 'diag')[[2]]


# Homoscedasticity 
# Note that this plot is only based on the fixed effects in the model
# Amount and distance of points scattered above/below line should be equal or randomly spread

# model 0
sjPlot::plot_model(mod0, type = 'diag')[[4]]

# model 1
sjPlot::plot_model(mod1, type = 'diag')[[4]]

# model 2
sjPlot::plot_model(mod2, type = 'diag')[[4]]


# Linearity
# We should not see a systematic pattern

#model 0
plot(resid(mod0),dat$religiosity)
plot(resid(mod0),dat$cultural)
plot(resid(mod0),dat$ses)
plot(resid(mod0),dat$edu)

# model 1
plot(resid(mod1),dat$religiosity)
plot(resid(mod1),dat$cultural)
plot(resid(mod1),dat$ses)
plot(resid(mod1),dat$edu)

# model 2
plot(resid(mod2),dat$religiosity)
plot(resid(mod2),dat$cultural)
plot(resid(mod2),dat$ses)
plot(resid(mod2),dat$edu)

# Eventuele code voor linearity met confidence interval
ggplot(Exam, aes(normexam, standLRT)) + geom_point() + geom_smooth(method=lm)


# Random effects independent of covariates
# We should not see a systematic pattern
plot(resid(mod1),dat$cultural)
plot(resid(mod1),Exam$standLRT)

plot(ranef(mod1),Exam$standLRT)
cor(unlist(ranef(mod1)$school),as.numeric(Exam$standLRT))

lme4::VarCorr(mod1)


### Extract results




### Plots?
