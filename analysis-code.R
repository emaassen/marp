# This is the final analysis code for the Many Analysts Religion Project 
# Team 041: E. Maassen & M.B. Nuijten

# Clean workspace
rm(list=ls()) 


### Load packages
library(faux) # to generate the mock data
library(lme4) # to estimate models
library(sjPlot) # to check assumptions of the model
library(glmmTMB) # we need this package for the sjPlot package
library(lattice) # to check assumptions of the model
library(tidyverse) # for data wrangling
library(lavaan) # for factor analysis 
library(psych) # for factor scores
library(GPArotation) # for polychoric EFA

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
# define model
wb_model <- '# First order factors
             wb_gen =~ wb_gen_1_cent + wb_gen_2_cent
             wb_phys =~ wb_phys_1_cent + wb_phys_2_cent + wb_phys_3_cent + wb_phys_4_cent + wb_phys_5_cent + wb_phys_6_cent + wb_phys_7_cent
             wb_psych =~ wb_psych_1_cent + wb_psych_2_cent + wb_psych_3_cent + wb_psych_4_cent + wb_psych_5_cent + wb_psych_6_cent 
             wb_soc =~ wb_soc_1_cent + wb_soc_2_cent + wb_soc_3_cent
             
             # Second order factor
             wb =~ wb_gen + wb_phys + wb_psych + wb_soc'

# run CFA
wb_res <- cfa(wb_model, data=wb_items_centered)
summary(wb_res)

# check fit
fitmeasures(wb_res)["pvalue"] # we want the p-value to be > .05
# pvalue == 0; not a good fit
fitmeasures(wb_res)["rmsea"] # we want the rmsea to be < 0.08
# rmsea == .0847; not a good fit
fitmeasures(wb_res)["srmr"] # we want the srmr to be < 0.08
# srmr == 0.06; reasonable fit

# because the model does not fit the data, we use the variable wb_overall_mean 
# as the DV. (as preregistered)
wellbeing_cent <- dat$wb_overall_mean-mean(dat$wb_overall_mean)


### Factor analysis - religiosity

# Polychoric factor analysis with one factor
poly_model = fa(rel_items_centered, nfactor=1, cor="poly", fm="mle", rotate = "none")
# Warning message:
#   In cor.smooth(mat) : Matrix was not positive definite, smoothing was done
save(poly_model, file = "poly_model")
poly_model$loadings

# Polychoric factor analysis with two and three factors 
# we check this to verify whether the variables that do not load strongly
# on the only factor in the previous EFA, now load on other factor(s)
poly_model2 = fa(rel_items_centered, nfactor=2, cor="poly", fm="mle", rotate = "promax")
# Warning message:
#   In cor.smooth(mat) : Matrix was not positive definite, smoothing was done
save(poly_model2, file = "poly_model")
poly_model2$loadings

poly_model3 = fa(rel_items_centered, nfactor=3, cor="poly", fm="mle", rotate = "promax")
# Warning message:
#   In cor.smooth(mat) : Matrix was not positive definite, smoothing was done
save(poly_model3, file = "poly_model")
poly_model3$loadings

# Conclusion poly_models: the 2-factor model seems to have the best fit. As an
# extra check, also look at eigenvalues (SS loadings in the output). The second
# factor has an eigenvalue > 1 (this is not the case for the third factor in 
# model 3), so the second model with 2 factors is the best choice.

# In the 2-factor model, 2 items clearly loaded on factor 2 (rel_1 and rel_2; 
# both have to do with frequency of religious actions). As we decided in our
# preregistration: we will not include these two items in our final factor score
# for religion.

# keep items with loadings larger than 0.3 from the one factor model
loadings <- poly_model2$loadings[, "ML1"] > .3
rel_items_cent_subset <- rel_items_centered[,loadings]

# run EFA with one factor with remaining loadings
# we need to do this again because we want to calculate factor scores
# on only the remaining variables
poly_model = fa(rel_items_cent_subset, nfactor=1, cor="poly", fm="mle", rotate = "none")
# In smc, smcs < 0 were set to .0
# In smc, smcs < 0 were set to .0
# Warning message:
#   In cor.smooth(mat) : Matrix was not positive definite, smoothing was done
rel_scores <- factor.scores(rel_items_cent_subset, poly_model, method="Bartlett")

relig <- rel_scores$scores

### Construct cultural norms variable

# calculate average
cnorm_mean <- rowMeans(cnorm_items_centered, na.rm = TRUE)

### Combine all relevant variables into one dataframe
dat_final <- data.frame(wellbeing_cent, relig, cnorm_mean, ses_cent, 
                                 edu_cent, country = dat$country)
colnames(dat_final) <- c("wellbeing", "relig", "cnorm_mean", "ses_cent", 
                         "edu_cent", "country")

### Specify and estimate models

# Null model

mod0 <- lmer(wellbeing ~ 1 + (1 | country), REML=TRUE, data=dat_final) 

### 1st random intercept model (RQ1, without interaction)
mod1 <- lmer(wellbeing ~ relig + ses_cent + edu_cent + (1 | country), REML=FALSE, data=dat_final)

### 2nd random intercept model (RQ2, with interaction)
mod2 <- lmer(wellbeing ~ relig + cnorm_mean + relig*cnorm_mean + ses_cent + edu_cent + (1 | country), REML=FALSE, data=dat_final)


### Assumptions check

# Normality of residuals 
# Dots should be plotted along the line

# model 0
qqmath(mod0)
plot_model(mod0, type = 'diag')[[1]]

# model 1
qqmath(mod1)
plot_model(mod1, type = 'diag')[[1]]

# model 2
qqmath(mod2)
plot_model(mod1, type = 'diag')[[1]]


# Normality of random effects 
# Dots should be plotted along the line

# model 0
plot_model(mod0, type = 'diag')[[2]]

# model 1
plot_model(mod1, type = 'diag')[[2]]

# model 2
plot_model(mod2, type = 'diag')[[2]]


# Homoscedasticity 
# Note that this plot is only based on the fixed effects in the model
# Amount and distance of points scattered above/below line should be equal or randomly spread

# model 0
plot_model(mod0, type = 'diag')[[4]]

# model 1
plot_model(mod1, type = 'diag')[[4]]

# model 2
plot_model(mod2, type = 'diag')[[4]]


# Linearity
# We should not see a systematic pattern

#model 0
plot(resid(mod0),dat_final$relig)
plot(resid(mod0),dat_final$cnorm_mean)
plot(resid(mod0),dat_final$ses_cent)
plot(resid(mod0),dat_final$edu_cent)

# model 1

# create a temporary data set in which the participants who have missing
# values on ses are removed entirely
dat_temp <- dat_final[!is.na(dat_final$ses_cent), ]

plot(resid(mod1),dat_temp$relig)
plot(resid(mod1),dat_temp$cnorm_mean)
plot(resid(mod1),dat_temp$ses)
plot(resid(mod1),dat_temp$edu)

# model 2
plot(resid(mod2),dat_temp$relig)
plot(resid(mod2),dat_temp$cnorm_mean)
plot(resid(mod2),dat_temp$ses_cent)
plot(resid(mod2),dat_temp$edu_cent)


# Random effects independent of covariates
# We should not see a systematic pattern

# model 0
plot(ranef(mod0),dat_final$cnorms_mean)
plot(ranef(mod0),dat_final$ses_cent)
plot(ranef(mod0),dat_final$edu_cent)

# model 1
plot(ranef(mod1),dat_final$cnorms_mean)
plot(ranef(mod1),dat_final$ses_cent)
plot(ranef(mod1),dat_final$edu_cent)

# model 2
plot(ranef(mod2),dat_final$cnorms_mean)
plot(ranef(mod2),dat_final$ses_cent)
plot(ranef(mod2),dat_final$edu_cent)

### Extract results


