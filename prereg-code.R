# This is code accompanying the Many Analysts Religion Project preregistration 
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

### Load data
dat_raw <- source("generate_mock_data.R")[[1]]
View(dat)

### Clean data 

# only include data when participants passed the attention check
dat <- dat_raw[dat_raw$attention_check == 1, ]

# center wellbeing items
wb_items <- dat %>%
  select(starts_with("wb"))

wb_items_centered <- apply(wb_items, 2, function(x) x - mean(x, na.rm = TRUE))
colnames(wb_items_centered) <- paste0(colnames(wb_items), "_cent")

# dichotomize rel_3 item
dat$rel_3[dat$rel_3 != 1] <- 0

# center religiosity items
rel_items <- dat %>%
  select(starts_with("rel"))

rel_items_centered <- apply(rel_items, 2, function(x) x - mean(x, na.rm = TRUE))
colnames(rel_items_centered) <- paste0(colnames(rel_items), "_cent")

# center cultural norms items
cnorm_items <- dat %>%
  select(starts_with("cnorm"))

cnorm_items_centered <- apply(cnorm_items, 2, function(x) x - mean(x, na.rm = TRUE))
colnames(cnorm_items_centered) <- paste0(colnames(cnorm_items), "_cent")

# center control variables ses and education
ses_cent <- dat$ses - mean(dat$ses, na.rm = TRUE)
edu_cent <- dat$edu - mean(dat$edu, na.rm = TRUE)

### Factor analysis - wellbeing
# define model
wb_model <- '# First order factors
             wb_gen =~ wb_gen_1 + wb_gen_2
             wb_phys =~ wb_phys_1 + wb_phys_2 + wb_phys_3 + wb_phys_4 + wb_phys_5 + wb_phys_6 + wb_phys_7
             wb_psych =~ wb_psych_1 + wb_psych_2 + wb_psych_3 + wb_psych_4 + wb_psych_5 + wb_psych_6 
             wb_soc =~ wb_soc_1 + wb_soc_2 + wb_soc_3
             
             # Second order factor
             wb =~ wb_gen + wb_phys + wb_psych + wb_soc'

# run CFA
wb_res <- cfa(wb_model, data=dat)

# check fit
fitmeasures(wb_res)["pvalue"] # we want the p-value to be > .05
fitmeasures(wb_res)["rmsea"] # we want the rmsea to be < 0.08
fitmeasures(wb_res)["srmr"] # we want the srmr to be < 0.08

# if model fits: compute factor score for wb (through lavpredict)
wellbeing <- lavPredict(wb_res, method="bartlett")[,5]

# if bartlett method for factor score doesn't work, use regression method:
wellbeing <- lavPredict(wb_res, method="regression")[,5]

# if model does not fit the data, we use the variable wb_overall_mean as the DV.
wellbeing <- wb_overall_mean


### Factor analysis - religiosity
# extract relevant data
rel_varnames <- c(paste0("rel_", 1:9))
rel_dat <- dat[,rel_varnames]

# Polychoric factor analysis with one factor
poly_model = fa(rel_dat, nfactor=1, cor="poly", fm="mle", rotate = "none")
save(poly_model, file = "poly_model")
poly_model$loadings

# Polychoric factor analysis with two and three factors 
# we check this to verify whether the variables that do not load strongly
# on the only factor in the previous EFA, now load on other factor(s)
poly_model2 = fa(rel_dat, nfactor=2, cor="poly", fm="mle", rotate = "promax")
save(poly_model2, file = "poly_model")
poly_model2$loadings

poly_model3 = fa(rel_dat, nfactor=3, cor="poly", fm="mle", rotate = "promax")
save(poly_model3, file = "poly_model")
poly_model3$loadings

# keep items with loadings larger than 0.3 from the one factor model
loadings <- poly_model$loadings > .3
rel_dat <- rel_dat[,loadings]

# run EFA with one factor with remaining loadings
# we need to do this again because we want to calculate factor scores
# on only the remaining variables
poly_model = fa(rel_dat, nfactor=1, cor="poly", fm="mle", rotate = "none")
rel_scores <- factor.scores(rel_dat,poly_model, method="Bartlett")

# if bartlett method for factor score doesn't work, use regression method:
rel_scores <- factor.scores(rel_dat,poly_model)

relig <- rel_scores$scores

### Construct cultural norms variable

# calculate average
cnorm_mean <- rowMeans(cnorm_items_centered, na.rm = TRUE)

### Combine all relevant variables into one dataframe
dat_final <- as.data.frame(cbind(wellbeing,relig,cnorm_mean,ses_cent,edu_cent,dat$country))
colnames(dat_final) <- c("wellbeing","relig","cnorm_mean","ses_cent","edu_cent","country")

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
plot(resid(mod1),dat_final$relig)
plot(resid(mod1),dat_final$cnorm_mean)
plot(resid(mod1),dat_final$ses)
plot(resid(mod1),dat_final$edu)

# model 2
plot(resid(mod2),dat_final$relig)
plot(resid(mod2),dat_final$cnorm_mean)
plot(resid(mod2),dat_final$ses_cent)
plot(resid(mod2),dat_final$edu_cent)


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


