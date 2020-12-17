# This is code accompanying the Many Analysts Religion Project preregistration 
# Team 041: E. Maassen & M.B. Nuijten

# Clean workspace
#rm(list=ls()) 


### Load packages
library(lme4) # to estimate models
library(sjPlot) # to check assumptions of the model
library(glmmTMB) # we need this package for the sjPlot package
library(lattice) # check if still needed
library(mlmRev) # check if still needed

### Load data
dat <- read.csv("marpdata.csv")
view(dat)



### Factor analysis - wellbeing



### Factor analysis - religiosity



### Construct cultural norms variable



### Specify and estimate models

# Null model
mod0 <- lmer(normexam ~ 1 + (1 | school), data=Exam)
mod0 <- lmer(wellbeing ~ 1 + (1 | country), REML=TRUE, data=dat) 


### 1st random intercept model (RQ1, without interaction)
mod1 <- lmer(normexam ~ standLRT + (1 | school), data=Exam)
mod1 <- lmer(wellbeing ~ religosity + ses + edu + (1 | country), REML=TRUE, data=dat)


### 2nd random intercept model (RQ2, with interaction)
mod2 <- lmer(wellbeing ~ religosity + cultural + religiosity*cultural + ses + edu + (1 | country), REML=TRUE, data=dat)


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

VarCorr(mod1)


### Extract results




### Plots?
