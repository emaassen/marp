rm(list = ls())

# SETTINGS ------------------------
n <- 10000 # sample size

# GENERATE DATA ------------------------

## wellbeing ----

# generate variable names
wb_varnames <- c(paste0("wb_gen_", 1:2),
                 paste0("wb_phys_", 1:7),
                 paste0("wb_psych_", 1:6),
                 paste0("wb_soc_", 1:3))

wb_scores <- matrix(NA, nrow = n, ncol = length(wb_varnames))

#for(i in seq_along(wb_varnames)){
#  wb_scores[ , i] <- sample(1:5, size = n, replace = TRUE)
#  
#}

set.seed(017889)
wb_scores <- round(rnorm_multi(n = n, mu = 2.5, sd = 1, r = .25, varnames = wb_varnames),0)

colnames(wb_scores) <- wb_varnames
wb_scores <- data.frame(wb_scores)

## religiosity ----

#rel_1 <- sample(seq(0, 1, len = 7), size = n, replace = TRUE)
#rel_5 <- sample(seq(0, 1, len = 7), size = n, replace = TRUE)
#rel_6 <- sample(seq(0, 1, len = 7), size = n, replace = TRUE)
#rel_7 <- sample(seq(0, 1, len = 7), size = n, replace = TRUE)
#
#rel_2 <- sample(seq(0, 1, len = 8), size = n, replace = TRUE)
#
#rel_8 <- sample(seq(0, 1, len = 5), size = n, replace = TRUE)
#rel_9 <- sample(seq(0, 1, len = 5), size = n, replace = TRUE)
#
#rel_3 <- sample(seq(0, 1, len = 3), size = n, replace = TRUE)
#
#rel_4 <- sample(0:1, size = n, replace = TRUE)
#
#rel_scores <- data.frame(rel_1, rel_2, rel_3, rel_4, rel_5,
#                         rel_6, rel_7, rel_8, rel_9)


rel_varnames <- c(paste0("rel_", 1:9))

rel_scores <- abs(round(rnorm_multi(n = n,
                          mu = c(3.5, 4, 0.5, 0.5, 3.5, 3.5, 3.5, 0.5, 0.5),
                          sd = 1,
                          r = 0.25,
                          varnames = rel_varnames,
                          empirical = FALSE),0))

# change range of rel_scores to max of 7, so I can estimate polychoric FA
rel_scores[rel_scores == 8] <- 7

## cultural norms ----

cnorm_1 <- sample(seq(0, 1, len = 5), size = n, replace = TRUE)
cnorm_2 <- sample(seq(0, 1, len = 5), size = n, replace = TRUE)

## control variables ----

ses <- sample(1:10, size = n, replace = TRUE)
edu <- sample(1:7, size = n, replace = TRUE)

## country ----
country <- factor(sample(1:50, size = n, replace = TRUE))

## attention check ----
attention_check <- sample(0:1, size = n, replace = TRUE, prob = c(.1, .9))

# COMBINE DATA ------------------------

dat <- cbind(wb_scores, rel_scores, cnorm_1, cnorm_2, 
             ses, edu, country, attention_check)
