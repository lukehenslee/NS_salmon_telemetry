#===============================================================================
# Norton Sound salmon telemetry 
#
# Date: November, 2023
# 
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This code uses telemetry data to determine important variables influ-
# encing terminal stock membership of coho salmon in Norton Sound, AK
#===============================================================================
# NOTES: This script uses 'tags_manipulated.csv' as a data source
#===============================================================================

# Load packages ################################################################
library(tidyverse)
library(VGAM)
library(boot)


# Set working directory ########################################################
setwd("C:/Users/lhhenslee/Desktop/git_repo/salmon_telemetry")

# Import data ##################################################################
  # 'tags_manipulated_colClasses.csv' is just a list for 'colClasses' argument  
  # of read.csv() 
col <- read.csv("data/tags_manipulated_colClasses.csv", header = T)

# Import 'master_mcode.csv'
tags <- read.csv("data/tags_manipulated.csv", colClasses = paste(col[1,]))
  
# Summary statistics ####
  coho <- tags %>% 
    filter(species == 'coho')
  
  ## Sample size by year, sd
  table(coho$year, coho$capture.loc)
  table(coho$year)
  table(coho$capture.loc)
  
# Logistic modeling ####
  
  # Just keep coho that we tracked to spawning region
  coho <- tags %>% 
    filter(species == 'coho', !is.na(spawn.group))
  
  ## Full dataset ####
  
  # Just keep columns we want to analyze 
  coho.compare <- coho[,c(9:10, 14:15, 25, 37:40, 42)]
  coho.compare %>% summarise_all(~ sum(is.na(.))) # There's a lot of NA for age 
    # and wind, so if we find those aren't significant, we'll drop them
  
  coho.compare.complete <- coho.compare[complete.cases(coho.compare),]
  
  ### Step AIC ####
  summary(full <- glm(terminal.stock ~ . + yday:year, 
                      data = coho.compare.complete, 
                      family = binomial))
  
    step(full, 
       direction = 'both', trace = 0)
    
    # Model with sex, length, lat, capture.loc and year scores best
  summary(best <- glm(formula = terminal.stock ~ sex + length + lat + 
                      capture.loc + year * yday, 
                      family = binomial, 
                      data = coho.compare.complete))
  ### Build nested models ####
  
  ## Spatiotemporal and demographic
  summary(spatemp.dem <- glm(terminal.stock ~  year * yday + capture.loc + 
                               lat +
                                length + sex,
                             family = binomial,
                             data = coho.compare.complete))
  
  ## Spatiotemporal and abiotic 
  summary(spatemp.ab <- glm(terminal.stock ~  year * yday + capture.loc + 
                              lat +
                              u.wind + v.wind ,
                             family = binomial,
                             data = coho.compare.complete))
  
  ## Demographic and abiotic 
  summary(dem.ab <- glm(terminal.stock ~  sex + length +
                              u.wind + v.wind ,
                            family = binomial,
                            data = coho.compare.complete))
  
  ## Spatiotemporal
  summary(spatemp <- glm(terminal.stock ~  year * yday + capture.loc + lat,
                 family = binomial,
                 data = coho.compare.complete))
  
  ## Demographic
  summary(dem <- glm(terminal.stock ~  sex + length,
                         family = binomial,
                         data = coho.compare.complete))
  
  ## Abiotic
  summary(ab <- glm(terminal.stock ~ u.wind + v.wind ,
                     family = binomial,
                     data = coho.compare.complete))
  
  ## Intercept only
  summary(ri <- glm(terminal.stock ~ 1,
                    family = binomial,
                    data = coho.compare.complete))
  
  ### AIC model selection ####
  glm.aic <- AIC(full, best, spatemp.dem, spatemp.ab, dem.ab, spatemp, 
                 dem, ab, ri)
  glm.aic$delta <- glm.aic$AIC - min(glm.aic$AIC)
  
  ## Pruned dataset ####
  
    # We found that age and wind are not significant, so drop to have a larger
    # dataset
  
  # Just keep columns we want to analyze 
  coho.compare.2 <- coho[,c(9:10, 15, 25, 37, 40, 42)]
  coho.compare.2 %>% summarise_all(~ sum(is.na(.))) # No missing values!
    # Also, create ID for each capture event (by unique combination of lat,
    # yday, and year)
  coho.compare.2 <- coho.compare.2 %>% 
    mutate(ID = group_indices(., lat, yday, year))
  
  ### Step AIC ####
  summary(full.2 <- glm(terminal.stock ~ ., 
                      data = coho.compare.2, 
                      family = binomial))
  
  step(full.2, 
       direction = 'both', trace = 0)
  
  # Model with sex, length, lat, capture.loc, year, and yday scores best
  summary(best.2 <- glm(formula = terminal.stock ~ sex + length + lat + 
                        capture.loc + year + yday, 
                      family = binomial, 
                      data = coho.compare.2))
  
    # Let's build a global model with interaction
  summary(full.2.int <- glm(terminal.stock ~ . + year:yday,
                            data = coho.compare.2,
                            family = binomial))
  
  step(full.2.int, direction = 'both', trace = 0)
  
   # Model with sex, length, lat, capture.loc, year, yday and year:yday 
  summary(best.2.int <- glm(formula = terminal.stock ~ sex + length + lat + 
                              capture.loc + year + yday + year:yday, 
                            family = binomial, data = coho.compare.2))
  
  ### Build nested models ####
  
  ## Global with interaction
  
  ## Spatiotemporal and demographic
  summary(spatemp.dem.2 <- glm(terminal.stock ~  year * yday + capture.loc + 
                               lat +
                               length + sex,
                             family = binomial,
                             data = coho.compare.2))
  
  ## Spatiotemporal
  summary(spatemp.2 <- glm(terminal.stock ~  year * yday + capture.loc + lat,
                         family = binomial,
                         data = coho.compare.2))
  
  ## Demographic
  summary(dem.2 <- glm(terminal.stock ~  sex + length,
                     family = binomial,
                     data = coho.compare.2))
  
  ## Intercept only
  summary(ri.2 <- glm(terminal.stock ~ 1,
                    family = binomial,
                    data = coho.compare.2))
  
  ### AIC model selection ####
  glm.aic.2 <- AIC(full.2, best.2, full.2.int, best.2.int, spatemp.dem.2, 
                   spatemp.2, dem.2, ri.2)
  glm.aic.2$delta <- glm.aic.2$AIC - min(glm.aic.2$AIC)
  
  final.glm <- glm(terminal.stock ~ sex + capture.loc + year*yday + lat,
                   family = binomial,
                   data = coho.compare.2)
  
  summary(final.glm)
  
  
# Bootstrapping for logistic coefficients ####
  # From Franz, lab 9
  # First, we append fitted values from the model to the original 
  # date frame, as we will need them for bootstrapping:
  pred <- predict(final.v2)
  
  dat <- cbind(coho, pred)
  
  # Writing a bootstrap function to work with 'boot':
  bf <- function(dat, i) {
    # create a bootstrap data set by adding re-sampled residuals 
    # Fit the model to this new bootstrapped data set:
    coef(glm(terminal.stock ~  julian.day + capture.loc + 
                      scale(lat, scale = F) + sex,
                    family = binomial,
                    data = dat[i,]))    
  }
  
  ## 5c. Run the bootstrap
  library(boot)
  # The 'boot' function uses the above function ('bf') to do the bootstrapping,
  # i.e. it runs bf with 1999 different sets of resampled residuals from 'resid(fit)':) 
  coho.boot <- boot(coho, bf, R = 5000)
  
  # Confidence intervals for individual predictions 
  # (predicted survival at the first, second, 10th and 40th wind values)
  # The 'boot.ci' function can return four different types of confidence intervals,
  # we extract only the percentile-based interval:
  boot.ci(coho.boot, type = "perc", index = 1)   # Confidence intervals for fitted value at x[1] = 285
  boot.ci(coho.boot, type = "perc", index = 2)   # CI at x[2] = 292.8
  boot.ci(coho.boot, type = "perc", index = 3)
  boot.ci(coho.boot, type = "perc", index = 4)
  boot.ci(coho.boot, type = "perc", index = 5)
  boot.ci(coho.boot, type = "perc", index = 6)
  boot.ci(coho.boot, type = "perc", index = 7)
  
 
# Scratch paper ####
  nrow(tags %>% 
    filter(year == '2021', species == 'coho'))
  
  nrow(tags %>% 
         filter(year == '2021', species == 'coho'))
  
  time.in <- lubridate::hm(tags$time.in)
  time.in <- 60 * lubridate::hour(time.in) + lubridate::minute(time.in)
  
  time.out <- lubridate::hm(tags$time.out)
  time.out <- 60 * lubridate::hour(time.out) + lubridate::minute(time.out)
  
  coho <- tags %>% filter(species == 'coho')
  
  nrow(coho %>% filter(year == '2021', capture.loc == '5', is.na(det.hist), is.na(recap)))

  time <- time.out - time.in  
  mean(time)
  range(time)

  time  
  
  vec <- c(0, 8, 4, 3, 1, 2)
  vec <- c(0, 8, 4, 3, 1, 2, 5) # 2 5 0 8 4 3 1 
  
  shft <- abs(which.max(vec) - length(vec) / 2)
  
  c(
    vec[(shft + 1):length(vec)],
    vec[1:shft]
  )
  
  library(SOfun)
  
  shifter(vec, which.max(vec) - length(vec) / 2)
  
  ## Wind effects ####
  # See what happens when we only inlcude winds >10mph
  coho.wind <- coho %>% 
    filter(wind.sp > 10)

  coho.compare.wind <- coho.wind[,c(9:10, 15, 25, 37:40, 42)]
  
  coho.compare.wind.complete <- coho.compare.wind[complete.cases(coho.compare.wind),]
  
  ## Global model ####
  summary(global <- glm(terminal.stock ~ year * yday + capture.loc + lat +
                          u.wind + v.wind +
                          sex + length,
                        data = coho.compare.wind.complete,
                        family = binomial))
  
  ## Spatiotemporal and demographic ####
  summary(spatemp.dem <- glm(terminal.stock ~  year * yday + capture.loc + 
                               lat +
                               length + sex,
                             family = binomial,
                             data = coho.compare.wind.complete))
  
  ## Spatiotemporal and abiotic ####
  summary(spatemp.ab <- glm(terminal.stock ~  year * yday + capture.loc + 
                              lat +
                              u.wind + v.wind ,
                            family = binomial,
                            data = coho.compare.wind.complete))
  
  ## Demographic and abiotic ####
  summary(dem.ab <- glm(terminal.stock ~  sex + length +
                          u.wind + v.wind ,
                        family = binomial,
                        data = coho.compare.wind.complete))
  
  ## Spatiotemporal ####
  summary(spatemp <- glm(terminal.stock ~  year * yday + capture.loc + lat,
                         family = binomial,
                         data = coho.compare.wind.complete))
  
  ## Demographic ####
  summary(dem <- glm(terminal.stock ~  sex + length,
                     family = binomial,
                     data = coho.compare.wind.complete))
  
  ## Abiotic ####
  summary(ab <- glm(terminal.stock ~ u.wind + v.wind ,
                    family = binomial,
                    data = coho.compare.wind.complete))
  
  ## Intercept only ####
  summary(ri <- glm(terminal.stock ~ 1,
                    family = binomial,
                    data = coho.compare.wind.complete))
  
  # AIC ###
  glm.aic.wind <- AIC(global, spatemp.dem, spatemp.ab, dem.ab, spatemp, dem, ab, ri)
  glm.aic.wind$delta <- glm.aic.wind$AIC - min(glm.aic.wind$AIC)
  