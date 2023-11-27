#===============================================================================
# Norton Sound salmon telemetry 
#
# Date: November, 2023
# 
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This code is used to manipulate salmon telemetry data for analysis
#===============================================================================
# NOTES: Original data source is from 'tags.csv'
#===============================================================================

# Load packages ################################################################
library(tidyverse)
library(data.table)
library(tools)
library(nlme)
library(lme4)
library(mclogit)
library(MASS)
library(VGAM)
library(boot)
library(mgcv)
library(visreg)

# Set working directory ########################################################
setwd("C:/Users/lhhenslee/Desktop/git_repo/salmon_telemetry")

# Import data ##################################################################

# Import telemetry data
# 'master_colClasses.csv' is just a list for 'colClasses' argument of read.csv() 
col <- read.csv("data/tags_colClasses.csv", header = T)

# Import 'tags.csv'
tags <- read.csv("data/tags.csv", colClasses = paste(col[1,]))

# Data manipulation ####
  ## Final fates ####
  final.fate <- vector()

  for(i in 1:nrow(tags)) {
    if(tags[i,30] %in% c('1a', '1b', '1c')) {
      final.fate[i] <- '1'
    } else {
      if(tags[i,30] %in% c('2a', '2b', '2c')) {
        final.fate[i] <- '2'
      } else {
        if(tags[i,30] %in% c('3a', '3b', '3c')) {
          final.fate[i] <- '3'
        } else {
          final.fate[i] <- '4'
        }
      }
    }
  }

  table(final.fate)
  
  tags$final.fate <- final.fate
  
  ## Subdistrict of capture ####
capture.loc <- vector()

for(i in 1:nrow(tags)) {
  if(is.na(tags[i,25]) == TRUE) {
    capture.loc[i] <- NA
  } else {
    if(tags[i,25] %in% c('6a', '6b')) {
      capture.loc[i] <- '6'
    } else {
        capture.loc[i] <- '5'
    }
  }
}

tags$capture.loc <- as.factor(capture.loc)

  ## Terminal membership response ####

terminal.stock <- vector()

for(i in 1:nrow(tags)) {
  if(is.na(tags[i,35] == T)) {
    terminal.stock[i] <- NA
  } else {
    if(tags[i,35] == tags[i,25]) {
      terminal.stock[i] <- '1'
    } else {
      terminal.stock[i] <- '0'
    }
  }
}

tags$terminal.stock <- as.factor(terminal.stock)

  ## Age ####

if(FALSE) {
  age <- vector()

  for(i in 1:nrow(tags)) {
    if(is.na(tags[i,9] == T)) {
      age[i] <- NA
    } else {
      if(tags[i,9] %in% c(3, 4, 5, 7, 8)) {
        age[i] <- NA
      } else {
        age[i] <- paste(tags[i,9])
      }
    }
  }


 tags$age <- as.factor(age)
 }

  ## Wind ####
# updated 9/27/23 to account for 'math direction'

  wind.dir <- vector()

  for(i in 1:nrow(tags)) {
    if(is.na(tags[i,23] == T)) {
      wind.dir[i] <- NA
    } else {
      if(tags[i,23] == 'N') {
        wind.dir[i] <- 360
      } else {
        if(tags[i,23] == 'NE') {
          wind.dir[i] <- 45
        } else {
          if(tags[i,23] == 'E') {
            wind.dir[i] <- 90
          } else {
            if(tags[i,23] == 'SE') {
              wind.dir[i] <- 135
            } else {
              if(tags[i,23] == 'S') {
                wind.dir[i] <- 180
              } else {
                if(tags[i,23] == 'SW') {
                  wind.dir[i] <- 225
                } else {
                  if(tags[i,23] == 'W') {
                    wind.dir[i] <- 270
                  } else {
                    if(tags[i,23] == 'NW') {
                      wind.dir[i] <- 315
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  hist(wind.dir)

  md <- 270 - wind.dir
  
  md <- ifelse(md < 0, md + 360, md)
  
  tags$wind.dir <- md
  
  # Convert to kph
    # Multiply mph by 1.609344
  
  tags$wind.sp <- round(tags$wind.sp * 1.61, 2)
  
  # U and V winds
  
  tags$u.wind <- round(tags$wind.sp * cos(tags$wind.dir), 2)
  
  tags$v.wind <- round(tags$wind.sp * sin(tags$wind.dir), 2)

  ## Year ####
  tags$year <- as.factor(year(mdy(tags$capture.date)))
  ## Secchi ####
  tags$secchi <- (tags$secchi.dis + tags$secchi.re)/2
  ## day of year ####
  tags$yday <- yday(mdy(tags$capture.date))
  
# Write file ####
  write.csv(tags, 'data/tags_manipulated.csv', row.names = F)
  
