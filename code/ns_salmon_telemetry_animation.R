#===============================================================================
# Preparing data for GIS animation
# 9/12/2022
# Luke Henslee, College of Fisheries and Ocean Sciences, UAF
#
#===============================================================================
#NOTES: This script combines capture data with detection data for animation with 
# GIS software.

#===============================================================================

# Load packages ################################################################
library(tidyverse)
library(zoo)


# Set working directory ########################################################
setwd("C:/Users/lhhenslee/Desktop/git_repo/NS_salmon_telemetry")

# Import data ##################################################################

## Detection data
  # Import colClasses
col <- read.csv("data/animation/tag_detections_colClasses.csv")
det <- read.csv('data/animation/tag_detections.csv', 
                colClasses = paste(col[1,]))

# Tidying data #################################################################
### Let's convert 'stock' to a friendlier label
table(det$stock)

det$stock <- ifelse(det$stock %in% c(4, 'N', 'S'), 'Transitory', 
       ifelse(det$stock == 5, 'Shaktoolik', 'Unalakleet')) 

### Lubridate 
det$ymd.hms <- mdy_hm(det$ymd.hms)

# Interpolate detections #######################################################

### Okay let's import the paths made on google earth
Y <- read.csv("data/animation/rivers/koyuk.csv")
I <- read.csv("data/animation/rivers/inglutalik.csv")
N <- read.csv("data/animation/rivers/ungalik.csv")
S <- read.csv("data/animation/rivers/shaktoolik.csv")
T <- read.csv("data/animation/rivers/tagoomenik.csv")
E <- read.csv("data/animation/rivers/egavik.csv")
U <- read.csv("data/animation/rivers/unalakleet.csv")
G <- read.csv("data/animation/rivers/golsovia.csv")

## Expand detection data ####
### We use detection location and timestamps to interpolate animation in a 
### straight line at a constant rate

### First separate data into lists by tag ID
det.list <- split(det, f = det$tag.ID)

### Now loop through each list and expand detection history into 'move.list'
move.list <- list() # Empty list

for(i in 1:length(det.list)){
  move.list[[i]] <- complete(det.list[[i]], 
                             ymd.hms = seq(min(ymd.hms),
                                           max(ymd.hms), by = '15 min')) %>% 
    fill(tag.ID, stock, sex) %>% 
    arrange(ymd.hms)
  # Koyuk
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.916964, 64.927713)) {
      # The above line identifies fish that were detected by one of the inriver receivers 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(Y)+1):(nrow(move.list[[i]]))),3] <- Y[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(Y)+1):(nrow(move.list[[i]]))),4] <- Y[,2]
  }
  # Inglutalik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.837735, 64.830505)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(I)+1):(nrow(move.list[[i]]))),3] <- I[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(I)+1):(nrow(move.list[[i]]))),4] <- I[,2]
  }
  # Ungalik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.557998, 64.548801)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(N)+1):(nrow(move.list[[i]]))),3] <- N[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(N)+1):(nrow(move.list[[i]]))),4] <- N[,2]
  }
  # Shaktoolik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.368624, 64.366297)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(S)+1):(nrow(move.list[[i]]))),3] <- S[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(S)+1):(nrow(move.list[[i]]))),4] <- S[,2]
  }
  # Tagoomenik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.318634,64.320926)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(T)+1):(nrow(move.list[[i]]))),3] <- T[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(T)+1):(nrow(move.list[[i]]))),4] <- T[,2]
  }
  # Egavik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.042944, 64.039991)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(E)+1):(nrow(move.list[[i]]))),3] <- E[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(E)+1):(nrow(move.list[[i]]))),4] <- E[,2]
  }
  # Unalakleet
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(63.557764,63.555623)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(U)+1):(nrow(move.list[[i]]))),3] <- U[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(U)+1):(nrow(move.list[[i]]))),4] <- U[,2]
  }
  # Golsovia
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(63.860209, 63.871681)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(G)+1):(nrow(move.list[[i]]))),3] <- G[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(G)+1):(nrow(move.list[[i]]))),4] <- G[,2]
  }
}

### We now have all animation with upriver animation!!

## Multiple rivers ####

### Some fish explored other streams en route to their spawning destination 
### We need to identify fish that visited multiple streams and animate their
### inriver adventures

### We'll loop through the move.list, identify fish that visited inriver sites
### and add the river path before the detection, then we need to invert the path
### and stick it to the back

for(i in 1:length(det.list)) {
  for(j in 1:nrow(move.list[[i]])) {
  # Koyuk
    if(move.list[[i]][j, 3] %in% c(64.916964, 64.927713)) {
      move.list[[i]][c((j - nrow(Y) + 1):j),3] <- Y[,1]
      move.list[[i]][c(j:c(j + nrow(Y) - 1)),3] <- rev(Y[,1])
      move.list[[i]][c((j - nrow(Y) + 1):j),4] <- Y[,2]
      move.list[[i]][c(j:c(j + nrow(Y) - 1)),4] <- rev(Y[,2])
    }
  
  # Inglutalik
    if(move.list[[i]][j, 3] %in% c(64.837735, 64.830505)) {
      move.list[[i]][c((j - nrow(I) - 1):j),3] <- I[,1]
      move.list[[i]][c(j:c(j + nrow(I) + 1)),3] <- rev(I[,1])
      move.list[[i]][c((j - nrow(I) + 1):j),4] <- I[,2]
      move.list[[i]][c(j:c(j + nrow(I) - 1)),4] <- rev(I[,2])
    }
    
  # Ungalik
    if(move.list[[i]][j, 3] %in% c(64.557998, 64.548801)) {
      move.list[[i]][c((j - nrow(N) + 1):j),3] <- N[,1]
      move.list[[i]][c(j:c(j + nrow(N) - 1)),3] <- rev(N[,1])
      move.list[[i]][c((j - nrow(N) + 1):j),4] <- N[,2]
      move.list[[i]][c(j:c(j + nrow(N) - 1)),4] <- rev(N[,2])
  }
    
  # Shaktoolik
    if(move.list[[i]][j, 3] %in% c(64.368624, 64.366297)) {
      move.list[[i]][c((j - nrow(S) + 1):j),3] <- S[,1]
      move.list[[i]][c(j:c(j + nrow(S) - 1)),3] <- rev(S[,1])
      move.list[[i]][c((j - nrow(S) + 1):j),4] <- S[,2]
      move.list[[i]][c(j:c(j + nrow(S) - 1)),4] <- rev(S[,2])
    }
    
  # Tagoomenik
    if(move.list[[i]][j, 3] %in% c(64.318634,64.320926)) {
      move.list[[i]][c((j - nrow(T) + 1):j),3] <- T[,1]
      move.list[[i]][c(j:c(j + nrow(T) - 1)),3] <- rev(T[,1])
      move.list[[i]][c((j - nrow(T) + 1):j),4] <- T[,2]
      move.list[[i]][c(j:c(j + nrow(T) - 1)),4] <- rev(T[,2])
    }
    
  # Egavik
    if(move.list[[i]][j, 3] %in% c(64.042944, 64.039991)) {
      move.list[[i]][c((j - nrow(E) + 1):j),3] <- E[,1]
      move.list[[i]][c(j:c(j + nrow(E) - 1)),3] <- rev(E[,1])
      move.list[[i]][c((j - nrow(E) + 1):j),4] <- E[,2]
      move.list[[i]][c(j:c(j + nrow(E) - 1)),4] <- rev(E[,2])
    }
    
  # Unalakleet
    if(move.list[[i]][j, 3] %in% c(63.557764,63.555623)) {
      move.list[[i]][c((j - nrow(U) + 1):j),3] <- U[,1]
      move.list[[i]][c(j:c(j + nrow(U) - 1)),3] <- rev(U[,1])
      move.list[[i]][c((j - nrow(U) + 1):j),4] <- U[,2]
      move.list[[i]][c(j:c(j + nrow(U) - 1)),4] <- rev(U[,2])
    } 
    
  # Golsovia
    if(move.list[[i]][j, 3] %in% c(63.860209, 63.871681)) {
      move.list[[i]][c((j - nrow(G) + 1):j),3] <- G[,1]
      move.list[[i]][c(j:c(j + nrow(G) - 1)),3] <- rev(G[,1])
      move.list[[i]][c((j - nrow(G) + 1):j),4] <- G[,2]
      move.list[[i]][c(j:c(j + nrow(G) - 1)),4] <- rev(G[,2])
    }
  }
  move.list[[i]]$lat <- na.approx(move.list[[i]]$lat)
  move.list[[i]]$long <- na.approx(move.list[[i]]$long)
}

# Finalize and write dataframe ####
animation <- do.call(rbind.data.frame, move.list)
animation$ymd.hms <- as.character(animation$ymd.hms)

animation_20 <- subset(animation, year(ymd.hms) == '2020')

animation_21 <- subset(animation, year(ymd.hms) == '2021')

# write.csv(animation_20, 'data/animation/animation_20.csv', row.names = F)
# write.csv(animation_21, 'data/animation/animation_21.csv', row.names = F)
