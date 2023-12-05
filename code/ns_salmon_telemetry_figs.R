#===============================================================================
# Norton Sound salmon telemetry figures 
#
# Date: November, 2023
# 
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This code produces publication-ready figures for AFS
#===============================================================================
# NOTES: This script uses 'coho_harv_fig.csv' as a data source
#===============================================================================

# Load packages ################################################################
library(tidyverse)
library(VGAM)
library(visreg)
library(extrafont)
library(ggh4x)
library(patchwork)
library(scales)

# Set working directory ########################################################
setwd("C:/Users/lhhenslee/Desktop/git_repo/salmon_telemetry")

# Import data ##################################################################
  # 'tags_manipulated_colClasses.csv' is just a list for 'colClasses' argument  
  # of read.csv() 
  col <- read.csv("data/tags_manipulated_colClasses.csv", header = T)

  # Import 'tags_manipulated.csv'
  tags <- read.csv("data/tags_manipulated.csv", colClasses = paste(col[1,]))

  # filter for coho in 2020 and 2021
  coho <- tags %>% 
  filter(species == 'coho' & year != 2022)

  fig.dat <- read.csv('data/fig_dat.csv')

# Create labels and color palettes, set font ####
  # Relevel stock factors
  fig.dat$Stock <- fct_relevel(fig.dat$Stock, 'Shaktoolik', 'Unalakleet')

  # labels for figs
  sd <- c('5' = 'Shaktoolik subdistrict', '6' = 'Unalakleet subdistrict',
          '2020' = '2020', '2021' = '2021')
  
  # Color palettes
  col.pal <- c("#FC8D62", "#8DA0CB", "#A6D854")
  stock.pal <- c("#E78AC3", "#66C2A5", "#FC8D62", "#8DA0CB", "#A6D854")
  
  # Fonts
  windowsFonts(Times=windowsFont("Times New Roman"))
  loadfonts(device = 'win')
  
  # Create factors for fig 2
  coho$tracked <- as.factor(ifelse(coho$final.fate %in% c('1', '2'), 'True', 'False'))
  coho$tracked <- fct_relevel(coho$tracked, 'True', 'False')
  
  # Create stock factors for fig 3
  coho.fig.3 <- subset(coho, is.na(coho$spawn.group) == F)
  
  coho.fig.3$Stock <- ifelse(coho.fig.3$spawn.group == '4', 'Norton Bay', 
                             ifelse(coho.fig.3$spawn.group == '5', 'Shaktoolik',
                                    ifelse(coho.fig.3$spawn.group == '6', 'Unalakleet', 
                                           ifelse(coho.fig.3$spawn.group == 'N', 'Northern', 'Southern'))))
  # ... and reorder the factor levels
  coho.fig.3$Stock <- fct_relevel(coho.fig.3$Stock, 'Northern', 'Norton Bay', 'Shaktoolik',
                                  'Unalakleet', 'Southern')
  
# Figure 2, tag deployment  ####

  tags.deployed <- ggplot(subset(coho), aes(x = stat.week, fill = tracked)) +
    geom_bar() +
    xlab('Statistical week') +
    ylab('Tags deployed') +
    labs(fill = 'Assigned to \nstock of origin') +
    scale_fill_manual(values = c("#8DA0CB", "#FC8D62")) +
    facet_grid(year ~ capture.loc, labeller = as_labeller(sd)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = seq(0, 100, 25)) +
    theme_classic() + 
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #position the legend on the figure
           legend.position = c(0.6,0.3),
           #adjust size of text for legend
           legend.title = element_text(size = 12), 
           legend.text = element_text(size = 12),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black"),
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  tags.deployed
  
  #ggsave(tags.deployed, file = "figs/tag_deployment_v2.png",  width = 21, height = 15, units = "cm", dpi = 300)
  
  ## 2020 ####
  tags.deployed.20 <- ggplot(subset(coho, year == '2020'), aes(x = stat.week, fill = tracked)) +
    geom_bar() +
    xlab('Statistical week') +
    ylab('Tags deployed') +
    labs(fill = 'Assigned to \nstock of origin') +
    scale_fill_manual(values = c("#8DA0CB", "#FC8D62")) +
    facet_grid(~ capture.loc, labeller = as_labeller(sd)) +
    scale_y_continuous(expand = c(0,0, 0.01, 0), limits = c(0, 100), breaks = seq(0, 100, 25)) +
    theme_classic() + 
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_blank(), #element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #position the legend on the figure
           #legend.position = 'bottom',
           #adjust size of text for legend
           legend.title = element_text(size = 14), 
           legend.text = element_text(size = 14),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black"),
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  tags.deployed.20
  
  ## 2021 ####
  tags.deployed.21 <- ggplot(subset(coho, year == '2021'), aes(x = stat.week, fill = tracked)) +
    geom_bar() +
    xlab('Statistical week') +
    ylab('Tags deployed') +
    labs(fill = 'Assigned to \nstock of origin') +
    scale_fill_manual(values = c("#8DA0CB", "#FC8D62")) +
    facet_grid(~ capture.loc, labeller = as_labeller(sd)) +
    scale_y_continuous(expand = c(0,0, 0.01, 0), limits = c(0, 75), breaks = seq(0, 75, 25)) +
    theme_classic() + 
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #position the legend on the figure
           legend.position = 'none',
           #adjust size of text for legend
           legend.title = element_text(size = 12), 
           legend.text = element_text(size = 12),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black"),
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  tags.deployed.21
  
  ## patchwork and save ####
  deploy.fig <- tags.deployed.20 + tags.deployed.21 + plot_annotation(tag_levels = 'A') +
    plot_layout(guides = 'collect', nrow = 2) & 
    theme(plot.tag = element_text(size = 16, face = 'bold'),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          #legend.position = 'bottom',
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14))
  
  deploy.fig
  
  ggsave(deploy.fig, file = "figs/fig_2/fig_2_v5.png",  width = 21, height = 15, units = "cm", dpi = 300)
  
# Figure 3, stock proportions ####
  
  stock.prop <- ggplot(coho.fig.3, aes(x = year, fill = Stock)) +
    geom_bar(position = 'fill') +
    facet_wrap(~ capture.loc, labeller = as_labeller(sd)) +
    scale_fill_manual(values = stock.pal) +
    scale_y_continuous(expand = c(0,0,.025,0), limits = c(0, 1), breaks = seq(0, 1, .25)) +
    xlab('Year') +
    ylab('Stock proportion') +
    theme_classic() + 
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #adjust size of text for legend,
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 14),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black"),
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  stock.prop
  
  ggsave(stock.prop, file = "figs/fig_3/fig_3_v3.png",  width = 20, height = 13.33, units = "cm", dpi = 300)
  
  stock.prop.20 <- ggplot(subset(coho.fig.3, year == '2020'), aes(x = stat.week, fill = Stock)) +
    geom_bar(position = 'fill') +
    facet_grid(~capture.loc, labeller = as_labeller(sd)) +
    scale_fill_manual(values = stock.pal) +
    scale_y_continuous(expand = c(0,0,.025,0), limits = c(0, 1), breaks = seq(0, 1, .25)) +
    xlab('Statistical week') +
    ylab('Stock membership proportion') +
    theme_classic() + 
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #adjust size of text for legend,
           legend.position = 'none',
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  stock.prop.20
  
  stock.prop.21 <- ggplot(subset(coho.fig.3, year == '2021'), aes(x = stat.week, fill = Stock)) +
    geom_bar(position = 'fill') +
    facet_grid(~capture.loc, labeller = as_labeller(sd)) +
    scale_fill_manual(values = stock.pal) +
    scale_y_continuous(expand = c(0,0,.025,0), limits = c(0, 1), breaks = seq(0, 1, .25)) +
    xlab('Statistical week') +
    ylab('Stock membership proportion') +
    theme_classic() + 
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #adjust size of text for legend,
           legend.position = 'none',
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  stock.prop.21
  
  ## patchwork and save ####
  layout <- '
  AAAC
  BBBC
  '
  
  stock.prop.fig <- stock.prop.20 + stock.prop.21 + stock.prop +
                              plot_layout(guides = 'collect', design = layout, widths = unit(c(1, -1), c('null', 'null'))) + plot_annotation(tag_levels = 'A') & 
                              theme(plot.tag = element_text(size = 16, face = 'bold'))
  
  stock.prop.fig
  
  # ggsave(stock.prop.fig, file = "figs/stock_prop_fig.png",  width = 40, height = 20, units = "cm", dpi = 300)

  
  # Figure 4, model interpretation ####
  
  ## Modify model ####
  
  # Create dataframe for model construction
  mod.dat <- coho
  
  # Create and relevel factors
  mod.dat$capture.loc <- ifelse(mod.dat$capture.loc == '5', 'Shaktoolik subdistrict', 'Unalakleet subdistrict')
  mod.dat$capture.loc <- fct_relevel(mod.dat$capture.loc, 'Unalakleet subdistrict', 'Shaktoolik subdistrict')
  
  # Construct final model
  final.glm <- glm(terminal.stock ~ sex + capture.loc + year*yday + lat,
                   family = binomial,
                   data = mod.dat)
  
  summary(final.glm)
  
  ## sex.fig ####
  sex.fig <- visreg(final.glm, 'sex', scale = 'response', gg = T) +
    scale_y_continuous(limits = c(.8, 1), expand = c(0,0)) +
    xlab('Sex') +
    ylab('Natal stock membership probability') +
    theme_classic() +
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #adjust size of text for legend,
           legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black"),
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))

  sex.fig
  
  ## year.fig ####
  year.fig <- visreg(final.glm, 'year', scale = 'response', gg = T) +
    scale_y_continuous(limits = c(.8, 1), expand = c(0,0)) +
    xlab('Year') +
    ylab('') +
    theme_classic() +
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #adjust size of text for legend,
           legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black"),
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  
  year.fig
  
  ## yday.fig ####
  yday.fig <- visreg(final.glm, 'yday', by = 'year', scale = 'response', gg = T) +
    scale_y_continuous(limits = c(.6, 1), expand = c(0,0), 
                       labels = number_format(accuracy = 0.01)) +
    xlab('Day of year') +
    ylab('Natal stock membership probability') +
    theme_classic() +
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #adjust size of text for legend,
           legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black"),
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  yday.fig
  
  ## sd.fig ####
  sd.fig <- visreg(final.glm, 'lat', by = 'capture.loc', scale = 'response',gg = T) +
    facet_wrap(vars(capture.loc), scales = 'free_x') +
    # Create data ranges for each facet with ggh4x
    facetted_pos_scales(x = list(capture.loc == 'Unalakleet subdistrict' ~ 
                                   scale_x_continuous(limits = c(63.89247, 64.131287),
                                                      breaks = seq(63.90,64.10,0.1),
                                                      labels = number_format(accuracy = 0.01)),
                                 capture.loc == 'Shaktoolik subdistrict' ~
                                   scale_x_continuous(limits = c(64.131287, 64.35800),
                                                      breaks = seq(64.15,64.35,0.1),
                                                      labels = number_format(accuracy = 0.01)))) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = number_format(accuracy = 0.01)) +
    xlab('Latitude (°N)') +
    ylab('Natal stock membership probability') +
    theme_classic() +
    theme (axis.title.y = element_blank(), #element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #adjust size of text for legend,
           legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black"),
           #change spacing between facets
           panel.spacing.y = unit(2, "lines"),
           #change faceted panel title size
           strip.text = element_text(size = 14),
           strip.switch.pad.grid = unit(0.2, "in"),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(linewidth = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(linewidth = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", linewidth = 0.5, lineend = "square"))
  
  sd.fig
  
  ## patchwork and save ####
  mod.fig <- sex.fig + year.fig + yday.fig + sd.fig + plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 16, face = 'bold'))
  
  mod.fig
  
  # ggsave(mod.fig, file = "figs/mod_fig_v2.png",  width = 32, height = 24, units = "cm", dpi = 300)
  
# Figure 5, catch partitioning ####
  # Make subdistrict column
  fig.dat$Subdistrict <- ifelse(fig.dat$capture.loc == '5', 'Shaktoolik subdistrict', 'Unalakleet subdistrict')
  
  ## 2020 ####
  fig.part.20 <- ggplot(subset(fig.dat, year == '2020'), aes(x = as.factor(julian.day), y = value, fill = Stock)) +
    geom_col(aes(fill = Stock), position = 'fill') +
    facet_grid(~ capture.loc, scales = 'free', labeller = as_labeller(sd)) +
    xlab("") +
    ylab('Estimated stock proportion') +
    scale_fill_manual(values = col.pal) +
    #set the limits and tick breaks for the y-axis
    scale_y_continuous(expand = c(0,0, 0.01, 0)) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    #adjust the order of the legend, make new labels, and select the symbol colors
    #makes the figure background white without grid lines
    theme_classic() +
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           #aspect.ratio = 1/3,
           text = element_text(family = "Times New Roman"),
           strip.text = element_text(size = 14),
           #position the legend on the figure
           #legend.position = 'none',
           #adjust size of text for legend
           #legend.text = element_text(size = 12),
           #margin for the plot
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(size = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(size = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))
  
  fig.part.20
  
  
  ## 2021 ####
  fig.part.21 <- ggplot(subset(fig.dat, year == '2021'), aes(x = as.factor(julian.day), y = value, fill = Stock)) +
    geom_col(aes(fill = Stock), position = 'fill') +
    facet_grid(~ capture.loc, scales = 'free', labeller = as_labeller(sd)) +
    xlab("Day of year of landing") +
    ylab("Estimated stock proportion") +
    scale_fill_manual(values = col.pal) +
    #set the limits and tick breaks for the y-axis
    scale_y_continuous(expand = c(0,0, 0.01, 0)) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    #adjust the order of the legend, make new labels, and select the symbol colors
    #makes the figure background white without grid lines
    theme_classic() +
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           #aspect.ratio = 1/3,
           text = element_text(family = "Times New Roman"),
           strip.text = element_text(size = 14),
           #position the legend on the figure
           #legend.position = 'none',
           #adjust size of text for legend
           #legend.text = element_text(size = 14),
           #margin for the plot
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(size = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(size = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))
  
  fig.part.21
  
  ## 2020 and 2021 sum ####
  fig.part.sum <- ggplot(fig.dat, aes(x = as.factor(year), y = value, fill = Stock)) +
    geom_col(aes(fill = Stock), position = 'fill') +
    facet_grid(~ Subdistrict, labeller = label_wrap_gen(width=10)) +
    xlab('Year') +
    ylab('Estimated stock proportion') +
    scale_fill_manual(values = col.pal) +
    #set the limits and tick breaks for the y-axis
    scale_y_continuous(expand = c(0,0, 0.01, 0)) +
    #adjust the order of the legend, make new labels, and select the symbol colors
    #makes the figure background white without grid lines
    theme_classic() +
    #below are theme settings that provide unlimited control of your figure and can be a template for other figures
    #set the size, spacing, and color for the y-axis and x-axis titles
    theme (axis.title.y = element_blank(), #element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
            #aspect.ratio =2,
           text = element_text(family = "Times New Roman"),
           strip.text = element_text(size = 14),
           #modify plot title, the B in this case
           plot.title = element_text(face = "bold", family = "Arial"),
           #position the legend on the figure
           #legend.position = c(0.3,0.85),
           #adjust size of text for legend
           #legend.position = 'none',
           #legend.title = element_text(size = 14),
           #legend.text = element_text(size = 14),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(size = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(size = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))
  
  fig.part.sum
  
  ## patchwork and save ####
  layout <- '
  AAC
  BBC
  '
  
  fig.part <- fig.part.20 + fig.part.21 + fig.part.sum + plot_annotation(tag_levels = 'A') +
    plot_layout(design = layout, guides = 'collect')  & 
    theme(plot.tag = element_text(size = 16, face = 'bold'),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          legend.position = 'bottom',
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14))
  
  fig.part
  
   ggsave(fig.part, file = "figs/fig_5/fig_part_v17.png",  width = 40, height = 20, units = "cm", dpi = 300)
  
  