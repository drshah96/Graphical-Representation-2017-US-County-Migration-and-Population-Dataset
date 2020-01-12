library("readxl")
library("pacman")
library("dplyr")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

#Q1 (a)
States_Population_2017_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("State","2017 Population")

States_Population_2017_raw
States_Population_2017_raw <- as.data.frame(States_Population_2017_raw)

States_Population_2017 <- States_Population_2017_raw[order(-States_Population_2017_raw$`2017 Population`),]
States_Population_2017

States_Population_2017 <- States_Population_2017[1:20,]
States_Population_2017

suppressPackageStartupMessages(library(dplyr))

d <- arrange(States_Population_2017, desc(`2017 Population`)) %>%
    mutate(
        cumsum = cumsum(States_Population_2017$`2017 Population`),
        freq = round(States_Population_2017$`2017 Population` / sum(States_Population_2017$`2017 Population`), 3),
        cum_freq = cumsum(freq)
    )
d

def_par <- par() 

## New margins
par(mar=c(5,5,4,5)) 

## bar plot, pc will hold x values for bars
pc = barplot(d$`2017 Population`,  
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(d$cumsum, na.rm = T)), 
             ylab = "Cummulative Counts" , cex.names = 0.7, 
             names.arg = d$State,
             main = "Pareto Chart (version 1)")

## Cumulative counts line 
lines(pc, d$cumsum, type = "b", cex = 0.7, pch = 19, col="cyan4")

## Framing plot
box(col = "grey62")

## adding axes
axis(side = 2, at = c(0, d$cumsum), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, d$cumsum), labels = paste(c(0, round(d$cum_freq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)

## restoring default paramenter
par(def_par) 

def_par <- par() 

# New margins
par(mar=c(5,5,4,5)) 

## plot bars, pc will hold x values for bars
pc = barplot(d$`2017 Population`,
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(d$`2017 Population`, na.rm = T)), 
             ylab = "Counts" , cex.names = 0.7, 
             names.arg = d$State,
             main = "Pareto Chart (version 2)")

## anotate left axis
axis(side = 2, at = c(0, d$`2017 Population`), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)

## frame plot
box( col = "grey62")

## Cumulative Frequency Lines 
px <- d$cum_freq * max(d$`2017 Population`, na.rm = T)
lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")

## Annotate Right Axis
axis(side = 4, at = c(0, px), labels = paste(c(0, round(d$cum_freq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")

## restoring default paramenter
par(def_par) 


################ Q1 (b)###############
States_Population_2017_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("County","State","2017 Population")

States_Population_2017_raw
States_Population_2017_raw <- as.data.frame(States_Population_2017_raw)

States_Population_2017 <- States_Population_2017_raw[order(-States_Population_2017_raw$`2017 Population`),]
States_Population_2017

States_Population_2017 <- States_Population_2017[1:20,]
States_Population_2017

States_Population_2017$Country_State <- paste(States_Population_2017$County,"_",States_Population_2017$State)
States_Population_2017$County <- NULL
States_Population_2017$State <- NULL
States_Population_2017

suppressPackageStartupMessages(library(dplyr))

d <- arrange(States_Population_2017, desc(`2017 Population`)) %>%
    mutate(
        cumsum = cumsum(States_Population_2017$`2017 Population`),
        freq = round(States_Population_2017$`2017 Population` / sum(States_Population_2017$`2017 Population`), 3),
        cum_freq = cumsum(freq)
    )
d

def_par <- par() 

# New margins
par(mar=c(5,5,4,5)) 

## plot bars, pc will hold x values for bars
pc = barplot(d$`2017 Population`,
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(d$`2017 Population`, na.rm = T)), 
             ylab = "Counts" , cex.names = 0.7, 
             names.arg = d$Country_State,
             main = "Pareto Chart (version 2)")

## anotate left axis
axis(side = 2, at = c(0, d$`2017 Population`), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)

## frame plot
box( col = "grey62")

## Cumulative Frequency Lines 
px <- d$cum_freq * max(d$`2017 Population`, na.rm = T)
lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")

## Annotate Right Axis
axis(side = 4, at = c(0, px), labels = paste(c(0, round(d$cum_freq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")

## restoring default paramenter
par(def_par) 
