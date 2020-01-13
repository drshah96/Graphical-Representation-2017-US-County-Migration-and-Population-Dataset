####### Q2 ######

library("readxl")
library("pacman")
library("dplyr")
library("ggplot2")
#install.packages("ggpubr")
library("ggpubr")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

States_MigrationRate_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("State","International Migration Rate")
States_MigrationRate_raw <- as.data.frame(States_MigrationRate_raw)

#States_MigrationRate_m
#States_MigrationRate_s <- table(States_MigrationRate_raw$State)
#States_MigrationRate_s <- as.data.frame(States_MigrationRate_s)
#States_MigrationRate_s

#States_MigrationRate_m <- table(States_MigrationRate_raw$`International Migration Rate`)
#States_MigrationRate_m <- as.data.frame(States_MigrationRate_m)
#names(States_MigrationRate_m) <- c("International Migration Rate","Frequency")

#m <- data.frame(States_MigrationRate_m$`International Migration Rate`, States_MigrationRate_raw$`International Migration Rate`)
#m1 <- merge(States_MigrationRate_m, States_MigrationRate_raw, by = c(States_MigrationRate_m$`International Migration Rate`,States_MigrationRate_raw$`International Migration Rate`))
#m2 <- group_by(States_MigrationRate_raw, States_MigrationRate_raw$`International Migration Rate`)
#m2

States_MigrationRate <- States_MigrationRate_raw %>%
    group_by(State) %>%
    summarize(m3 = sum(`International Migration Rate`))
names(States_MigrationRate) <- c("States","Frequency")
States_MigrationRate

suppressPackageStartupMessages(library(dplyr))

#sum(States_MigrationRate$`International Migration Rate`)

States_MigrationRate <- States_MigrationRate %>%
    mutate(
        cumsum = cumsum(States_MigrationRate$Frequency),
        freq = round(States_MigrationRate$Frequency / sum(States_MigrationRate$Frequency), 3),
        cum_freq = cumsum(freq)
    )
States_MigrationRate
tail(States_MigrationRate)

ggplot(data= States_MigrationRate, mapping = aes(x=States_MigrationRate$cum_freq,  color=States_MigrationRate$States)) +
   geom_histogram(fill="white")
  #  geom_col(aes(color=States_MigrationRate$States))                  
hist(States_MigrationRate$States)
hist(d$cum_freq)

