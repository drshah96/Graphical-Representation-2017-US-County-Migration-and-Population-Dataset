########## Q6 ############

library("readxl")
library("pacman")
library("dplyr")
library("ggplot2")
#install.packages("ggpubr")
library("ggpubr")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

International_MigrationRate_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("State","International Migration Rate")
International_MigrationRate_raw <- as.data.frame(International_MigrationRate_raw)
International_MigrationRate_raw

Domestic_MigrationRate_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("State","Domestic Migration Rate")
Domestic_MigrationRate_raw <- as.data.frame(Domestic_MigrationRate_raw)
Domestic_MigrationRate_raw

International_MigrationRate <- International_MigrationRate_raw %>%
    group_by(State) %>%
    summarize(Intl_Frequency = sum(`International Migration Rate`))
International_MigrationRate

Domestic_MigrationRate <- Domestic_MigrationRate_raw %>%
    group_by(State) %>%
    summarize(Dom_Frequency = sum(`Domestic Migration Rate`))
Domestic_MigrationRate

MigrationRate <- as.data.frame(International_MigrationRate)
MigrationRate$Dom_Frequency <- Domestic_MigrationRate$Dom_Frequency
MigrationRate

ggplot(MigrationRate, aes(x=MigrationRate$Intl_Frequency, y=MigrationRate$Dom_Frequency)) + 
    geom_point() + 
    geom_text(label=MigrationRate$State, 
              hjust=1.5, vjust = 0,
              check_overlap = TRUE) + 
    geom_smooth(method=lm)
