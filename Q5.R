############ Q5 #############

library("readxl")
library("pacman")
library("dplyr")
library("ggplot2")
#install.packages("ggpubr")
library("ggpubr")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

County_MigrationRate_raw <-  import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("County","International Migration Rate")
County_MigrationRate_raw <- as.data.frame(County_MigrationRate_raw)
County_MigrationRate_raw

County_MigrationRate <- County_MigrationRate_raw %>%
    group_by(County) %>%
    summarize(frequency = sum(`International Migration Rate`))
County_MigrationRate
summary(County_MigrationRate)

