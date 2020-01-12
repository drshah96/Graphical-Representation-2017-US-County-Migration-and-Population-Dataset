####### Q2 ######

library("readxl")
library("pacman")
library("dplyr")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

States_MigrationRate_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("State","International Migration Rate")

States_MigrationRate_raw <- as.data.frame(States_MigrationRate_raw)

States_MigrationRate_s <- table(States_MigrationRate_raw$State)
States_MigrationRate_s <- as.data.frame(States_MigrationRate_s)
States_MigrationRate_s

States_MigrationRate_m <- table(States_MigrationRate_raw$`International Migration Rate`)
States_MigrationRate_m <- as.data.frame(States_MigrationRate_m)
names(States_MigrationRate_m) <- c("International Migration Rate","Frequency")
class(States_MigrationRate_m)

m1 <- merge(States_MigrationRate_m, States_MigrationRate_raw, by = c(States_MigrationRate_m$`International Migration Rate`,States_MigrationRate_raw$`International Migration Rate`))

States_MigrationRate_raw
States_MigrationRate <- as.data.frame(States_MigrationRate_raw)

suppressPackageStartupMessages(library(dplyr))
sum(States_MigrationRate$`International Migration Rate`)
d <- States_MigrationRate %>%
    mutate(
        cumsum = cumsum(States_MigrationRate$`International Migration Rate`),
        freq = round(States_MigrationRate$`International Migration Rate` / sum(States_MigrationRate$`International Migration Rate`), 3),
        cum_freq = cumsum(freq)
    )
d
tail(d$cum_freq)
sum(d$freq)
hist(d$freq)
hist(d$cum_freq)


library(ggplot2)
# Basic histogram
ggplot(d, aes(x=d$freq)) + geom_histogram()
# Change colors
p<-ggplot(d, aes(x=d$freq)) + 
    geom_histogram(color="black", fill="white")
p

ggplot(d, aes(x=d$freq, color=d$State)) +
    geom_histogram(fill="white")
# Overlaid histograms
ggplot(d, aes(x=d$freq, color=d$State)) +
    geom_histogram( alpha=0.5, position="identity")

ggplot(d, aes(x=d$freq, fill=d$State, color=d$State)) +
    geom_histogram(position="identity")

p<-ggplot(d, aes(x=d$freq, fill=d$State, color=d$State)) +
    geom_histogram(position="identity", alpha=0.5)
