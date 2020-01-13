########### Q7 ##########

library("readxl")
library("pacman")
library("dplyr")
library("ggplot2")
#install.packages("ggpubr")
library("ggpubr")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

States_abrev <- import("Data/usstates-dataset/state-abbrevs.csv") %>%
    as.tibble() %>%
    select("state","abbreviation")
States_abrev <- as.data.frame(States_abrev)
States_abrev

States_areas <- import("Data/usstates-dataset/state-areas.csv") %>%
    as.tibble() %>%
    select("state","area (sq. mi)")
States_areas <- as.data.frame(States_areas)
States_areas

States_details <- as.data.frame(States_abrev)
States_details$area <- States_areas$`area (sq. mi)`
States_details

States_Population_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("State","2017 Population")
States_Population_raw <- as.data.frame(States_Population_raw)
States_Population_raw

States_Population <- States_Population_raw %>%
    group_by(State) %>%
    summarize(Total_Population = sum(`2017 Population`))
States_Population

States_details$Population <- States_Population$Total_Population
names(States_details) <- c("State","Abbreviation","Area sq. mi", "Population")
States_details

States_details$area <- (States_details$`Area sq. mi`) * 1.60934
names(States_details) <- c("State","Abbreviation","Area sq. mi", "Population", "Area sq. km")
States_details$area <- round(States_details$`Area sq. km`,2)
States_details$area <- NULL
States_details

States_details$Density <- States_details$Population / States_details$`Area sq. km`


ggplot(data=States_details, aes(x=States_details$Abbreviation, y=States_details$Density, group=1)) +
    geom_line(color="#236B8E",size = 1.5)+
    ggtitle("Population Density per Sq. KM") +
    xlab("States") + ylab("Population Density") +
    geom_point() +
    geom_text(label=round(States_details$Density,2), 
              hjust=0.3, vjust = -1,
              check_overlap = TRUE)

