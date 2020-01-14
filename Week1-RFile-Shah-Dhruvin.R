######### Module 1 #########

install.packages("e1071")
install.packages("ggpubr")
install.packages("expss")
install.packages("plotly")
library("readxl")
library("pacman")
library("dplyr")
library("ggplot2")
library("ggpubr")
library(e1071)
library(expss)
library(plotly)
install.packages("ggthemes")
library(ggthemes)
library(grid)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)



######### Q1 #########

States_Population_2017_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("State","2017 Population")

States_Population_2017_raw
States_Population_2017_raw <- as.data.frame(States_Population_2017_raw)

States_Population_2017 <- States_Population_2017_raw[order(-States_Population_2017_raw$`2017 Population`),]
States_Population_2017

States_Population_2017 <- States_Population_2017[1:20,]
States_Population_2017$`2017 Population`<- (States_Population_2017$`2017 Population`)/1000000

suppressPackageStartupMessages(library(dplyr))

d <- arrange(States_Population_2017, desc(`2017 Population`)) %>%
    mutate(
        cumsum = round(cumsum(States_Population_2017$`2017 Population`),3),
        freq = round(States_Population_2017$`2017 Population` / sum(States_Population_2017$`2017 Population`), 3),
        cum_freq = cumsum(freq)
    )
d

def_par <- par() 

par(mar=c(5,5,4,5)) 

pc = barplot(d$`2017 Population`,
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(d$`2017 Population`, na.rm = T)), 
             ylab = "Population" , xlab = "States", cex.names = 0.7, 
             names.arg = d$State,
             main = "Top 20 States - Pareto Chart")

axis(side = 2, at = c(0, round(d$`2017 Population`,2)), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)

box( col = "grey62")

px <- d$cum_freq * max(d$`2017 Population`, na.rm = T)
lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")

axis(side = 4, at = c(0, px), labels = paste(c(0, round(d$cum_freq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")

par(def_par)



######### Q2 #########

States_MigrationRate_raw <- import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("State","International Migration Rate")
States_MigrationRate_raw <- as.data.frame(States_MigrationRate_raw)

States_MigrationRate <- States_MigrationRate_raw %>%
    group_by(State) %>%
    summarize(Frequency = sum(`International Migration Rate`))

names(States_MigrationRate) <- c("States","Frequency")
States_MigrationRate

suppressPackageStartupMessages(library(dplyr))

States_MigrationRate <- States_MigrationRate %>%
    mutate(
        cumsum = cumsum(States_MigrationRate$Frequency),
        freq = round(States_MigrationRate$Frequency / sum(States_MigrationRate$Frequency), 3),
        cum_freq = cumsum(freq)
    )
States_MigrationRate
tail(States_MigrationRate)

hist(States_MigrationRate$Frequency,breaks = 50, main = "Migration Rate Frequency Histogram", xlab = "State Frequency", col = "#515585")
hist(States_MigrationRate$cum_freq,breaks = 50, main = "Migration Rate Cumulative Frequency Histogram", xlab = "State Cumulative Frequency", col = "#0F4C75")



######### Q3 #########

East_NaturalChange_raw <- import("Data/Natural Change Dataset/East Natural Chnage.xlsx") %>%
    as.tibble() %>%
    select("State","Natural Change")
East_NaturalChange_raw <- as.data.frame(East_NaturalChange_raw)
East_NaturalChange_raw

West_NaturalChange_raw <- import("Data/Natural Change Dataset/West Natural Chnage.xlsx") %>%
    as.tibble() %>%
    select("State","Natural Change")
West_NaturalChange_raw <- as.data.frame(West_NaturalChange_raw)
West_NaturalChange_raw

(East_NaturalChange_Min <- min(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_Max <- max(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_Range <- East_NaturalChange_Max - East_NaturalChange_Min)
(East_NaturalChange_Sum <- sum(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_Mean <- mean(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_Median <- median(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_StandardDeviation <- sd(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_Variance <- var(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_Quartile <- quantile(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_IQR <- IQR(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_Skewness <- skewness(East_NaturalChange_raw$`Natural Change`))
(East_NaturalChange_Kurtosis <- kurtosis(East_NaturalChange_raw$`Natural Change`))

(West_NaturalChange_Min <- min(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_Max <- max(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_Range <- West_NaturalChange_Max - West_NaturalChange_Min)
(West_NaturalChange_Sum <- sum(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_Mean <- mean(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_Median <- median(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_StandardDeviation <- sd(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_Variance <- var(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_Quartile <- quantile(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_IQR <- IQR(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_Skewness <- skewness(West_NaturalChange_raw$`Natural Change`))
(West_NaturalChange_Kurtosis <- kurtosis(West_NaturalChange_raw$`Natural Change`))

Parameters <- (c("East Natural Change", "West Natural Change"))
Minimum <- c(East_NaturalChange_Min,West_NaturalChange_Min)
Maximum <- c(East_NaturalChange_Max,West_NaturalChange_Max)
(Range <- c(East_NaturalChange_Range,West_NaturalChange_Range))
Sum <- c(East_NaturalChange_Sum,West_NaturalChange_Sum)
Mean <- c(East_NaturalChange_Mean,West_NaturalChange_Mean)
Median <- c(East_NaturalChange_Median,West_NaturalChange_Median)
Standard_Deviation <- c(East_NaturalChange_StandardDeviation,West_NaturalChange_StandardDeviation)
Variance <- c(East_NaturalChange_Variance,West_NaturalChange_Variance)
IQR <- c(East_NaturalChange_IQR,West_NaturalChange_IQR)
Skewness <- c(East_NaturalChange_Skewness,West_NaturalChange_Skewness)
Kurtosis <- c(East_NaturalChange_Kurtosis,West_NaturalChange_Kurtosis)

NaturalChange_Details <- data.frame(Parameters,Minimum,Maximum,Range,Sum,Mean,Median,Standard_Deviation,Variance,IQR,Skewness,Kurtosis)
view(NaturalChange_Details)



######### Q5 #########

County_MigrationRate_raw <-  import("Data/Module 1 Project Data.xlsx") %>%
    as.tibble() %>%
    select("County","International Migration Rate")
County_MigrationRate_raw <- as.data.frame(County_MigrationRate_raw)
County_MigrationRate_raw

County_MigrationRate <- County_MigrationRate_raw %>%
    group_by(County) %>%
    summarize(frequency = sum(`International Migration Rate`))
County_MigrationRate

p <- ggplot(County_MigrationRate, aes(x=0, y=County_MigrationRate$frequency))  +
    ylab("Migration Rate Frequency") +
    geom_boxplot() +
    ggtitle("County International Migration Rate Boxplot")

p <- ggplotly(p)
p



######### Q6 #########

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
    ggtitle("International vs Domestic States MigrationRate") +
    xlab("International Migration Rate") + ylab("Domestic Migration Rate") +
    geom_smooth(method=lm)



######### Q7 #########

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
States_details

ggplot(data=States_details, aes(x=States_details$Abbreviation, y=States_details$Density, group=1)) +
    geom_line(color="#236B8E",size = 1.5)+
    ggtitle("Population Density per Sq. KM") +
    xlab("States") + ylab("Population Density") +
    geom_point() +
    geom_text(label=round(States_details$Density,2), 
              hjust=0.3, vjust = -1,
              check_overlap = TRUE)

######### END #########