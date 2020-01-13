########### Q3 ##############

install.packages("e1071")
install.packages("ggpubr")
install.packages("expss")
library("readxl")
library("pacman")
library("dplyr")
library("ggplot2")
library("ggpubr")
library(e1071)
library(expss)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

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
NaturalChange_Details
