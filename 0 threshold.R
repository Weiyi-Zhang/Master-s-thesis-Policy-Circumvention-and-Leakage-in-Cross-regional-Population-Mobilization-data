#--------------------
#Objective: 毕业论文数据处理与分析: threshold
#Updated: 8th March, 2023
#Email: weiyizhang1012@qq.com
#Content Credits: Weiyi Zhang
#--------------------

##--Housekeeping--
rm(list = ls())
##----------------

##--Packages-- 
library(tidyverse)
library(readxl)
library(openxlsx)
library(officer)
library(plyr)
library(matrixStats)
##------------

##--Reading the raw data--
setwd("E:/zwy-Master-1/毕业论文/数据")
raw <- read.csv("传染病背景下人员跨区流动调查_数据详情表_原始数据_202303081727.csv")
##------------------------

##--Data Mask--
raw <- select(raw, -c(1:2, 49:56))
##------------------------

##--Summarise--
valid <- raw %>%
  select(-c(1:2,20:22,30))
valid[nrow(valid)+1,] <- colMeans(valid, na.rm = T)
##------------------------

##--Outputting--
result <- valid[nrow(valid),]
write.csv(result, "Results of survey for threshold.csv")
write.csv(raw, "传染病背景下人员跨区流动调查_脱敏数据.csv")
##------------------------

##--Housekeeping--
rm(list = ls())
##----------------