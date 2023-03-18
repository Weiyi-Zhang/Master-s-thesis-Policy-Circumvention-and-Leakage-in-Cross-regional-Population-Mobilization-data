#--------------------
#Objective: ��ҵ�������ݴ��������: threshold
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
setwd("E:/zwy-Master-1/��ҵ����/����")
raw <- read.csv("��Ⱦ����������Ա������������_���������_ԭʼ����_202303081727.csv")
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
write.csv(raw, "��Ⱦ����������Ա������������_��������.csv")
##------------------------

##--Housekeeping--
rm(list = ls())
##----------------