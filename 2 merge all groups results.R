#--------------------
#Objective: 毕业论文数据处理与分析-合并所有组合方案的结果
#Updated: 18th March, 2023
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
library(ggpubr) # 用于排布多张图在同一画布上
##------------

##--Reading the raw data--
setwd("E:/zwy-Master-1/毕业论文/数据")
##------------------------

##--Merging--
R <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
M <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
R_integral <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
M_integral <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
tt <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
tt_integral <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
ftt_integral <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
gut_integral <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
TM <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
TM_integral <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))
rc <- data.frame(matrix(NA, nrow = 70, ncol = 4*6))

a <- c("departure and no cooperation", "departure and united policy", "departure and data sharing", "arrival and no cooperation", "arrival and united policy", "arrival and data sharing")
t <- c("3days", "7days", "14days", "21days")

for (i in 1:6) {
  raw_R <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "R")
  raw_R <- select(raw_R, -"tick")
  R[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_R
  b <- paste(a[i], "R", t, sep = "_")
  colnames(R)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- b
  
  raw_M <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "M")
  raw_M <- select(raw_M, -"tick")
  M[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_M
  c <- paste(a[i], "M", t, sep = "_")
  colnames(M)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- c
  
  raw_R_integral <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "R_integral")
  raw_R_integral <- select(raw_R_integral, -"tick")
  R_integral[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_R_integral
  d <- paste(a[i], "R_integral", t, sep = "_")
  colnames(R_integral)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- d
  
  raw_M_integral <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "M_integral")
  raw_M_integral <- select(raw_M_integral, -"tick")
  M_integral[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_M_integral
  e <- paste(a[i], "M_integral", t, sep = "_")
  colnames(M_integral)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- e
  
  raw_tt <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "transit_traveler")
  raw_tt <- select(raw_tt, -"tick")
  tt[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_tt
  f <- paste(a[i], "tt", t, sep = "_")
  colnames(tt)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- f
  
  raw_tt_integral <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "transit_traveler_integral")
  raw_tt_integral <- select(raw_tt_integral, -"tick")
  tt_integral[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_tt_integral
  g <- paste(a[i], "tt_integral", t, sep = "_")
  colnames(tt_integral)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- g
  
  raw_ftt_integral <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "failed_transit_integral")
  raw_ftt_integral <- select(raw_ftt_integral, -"tick")
  ftt_integral[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_ftt_integral
  h <- paste(a[i], "ftt_integral", t, sep = "_")
  colnames(ftt_integral)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- h
  
  raw_gut_integral <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "give_up_traveler_integral")
  raw_gut_integral <- select(raw_gut_integral, -"tick")
  gut_integral[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_gut_integral
  j <- paste(a[i], "gut_integral", t, sep = "_")
  colnames(gut_integral)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- j
  
  raw_TM <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "TM")
  raw_TM <- select(raw_TM, -"tick")
  TM[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_TM
  k <- paste(a[i], "TM", t, sep = "_")
  colnames(TM)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- k
  
  raw_TM_integral <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "TM_integral")
  raw_TM_integral <- select(raw_TM_integral, -"tick")
  TM_integral[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_TM_integral
  l <- paste(a[i], "TM_integral", t, sep = "_")
  colnames(TM_integral)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- l
  
  raw_rc <- read_excel(paste(a[i], "data.xlsx", sep = "-"), sheet = "risk_city_integral")
  raw_rc <- select(raw_rc, -"tick")
  rc[,c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- raw_rc
  m <- paste(a[i], "rc", t, sep = "_")
  colnames(rc)[c((i-1)*4+1, (i-1)*4+2, (i-1)*4+3, i*4)] <- m
}

rm(a, t, b, c, d, e, f, g, h, j, k, l, m)
rm(raw_R, raw_M, raw_R_integral, raw_M_integral, raw_tt, raw_tt_integral, raw_ftt_integral, raw_gut_integral, raw_TM, raw_TM_integral, raw_rc)

R$tick <- 1:nrow(R)
M$tick <- 1:nrow(M)
R_integral$tick <- 1:nrow(R_integral)
M_integral$tick <- 1:nrow(M_integral)
tt$tick <- 1:nrow(tt)
tt_integral$tick <- 1:nrow(tt_integral)
ftt_integral$tick <- 1:nrow(ftt_integral)
gut_integral$tick <- 1:nrow(gut_integral)
TM$tick <- 1:nrow(TM)
TM_integral$tick <- 1:nrow(TM_integral)
rc$tick <- 1:nrow(rc)
##-----------------------

##--Outputting--
l <- list("R" = R, "M" = M, "R_integral" = R_integral, "M_integral" = M_integral, "transit_traveler" = tt, "transit_traveler_integral" = tt_integral, "failed_transit_integral" = ftt_integral, "give_up_traveler_integral" = gut_integral, "TM" = TM, "TM_integral" = TM_integral, "risk_city_integral" = rc)
write.xlsx(l, "Results of simulation.xlsx")
##-----------------------

##--Housekeeping--
rm(list = ls())
##----------------