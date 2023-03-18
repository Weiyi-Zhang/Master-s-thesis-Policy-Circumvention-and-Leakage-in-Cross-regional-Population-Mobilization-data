#--------------------
#Objective: 毕业论文数据处理与分析-departure and united policy
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
raw_dco <- read.csv("leakage-departure and cooperation one experiment-departure and cooperation one-spreadsheet.csv")
##------------------------

##--Formatting--
# time_option: 3, 7, 14, 21 4*350=1400
# dco
combinations <- raw_dco[c(2,3),]
combinations <- combinations[,which(combinations[1,] != "")]
combinations <- combinations %>%
  select(seq(11,351,10))
for (i in 1:ncol(combinations)) {
  combinations[3,i] <- paste("(",combinations[1,i],",",combinations[2,i],")",sep = "")
}

dco <- raw_dco[c(11:nrow(raw_dco)),]
rownames(dco) <- 1:nrow(dco)
dco <- select(dco, -1)
dco[1,] <- gsub(pattern = "compute-", replacement = "", dco[1,])
dco_3days <- select(dco, 1:3150)
dco_7days <- select(dco, 3151:6300)
dco_14days <- select(dco, 6301:9450)
dco_21days <- select(dco, 9451:12600)
for (i in 1:ncol(combinations)) {
  for (j in 1:10) {
    for (w in 1:9) {
      colnames(dco_3days)[(i-1)*90+(j-1)*9+w] <- paste(dco_3days[1,(i-1)*90+(j-1)*9+w], j, combinations[3,i], sep = "_")
      colnames(dco_7days)[(i-1)*90+(j-1)*9+w] <- paste(dco_7days[1,(i-1)*90+(j-1)*9+w], j, combinations[3,i], sep = "_")
      colnames(dco_14days)[(i-1)*90+(j-1)*9+w] <- paste(dco_14days[1,(i-1)*90+(j-1)*9+w], j, combinations[3,i], sep = "_")
      colnames(dco_21days)[(i-1)*90+(j-1)*9+w] <- paste(dco_21days[1,(i-1)*90+(j-1)*9+w], j, combinations[3,i], sep = "_")
    }
  }
}
dco_3days <- dco_3days[-c(1,2),]
dco_7days <- dco_7days[-c(1,2),]
dco_14days <- dco_14days[-c(1,2),]
dco_21days <- dco_21days[-c(1,2),]
rownames(dco_3days) <- 1:nrow(dco_3days)
rownames(dco_7days) <- 1:nrow(dco_7days)
rownames(dco_14days) <- 1:nrow(dco_14days)
rownames(dco_21days) <- 1:nrow(dco_21days)
for (i in 1:ncol(dco_3days)) {
  dco_3days[,i] <- as.numeric(dco_3days[,i])
  dco_7days[,i] <- as.numeric(dco_7days[,i])
  dco_14days[,i] <- as.numeric(dco_14days[,i])
  dco_21days[,i] <- as.numeric(dco_21days[,i])
}
##------------------------

##--Variance(R&M)--
# dco: R, M
# 3days
dco_3days_for_R <- dco_3days %>%
  select(seq(1, 3142, by = 9))
dco_3days_for_M <- dco_3days %>%
  select(seq(2, 3143, by = 9))

# create blank frames to store variances of variables
variance_3days_R <- data.frame(matrix(NA, nrow = nrow(dco_3days_for_R), ncol = 35))
variance_3days_M <- data.frame(matrix(NA, nrow = nrow(dco_3days_for_M), ncol = 35))
# computing variances of repeated groups and rename the columns
for (i in 1:35) {
  # R: Risk
  testgroup_R <- dco_3days_for_R %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  testgroup_R <- as.matrix(testgroup_R)
  variance_3days_R[,i] <- rowSds(testgroup_R)
  colnames(variance_3days_R)[i] <- paste("Rsd", combinations[3,i], sep = "_")
  # M: Mobility
  testgroup_M <- dco_3days_for_M %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  testgroup_M <- as.matrix(testgroup_M)
  variance_3days_M[,i] <- rowSds(testgroup_M)
  colnames(variance_3days_M)[i] <- paste("Msd", combinations[3,i], sep = "_")
}
rm(testgroup_R, testgroup_M)

# 7days
dco_7days_for_R <- dco_7days %>%
  select(seq(1, 3142, by = 9))
dco_7days_for_M <- dco_7days %>%
  select(seq(2, 3143, by = 9))

# create blank frames to store variances of variables
variance_7days_R <- data.frame(matrix(NA, nrow = nrow(dco_7days_for_R), ncol = 35))
variance_7days_M <- data.frame(matrix(NA, nrow = nrow(dco_7days_for_M), ncol = 35))
# computing variances of repeated groups and rename the columns
for (i in 1:35) {
  # R: Risk
  testgroup_R <- dco_7days_for_R %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  testgroup_R <- as.matrix(testgroup_R)
  variance_7days_R[,i] <- rowSds(testgroup_R)
  colnames(variance_7days_R)[i] <- paste("Rsd", combinations[3,i], sep = "_")
  # M: Mobility
  testgroup_M <- dco_7days_for_M %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  testgroup_M <- as.matrix(testgroup_M)
  variance_7days_M[,i] <- rowSds(testgroup_M)
  colnames(variance_7days_M)[i] <- paste("Msd", combinations[3,i], sep = "_")
}
rm(testgroup_R, testgroup_M)

# 14days
dco_14days_for_R <- dco_14days %>%
  select(seq(1, 3142, by = 9))
dco_14days_for_M <- dco_14days %>%
  select(seq(2, 3143, by = 9))

# create blank frames to store variances of variables
variance_14days_R <- data.frame(matrix(NA, nrow = nrow(dco_14days_for_R), ncol = 35))
variance_14days_M <- data.frame(matrix(NA, nrow = nrow(dco_14days_for_M), ncol = 35))
# computing variances of repeated groups and rename the columns
for (i in 1:35) {
  # R: Risk
  testgroup_R <- dco_14days_for_R %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  testgroup_R <- as.matrix(testgroup_R)
  variance_14days_R[,i] <- rowSds(testgroup_R)
  colnames(variance_14days_R)[i] <- paste("Rsd", combinations[3,i], sep = "_")
  # M: Mobility
  testgroup_M <- dco_14days_for_M %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  testgroup_M <- as.matrix(testgroup_M)
  variance_14days_M[,i] <- rowSds(testgroup_M)
  colnames(variance_14days_M)[i] <- paste("Msd", combinations[3,i], sep = "_")
}
rm(testgroup_R, testgroup_M)

# 21days
dco_21days_for_R <- dco_21days %>%
  select(seq(1, 3142, by = 9))
dco_21days_for_M <- dco_21days %>%
  select(seq(2, 3143, by = 9))

# create blank frames to store variances of variables
variance_21days_R <- data.frame(matrix(NA, nrow = nrow(dco_21days_for_R), ncol = 35))
variance_21days_M <- data.frame(matrix(NA, nrow = nrow(dco_21days_for_M), ncol = 35))
# computing variances of repeated groups and rename the columns
for (i in 1:35) {
  # R: Risk
  testgroup_R <- dco_21days_for_R %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  testgroup_R <- as.matrix(testgroup_R)
  variance_21days_R[,i] <- rowSds(testgroup_R)
  colnames(variance_21days_R)[i] <- paste("Rsd", combinations[3,i], sep = "_")
  # M: Mobility
  testgroup_M <- dco_21days_for_M %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  testgroup_M <- as.matrix(testgroup_M)
  variance_21days_M[,i] <- rowSds(testgroup_M)
  colnames(variance_21days_M)[i] <- paste("Msd", combinations[3,i], sep = "_")
}
rm(testgroup_R, testgroup_M)
##------------------------

##--visualization of variance(R&M)--
# dco: R, M
# 3days
variance_3days_R$tick <- 1:nrow(variance_3days_R)
plot_variance_R <- variance_3days_R %>%
  gather(key = "var", value = "value", 1:35)
plot_variance_3days_R <- plot_variance_R %>%
  ggplot(aes(x = tick, y = value, colour = var)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "R标准差",
    colour = "(initial-outbreak-size,number-agents)",
    title = "3天管制期"
  )

variance_3days_M$tick <- 1:nrow(variance_3days_M)
plot_variance_M <- variance_3days_M %>%
  gather(key = "var", value = "value", 1:35)
plot_variance_3days_M <- plot_variance_M %>%
  ggplot(aes(x = tick, y = value, colour = var)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "M标准差",
    colour = "(initial-outbreak-size,number-agents)",
    title = "3天管制期"
  )
rm(plot_variance_R, plot_variance_M)

# 7days
variance_7days_R$tick <- 1:nrow(variance_7days_R)
plot_variance_R <- variance_7days_R %>%
  gather(key = "var", value = "value", 1:35)
plot_variance_7days_R <- plot_variance_R %>%
  ggplot(aes(x = tick, y = value, colour = var)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "R标准差",
    colour = "(initial-outbreak-size,number-agents)",
    title = "7天管制期"
  )

variance_7days_M$tick <- 1:nrow(variance_7days_M)
plot_variance_M <- variance_7days_M %>%
  gather(key = "var", value = "value", 1:35)
plot_variance_7days_M <- plot_variance_M %>%
  ggplot(aes(x = tick, y = value, colour = var)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "M标准差",
    colour = "(initial-outbreak-size,number-agents)",
    title = "7天管制期"
  )
rm(plot_variance_R, plot_variance_M)

# 14days
variance_14days_R$tick <- 1:nrow(variance_14days_R)
plot_variance_R <- variance_14days_R %>%
  gather(key = "var", value = "value", 1:35)
plot_variance_14days_R <- plot_variance_R %>%
  ggplot(aes(x = tick, y = value, colour = var)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "R标准差",
    colour = "(initial-outbreak-size,number-agents)",
    title = "14天管制期"
  )

variance_14days_M$tick <- 1:nrow(variance_14days_M)
plot_variance_M <- variance_14days_M %>%
  gather(key = "var", value = "value", 1:35)
plot_variance_14days_M <- plot_variance_M %>%
  ggplot(aes(x = tick, y = value, colour = var)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "M标准差",
    colour = "(initial-outbreak-size,number-agents)",
    title = "14天管制期"
  )
rm(plot_variance_R, plot_variance_M)

# 21days
variance_21days_R$tick <- 1:nrow(variance_21days_R)
plot_variance_R <- variance_21days_R %>%
  gather(key = "var", value = "value", 1:35)
plot_variance_21days_R <- plot_variance_R %>%
  ggplot(aes(x = tick, y = value, colour = var)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "R标准差",
    colour = "(initial-outbreak-size,number-agents)",
    title = "21天管制期"
  )

variance_21days_M$tick <- 1:nrow(variance_21days_M)
plot_variance_M <- variance_21days_M %>%
  gather(key = "var", value = "value", 1:35)
plot_variance_21days_M <- plot_variance_M %>%
  ggplot(aes(x = tick, y = value, colour = var)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "M标准差",
    colour = "(initial-outbreak-size,number-agents)",
    title = "21天管制期"
  )
rm(plot_variance_R, plot_variance_M)

# 利用ggarrange将图像排列在一起
plot_variance_dco <- ggarrange(plot_variance_3days_R, plot_variance_7days_R, plot_variance_14days_R, plot_variance_21days_R,
                               plot_variance_3days_M, plot_variance_7days_M, plot_variance_14days_M, plot_variance_21days_M,
                               ncol = 4,nrow =2)
plot_variance_dco # 手动export大小合适的图

rm(variance_3days_R, variance_3days_M, variance_7days_R, variance_7days_M, variance_14days_R, variance_14days_M, variance_21days_R, variance_21days_M)
rm(plot_variance_3days_R, plot_variance_3days_M, plot_variance_7days_R, plot_variance_7days_M, plot_variance_14days_R, plot_variance_14days_M, plot_variance_21days_R, plot_variance_21days_M)
##------------------------

##--Discrete integral--
# dco: R, M, tt(transit_traveler), ftt(failed_transit_traveler), gut(give_up_traveler), TM, rc(risk city)
#tt(flow), ftt(stock), gut(stock), rc(stock)

# 3days
# create blank frames to store means of repeated simulations
means_integrals_3days_R <- data.frame(matrix(NA, nrow = nrow(dco_3days_for_R), ncol = 70))
means_integrals_3days_M <- data.frame(matrix(NA, nrow = nrow(dco_3days_for_M), ncol = 70))
means_integrals_3days_tt <- data.frame(matrix(NA, nrow = nrow(dco_3days), ncol = 70))
means_3days_ftt <- data.frame(matrix(NA, nrow = nrow(dco_3days), ncol = 35))
means_3days_gut <- data.frame(matrix(NA, nrow = nrow(dco_3days), ncol = 35))
means_integrals_3days_TM <- data.frame(matrix(NA, nrow = nrow(dco_3days), ncol = 70))
means_3days_rc <- data.frame(matrix(NA, nrow = nrow(dco_3days), ncol = 35))
# computing means of repeated groups and rename the columns
dco_3days_for_tt <- dco_3days %>%
  select(seq(6, 3147, by = 9))
dco_3days_for_ftt <- dco_3days %>%
  select(seq(7, 3148, by = 9))
dco_3days_for_gut <- dco_3days %>%
  select(seq(8, 3149, by = 9))
dco_3days_for_TM <- dco_3days %>%
  select(seq(3, 3144, by = 9))
dco_3days_for_rc <- dco_3days %>%
  select(seq(9, 3150, by = 9))

for (i in 1:35) {
  # R: Risk
  testgroup_R <- dco_3days_for_R %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_3days_R[,i*2-1] <- rowMeans(testgroup_R)
  means_integrals_3days_R[,i*2] <- cumsum(means_integrals_3days_R[,i*2-1])
  colnames(means_integrals_3days_R)[i*2-1] <- paste("Rmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_3days_R)[i*2] <- paste("Rintegral", combinations[3,i], sep = "_")
  # M: Mobility
  testgroup_M <- dco_3days_for_M %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_3days_M[,i*2-1] <- rowMeans(testgroup_M)
  means_integrals_3days_M[,i*2] <- cumsum(means_integrals_3days_M[,i*2-1])
  colnames(means_integrals_3days_M)[i*2-1] <- paste("Mmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_3days_M)[i*2] <- paste("Mintegral", combinations[3,i], sep = "_")
  # tt: transit_traveler
  testgroup_tt <- dco_3days_for_tt %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_3days_tt[,i*2-1] <- rowMeans(testgroup_tt)
  means_integrals_3days_tt[,i*2] <- cumsum(means_integrals_3days_tt[,i*2-1])
  colnames(means_integrals_3days_tt)[i*2-1] <- paste("ttmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_3days_tt)[i*2] <- paste("ttintegral", combinations[3,i], sep = "_")
  # ftt: failed_transit_traveler
  testgroup_ftt <- dco_3days_for_ftt %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_3days_ftt[,i] <- rowMeans(testgroup_ftt)
  colnames(means_3days_ftt)[i] <- paste("fttintegral", combinations[3,i], sep = "_")
  # gut: give_up_traveler
  testgroup_gut <- dco_3days_for_gut %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_3days_gut[,i] <- rowMeans(testgroup_gut)
  colnames(means_3days_gut)[i] <- paste("gutintegral", combinations[3,i], sep = "_")
  # TM: Transit_Mobility
  testgroup_TM <- dco_3days_for_TM %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_3days_TM[,i*2-1] <- rowMeans(testgroup_TM)
  means_integrals_3days_TM[,i*2] <- cumsum(means_integrals_3days_TM[,i*2-1])
  colnames(means_integrals_3days_TM)[i*2-1] <- paste("TMmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_3days_TM)[i*2] <- paste("TMintegral", combinations[3,i], sep = "_")
  # rc: risk_city
  testgroup_rc <- dco_3days_for_rc %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_3days_rc[,i] <- rowMeans(testgroup_rc)
  colnames(means_3days_rc)[i] <- paste("rcintegral", combinations[3,i], sep = "_")
}
rm(testgroup_R, testgroup_M, testgroup_tt, testgroup_ftt, testgroup_gut, testgroup_TM, testgroup_rc)
rm(dco_3days_for_R, dco_3days_for_M, dco_3days_for_tt, dco_3days_for_ftt, dco_3days_for_gut, dco_3days_for_TM, dco_3days_for_rc)

# 7days
# create blank frames to store means of repeated simulations
means_integrals_7days_R <- data.frame(matrix(NA, nrow = nrow(dco_7days_for_R), ncol = 70))
means_integrals_7days_M <- data.frame(matrix(NA, nrow = nrow(dco_7days_for_M), ncol = 70))
means_integrals_7days_tt <- data.frame(matrix(NA, nrow = nrow(dco_7days), ncol = 70))
means_7days_ftt <- data.frame(matrix(NA, nrow = nrow(dco_7days), ncol = 35))
means_7days_gut <- data.frame(matrix(NA, nrow = nrow(dco_7days), ncol = 35))
means_integrals_7days_TM <- data.frame(matrix(NA, nrow = nrow(dco_7days), ncol = 70))
means_7days_rc <- data.frame(matrix(NA, nrow = nrow(dco_7days), ncol = 35))
# computing means of repeated groups and rename the columns
dco_7days_for_tt <- dco_7days %>%
  select(seq(6, 3147, by = 9))
dco_7days_for_ftt <- dco_7days %>%
  select(seq(7, 3148, by = 9))
dco_7days_for_gut <- dco_7days %>%
  select(seq(8, 3149, by = 9))
dco_7days_for_TM <- dco_7days %>%
  select(seq(3, 3144, by = 9))
dco_7days_for_rc <- dco_7days %>%
  select(seq(9, 3150, by = 9))

for (i in 1:35) {
  # R: Risk
  testgroup_R <- dco_7days_for_R %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_7days_R[,i*2-1] <- rowMeans(testgroup_R)
  means_integrals_7days_R[,i*2] <- cumsum(means_integrals_7days_R[,i*2-1])
  colnames(means_integrals_7days_R)[i*2-1] <- paste("Rmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_7days_R)[i*2] <- paste("Rintegral", combinations[3,i], sep = "_")
  # M: Mobility
  testgroup_M <- dco_7days_for_M %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_7days_M[,i*2-1] <- rowMeans(testgroup_M)
  means_integrals_7days_M[,i*2] <- cumsum(means_integrals_7days_M[,i*2-1])
  colnames(means_integrals_7days_M)[i*2-1] <- paste("Mmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_7days_M)[i*2] <- paste("Mintegral", combinations[3,i], sep = "_")
  # tt: transit_traveler
  testgroup_tt <- dco_7days_for_tt %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_7days_tt[,i*2-1] <- rowMeans(testgroup_tt)
  means_integrals_7days_tt[,i*2] <- cumsum(means_integrals_7days_tt[,i*2-1])
  colnames(means_integrals_7days_tt)[i*2-1] <- paste("ttmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_7days_tt)[i*2] <- paste("ttintegral", combinations[3,i], sep = "_")
  # ftt: failed_transit_traveler
  testgroup_ftt <- dco_7days_for_ftt %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_7days_ftt[,i] <- rowMeans(testgroup_ftt)
  colnames(means_7days_ftt)[i] <- paste("fttintegral", combinations[3,i], sep = "_")
  # gut: give_up_traveler
  testgroup_gut <- dco_7days_for_gut %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_7days_gut[,i] <- rowMeans(testgroup_gut)
  colnames(means_7days_gut)[i] <- paste("gutintegral", combinations[3,i], sep = "_")
  # TM: Transit_Mobility
  testgroup_TM <- dco_7days_for_TM %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_7days_TM[,i*2-1] <- rowMeans(testgroup_TM)
  means_integrals_7days_TM[,i*2] <- cumsum(means_integrals_7days_TM[,i*2-1])
  colnames(means_integrals_7days_TM)[i*2-1] <- paste("TMmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_7days_TM)[i*2] <- paste("TMintegral", combinations[3,i], sep = "_")
  # rc: risk_city
  testgroup_rc <- dco_7days_for_rc %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_7days_rc[,i] <- rowMeans(testgroup_rc)
  colnames(means_7days_rc)[i] <- paste("rcintegral", combinations[3,i], sep = "_")
}
rm(testgroup_R, testgroup_M, testgroup_tt, testgroup_ftt, testgroup_gut, testgroup_TM, testgroup_rc)
rm(dco_7days_for_R, dco_7days_for_M, dco_7days_for_tt, dco_7days_for_ftt, dco_7days_for_gut, dco_7days_for_TM, dco_7days_for_rc)

# 14days
# create blank frames to store means of repeated simulations
means_integrals_14days_R <- data.frame(matrix(NA, nrow = nrow(dco_14days_for_R), ncol = 70))
means_integrals_14days_M <- data.frame(matrix(NA, nrow = nrow(dco_14days_for_M), ncol = 70))
means_integrals_14days_tt <- data.frame(matrix(NA, nrow = nrow(dco_14days), ncol = 70))
means_14days_ftt <- data.frame(matrix(NA, nrow = nrow(dco_14days), ncol = 35))
means_14days_gut <- data.frame(matrix(NA, nrow = nrow(dco_14days), ncol = 35))
means_integrals_14days_TM <- data.frame(matrix(NA, nrow = nrow(dco_14days), ncol = 70))
means_14days_rc <- data.frame(matrix(NA, nrow = nrow(dco_14days), ncol = 35))
# computing means of repeated groups and rename the columns
dco_14days_for_tt <- dco_14days %>%
  select(seq(6, 3147, by = 9))
dco_14days_for_ftt <- dco_14days %>%
  select(seq(7, 3148, by = 9))
dco_14days_for_gut <- dco_14days %>%
  select(seq(8, 3149, by = 9))
dco_14days_for_TM <- dco_14days %>%
  select(seq(3, 3144, by = 9))
dco_14days_for_rc <- dco_14days %>%
  select(seq(9, 3150, by = 9))

for (i in 1:35) {
  # R: Risk
  testgroup_R <- dco_14days_for_R %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_14days_R[,i*2-1] <- rowMeans(testgroup_R)
  means_integrals_14days_R[,i*2] <- cumsum(means_integrals_14days_R[,i*2-1])
  colnames(means_integrals_14days_R)[i*2-1] <- paste("Rmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_14days_R)[i*2] <- paste("Rintegral", combinations[3,i], sep = "_")
  # M: Mobility
  testgroup_M <- dco_14days_for_M %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_14days_M[,i*2-1] <- rowMeans(testgroup_M)
  means_integrals_14days_M[,i*2] <- cumsum(means_integrals_14days_M[,i*2-1])
  colnames(means_integrals_14days_M)[i*2-1] <- paste("Mmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_14days_M)[i*2] <- paste("Mintegral", combinations[3,i], sep = "_")
  # tt: transit_traveler
  testgroup_tt <- dco_14days_for_tt %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_14days_tt[,i*2-1] <- rowMeans(testgroup_tt)
  means_integrals_14days_tt[,i*2] <- cumsum(means_integrals_14days_tt[,i*2-1])
  colnames(means_integrals_14days_tt)[i*2-1] <- paste("ttmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_14days_tt)[i*2] <- paste("ttintegral", combinations[3,i], sep = "_")
  # ftt: failed_transit_traveler
  testgroup_ftt <- dco_14days_for_ftt %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_14days_ftt[,i] <- rowMeans(testgroup_ftt)
  colnames(means_14days_ftt)[i] <- paste("fttintegral", combinations[3,i], sep = "_")
  # gut: give_up_traveler
  testgroup_gut <- dco_14days_for_gut %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_14days_gut[,i] <- rowMeans(testgroup_gut)
  colnames(means_14days_gut)[i] <- paste("gutintegral", combinations[3,i], sep = "_")
  # TM: Transit_Mobility
  testgroup_TM <- dco_14days_for_TM %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_14days_TM[,i*2-1] <- rowMeans(testgroup_TM)
  means_integrals_14days_TM[,i*2] <- cumsum(means_integrals_14days_TM[,i*2-1])
  colnames(means_integrals_14days_TM)[i*2-1] <- paste("TMmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_14days_TM)[i*2] <- paste("TMintegral", combinations[3,i], sep = "_")
  # rc: risk_city
  testgroup_rc <- dco_14days_for_rc %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_14days_rc[,i] <- rowMeans(testgroup_rc)
  colnames(means_14days_rc)[i] <- paste("rcintegral", combinations[3,i], sep = "_")
}
rm(testgroup_R, testgroup_M, testgroup_tt, testgroup_ftt, testgroup_gut, testgroup_TM, testgroup_rc)
rm(dco_14days_for_R, dco_14days_for_M, dco_14days_for_tt, dco_14days_for_ftt, dco_14days_for_gut, dco_14days_for_TM, dco_14days_for_rc)

# 21days
# create blank frames to store means of repeated simulations
means_integrals_21days_R <- data.frame(matrix(NA, nrow = nrow(dco_21days_for_R), ncol = 70))
means_integrals_21days_M <- data.frame(matrix(NA, nrow = nrow(dco_21days_for_M), ncol = 70))
means_integrals_21days_tt <- data.frame(matrix(NA, nrow = nrow(dco_21days), ncol = 70))
means_21days_ftt <- data.frame(matrix(NA, nrow = nrow(dco_21days), ncol = 35))
means_21days_gut <- data.frame(matrix(NA, nrow = nrow(dco_21days), ncol = 35))
means_integrals_21days_TM <- data.frame(matrix(NA, nrow = nrow(dco_21days), ncol = 70))
means_21days_rc <- data.frame(matrix(NA, nrow = nrow(dco_21days), ncol = 35))
# computing means of repeated groups and rename the columns
dco_21days_for_tt <- dco_21days %>%
  select(seq(6, 3147, by = 9))
dco_21days_for_ftt <- dco_21days %>%
  select(seq(7, 3148, by = 9))
dco_21days_for_gut <- dco_21days %>%
  select(seq(8, 3149, by = 9))
dco_21days_for_TM <- dco_21days %>%
  select(seq(3, 3144, by = 9))
dco_21days_for_rc <- dco_21days %>%
  select(seq(9, 3150, by = 9))

for (i in 1:35) {
  # R: Risk
  testgroup_R <- dco_21days_for_R %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_21days_R[,i*2-1] <- rowMeans(testgroup_R)
  means_integrals_21days_R[,i*2] <- cumsum(means_integrals_21days_R[,i*2-1])
  colnames(means_integrals_21days_R)[i*2-1] <- paste("Rmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_21days_R)[i*2] <- paste("Rintegral", combinations[3,i], sep = "_")
  # M: Mobility
  testgroup_M <- dco_21days_for_M %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_21days_M[,i*2-1] <- rowMeans(testgroup_M)
  means_integrals_21days_M[,i*2] <- cumsum(means_integrals_21days_M[,i*2-1])
  colnames(means_integrals_21days_M)[i*2-1] <- paste("Mmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_21days_M)[i*2] <- paste("Mintegral", combinations[3,i], sep = "_")
  # tt: transit_traveler
  testgroup_tt <- dco_21days_for_tt %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_21days_tt[,i*2-1] <- rowMeans(testgroup_tt)
  means_integrals_21days_tt[,i*2] <- cumsum(means_integrals_21days_tt[,i*2-1])
  colnames(means_integrals_21days_tt)[i*2-1] <- paste("ttmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_21days_tt)[i*2] <- paste("ttintegral", combinations[3,i], sep = "_")
  # ftt: failed_transit_traveler
  testgroup_ftt <- dco_21days_for_ftt %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_21days_ftt[,i] <- rowMeans(testgroup_ftt)
  colnames(means_21days_ftt)[i] <- paste("fttintegral", combinations[3,i], sep = "_")
  # gut: give_up_traveler
  testgroup_gut <- dco_21days_for_gut %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_21days_gut[,i] <- rowMeans(testgroup_gut)
  colnames(means_21days_gut)[i] <- paste("gutintegral", combinations[3,i], sep = "_")
  # TM: Transit_Mobility
  testgroup_TM <- dco_21days_for_TM %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_integrals_21days_TM[,i*2-1] <- rowMeans(testgroup_TM)
  means_integrals_21days_TM[,i*2] <- cumsum(means_integrals_21days_TM[,i*2-1])
  colnames(means_integrals_21days_TM)[i*2-1] <- paste("TMmeans", combinations[3,i], sep = "_")
  colnames(means_integrals_21days_TM)[i*2] <- paste("TMintegral", combinations[3,i], sep = "_")
  # rc: risk_city
  testgroup_rc <- dco_21days_for_rc %>%
    select(seq(from = (i-1)*10+1, to = i*10, by = 1))
  means_21days_rc[,i] <- rowMeans(testgroup_rc)
  colnames(means_21days_rc)[i] <- paste("rcintegral", combinations[3,i], sep = "_")
}
rm(testgroup_R, testgroup_M, testgroup_tt, testgroup_ftt, testgroup_gut, testgroup_TM, testgroup_rc)
rm(dco_21days_for_R, dco_21days_for_M, dco_21days_for_tt, dco_21days_for_ftt, dco_21days_for_gut, dco_21days_for_TM, dco_21days_for_rc)
##------------------------

##--Trends of R & M across groups--
# 3days
means_3days_R <- means_integrals_3days_R %>%
  select(seq(1, 69, by = 2)) %>%
  mutate(tick = 1:nrow(means_integrals_3days_R)) %>%
  gather(key = "group", value = "Rmeans", 1:35)
plot_means_3days_R <- means_3days_R %>%
  ggplot(aes(x = tick, y = Rmeans, colour = group)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "R",
    title = "3天管制期"
  )
means_3days_M <- means_integrals_3days_M %>%
  select(seq(1, 69, by = 2)) %>%
  mutate(tick = 1:nrow(means_integrals_3days_M)) %>%
  gather(key = "group", value = "Mmeans", 1:35)
plot_means_3days_M <- means_3days_M %>%
  ggplot(aes(x = tick, y = Mmeans, colour = group)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "M",
    title = "3天管制期"
  )

# 7days
means_7days_R <- means_integrals_7days_R %>%
  select(seq(1, 69, by = 2)) %>%
  mutate(tick = 1:nrow(means_integrals_7days_R)) %>%
  gather(key = "group", value = "Rmeans", 1:35)
plot_means_7days_R <- means_7days_R %>%
  ggplot(aes(x = tick, y = Rmeans, colour = group)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "R",
    title = "7天管制期"
  )
means_7days_M <- means_integrals_7days_M %>%
  select(seq(1, 69, by = 2)) %>%
  mutate(tick = 1:nrow(means_integrals_7days_M)) %>%
  gather(key = "group", value = "Mmeans", 1:35)
plot_means_7days_M <- means_7days_M %>%
  ggplot(aes(x = tick, y = Mmeans, colour = group)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "M",
    title = "7天管制期"
  )

# 14days
means_14days_R <- means_integrals_14days_R %>%
  select(seq(1, 69, by = 2)) %>%
  mutate(tick = 1:nrow(means_integrals_14days_R)) %>%
  gather(key = "group", value = "Rmeans", 1:35)
plot_means_14days_R <- means_14days_R %>%
  ggplot(aes(x = tick, y = Rmeans, colour = group)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "R",
    title = "14天管制期"
  )
means_14days_M <- means_integrals_14days_M %>%
  select(seq(1, 69, by = 2)) %>%
  mutate(tick = 1:nrow(means_integrals_14days_M)) %>%
  gather(key = "group", value = "Mmeans", 1:35)
plot_means_14days_M <- means_14days_M %>%
  ggplot(aes(x = tick, y = Mmeans, colour = group)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "M",
    title = "14天管制期"
  )

# 21days
means_21days_R <- means_integrals_21days_R %>%
  select(seq(1, 69, by = 2)) %>%
  mutate(tick = 1:nrow(means_integrals_21days_R)) %>%
  gather(key = "group", value = "Rmeans", 1:35)
plot_means_21days_R <- means_21days_R %>%
  ggplot(aes(x = tick, y = Rmeans, colour = group)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "R",
    title = "21天管制期"
  )
means_21days_M <- means_integrals_21days_M %>%
  select(seq(1, 69, by = 2)) %>%
  mutate(tick = 1:nrow(means_integrals_21days_M)) %>%
  gather(key = "group", value = "Mmeans", 1:35)
plot_means_21days_M <- means_21days_M %>%
  ggplot(aes(x = tick, y = Mmeans, colour = group)) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "时间（天）",
    y = "M",
    title = "21天管制期"
  )

# 利用ggarrange将图像排列在一起
plot_means_trend_across_groups <- ggarrange(plot_means_3days_R, plot_means_7days_R, plot_means_14days_R,
                                            plot_means_21days_R,
                                            plot_means_3days_M, plot_means_7days_M, plot_means_14days_M,
                                            plot_means_21days_M,
                                            ncol = 4,nrow =2)
plot_means_trend_across_groups # 手动export大小合适的图

rm(means_3days_R, means_3days_M, means_7days_R, means_7days_M, means_14days_R, means_14days_M, means_21days_R, means_21days_M)
rm(plot_means_3days_R, plot_means_3days_M, plot_means_7days_R, plot_means_7days_M, plot_means_14days_R, plot_means_14days_M, plot_means_21days_R, plot_means_21days_M)
##------------------------

rm(combinations, raw_dco)

##--Computing means across groups--
## R
r3 <- means_integrals_3days_R %>%
  select(seq(1, 69, by = 2))
r3$Rmeans_3days <- rowMeans(r3)
r7 <- means_integrals_7days_R %>%
  select(seq(1, 69, by = 2))
r7$Rmeans_7days <- rowMeans(r7)
r14 <- means_integrals_14days_R %>%
  select(seq(1, 69, by = 2))
r14$Rmeans_14days <- rowMeans(r14)
r21 <- means_integrals_21days_R %>%
  select(seq(1, 69, by = 2))
r21$Rmeans_21days <- rowMeans(r21)
means_R <- data.frame(r3$Rmeans_3days, r7$Rmeans_7days, r14$Rmeans_14days, r21$Rmeans_21days, tick = 1:nrow(r3))
colnames(means_R) <- c("3天管制期", "7天管制期", "14天管制期", "21天管制期", "tick")

rm(r3, r7, r14, r21)

# add a plot for integral of R to better show the difference
r3 <- means_integrals_3days_R %>%
  select(seq(2, 70, by = 2))
r3$Rintegral_3days <- rowMeans(r3)
r7 <- means_integrals_7days_R %>%
  select(seq(2, 70, by = 2))
r7$Rintegral_7days <- rowMeans(r7)
r14 <- means_integrals_14days_R %>%
  select(seq(2, 70, by = 2))
r14$Rintegral_14days <- rowMeans(r14)
r21 <- means_integrals_21days_R %>%
  select(seq(2, 70, by = 2))
r21$Rintegral_21days <- rowMeans(r21)
integral_R <- data.frame(r3$Rintegral_3days, r7$Rintegral_7days, r14$Rintegral_14days, r21$Rintegral_21days, tick = 1:nrow(r3))
colnames(integral_R) <- c("3天管制期", "7天管制期", "14天管制期", "21天管制期", "tick")

rm(r3, r7, r14, r21)

# visualization
means_R_for_plot <- means_R %>%
  gather(key = "days", value = "R", 1:4)
plot_R <- means_R_for_plot %>%
  ggplot(aes(x = tick, y = R, fill = days)) +
  geom_area(alpha = 0.5, position = "identity") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_fill_brewer(name = "", palette = "Set3", limits = c("3天管制期", "7天管制期", "14天管制期", "21天管制期")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "时间（天）",
    y = "R",
    title = "流量"
  )
rm(means_R_for_plot)

integral_R_for_plot <- integral_R %>%
  gather(key = "days", value = "R", 1:4)
plot_R_integral <- integral_R_for_plot %>%
  ggplot(aes(x = tick, y = R, colour = days)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_colour_brewer(name = "", palette = "Set2", limits = c("3天管制期", "7天管制期", "14天管制期", "21天管制期")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "时间（天）",
    y = "R累计",
    title = "存量"
  )
rm(integral_R_for_plot)

# 利用ggarrange将图像排列在一起
plot_R_means_integral <- ggarrange(plot_R, plot_R_integral,
                                   ncol = 1, nrow = 2)
plot_R_means_integral # 手动export大小合适的图

## M
m3 <- means_integrals_3days_M %>%
  select(seq(1, 69, by = 2))
m3$Mmeans_3days <- rowMeans(m3)
m7 <- means_integrals_7days_M %>%
  select(seq(1, 69, by = 2))
m7$Mmeans_7days <- rowMeans(m7)
m14 <- means_integrals_14days_M %>%
  select(seq(1, 69, by = 2))
m14$Mmeans_14days <- rowMeans(m14)
m21 <- means_integrals_21days_M %>%
  select(seq(1, 69, by = 2))
m21$Mmeans_21days <- rowMeans(m21)
means_M <- data.frame(m3$Mmeans_3days, m7$Mmeans_7days, m14$Mmeans_14days, m21$Mmeans_21days, tick = 1:nrow(m3))
colnames(means_M) <- c("3天管制期", "7天管制期", "14天管制期", "21天管制期", "tick")

rm(m3, m7, m14, m21)

# compute integral of M to be consistent with results of R
m3 <- means_integrals_3days_M %>%
  select(seq(2, 70, by = 2))
m3$Mintegral_3days <- rowMeans(m3)
m7 <- means_integrals_7days_M %>%
  select(seq(2, 70, by = 2))
m7$Mintegral_7days <- rowMeans(m7)
m14 <- means_integrals_14days_M %>%
  select(seq(2, 70, by = 2))
m14$Mintegral_14days <- rowMeans(m14)
m21 <- means_integrals_21days_M %>%
  select(seq(2, 70, by = 2))
m21$Mintegral_21days <- rowMeans(m21)
integral_M <- data.frame(m3$Mintegral_3days, m7$Mintegral_7days, m14$Mintegral_14days, m21$Mintegral_21days, tick = 1:nrow(m3))
colnames(integral_M) <- c("3天管制期", "7天管制期", "14天管制期", "21天管制期", "tick")

rm(m3, m7, m14, m21)

# visualization
means_M_for_plot <- means_M %>%
  gather(key = "days", value = "M", 1:4)
plot_M <- means_M_for_plot %>%
  ggplot(aes(x = tick, y = M, fill = days)) +
  geom_area(alpha = 0.5, position = "identity") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_fill_brewer(name = "", palette = "Set3", limits = c("3天管制期", "7天管制期", "14天管制期", "21天管制期")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "时间（天）",
    y = "M",
    title = "流量"
  )
plot_M # 手动export大小合适的图
rm(means_M_for_plot)

## transit_traveler
tt3 <- means_integrals_3days_tt %>%
  select(seq(1, 69, by = 2))
tt3$ttmeans_3days <- rowMeans(tt3)
tt7 <- means_integrals_7days_tt %>%
  select(seq(1, 69, by = 2))
tt7$ttmeans_7days <- rowMeans(tt7)
tt14 <- means_integrals_14days_tt %>%
  select(seq(1, 69, by = 2))
tt14$ttmeans_14days <- rowMeans(tt14)
tt21 <- means_integrals_21days_tt %>%
  select(seq(1, 69, by = 2))
tt21$ttmeans_21days <- rowMeans(tt21)
means_tt <- data.frame(tt3$ttmeans_3days, tt7$ttmeans_7days, tt14$ttmeans_14days, tt21$ttmeans_21days, tick = 1:nrow(tt3))
colnames(means_tt) <- c("ttmeans_3days", "ttmeans_7days", "ttmeans_14days", "ttmeans_21days", "tick")

rm(tt3, tt7, tt14, tt21)

# compute integral of tt to be consistent with results of ftt, gut
tt3 <- means_integrals_3days_tt %>%
  select(seq(2, 70, by = 2))
tt3$ttintegral_3days <- rowMeans(tt3)
tt7 <- means_integrals_7days_tt %>%
  select(seq(2, 70, by = 2))
tt7$ttintegral_7days <- rowMeans(tt7)
tt14 <- means_integrals_14days_tt %>%
  select(seq(2, 70, by = 2))
tt14$ttintegral_14days <- rowMeans(tt14)
tt21 <- means_integrals_21days_tt %>%
  select(seq(2, 70, by = 2))
tt21$ttintegral_21days <- rowMeans(tt21)
integral_tt <- data.frame(tt3$ttintegral_3days, tt7$ttintegral_7days, tt14$ttintegral_14days, tt21$ttintegral_21days, tick = 1:nrow(tt3))
colnames(integral_tt) <- c("ttintegral_3days", "ttintegral_7days", "ttintegral_14days", "ttintegral_21days", "tick")

rm(tt3, tt7, tt14, tt21)

## failed_transit_traveler
means_3days_ftt$fttintegral_3days <- rowMeans(means_3days_ftt)
means_7days_ftt$fttintegral_7days <- rowMeans(means_7days_ftt)
means_14days_ftt$fttintegral_14days <- rowMeans(means_14days_ftt)
means_21days_ftt$fttintegral_21days <- rowMeans(means_21days_ftt)
integral_ftt <- data.frame(means_3days_ftt$fttintegral_3days, means_7days_ftt$fttintegral_7days, means_14days_ftt$fttintegral_14days, means_21days_ftt$fttintegral_21days, tick = 1:nrow(means_3days_ftt))
colnames(integral_ftt) <- c("fttintegral_3days", "fttintegral_7days", "fttintegral_14days", "fttintegral_21days", "tick")

## give_up_traveler
means_3days_gut$gutintegral_3days <- rowMeans(means_3days_gut)
means_7days_gut$gutintegral_7days <- rowMeans(means_7days_gut)
means_14days_gut$gutintegral_14days <- rowMeans(means_14days_gut)
means_21days_gut$gutintegral_21days <- rowMeans(means_21days_gut)
integral_gut <- data.frame(means_3days_gut$gutintegral_3days, means_7days_gut$gutintegral_7days, means_14days_gut$gutintegral_14days, means_21days_gut$gutintegral_21days, tick = 1:nrow(means_3days_gut))
colnames(integral_gut) <- c("gutintegral_3days", "gutintegral_7days", "gutintegral_14days", "gutintegral_21days", "tick")

## TM
tm3 <- means_integrals_3days_TM %>%
  select(seq(1, 69, by = 2))
tm3$TMmeans_3days <- rowMeans(tm3)
tm7 <- means_integrals_7days_TM %>%
  select(seq(1, 69, by = 2))
tm7$TMmeans_7days <- rowMeans(tm7)
tm14 <- means_integrals_14days_TM %>%
  select(seq(1, 69, by = 2))
tm14$TMmeans_14days <- rowMeans(tm14)
tm21 <- means_integrals_21days_TM %>%
  select(seq(1, 69, by = 2))
tm21$TMmeans_21days <- rowMeans(tm21)
means_TM <- data.frame(tm3$TMmeans_3days, tm7$TMmeans_7days, tm14$TMmeans_14days, tm21$TMmeans_21days, tick = 1:nrow(tm3))
colnames(means_TM) <- c("TMmeans_3days", "TMmeans_7days", "TMmeans_14days", "TMmeans_21days", "tick")

rm(tm3, tm7, tm14, tm21)

# compute integral of TM to be consistent with results of R, M
tm3 <- means_integrals_3days_TM %>%
  select(seq(2, 70, by = 2))
tm3$TMintegral_3days <- rowMeans(tm3)
tm7 <- means_integrals_7days_TM %>%
  select(seq(2, 70, by = 2))
tm7$TMintegral_7days <- rowMeans(tm7)
tm14 <- means_integrals_14days_TM %>%
  select(seq(2, 70, by = 2))
tm14$TMintegral_14days <- rowMeans(tm14)
tm21 <- means_integrals_21days_TM %>%
  select(seq(2, 70, by = 2))
tm21$TMintegral_21days <- rowMeans(tm21)
integral_TM <- data.frame(tm3$TMintegral_3days, tm7$TMintegral_7days, tm14$TMintegral_14days, tm21$TMintegral_21days, tick = 1:nrow(tm3))
colnames(integral_TM) <- c("TMintegral_3days", "TMintegral_7days", "TMintegral_14days", "TMintegral_21days", "tick")

rm(tm3, tm7, tm14, tm21)

## risk_city
means_3days_rc$rcintegral_3days <- rowMeans(means_3days_rc)
means_7days_rc$rcintegral_7days <- rowMeans(means_7days_rc)
means_14days_rc$rcintegral_14days <- rowMeans(means_14days_rc)
means_21days_rc$rcintegral_21days <- rowMeans(means_21days_rc)
integral_rc <- data.frame(means_3days_rc$rcintegral_3days, means_7days_rc$rcintegral_7days, means_14days_rc$rcintegral_14days, means_21days_rc$rcintegral_21days, tick = 1:nrow(means_3days_rc))
colnames(integral_rc) <- c("rcintegral_3days", "rcintegral_7days", "rcintegral_14days", "rcintegral_21days", "tick")
##------------------------

##--Visualization--
# dco: R, M
# 3days
a <- means_integrals_3days_R %>%
  select(seq(1, 69, by = 2))
a$Rmeans <- rowMeans(a)
b <- means_integrals_3days_M %>%
  select(seq(1, 69, by = 2))
b$Mmeans <- rowMeans(b)
plot_for_allgroup_means <- data.frame(a$Rmeans, b$Mmeans)
rm(a, b)
colnames(plot_for_allgroup_means) <- c("Rmeans", "Mmeans")
plot_for_allgroup_means$tick <- 1:nrow(plot_for_allgroup_means)

a <- means_integrals_3days_R %>%
  select(seq(2, 70, by = 2))
a$Rintegral <- rowMeans(a)
b <- means_integrals_3days_M %>%
  select(seq(2, 70, by = 2))
b$Mintegral <- rowMeans(b)
plot_for_allgroup_integrals <- data.frame(a$Rintegral, b$Mintegral)
rm(a, b)
colnames(plot_for_allgroup_integrals) <- c("Rintegral", "Mintegral")
plot_for_allgroup_integrals$tick <- 1:nrow(plot_for_allgroup_integrals)

#version3: 组合均值的平均绝对数值
plot_means_3days_all <- plot_for_allgroup_means %>%
  ggplot(aes(x = tick)) +
  geom_area(aes(y = Mmeans), fill = "skyblue") +
  geom_area(aes(y = Rmeans), fill = "red") +
  scale_y_continuous(
    # features of the first axis
    name = "Mmeans",
    limits = c(0, 1250),
    breaks = c(0,250,500,750,1000,1250),
    # add a second axis and specify its features
    sec.axis = sec_axis(trans = ~., name = "Rmeans")
  ) +
  theme_bw() +
  labs(
    x = "tick",
    title = "3 days + departure + united restriction"
  )

#version4: 组合均值的积分绝对数值
plot_integral_3days_all <- plot_for_allgroup_integrals %>%
  ggplot(aes(x = tick)) +
  geom_area(aes(y = Mintegral), fill = "skyblue") +
  geom_area(aes(y = Rintegral), fill = "red") +
  scale_y_continuous(
    # features of the first axis
    name = "Mintegral",
    limits = c(0, 82000),
    breaks = c(0,20000,40000,60000,80000),
    # add a second axis and specify its features
    sec.axis = sec_axis(trans = ~., name = "Rintegral")
  ) +
  theme_bw() +
  labs(
    x = "tick",
    title = "3 days + departure + united restriction"
  )

# 7days
a <- means_integrals_7days_R %>%
  select(seq(1, 69, by = 2))
a$Rmeans <- rowMeans(a)
b <- means_integrals_7days_M %>%
  select(seq(1, 69, by = 2))
b$Mmeans <- rowMeans(b)
plot_for_allgroup_means <- data.frame(a$Rmeans, b$Mmeans)
rm(a, b)
colnames(plot_for_allgroup_means) <- c("Rmeans", "Mmeans")
plot_for_allgroup_means$tick <- 1:nrow(plot_for_allgroup_means)

a <- means_integrals_7days_R %>%
  select(seq(2, 70, by = 2))
a$Rintegral <- rowMeans(a)
b <- means_integrals_7days_M %>%
  select(seq(2, 70, by = 2))
b$Mintegral <- rowMeans(b)
plot_for_allgroup_integrals <- data.frame(a$Rintegral, b$Mintegral)
rm(a, b)
colnames(plot_for_allgroup_integrals) <- c("Rintegral", "Mintegral")
plot_for_allgroup_integrals$tick <- 1:nrow(plot_for_allgroup_integrals)

#version3: 组合均值的平均绝对数值
plot_means_7days_all <- plot_for_allgroup_means %>%
  ggplot(aes(x = tick)) +
  geom_area(aes(y = Mmeans), fill = "skyblue") +
  geom_area(aes(y = Rmeans), fill = "red") +
  scale_y_continuous(
    # features of the first axis
    name = "Mmeans",
    limits = c(0, 1250),
    breaks = c(0,250,500,750,1000,1250),
    # add a second axis and specify its features
    sec.axis = sec_axis(trans = ~., name = "Rmeans")
  ) +
  theme_bw() +
  labs(
    x = "tick",
    title = "7 days + departure + united restriction"
  )

#version4: 组合均值的积分绝对数值
plot_integral_7days_all <- plot_for_allgroup_integrals %>%
  ggplot(aes(x = tick)) +
  geom_area(aes(y = Mintegral), fill = "skyblue") +
  geom_area(aes(y = Rintegral), fill = "red") +
  scale_y_continuous(
    # features of the first axis
    name = "Mintegral",
    limits = c(0, 82000),
    breaks = c(0,20000,40000,60000,80000),
    # add a second axis and specify its features
    sec.axis = sec_axis(trans = ~., name = "Rintegral")
  ) +
  theme_bw() +
  labs(
    x = "tick",
    title = "7 days + departure + united restriction"
  )

# 14days
a <- means_integrals_14days_R %>%
  select(seq(1, 69, by = 2))
a$Rmeans <- rowMeans(a)
b <- means_integrals_14days_M %>%
  select(seq(1, 69, by = 2))
b$Mmeans <- rowMeans(b)
plot_for_allgroup_means <- data.frame(a$Rmeans, b$Mmeans)
rm(a, b)
colnames(plot_for_allgroup_means) <- c("Rmeans", "Mmeans")
plot_for_allgroup_means$tick <- 1:nrow(plot_for_allgroup_means)

a <- means_integrals_14days_R %>%
  select(seq(2, 70, by = 2))
a$Rintegral <- rowMeans(a)
b <- means_integrals_14days_M %>%
  select(seq(2, 70, by = 2))
b$Mintegral <- rowMeans(b)
plot_for_allgroup_integrals <- data.frame(a$Rintegral, b$Mintegral)
rm(a, b)
colnames(plot_for_allgroup_integrals) <- c("Rintegral", "Mintegral")
plot_for_allgroup_integrals$tick <- 1:nrow(plot_for_allgroup_integrals)

#version3: 组合均值的平均绝对数值
plot_means_14days_all <- plot_for_allgroup_means %>%
  ggplot(aes(x = tick)) +
  geom_area(aes(y = Mmeans), fill = "skyblue") +
  geom_area(aes(y = Rmeans), fill = "red") +
  scale_y_continuous(
    # features of the first axis
    name = "Mmeans",
    limits = c(0, 1250),
    breaks = c(0,250,500,750,1000,1250),
    # add a second axis and specify its features
    sec.axis = sec_axis(trans = ~., name = "Rmeans")
  ) +
  theme_bw() +
  labs(
    x = "tick",
    title = "14 days + departure + united restriction"
  )

#version4: 组合均值的积分绝对数值
plot_integral_14days_all <- plot_for_allgroup_integrals %>%
  ggplot(aes(x = tick)) +
  geom_area(aes(y = Mintegral), fill = "skyblue") +
  geom_area(aes(y = Rintegral), fill = "red") +
  scale_y_continuous(
    # features of the first axis
    name = "Mintegral",
    limits = c(0, 82000),
    breaks = c(0,20000,40000,60000,80000),
    # add a second axis and specify its features
    sec.axis = sec_axis(trans = ~., name = "Rintegral")
  ) +
  theme_bw() +
  labs(
    x = "tick",
    title = "14 days + departure + united restriction"
  )

# 21days
a <- means_integrals_21days_R %>%
  select(seq(1, 69, by = 2))
a$Rmeans <- rowMeans(a)
b <- means_integrals_21days_M %>%
  select(seq(1, 69, by = 2))
b$Mmeans <- rowMeans(b)
plot_for_allgroup_means <- data.frame(a$Rmeans, b$Mmeans)
rm(a, b)
colnames(plot_for_allgroup_means) <- c("Rmeans", "Mmeans")
plot_for_allgroup_means$tick <- 1:nrow(plot_for_allgroup_means)

a <- means_integrals_21days_R %>%
  select(seq(2, 70, by = 2))
a$Rintegral <- rowMeans(a)
b <- means_integrals_21days_M %>%
  select(seq(2, 70, by = 2))
b$Mintegral <- rowMeans(b)
plot_for_allgroup_integrals <- data.frame(a$Rintegral, b$Mintegral)
rm(a, b)
colnames(plot_for_allgroup_integrals) <- c("Rintegral", "Mintegral")
plot_for_allgroup_integrals$tick <- 1:nrow(plot_for_allgroup_integrals)

#version3: 组合均值的平均绝对数值
plot_means_21days_all <- plot_for_allgroup_means %>%
  ggplot(aes(x = tick)) +
  geom_area(aes(y = Mmeans), fill = "skyblue") +
  geom_area(aes(y = Rmeans), fill = "red") +
  scale_y_continuous(
    # features of the first axis
    name = "Mmeans",
    limits = c(0, 1250),
    breaks = c(0,250,500,750,1000,1250),
    # add a second axis and specify its features
    sec.axis = sec_axis(trans = ~., name = "Rmeans")
  ) +
  theme_bw() +
  labs(
    x = "tick",
    title = "21 days + departure + united restriction"
  )

#version4: 组合均值的积分绝对数值
plot_integral_21days_all <- plot_for_allgroup_integrals %>%
  ggplot(aes(x = tick)) +
  geom_area(aes(y = Mintegral), fill = "skyblue") +
  geom_area(aes(y = Rintegral), fill = "red") +
  scale_y_continuous(
    # features of the first axis
    name = "Mintegral",
    limits = c(0, 82000),
    breaks = c(0,20000,40000,60000,80000),
    # add a second axis and specify its features
    sec.axis = sec_axis(trans = ~., name = "Rintegral")
  ) +
  theme_bw() +
  labs(
    x = "tick",
    title = "21 days + departure + united restriction"
  )

# 利用ggarrange将图像排列在一起
plot_RM_dco <- ggarrange(plot_means_3days_all, plot_means_7days_all, plot_means_14days_all, plot_means_21days_all,
                         plot_integral_3days_all, plot_integral_7days_all, plot_integral_14days_all, plot_integral_21days_all,
                         ncol = 4,nrow =2)

rm(plot_means_3days_all, plot_means_7days_all, plot_means_14days_all, plot_means_21days_all)
rm(plot_integral_3days_all, plot_integral_7days_all, plot_integral_14days_all, plot_integral_21days_all)
##------------------------

##--Outputting--
l <- list("M" = means_M, "R" = means_R, "M_integral" = integral_M, "R_integral" = integral_R, "transit_traveler" = means_tt, "transit_traveler_integral" = integral_tt,  "failed_transit_integral" = integral_ftt, "give_up_traveler_integral" = integral_gut, "TM" = means_TM, "TM_integral" = integral_TM, "risk_city_integral" = integral_rc)
write.xlsx(l, "departure and united policy-data.xlsx")
##------------------------

##--Housekeeping--
rm(list = ls())
##----------------