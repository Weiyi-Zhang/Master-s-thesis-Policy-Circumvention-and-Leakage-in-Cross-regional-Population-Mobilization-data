#--------------------
#Objective: 毕业论文数据处理与分析-规避现象分析
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
raw_tt <- read_excel("Results of simulation.xlsx", sheet = "transit_traveler")
##------------------------

##--scale of travelers who circumvent--
## visualization across all groups
tt <- raw_tt %>%
  gather(key = "group", value = "transit_traveler", 1:24)

# use different linetypes to distinguish
tt$type <- str_sub(tt$group, 1, 3)

# rename
tt$type <- dplyr::recode(tt$type,
                         dnc = "远端+无配合",
                         dco = "远端+政策统一",
                         dct = "远端+数据共享",
                         anc = "近端+无配合",
                         aco = "近端+政策统一",
                         act = "近端+数据共享")
tt$group <- str_sub(tt$group, 8)
tt$group <- gsub(pattern = "days", replacement = "天", tt$group)
tt$group <- paste(tt$type, tt$group, sep = "+")

# modify the legend in descending order
order <- tt %>%
  filter(tick == 70)
order <- order[order(order[,"transit_traveler"], decreasing = T),]
order <- as.vector(order$group)
tt$group <- factor(tt$group, levels = order)

plot_tt <- tt %>%
  ggplot(aes(x = tick, y = transit_traveler, colour = group, linetype = type)) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  labs(
    x = "时间（天）",
    y = "避难者规模transit_traveler",
    linetype = "",
    colour = "政策组合（稳定水平降序排列）"
  )
plot_tt # 手动export大小合适的图

## dnc & dct
# modify the legend order
order1 <- c("远端+无配合+3天", "远端+无配合+7天", "远端+无配合+14天", "远端+无配合+21天")
order2 <- c("远端+数据共享+3天", "远端+数据共享+7天", "远端+数据共享+14天", "远端+数据共享+21天")
dnc_tt <- tt %>%
  filter(type == "远端+无配合")
dnc_tt$group <- factor(dnc_tt$group, levels = order1)
dct_tt <- tt %>%
  filter(type == "远端+数据共享")
dct_tt$group <- factor(dct_tt$group, levels = order2)

plot_dnc_tt <- dnc_tt %>%
  ggplot(aes(x = tick, y = transit_traveler, colour = group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "时间（天）",
    y = "transit_traveler",
    colour = "",
    title = "远端查验+无配合"
  )

plot_dct_tt <- dct_tt %>%
  ggplot(aes(x = tick, y = transit_traveler, colour = group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "时间（天）",
    y = "transit_traveler",
    colour = "",
    title = "远端查验+数据共享"
  ) 

# 利用ggarrange将图像排列在一起
plot_dnc_dct <- ggarrange(plot_dnc_tt, plot_dct_tt, ncol = 1,nrow =2)
plot_dnc_dct # 手动export大小合适的图

## anc & act
# modify the legend order
order3 <- c("近端+无配合+3天", "近端+无配合+7天", "近端+无配合+14天", "近端+无配合+21天")
order4 <- c("近端+数据共享+3天", "近端+数据共享+7天", "近端+数据共享+14天", "近端+数据共享+21天")
anc_tt <- tt %>%
  filter(type == "近端+无配合")
anc_tt$group <- factor(anc_tt$group, levels = order3)
act_tt <- tt %>%
  filter(type == "近端+数据共享")
act_tt$group <- factor(act_tt$group, levels = order4)

plot_anc_tt <- anc_tt %>%
  ggplot(aes(x = tick, y = transit_traveler, colour = group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "时间（天）",
    y = "transit_traveler",
    colour = "",
    title = "近端查验+无配合"
  )

plot_act_tt <- act_tt %>%
  ggplot(aes(x = tick, y = transit_traveler, colour = group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "时间（天）",
    y = "transit_traveler",
    colour = "",
    title = "近端查验+数据共享"
  )

# 利用ggarrange将图像排列在一起
plot_anc_act <- ggarrange(plot_anc_tt, plot_act_tt, ncol = 1,nrow =2)
plot_anc_act # 手动export大小合适的图
##------------------------

##--Housekeeping--
rm(list = ls())
##----------------