#--------------------
#Objective: 毕业论文数据处理与分析-R&M帕累托效率分析
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
library(gg.gap) # 用于绘制断裂坐标轴
##------------

##--Reading the raw data--
setwd("E:/zwy-Master-1/毕业论文/数据")
M <- read.xlsx("Results of simulation.xlsx", sheet = "M")
R <- read.xlsx("Results of simulation.xlsx", sheet = "R")
M_integral <- read.xlsx("Results of simulation.xlsx", sheet = "M_integral")
R_integral <- read.xlsx("Results of simulation.xlsx", sheet = "R_integral")
##------------------------

##--Visualization(R&M under different policies)--
# M
plot_for_M <- M %>%
  gather(key = "group", value = "M", 1:24)

# use different linetypes to distinguish
plot_for_M$type <- str_sub(plot_for_M$group, 1, 3)

# rename
plot_for_M$type <- dplyr::recode(plot_for_M$type,
                                 dnc = "远端+无配合",
                                 dco = "远端+政策统一",
                                 dct = "远端+数据共享",
                                 anc = "近端+无配合",
                                 aco = "近端+政策统一",
                                 act = "近端+数据共享")
plot_for_M$group <- str_sub(plot_for_M$group, 7)
plot_for_M$group <- gsub(pattern = "days", replacement = "天", plot_for_M$group)
plot_for_M$group <- paste(plot_for_M$type, plot_for_M$group, sep = "+")

# modify the legend in descending order
order <- plot_for_M %>%
  filter(tick == 70)
order <- order[order(order[,"M"], decreasing = T),]
order <- as.vector(order$group)
plot_for_M$group <- factor(plot_for_M$group, levels = order)

plot_M <- plot_for_M %>%
  ggplot(aes(x = tick, y = M, colour = group, linetype = type)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "right"
  ) +
  labs(
    x = "时间（天）",
    y = "核心城市人员流动性M",
    linetype = "",
    colour = "政策组合（稳定水平降序排列）"
  )
plot_M # 手动export大小合适的图

# R
plot_for_R <- R %>%
  gather(key = "group", value = "R", 1:24)

# use different linetypes to distinguish
plot_for_R$type <- str_sub(plot_for_R$group, 1, 3)

# rename
plot_for_R$type <- dplyr::recode(plot_for_R$type,
                                 dnc = "远端+无配合",
                                 dco = "远端+政策统一",
                                 dct = "远端+数据共享",
                                 anc = "近端+无配合",
                                 aco = "近端+政策统一",
                                 act = "近端+数据共享")
plot_for_R$group <- str_sub(plot_for_R$group, 7)
plot_for_R$group <- gsub(pattern = "days", replacement = "天", plot_for_R$group)
plot_for_R$group <- paste(plot_for_R$type, plot_for_R$group, sep = "+")

# modify the legend in descending order
order <- plot_for_R %>%
  filter(tick == 70)
order <- order[order(order[,"R"], decreasing = T),]
order <- as.vector(order$group)
plot_for_R$group <- factor(plot_for_R$group, levels = order)

plot_R <- plot_for_R %>%
  ggplot(aes(x = tick, y = R, colour = group, linetype = type)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "right"
  ) +
  labs(
    x = "时间（天）",
    y = "核心城市病毒传入风险R",
    linetype = "",
    colour = "政策组合（稳定水平降序排列）"
  )
plot_R # 手动export大小合适的图
##-----------------------

##--Pareto efficiency analysis--
# 流量flow
R_inverse <- select(R, -"tick")
R_inverse <- 1/R_inverse
R_inverse$tick <- 1:nrow(R_inverse)
R_inverse <- R_inverse %>%
  gather(key = "group", value = "R_inverse", 1:24)
R_inverse$group <- gsub(pattern = "R_", replacement = "", R_inverse$group)

M <- M %>%
  gather(key = "group", value = "M", 1:24)
M$group <- gsub(pattern = "M_", replacement = "", M$group)

# 存量stock
R_integral_inverse <- select(R_integral, -"tick")
R_integral_inverse <- 1/R_integral_inverse
R_integral_inverse$tick <- 1:nrow(R_integral_inverse)
R_integral_inverse <- R_integral_inverse %>%
  gather(key = "group", value = "R_integral_inverse", 1:24)
R_integral_inverse$group <- gsub(pattern = "R_integral_", replacement = "", R_integral_inverse$group)

M_integral <- M_integral %>%
  gather(key = "group", value = "M_integral", 1:24)
M_integral$group <- gsub(pattern = "M_integral_", replacement = "", M_integral$group)

merge <- M %>%
  left_join(R_inverse, by = c("tick", "group")) %>%
  left_join(M_integral, by = c("tick", "group")) %>%
  left_join(R_integral_inverse, by = c("tick", "group"))
##------------------------

##--Visualization--
merge <- merge %>%
  filter(R_integral_inverse != Inf)
merge$type <- str_sub(merge$group, 1, 3)
# rename
merge$type <- dplyr::recode(merge$type,
                            dnc = "远端+无配合",
                            dco = "远端+政策统一",
                            dct = "远端+数据共享",
                            anc = "近端+无配合",
                            aco = "近端+政策统一",
                            act = "近端+数据共享")
merge$group <- str_sub(merge$group, 5)
merge$group <- gsub(pattern = "days", replacement = "天", merge$group)
merge$group <- paste(merge$type, merge$group, sep = "+")

###############disuse#####################
## 3 dimensions: R_integral_inverse, M_integral, tick
## pick 20 colours
#colour_list_r <- c(0, 0, 165, 184, 0, 139, 139, 30, 47, 50, 178, 255, 255, 72, 0, 199, 107, 160, 255, 46)
#colour_list_g <- c(0, 0, 42, 134, 100, 0, 0, 144, 79, 205, 34, 215, 105, 209, 0, 21, 142, 32, 0, 139)
#colour_list_b <- c(0, 255, 42, 11, 0, 139, 0, 255, 79, 50, 34, 0, 180, 204, 128, 133, 35, 240, 0, 87)
#group_list <- unique(merge$group)
#merge_for_plot <- merge %>%
#  filter(R_integral_inverse != Inf) %>%
#  filter(group == group_list[1]) %>%
#  mutate(r = seq(255, colour_list_r[1], length.out = 70),
#         g = seq(255, colour_list_g[1], length.out = 70),
#         b = seq(255, colour_list_b[1], length.out = 70),
#         group_tick = paste(group, tick, sep = "")) %>%
#  mutate(colour = rgb(r, g, b, max = 255))
#for (i in 2:20) {
#  a <- merge %>%
#    filter(R_integral_inverse != Inf) %>%
#    filter(group == group_list[i]) %>%
#    mutate(r = seq(255, colour_list_r[i], length.out = 70),
#           g = seq(255, colour_list_g[i], length.out = 70),
#           b = seq(255, colour_list_b[i], length.out = 70),
#           group_tick = paste(group, tick, sep = "")) %>%
#    mutate(colour = rgb(r, g, b, max = 255))
#  
#  merge_for_plot <- rbind(merge_for_plot, a)
#}
#rm(a)
#
#m <- as.vector(merge_for_plot$colour)
#f <- as.vector(merge_for_plot$group_tick)
#merge_for_plot$group_tick <- factor(merge_for_plot$group_tick, levels = f,
#                  labels = f)
#
#merge_for_plot1 <- merge_for_plot %>%
#  filter(tick %in% 2:30)
#merge_for_plot2 <- merge_for_plot %>%
#  filter(tick %in% 30:70)
#
#p <- merge_for_plot2 %>%
#  filter(tick != 1) %>%
#  ggplot(aes(x = M_integral, y = R_integral_inverse, colour = group_tick, size = tick)) +
#  geom_point() +
#  scale_color_manual(values = m) +
#  theme_bw() +
#  theme(legend.position = "none")
#p
#p %>%
#  gg.gap(segments = c(0.0002,0.028),
#         tick_width = c(0.0001,0.001),
#         ylim = c(0, 0.032))
###############disuse#####################

## cross-section result
# 70tick
plot_70tick <- merge %>%
  filter(tick == 70) %>%
  ggplot(aes(x = M_integral, y = R_integral_inverse, colour = group)) +
  geom_point(size = 3) +
  theme_bw() +
  theme(
    legend.position = "right"
  ) +
  labs(
    x = "累计人员流动性M",
    y = "累计病毒传入风险R的倒数"
  )
plot_70tick
plot_70tick_broken <- plot_70tick %>%
  gg.gap(segments = c(0.0002,0.028),
         tick_width = c(0.0001,0.001),
         ylim = c(0, 0.032))
plot_70tick_broken # 手动export大小合适的图

# 10tick
plot_10tick <- merge %>%
  filter(tick == 10) %>%
  ggplot(aes(x = M_integral, y = R_integral_inverse, colour = group)) +
  geom_point(size = 3) +
  theme_bw() +
  theme(
    legend.position = "right"
  ) +
  labs(
    x = "累计人员流动性M",
    y = "累计病毒传入风险R的倒数"
  )
plot_10tick
plot_10tick_broken <- plot_10tick %>%
  gg.gap(segments = c(0.004,0.045),
         tick_width = c(0.001,0.001),
         ylim = c(0, 0.052)) 
plot_10tick_broken # 手动export大小合适的图

# 20tick
plot_20tick <- merge %>%
  filter(tick == 20) %>%
  ggplot(aes(x = M_integral, y = R_integral_inverse, colour = group)) +
  geom_point(size = 3) +
  theme_bw() +
  theme(
    legend.position = "right"
  ) +
  labs(
    x = "累计人员流动性M",
    y = "累计病毒传入风险R的倒数"
  )
plot_20tick
plot_20tick_broken <- plot_20tick %>%
  gg.gap(segments = c(0.001,0.028),
         tick_width = c(0.0002,0.001),
         ylim = c(0, 0.032))
plot_20tick_broken # 手动export大小合适的图

# 40tick
plot_40tick <- merge %>%
  filter(tick == 40) %>%
  ggplot(aes(x = M_integral, y = R_integral_inverse, colour = group)) +
  geom_point(size = 3) +
  theme_bw() +
  theme(
    legend.position = "right"
  ) +
  labs(
    x = "累计人员流动性M",
    y = "累计病毒传入风险R的倒数"
  )
plot_40tick
plot_40tick_broken <- plot_40tick %>%
  gg.gap(segments = c(0.0004,0.028),
         tick_width = c(0.0001,0.001),
         ylim = c(0, 0.032))
plot_40tick_broken # 手动export大小合适的图
##------------------------

##--Housekeeping--
rm(list = ls())
##----------------