#--------------------
#Objective: ��ҵ�������ݴ��������-����������
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
library(ggpubr) # �����Ų�����ͼ��ͬһ������
##------------

##--Reading the raw data--
setwd("E:/zwy-Master-1/��ҵ����/����")
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
                         dnc = "Զ��+�����",
                         dco = "Զ��+����ͳһ",
                         dct = "Զ��+���ݹ���",
                         anc = "����+�����",
                         aco = "����+����ͳһ",
                         act = "����+���ݹ���")
tt$group <- str_sub(tt$group, 8)
tt$group <- gsub(pattern = "days", replacement = "��", tt$group)
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
    x = "ʱ�䣨�죩",
    y = "�����߹�ģtransit_traveler",
    linetype = "",
    colour = "������ϣ��ȶ�ˮƽ�������У�"
  )
plot_tt # �ֶ�export��С���ʵ�ͼ

## dnc & dct
# modify the legend order
order1 <- c("Զ��+�����+3��", "Զ��+�����+7��", "Զ��+�����+14��", "Զ��+�����+21��")
order2 <- c("Զ��+���ݹ���+3��", "Զ��+���ݹ���+7��", "Զ��+���ݹ���+14��", "Զ��+���ݹ���+21��")
dnc_tt <- tt %>%
  filter(type == "Զ��+�����")
dnc_tt$group <- factor(dnc_tt$group, levels = order1)
dct_tt <- tt %>%
  filter(type == "Զ��+���ݹ���")
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
    x = "ʱ�䣨�죩",
    y = "transit_traveler",
    colour = "",
    title = "Զ�˲���+�����"
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
    x = "ʱ�䣨�죩",
    y = "transit_traveler",
    colour = "",
    title = "Զ�˲���+���ݹ���"
  ) 

# ����ggarrange��ͼ��������һ��
plot_dnc_dct <- ggarrange(plot_dnc_tt, plot_dct_tt, ncol = 1,nrow =2)
plot_dnc_dct # �ֶ�export��С���ʵ�ͼ

## anc & act
# modify the legend order
order3 <- c("����+�����+3��", "����+�����+7��", "����+�����+14��", "����+�����+21��")
order4 <- c("����+���ݹ���+3��", "����+���ݹ���+7��", "����+���ݹ���+14��", "����+���ݹ���+21��")
anc_tt <- tt %>%
  filter(type == "����+�����")
anc_tt$group <- factor(anc_tt$group, levels = order3)
act_tt <- tt %>%
  filter(type == "����+���ݹ���")
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
    x = "ʱ�䣨�죩",
    y = "transit_traveler",
    colour = "",
    title = "���˲���+�����"
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
    x = "ʱ�䣨�죩",
    y = "transit_traveler",
    colour = "",
    title = "���˲���+���ݹ���"
  )

# ����ggarrange��ͼ��������һ��
plot_anc_act <- ggarrange(plot_anc_tt, plot_act_tt, ncol = 1,nrow =2)
plot_anc_act # �ֶ�export��С���ʵ�ͼ
##------------------------

##--Housekeeping--
rm(list = ls())
##----------------