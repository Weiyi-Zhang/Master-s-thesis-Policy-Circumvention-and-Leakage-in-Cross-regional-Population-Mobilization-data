#--------------------
#Objective: ��ҵ�������ݴ��������-�����������
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
raw_TM <- read_excel("Results of simulation.xlsx", sheet = "TM")
raw_ftt <- read_excel("Results of simulation.xlsx", sheet = "failed_transit_integral")
raw_rc <- read_excel("Results of simulation.xlsx", sheet = "risk_city_integral")
##------------------------

##--scale of the mobility by transition--
## visualization across all groups
TM <- raw_TM %>%
  gather(key = "group", value = "TM", 1:24)

# use different linetypes to distinguish
TM$type <- str_sub(TM$group, 1, 3)

# rename
TM$type <- dplyr::recode(TM$type,
                         dnc = "Զ��+�����",
                         dco = "Զ��+����ͳһ",
                         dct = "Զ��+���ݹ���",
                         anc = "����+�����",
                         aco = "����+����ͳһ",
                         act = "����+���ݹ���")
TM$group <- str_sub(TM$group, 8)
TM$group <- gsub(pattern = "days", replacement = "��", TM$group)
TM$group <- paste(TM$type, TM$group, sep = "+")

# modify the legend in descending order
order <- TM %>%
  filter(tick == 70)
order <- order[order(order[,"TM"], decreasing = T),]
order <- as.vector(order$group)
TM$group <- factor(TM$group, levels = order)

plot_TM <- TM %>%
  ggplot(aes(x = tick, y = TM, colour = group, linetype = type)) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "right"
  ) +
  labs(
    x = "ʱ�䣨�죩",
    y = "�������ѵ�����ĳ��е�������TM",
    linetype = "",
    colour = "������ϣ��ȶ�ˮƽ�������У�",
  )
plot_TM # �ֶ�export��С���ʵ�ͼ

## dnc & dct
# modify the legend order
order1 <- c("Զ��+�����+3��", "Զ��+�����+7��", "Զ��+�����+14��", "Զ��+�����+21��")
order2 <- c("Զ��+���ݹ���+3��", "Զ��+���ݹ���+7��", "Զ��+���ݹ���+14��", "Զ��+���ݹ���+21��")
dnc_TM <- TM %>%
  filter(type == "Զ��+�����")
dnc_TM$group <- factor(dnc_TM$group, levels = order1)
dct_TM <- TM %>%
  filter(type == "Զ��+���ݹ���")
dct_TM$group <- factor(dct_TM$group, levels = order2)

plot_dnc_TM <- dnc_TM %>%
  ggplot(aes(x = tick, y = TM, colour = group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "ʱ�䣨�죩",
    y = "TM",
    colour = "",
    title = "Զ�˲���+�����"
  )

plot_dct_TM <- dct_TM %>%
  ggplot(aes(x = tick, y = TM, colour = group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "ʱ�䣨�죩",
    y = "TM",
    colour = "",
    title = "Զ�˲���+���ݹ���"
  ) 

# ����ggarrange��ͼ��������һ��
plot_dnc_dct_TM <- ggarrange(plot_dnc_TM, plot_dct_TM, ncol = 1,nrow =2)
plot_dnc_dct_TM # �ֶ�export��С���ʵ�ͼ

## anc & act
# modify the legend order
order3 <- c("����+�����+3��", "����+�����+7��", "����+�����+14��", "����+�����+21��")
order4 <- c("����+���ݹ���+3��", "����+���ݹ���+7��", "����+���ݹ���+14��", "����+���ݹ���+21��")
anc_TM <- TM %>%
  filter(type == "����+�����")
anc_TM$group <- factor(anc_TM$group, levels = order3)
act_TM <- TM %>%
  filter(type == "����+���ݹ���")
act_TM$group <- factor(act_TM$group, levels = order4)

plot_anc_TM <- anc_TM %>%
  ggplot(aes(x = tick, y = TM, colour = group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "ʱ�䣨�죩",
    y = "TM",
    colour = "",
    title = "����+�����"
  )

plot_act_TM <- act_TM %>%
  ggplot(aes(x = tick, y = TM, colour = group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "ʱ�䣨�죩",
    y = "TM",
    colour = "",
    title = "����+���ݹ���"
  )

# ����ggarrange��ͼ��������һ��
plot_anc_act_TM <- ggarrange(plot_anc_TM, plot_act_TM, ncol = 1,nrow =2)
plot_anc_act_TM # �ֶ�export��С���ʵ�ͼ
##------------------------

##--scale of travelers who failed in circumvention--
## visualization across all groups
ftt <- raw_ftt %>%
  gather(key = "group", value = "failed_transit_traveler", 1:24)

# use different linetypes to distinguish
ftt$type <- str_sub(ftt$group, 1, 3)

# rename
ftt$type <- dplyr::recode(ftt$type,
                          dnc = "Զ��+�����",
                          dco = "Զ��+����ͳһ",
                          dct = "Զ��+���ݹ���",
                          anc = "����+�����",
                          aco = "����+����ͳһ",
                          act = "����+���ݹ���")
ftt$group <- str_sub(ftt$group, 18)
ftt$group <- gsub(pattern = "days", replacement = "��", ftt$group)
ftt$group <- paste(ftt$type, ftt$group, sep = "+")

# modify the legend in descending order
order5 <- ftt %>%
  filter(tick == 70)
order5 <- order5[order(order5[,"failed_transit_traveler"], decreasing = T),]
order5 <- as.vector(order5$group)
ftt$group <- factor(ftt$group, levels = order5)

plot_ftt <- ftt %>%
  ggplot(aes(x = tick, y = failed_transit_traveler, colour = group, linetype = type)) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  labs(
    x = "ʱ�䣨�죩",
    y = "failed_transit_traveler",
    linetype = "�ۼƱ���ʧ�ܵĳ���������",
    colour = "������ϣ��ȶ�ˮƽ�������У�"
  )
plot_ftt # �ֶ�export��С���ʵ�ͼ

## dnc & dct
# modify the legend order
order6 <- c("dnc_ftt_integral_3days", "dnc_ftt_integral_7days", "dnc_ftt_integral_14days", "dnc_ftt_integral_21days")
order7 <- c("dct_ftt_integral_3days", "dct_ftt_integral_7days", "dct_ftt_integral_14days", "dct_ftt_integral_21days")
dnc_ftt <- ftt %>%
  filter(type == "departure and no cooperation")
dnc_ftt$group <- factor(dnc_ftt$group, levels = order6)
dct_ftt <- ftt %>%
  filter(type == "departure and data sharing")
dct_ftt$group <- factor(dct_ftt$group, levels = order7)

plot_dnc_ftt <- dnc_ftt %>%
  ggplot(aes(x = tick, y = failed_transit_traveler, colour = group)) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "tick",
    y = "failed_transit_traveler",
    colour = "group",
    title = "departure and no cooperation"
  )

plot_dct_ftt <- dct_ftt %>%
  ggplot(aes(x = tick, y = failed_transit_traveler, colour = group)) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "tick",
    y = "failed_transit_traveler",
    colour = "group",
    title = "departure and data sharing"
  ) 

# ����ggarrange��ͼ��������һ��
plot_dnc_dct_ftt <- ggarrange(plot_dnc_ftt, plot_dct_ftt, ncol = 1,nrow =2)
# �ֶ�export��С���ʵ�ͼ

## anc & act
# modify the legend order
order8 <- c("anc_ftt_integral_3days", "anc_ftt_integral_7days", "anc_ftt_integral_14days", "anc_ftt_integral_21days")
order9 <- c("act_ftt_integral_3days", "act_ftt_integral_7days", "act_ftt_integral_14days", "act_ftt_integral_21days")
anc_ftt <- ftt %>%
  filter(type == "arrival and no cooperation")
anc_ftt$group <- factor(anc_ftt$group, levels = order8)
act_ftt <- ftt %>%
  filter(type == "arrival and data sharing")
act_ftt$group <- factor(act_ftt$group, levels = order9)

plot_anc_ftt <- anc_ftt %>%
  ggplot(aes(x = tick, y = failed_transit_traveler, colour = group)) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "tick",
    y = "failed_transit_traveler",
    colour = "group",
    title = "arrival and no cooperation"
  )

plot_act_ftt <- act_ftt %>%
  ggplot(aes(x = tick, y = failed_transit_traveler, colour = group)) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "tick",
    y = "failed_transit_traveler",
    colour = "group",
    title = "arrival and data sharing"
  )

# ����ggarrange��ͼ��������һ��
plot_anc_act_ftt <- ggarrange(plot_anc_ftt, plot_act_ftt, ncol = 1,nrow =2)
# �ֶ�export��С���ʵ�ͼ
##------------------------

##--number of risk city--
## visualization across all groups
rc <- raw_rc %>%
  gather(key = "group", value = "risk_city", 1:24)

# use different linetypes to distinguish
rc$type <- str_sub(rc$group, 1, 3)

# rename
rc$type <- dplyr::recode(rc$type,
                         dnc = "Զ��+�����",
                         dco = "Զ��+����ͳһ",
                         dct = "Զ��+���ݹ���",
                         anc = "����+�����",
                         aco = "����+����ͳһ",
                         act = "����+���ݹ���")
rc$group <- str_sub(rc$group, 8)
rc$group <- gsub(pattern = "days", replacement = "��", rc$group)
rc$group <- paste(rc$type, rc$group, sep = "+")

# modify the legend in descending order
order <- rc %>%
  filter(tick == 70)
order <- order[order(order[,"risk_city"], decreasing = T),]
order <- as.vector(order$group)
rc$group <- factor(rc$group, levels = order)

plot_rc <- rc %>%
  ggplot(aes(x = tick, y = risk_city, colour = group, linetype = type)) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_bw() +
  labs(
    x = "ʱ�䣨�죩",
    y = "���߳�������risk_city",
    linetype = "",
    colour = "������ϣ��ȶ�ˮƽ�������У�"
  )
plot_rc # �ֶ�export��С���ʵ�ͼ
##------------------------

##--Housekeeping--
rm(list = ls())
##----------------