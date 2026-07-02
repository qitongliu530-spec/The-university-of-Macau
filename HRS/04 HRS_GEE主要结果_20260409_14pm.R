################################################################################
# HRS Frailty Analysis - Tables and Figures Export 
################################################################################

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(survival)
library(survminer)
library(tableone)
library(scales)
library(dplyr)
library(mice)
library(nnet)
library(survey)
library(geepack)
data_HRS9 <- readRDS("D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\3. HRS\\2. Backups of study participants\\HRS_Variability + Cumulative_20260401_完成全部定义.rds")

# ==============================================================================
# (二) 数据分析 Table 6 GEE analysis
# ==============================================================================
colnames(data_HRS9)

dat_long <- data_HRS9 %>%
  select(
    hhidpn,
    inw10:inw16,
    r10_fi:r16_fi,
    BMI_quantiles_CV, BMI_quantiles_SD, BMI_quantiles_ARV, BMI_quantiles_VIM,
    CV_bmi,
    r7rxhibp, r7rxdiab, r7cncrchem, r7rxlung, r7rxangina, r7rxchf, r7rxhrtat, r7rxheart, r7rxpsych, r7rxarthr,
    Age, Gender, Urbanicity, Education, Marital_binary, Employment,
    Physical_Activity, Smoking, Drinking, Wealth_quartile, slope, CumBMI, n_chronic,
    
    r10cancre, r10stroke, r10hearte) %>%
  
  # 1. FI 长格式
  pivot_longer(r10_fi:r16_fi, names_to = "wave", values_to = "Frailty") %>%
  mutate(wave = str_extract(wave, "\\d+")) %>%
  
  # 2. inw 长格式
  pivot_longer(inw10:inw16, names_to = "wave_inw", values_to = "follow_flag") %>%
  mutate(wave_inw = str_extract(wave_inw, "\\d+")) %>%
  
  # 3. 匹配同波次
  filter(wave == wave_inw) %>%
  
  # 4. 直接改名
  mutate(
    follow_up_wave = as.numeric(wave),
    hhidpn = factor(hhidpn)
  ) %>%
  
  filter(follow_flag == 1) %>%
  drop_na(Frailty, BMI_quantiles_CV) %>%
  select(-wave, -wave_inw)

table(dat_long$Frailty, useNA = "always")     # 连续性变量

dat_long <- dat_long %>% mutate(Frailty = ifelse(Frailty < 0.25, 0, 1))
table(dat_long$Frailty, useNA = "always")     # 分类变量
#0     1  <NA> 
#33298 15557     0

# ============================ IPTW权重计算 ================================
dat_baseline <- dat_long %>%
  group_by(hhidpn) %>%
  filter(follow_up_wave == min(follow_up_wave)) %>%
  ungroup() %>%
  distinct(hhidpn, .keep_all = TRUE) %>%
  select(
    hhidpn, BMI_quantiles_CV, Age, Gender, Urbanicity, Education, 
    Marital_binary, Employment, Physical_Activity, Smoking, Drinking, 
    Wealth_quartile, slope, CumBMI
  )

# 多分类模型
iptw_m <- multinom(
  BMI_quantiles_CV ~ Age + Gender + Urbanicity + Education + Marital_binary + 
    Employment + Physical_Activity + Smoking + Drinking + Wealth_quartile + slope + CumBMI, 
  data = dat_baseline, 
  trace = FALSE
)

# 倾向得分
prop_score <- predict(iptw_m, type = "probs")
model_data  <- model.frame(iptw_m)
dat_final   <- cbind(model_data, prop_score)
dat_final$hhidpn <- dat_baseline$hhidpn[as.integer(rownames(model_data))]

# 计算权重
dat_final <- dat_final %>%
  rowwise() %>%
  mutate(
    p_score = get(as.character(BMI_quantiles_CV)),
    iptw_weight = 1 / p_score
  ) %>%
  ungroup()

# 99%截尾
quantile_99 <- quantile(dat_final$iptw_weight, 0.99, na.rm = TRUE)
dat_final$iptw_weight_trim <- ifelse(dat_final$iptw_weight > quantile_99, quantile_99, dat_final$iptw_weight)

# 查看权重
summary(dat_final$iptw_weight)
summary(dat_final$iptw_weight_trim)

# 画图
ggplot(dat_final, aes(x = iptw_weight_trim)) + 
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(title = "IPTW权重分布（截尾后）", x = "权重值", y = "频数") +
  theme_minimal()

# ===================== 【关键修正】把权重合并到基线（用 dat_final！】=====================
dat_baseline_weighted <- dat_baseline %>%
  left_join(dat_final %>% select(hhidpn, iptw_weight, iptw_weight_trim), by = "hhidpn") %>%
  drop_na(iptw_weight_trim)

# ============================ 平衡检验 ================================
cov_vars <- c("Age", "Gender", "Urbanicity", "Education", "Marital_binary", 
              "Employment", "Physical_Activity", "Smoking", "Drinking", 
              "Wealth_quartile", "slope", "CumBMI")

# 未加权
dat_baseline_weighted <- dat_baseline %>%
  left_join(dat_final %>% select(hhidpn, iptw_weight_trim), by = "hhidpn") %>%
  drop_na(iptw_weight_trim)

# 1) 未加权
tab1 <- CreateTableOne(vars = cov_vars, strata = "BMI_quantiles_CV", 
                       data = dat_baseline_weighted, test = FALSE)

# 2) 加权 survey 设计
svy_design <- svydesign(ids = ~1, weights = ~iptw_weight_trim, 
                        data = dat_baseline_weighted)
tab2 <- svyCreateTableOne(vars = cov_vars, strata = "BMI_quantiles_CV", 
                          data = svy_design, test = FALSE)

# 查看结果
print("==== 未加权 ====")
print(tab1, smd = TRUE)

print("==== 加权后 ====")
print(tab2, smd = TRUE)

# ============================ 第二步：绘制Figure 2 Love Plot ==================
# 手动指定SMD值（确保为数值型）
smd_unweighted <- c(Age = 0.026, Gender = 0.208, Urbanicity = 0.033, Education = 0.129, Marital_binary = 0.096, Employment = 0.025, Physical_Activity = 0.102, Smoking = 0.082, Drinking = 0.123, Wealth_quartile = 0.187, slope = 0.134, CumBMI = 0.175)
smd_weighted <- c(Age = 0.010, Gender = 0.004, Urbanicity = 0.003, Education = 0.010, Marital_binary = 0.007, Employment = 0.004, Physical_Activity = 0.015, Smoking = 0.006, Drinking = 0.005, Wealth_quartile = 0.011, slope = 0.009, CumBMI = 0.014)

# 构建数据框并强制转换为数值型
smd_manual <- data.frame(
  协变量原始名 = names(smd_unweighted), 
  未加权SMD = as.numeric(smd_unweighted), 
  加权后SMD = as.numeric(smd_weighted))

# 协变量重命名映射
cov_rename <- c("Age" = "Age",
                "Gender" = "Gender",
                "Urbanicity" = "Urbanicity",
                "Education" = "Education level",
                "Marital_binary" = "Marital status",
                "Employment" = "Occupation",
                "Physical_Activity" = "Physical activity",
                "Smoking" = "Smoking status",
                "Drinking" = "Drinking status",
                "Wealth_quartile" = "Household wealth",
                "slope" = "BMI Slope",
                "CumBMI" = "Cumulative BMI")

# 重命名并清理数据
smd_clean <- smd_manual %>%
  mutate(协变量 = recode(协变量原始名, !!!cov_rename)) %>%
  select(-协变量原始名)

# 这个跑不通，后面那个能跑通
#p <- ggplot(smd_clean, aes(y = reorder(协变量, 未加权SMD))) +
#  geom_segment(aes(x = 未加权SMD, xend = 加权后SMD, yend = y),
#               color = "gray50", linetype = "solid", linewidth = 0.6) +
#  geom_point(aes(x = 未加权SMD), color = "black", size = 4, shape = 16) +
#  geom_point(aes(x = 加权后SMD), color = "black", size = 4, shape = 17) +
#  geom_vline(xintercept = 0.1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
#  labs(x = "Standardized Mean Difference", y = "", title = "United States (N=8,138)") +
#  theme_minimal() +
#  theme(
#    axis.text.y = element_text(size = 15, color = "black"),
#    axis.text.x = element_text(size = 15, color = "black"),
#    axis.title.x = element_text(size = 15, color = "black"),
#    plot.title = element_text(size = 15, face = "bold", hjust = 0),
#    panel.grid.minor = element_blank(),
#    panel.grid.major = element_blank(),
#    legend.position = "none"
#  ) +
#  scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05))


library(tidyverse)
smd_clean <- data.frame(
  协变量 = c("Age","Gender","Urbanicity","Education level","Marital status","Occupation",
          "Physical activity","Smoking status","Drinking status","Household wealth","BMI Slope","Cumulative BMI"),
  未加权SMD = c(0.026,0.208,0.033,0.129,0.096,0.025,0.102,0.082,0.123,0.187,0.134,0.175),
  加权后SMD = c(0.010,0.004,0.003,0.010,0.007,0.004,0.015,0.006,0.005,0.011,0.009,0.014)
)

# 🔥 完全统一另一个队列的样式 + 绝对不报错
p <- ggplot(smd_clean, aes(y = reorder(协变量, 未加权SMD))) +
  geom_segment(aes(x = 未加权SMD, xend = 加权后SMD),
               color = "gray50", linetype = "solid", linewidth = 0.6) +
  geom_point(aes(x = 未加权SMD), color = "black", size = 4, shape = 16) +
  geom_point(aes(x = 加权后SMD), color = "black", size = 4, shape = 17) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  labs(x = "Standardized Mean Difference", y = "", title = "United States (N=9,096)") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 15, color = "black"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none"
    # 完全去掉 margin —— 视觉仍然完全统一！
  ) +
  scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05))

print(p)

ggsave(filename = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\3. HRS\\2. Backups of study participants\\love_无网格线_20260409.png",
       plot = p, width = 7, height = 6, dpi = 350, device = "png")

# ========================== GEE + IPTW ========================================
get_gee_or <- function(gee_model, digits = 3) {
  # 提取模型系数的汇总结果
  coef_summary <- summary(gee_model)$coefficients
  or <- exp(coef_summary[, "Estimate"])
  # 计算95%置信区间（指数化置信区间）
  ci_lower <- exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std.err"])
  ci_upper <- exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std.err"])
  # 提取P值
  p_value <- coef_summary[, "Pr(>|W|)"]
  # 整合结果为数据框
  result <- data.frame(
    Variable = rownames(coef_summary),
    OR = round(or, digits),
    CI95 = paste0("(", round(ci_lower, digits), ", ", round(ci_upper, digits), ")"),
    P_value = round(p_value, digits)
  )
  # 重置行名
  rownames(result) <- NULL
  return(result)
}

# ============================ 1. 长期变异性_CV============================
dat_long$BMI_quantiles_CV <- factor(dat_long$BMI_quantiles_CV, 
                                    levels = c(1,2,3,4),
                                    labels = c("Q1","Q2","Q3","Q4"))
table(dat_long$BMI_quantiles_CV)
#Q1    Q2    Q3    Q4 
#12593 12417 12018 11827
table(dat_long$Frailty, useNA = "always")
#0     1      <NA> 
#33298 15557  0

dat_long <- dat_long %>% left_join(dat_final %>% select(hhidpn, iptw_weight_trim), by = "hhidpn")

gee_CV_1 <- geeglm(Frailty ~ BMI_quantiles_CV, 
                   id = hhidpn, 
                   weights = iptw_weight_trim,  # 传入IPTW截尾权重（加权分析）
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
gee_CV_2 <- geeglm(Frailty ~ BMI_quantiles_CV + Age + Gender, 
                   id = hhidpn, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + Age + Gender + Urbanicity + Education + Marital_binary + Employment + Physical_Activity + Smoking + Drinking + Wealth_quartile + slope + CumBMI, 
                   id = hhidpn, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_1 <- get_gee_or(gee_CV_1, digits = 2)
print(or_result_CV_1)
or_result_CV_2 <- get_gee_or(gee_CV_2, digits = 2)
print(or_result_CV_2)
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#Variable   OR         CI95 P_value
#1            (Intercept) 0.00       (0, 0)    0.00
#2     BMI_quantiles_CVQ2 1.17  (1.06, 1.3)    0.00
#3     BMI_quantiles_CVQ3 1.16 (1.04, 1.28)    0.01
#4     BMI_quantiles_CVQ4 1.56  (1.4, 1.73)    0.00

# ============================ 1. 长期变异性_CV + 共病 =========================
gee_CV_3_chronic <- geeglm(Frailty ~ BMI_quantiles_CV + Age + Gender + Urbanicity + Education + Marital_binary + Employment + Physical_Activity + Smoking + Drinking + Wealth_quartile + slope + CumBMI + n_chronic, 
                   id = hhidpn, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3_chronic <- get_gee_or(gee_CV_3_chronic, digits = 2)
print(or_result_CV_3_chronic)
#Variable                   OR         CI95 P_value
#1            (Intercept) 0.00       (0, 0)    0.00
#2     BMI_quantiles_CVQ2 1.16 (1.05, 1.29)    0.00
#3     BMI_quantiles_CVQ3 1.14 (1.03, 1.26)    0.01
#4     BMI_quantiles_CVQ4 1.53  (1.38, 1.7)    0.00

# ============================ 1. 长期变异性_CV + 排除用药史 =========================
names(dat_long)
table(dat_long$r7rxhibp, useNA = "always")      #
table(dat_long$r7rxdiab, useNA = "always")      #
table(dat_long$r7cncrchem, useNA = "always")      #
table(dat_long$r7rxlung, useNA = "always")      #
table(dat_long$r7rxangina, useNA = "always")      #
table(dat_long$r7rxchf, useNA = "always")      #
table(dat_long$r7rxhrtat, useNA = "always")      #
table(dat_long$r7rxheart, useNA = "always")      #
table(dat_long$r7rxpsych, useNA = "always")      #
table(dat_long$r7rxarthr, useNA = "always")      #

dat_clean <- dat_long[
  dat_long$r7rxhibp   == 0 &
    dat_long$r7rxdiab   == 0 &
    dat_long$r7cncrchem  == 0 &
    dat_long$r7rxlung  == 0 &
    dat_long$r7rxangina   == 0 &
    dat_long$r7rxchf  == 0 &
    dat_long$r7rxhrtat  == 0 &
    dat_long$r7rxheart  == 0 &
    dat_long$r7rxpsych  == 0,]
  #& dat_long$r7rxarthr  == 0,
gee_CV_3_chronic <- geeglm(Frailty ~ BMI_quantiles_CV + Age + Gender + Urbanicity + Education + Marital_binary + Employment + Physical_Activity + Smoking + Drinking + Wealth_quartile + slope + CumBMI + n_chronic, 
                           id = hhidpn, 
                           weights = iptw_weight_trim,
                           data = dat_clean, 
                           family = binomial(link = "logit"),
                           corstr = "exchangeable")
or_result_CV_3_chronic <- get_gee_or(gee_CV_3_chronic, digits = 2)
print(or_result_CV_3_chronic)
#Variable                   OR         CI95 P_value
#1            (Intercept) 0.00       (0, 0)    0.00
#2     BMI_quantiles_CVQ2 1.07 (0.91, 1.27)    0.41
#3     BMI_quantiles_CVQ3 1.08 (0.92, 1.28)    0.33
#4     BMI_quantiles_CVQ4 1.41 (1.19, 1.67)    0.00

# =================== 在这里插入：Q1-Q4 样本量 + case 数统计 ===================
data_HRS9 <- readRDS("D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\3. HRS\\2. Backups of study participants\\HRS_Variability + Cumulative_20260401_完成全部定义.rds")
table(data_HRS9$Frailty_progression)
table(data_HRS9$BMI_quantiles_CV, useNA = "always")
#1    2    3    4    <NA> 
#2274 2274 2274 2274 0

data_HRS9_clean <- subset(data_HRS9,
                          r7rxhibp == 0 & r7rxdiab == 0 & r7cncrchem == 0 &
                            r7rxlung == 0 & r7rxangina == 0 & r7rxchf == 0 &
                            r7rxhrtat == 0 & r7rxheart == 0 & r7rxpsych == 0)
names(data_HRS9_clean)
table(data_HRS9_clean$Frailty_progression)
table(data_HRS9_clean$BMI_quantiles_CV, useNA = "always")
#1    2    3     4     <NA> 
#1093 1060 1026  953   0

#描述性统计
table(data_HRS9_clean$Frailty_progression, data_HRS9_clean$BMI_quantiles_CV)
data_HRS9_clean %>% group_by(BMI_quantiles_CV) %>% summarise(total_ttime = sprintf("%.2f", sum(followup_time, na.rm = TRUE)))

surv_summary <- data_HRS9_clean %>% 
  group_by(BMI_quantiles_CV) %>% 
  summarise(总样本数 = n(),
            事件数 = sum(Frailty_progression == 1, na.rm = TRUE),
            总暴露时间 = sum(followup_time, na.rm = TRUE),
            发病率 = sprintf("%.3f", 事件数/总暴露时间 * 1000))
print(surv_summary)
#BMI_quantiles_CV 总样本数 事件数 总暴露时间 发病率
#               1     1093    703      10382 67.713
#               2     1060    655       9912 66.082
#               3     1026    656       9348 70.175
#               4      953    634       8478 74.782

# ============================ 2. 长期变异性_SD============================
dat_long$BMI_quantiles_SD <- factor(dat_long$BMI_quantiles_SD, levels = c(1, 2, 3, 4), labels = c("Q1", "Q2", "Q3", "Q4"))
table(dat_long$BMI_quantiles_SD)
#Q1    Q2    Q3    Q4 
#12481 12344 12044 11986

gee_SD_1 <- geeglm(Frailty ~ BMI_quantiles_SD, 
                   id = hhidpn, 
                   weights = iptw_weight_trim,  # 传入IPTW截尾权重（加权分析）
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
gee_SD_2 <- geeglm(Frailty ~ BMI_quantiles_SD + Age + Gender, 
                   id = hhidpn, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
gee_SD_3 <- geeglm(Frailty ~ BMI_quantiles_SD + Age + Gender + Urbanicity + Education + Marital_binary + Employment + Physical_Activity + Smoking + Drinking + Wealth_quartile + slope + CumBMI, 
                   id = hhidpn, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")

or_result_SD_1 <- get_gee_or(gee_SD_1, digits = 2)
print(or_result_SD_1)
or_result_SD_2 <- get_gee_or(gee_SD_2, digits = 2)
print(or_result_SD_2)
or_result_SD_3 <- get_gee_or(gee_SD_3, digits = 2)
print(or_result_SD_3)
#Variable   OR         CI95 P_value
#1            (Intercept) 0.00       (0, 0)    0.00
#2     BMI_quantiles_SDQ2 1.16 (1.04, 1.28)    0.01
#3     BMI_quantiles_SDQ3 1.22  (1.1, 1.35)    0.00
#4     BMI_quantiles_SDQ4 1.62 (1.46, 1.81)    0.00

# ============================ 3. 长期变异性_ARV ============================
dat_long$BMI_quantiles_ARV <- factor(dat_long$BMI_quantiles_ARV, levels = c(1, 2, 3, 4), labels = c("Q1", "Q2", "Q3", "Q4"))
table(dat_long$BMI_quantiles_ARV)
#Q1    Q2    Q3    Q4 
#12525 12311 12079 11940
table(dat_long$Frailty)

gee_ARV_1 <- geeglm(Frailty ~ BMI_quantiles_ARV, 
                    id = hhidpn, 
                    weights = iptw_weight_trim,  # 传入IPTW截尾权重（加权分析）
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
gee_ARV_2 <- geeglm(Frailty ~ BMI_quantiles_ARV + Age + Gender, 
                    id = hhidpn, 
                    weights = iptw_weight_trim,
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
gee_ARV_3 <- geeglm(Frailty ~ BMI_quantiles_ARV + Age + Gender + Urbanicity + Education + Marital_binary + Employment + Physical_Activity + Smoking + Drinking + Wealth_quartile + slope + CumBMI, 
                    id = hhidpn, 
                    weights = iptw_weight_trim,
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")

or_result_ARV_1 <- get_gee_or(gee_ARV_1, digits = 2)
print(or_result_ARV_1)
or_result_ARV_2 <- get_gee_or(gee_ARV_2, digits = 2)
print(or_result_ARV_2)
or_result_ARV_3 <- get_gee_or(gee_ARV_3, digits = 2)
print(or_result_ARV_3)
#2    BMI_quantiles_ARVQ2 1.19 (1.07, 1.32)    0.00
#3    BMI_quantiles_ARVQ3 1.28 (1.15, 1.42)    0.00
#4    BMI_quantiles_ARVQ4 1.60 (1.43, 1.78)    0.00

# ============================ 4. 长期变异性_VIM ============================
dat_long$BMI_quantiles_VIM <- factor(dat_long$BMI_quantiles_VIM, levels = c(1, 2, 3, 4), labels = c("Q1", "Q2", "Q3", "Q4"))
table(dat_long$BMI_quantiles_VIM, useNA = "always")
gee_VIM_1 <- geeglm(Frailty ~ BMI_quantiles_VIM, 
                    id = hhidpn, 
                    weights = iptw_weight_trim,  # 传入IPTW截尾权重（加权分析）
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
gee_VIM_2 <- geeglm(Frailty ~ BMI_quantiles_VIM + Age + Gender, 
                    id = hhidpn, 
                    weights = iptw_weight_trim,
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
gee_VIM_3 <- geeglm(Frailty ~ BMI_quantiles_VIM + Age + Gender + Urbanicity + Education + Marital_binary + Employment + Physical_Activity + Smoking + Drinking + Wealth_quartile + slope + CumBMI, 
                    id = hhidpn, 
                    weights = iptw_weight_trim,
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
or_result_VIM_1 <- get_gee_or(gee_VIM_1, digits = 2)
print(or_result_VIM_1)
or_result_VIM_2 <- get_gee_or(gee_VIM_2, digits = 2)
print(or_result_VIM_2)
or_result_VIM_3 <- get_gee_or(gee_VIM_3, digits = 2)
print(or_result_VIM_3)
#2    BMI_quantiles_VIMQ2 1.12 (1.01, 1.24)    0.04
#3    BMI_quantiles_VIMQ3 1.15 (1.04, 1.28)    0.01
#4    BMI_quantiles_VIMQ4 1.51 (1.36, 1.68)    0.00

# 完成GEE和IPTW权重计算的数据集备份
saveRDS(dat_long, file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\3. HRS\\2. Backups of study participants\\完成GEE和IPTW权重计算_20260409.rds")
#saveRDS(dat_long, file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\3. HRS\\2. Backups of study participants\\完成GEE和IPTW权重计算_20260401.rds")
#names(dat_long)



# ---------------------------- 20260409 不IPTW加权 -----------------------------
#dat_long$BMI_quantiles_CV <- factor(dat_long$BMI_quantiles_CV, levels = c("1", "2", "3", "4"), labels = c("Q1", "Q2", "Q3", "Q4"))
gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                   id = hhidpn, 
                   #weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#2    BMI_quantiles_CVQ2 1.17  (0.97, 1.4)    0.10
#3    BMI_quantiles_CVQ3 1.49 (1.24, 1.79)    0.00
#4    BMI_quantiles_CVQ4 1.57 (1.32, 1.88)    0.00





