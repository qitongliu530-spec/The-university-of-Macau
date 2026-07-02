library(haven)
library(dplyr)
library(tidyverse)
library(purrr)
library(summarytools)
library(foreign)   #用于导出SAS XPT格式
library(writexl)
library(gee)
library(geepack)
library(tidyr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(corrr)
library(openxlsx)
library(janitor)
library(scales)     #小数点
library(zoo)
library(rstatix)
library(tableone)
library(mice)
library(stringr)
library(psych)
library(viridis)   
library(ggtext)    
library(scales) 
library(knitr)
library(kableExtra)
library(survival)
library(survminer)
library(nnet)       # IPTW拟合多分类逻辑回归
library(MatchIt)    # IPTW 后续平衡检验（可选，推荐）
library(cobalt)     # 绘制 IPTW的Love plot
library(survey)
library(minpack.lm, lib.loc = "D:/MyRPackages")

data_SHARE9 <- readRDS(file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\1. SHARE\\2. Backups of study participants\\SHARE_Variability + Cumulative_20251128_完成X定义.rds")
# ==============================================================================
# (二) 数据分析 Table 6 GEE analysis
# ==============================================================================
# 复制原始数据，避免修改原数据
data_SHARE9_clean <- data_SHARE9
names(data_SHARE9)

# 转换格式
dat_long <- data_SHARE9_clean %>%
  select(mergeid,
         inw7, inw8, inw9, r7iwstat, r8iwstat, r9iwstat,
         Frailty_w7, Frailty_w8, Frailty_w9,
         BMI_quantiles_CV, BMI_quantiles_SD, BMI_quantiles_ARV, BMI_quantiles_VIM,
         age, Gender, Rrbanicity, educate, marital_st, employ_st, physical_activity, smoke, drink_ever_3, wealth, slope, CumBMI, n_chronic,
         r4rxhibp, r4rxdiab, r4rxlung, r4rxheart, r4rxpsych, r5rxinflm,
         cancer, heart, stroke
  ) %>%
  # 第一步拆分：仅处理随波次变化的结局变量（带_w后缀）
  pivot_longer(cols = starts_with("Frailty_w"),  # 仅拆分结局变量，精准指定
               names_to = c(".value", "wave"),
               names_pattern = "(.*)_w(\\d+)",
               values_drop_na = TRUE  # 剔除结局变量的缺失值，减少后续筛选
  ) %>%
  # 第二步拆分：仅处理随访变量，避免纳入非拆分列
  pivot_longer(
    cols = c(inw7, inw8, inw9, r7iwstat, r8iwstat, r9iwstat), 
    names_to = c("var_prefix", "wave_num"),
    names_pattern = "(.*)(\\d+)",  # 匹配“前缀+数字”（如inw7→inw+7，r7iwstat→r+7）
    values_drop_na = TRUE
  ) %>%
  # 转换波次为数值型，筛选波次一致的记录
  mutate(wave = as.numeric(wave), wave_num = as.numeric(wave_num)
  ) %>% filter(wave == wave_num) %>%
  # 重塑随访变量为宽格式
  pivot_wider(names_from = var_prefix, values_from = value
  ) %>%
  # 数据清洗与重命名
  rename(
    follow_flag = inw,               # follow_flag=1：该波次有随访计划
    interview_status = r,            # interview_status=1：该波次实际完成随访
    follow_up_wave = wave
  ) %>%
  mutate(
    mergeid = factor(mergeid),
    follow_up_wave = as.numeric(follow_up_wave)
  ) %>%
  # 筛选有效随访：同时满足“有随访计划”和“实际完成随访”
  filter(follow_flag == 1 & interview_status == 1) %>%
  # 剔除结局/暴露的缺失值
  drop_na(Frailty, BMI_quantiles_CV) %>%
  # 移除多余列
  select(-wave_num)

# 查看转换后的数据结构
head(dat_long)
table(dat_long$mergeid) %>% head()

# ============================ IPTW权重计算 ====================================
dat_baseline <- dat_long %>%
  group_by(mergeid) %>%
  slice(1) %>%  # 取每个个体的首次观测作为基线
  ungroup() %>%
  select(mergeid, BMI_quantiles_CV, age, Gender, Rrbanicity, educate, marital_st, 
         employ_st, physical_activity, smoke, drink_ever_3, wealth, slope, CumBMI)

# 拟合多分类逻辑回归模型，估计倾向得分
iptw_m <- multinom(BMI_quantiles_CV ~ age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                   data = dat_baseline, 
                   trace = FALSE)

# 预测每个个体的倾向得分（条件概率）
dat_baseline$propensity_scores <- predict(iptw_m, type = "probs")  
summary(dat_baseline$propensity_scores)

# 定义暴露变量的因子水平（根据实际情况调整，此处为示例）
unique(dat_baseline$BMI_quantiles_CV)
dat_baseline$BMI_quantiles_CV <- factor(dat_baseline$BMI_quantiles_CV)
exposure_levels <- levels(dat_baseline$BMI_quantiles_CV)
cat("暴露变量的水平：", exposure_levels, "\n")

# 计算IPTW权重
dat_baseline$iptw_weight <- NA     # 初始化权重列
# 循环计算每个暴露组的权重
for (i in 1:length(exposure_levels)) {  
  dat_baseline$iptw_weight[dat_baseline$BMI_quantiles_CV == exposure_levels[i]] <- 
    1 / dat_baseline$propensity_scores[dat_baseline$BMI_quantiles_CV == exposure_levels[i], i]
}

# 权重截尾（Trimming）：处理极端权重
quantile_99 <- quantile(dat_baseline$iptw_weight, 0.99, na.rm = TRUE)
dat_baseline$iptw_weight_trim <- ifelse(dat_baseline$iptw_weight > quantile_99, quantile_99, dat_baseline$iptw_weight)

# 查看权重分布（原始vs截尾）
cat("原始权重统计：\n")
summary(dat_baseline$iptw_weight)
cat("截尾后权重统计：\n")
summary(dat_baseline$iptw_weight_trim)

ggplot(dat_baseline, aes(x = iptw_weight_trim)) + geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(title = "IPTW权重分布（截尾后）", x = "权重值", y = "频数") +
  theme_minimal()

# 将权重合并回长格式数据dat_long
dat_long <- dat_long %>% left_join(dat_baseline %>% select(mergeid, iptw_weight_trim), by = "mergeid")

# ============================ 协变量平衡检验：svyCreateTableOne 方案 ============================
cov_vars <- c("age", "Gender", "Rrbanicity", "educate", "marital_st", "employ_st", "physical_activity", "smoke", "drink_ever_3", "wealth", "slope", "CumBMI")

# 数据清理
dat_baseline_clean <- dat_baseline %>% drop_na(iptw_weight_trim) %>% mutate(BMI_quantiles_CV = factor(BMI_quantiles_CV))

# 构建加权设计
svy_design <- svydesign(ids = ~1, weights = ~iptw_weight_trim, data = dat_baseline_clean)

# 生成未加权平衡表并提取SMD
table_unweighted <- CreateTableOne(vars = cov_vars, strata = "BMI_quantiles_CV", data = dat_baseline_clean, test = FALSE)
smd_unweighted <- print(table_unweighted, smd = TRUE)[, "SMD"]  # 未加权SMD

# 生成加权平衡表并提取SMD
table_weighted <- svyCreateTableOne(vars = cov_vars, strata = "BMI_quantiles_CV", data = svy_design, test = FALSE)
smd_weighted <- print(table_weighted, smd = TRUE)[, "SMD"]  # 加权SMD

smd_result <- data.frame(
  协变量 = names(smd_unweighted),
  未加权SMD = as.numeric(smd_unweighted),
  加权后SMD = as.numeric(smd_weighted),
  stringsAsFactors = FALSE)

print(smd_result, digits = 3)

# ============================ 第二步：绘制Figure 2 Love Plot ==================
# 手动指定SMD值（确保为数值型）
smd_unweighted <- c(age = 0.025, Gender = 0.126, Rrbanicity = 0.017, educate = 0.203, marital_st = 0.038, employ_st = 0.043, physical_activity = 0.102, smoke = 0.115, drink_ever_3 = 0.213, wealth = 0.222, slope = 0.120, CumBMI = 0.208)
smd_weighted <- c(age = 0.007, Gender = 0.011, Rrbanicity = 0.003, educate = 0.006, marital_st = 0.010, employ_st = 0.002, physical_activity = 0.006, smoke = 0.004, drink_ever_3 = 0.002, wealth = 0.005, slope = 0.012, CumBMI = 0.004)

# 构建数据框并强制转换为数值型
smd_manual <- data.frame(协变量原始名 = names(smd_unweighted), 未加权SMD = as.numeric(smd_unweighted), 加权后SMD = as.numeric(smd_weighted))

# 协变量重命名映射
cov_rename <- c("age" = "Age",
                "Gender" = "Gender",
                "Rrbanicity" = "Urbanicity",
                "educate" = "Education level",
                "marital_st" = "Marital status",
                "employ_st" = "Occupation",
                "physical_activity" = "Physical activity",
                "smoke" = "Smoking status",
                "drink_ever_3" = "Drinking status",
                "wealth" = "Household wealth",
                "slope" = "BMI Slope",
                "CumBMI" = "Cumulative BMI")

# 重命名并清理数据
smd_clean <- smd_manual %>% mutate(协变量 = recode(协变量原始名, !!!cov_rename)) %>%
  select(-协变量原始名) %>%
  mutate(未加权SMD = as.numeric(未加权SMD), 加权后SMD = as.numeric(加权后SMD))

str(smd_clean)

ggsave(
  filename = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\1. SHARE\\2. Backups of study participants\\love_无网格线_20260310.png",
  plot = ggplot(smd_clean, aes(y = reorder(协变量, 未加权SMD))) +
    geom_segment(aes(x = 未加权SMD, xend = 加权后SMD, yend = after_stat(y)), color = "gray50", linetype = "solid", linewidth = 0.6) +
    geom_point(aes(x = 未加权SMD), color = "darkred", size = 3, shape = 16) +
    geom_point(aes(x = 加权后SMD), color = "darkblue", size = 3, shape = 17) +
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    labs(x = "Standardized Mean Difference", y = "", title = "C. European countries (N=18,750)") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12, color = "black", face = "bold"),
      axis.text.x = element_text(size = 13, color = "black", face = "bold"),
      axis.title.x = element_text(size = 10, color = "#666666", face = "bold"),
      # ✅ 核心修改：图标题字号从10增大至13，保留加粗/左对齐
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      panel.grid.minor = element_blank(),  # 隐藏所有次要网格线
      panel.grid.major = element_blank(),  # 隐藏所有主要网格线
      legend.position = "none",
      plot.margin = margin(t = 5, r = 5, b = 3, l = 2, unit = "pt")
    ) +
    scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)),
  width = 5,         # 图片宽度（英寸）
  height = 6,        # 图片高度（英寸）
  dpi = 350,         # 分辨率
  device = "png"     # 导出格式
)

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
dat_long$BMI_quantiles_CV <- factor(dat_long$BMI_quantiles_CV)

gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                   id = mergeid, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#2     BMI_quantiles_CV2 1.07 (0.96, 1.19)    0.21
#3     BMI_quantiles_CV3 1.37 (1.24, 1.52)    0.00
#4     BMI_quantiles_CV4 1.71  (1.54, 1.9)    0.00

# ============================ 1. 长期变异性_CV + 共病 =========================
dat_long$BMI_quantiles_CV <- factor(dat_long$BMI_quantiles_CV)

gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI + n_chronic, 
                   id = mergeid, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#2     BMI_quantiles_CV2 1.05 (0.94, 1.17)    0.35
#3     BMI_quantiles_CV3 1.34 (1.21, 1.49)    0.00
#4     BMI_quantiles_CV4 1.75 (1.57, 1.94)    0.00

# ============================ 1. 长期变异性_CV + 排除用药史 =========================
names(dat_long)
table(dat_long$r4rxhibp, useNA = "always")      #
table(dat_long$r4rxdiab, useNA = "always")      #
table(dat_long$r4rxlung, useNA = "always")      #
table(dat_long$r4rxheart, useNA = "always")      #
table(dat_long$r4rxpsych, useNA = "always")      #
table(dat_long$r5rxinflm, useNA = "always")      #

dat_clean <- dat_long[
  dat_long$r4rxhibp   == 0 &
    dat_long$r4rxdiab   == 0 &
    dat_long$r4rxlung   == 0 &
    dat_long$r4rxheart  == 0 &
    dat_long$r4rxpsych  == 0,] # & dat_long$r5rxinflm  == 0,]

cat("筛选前总样本数：", nrow(dat_long), "\n")     # 31813
cat("剔除所有用药者后样本数：", nrow(dat_clean), "\n")     # 17755
cat("本次剔除人数：", nrow(dat_long) - nrow(dat_clean), "\n")     # 14058

gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI + n_chronic, 
                   id = mergeid, 
                   weights = iptw_weight_trim,
                   data = dat_clean, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#2     BMI_quantiles_CV2 1.05 (0.89, 1.24)    0.56
#3     BMI_quantiles_CV3 1.44 (1.22, 1.69)    0.00
#4     BMI_quantiles_CV4 1.90 (1.62, 2.24)    0.00

# =================== 在这里插入：Q1-Q4 样本量 + case 数统计 ===================
data_SHARE9 <- readRDS(file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\1. SHARE\\2. Backups of study participants\\SHARE_Variability + Cumulative_20251128_完成X定义.rds")
table(data_SHARE9$Frailty)
table(data_SHARE9$BMI_quantiles_CV, useNA = "always")
#1    2    3    4    <NA> 
#4688 4688 4687 4687    0

data_SHARE9_clean <- subset(data_SHARE9, r4rxhibp == 0 & r4rxdiab == 0 & r4rxlung == 0 & r4rxheart == 0 & r4rxpsych == 0)
names(data_SHARE9_clean)
table(data_SHARE9_clean$Frailty)
table(data_SHARE9_clean$BMI_quantiles_CV, useNA = "always")
#1    2    3     4     <NA> 
#2709 2649 2502 2487    0

#描述性统计
table(data_SHARE9_clean$Frailty, data_SHARE9_clean$BMI_quantiles_CV)
data_SHARE9_clean %>% group_by(BMI_quantiles_CV) %>% summarise(total_ttime = sprintf("%.2f", sum(survival衰弱, na.rm = TRUE)))

surv_summary <- data_SHARE9_clean %>% 
  group_by(BMI_quantiles_CV) %>% 
  summarise(总样本数 = n(),
            事件数 = sum(Frailty == 1, na.rm = TRUE),
            总暴露时间 = sum(survival衰弱, na.rm = TRUE),
            发病率 = sprintf("%.3f", 事件数/总暴露时间 * 1000))
print(surv_summary)
#BMI_quantiles_CV 总样本数 事件数 总暴露时间 发病率
#<int>    <int>  <int>      <dbl> <chr> 
#  1                1     2709    402     15130. 26.569
#2                2     2649    445     14407. 30.887
#3                3     2502    555     13129. 42.272
#4                4     2487    698     12090. 57.731

# ============================ 2. 长期变异性_SD============================
gee_SD_1 <- geeglm(Frailty ~ BMI_quantiles_SD, 
                   id = mergeid, 
                   weights = iptw_weight_trim,  # 传入IPTW截尾权重（加权分析）
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
gee_SD_2 <- geeglm(Frailty ~ BMI_quantiles_SD + age + Gender, 
                   id = mergeid, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
gee_SD_3 <- geeglm(Frailty ~ BMI_quantiles_SD + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                   id = mergeid, 
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


# ============================ 3. 长期变异性_ARV ============================
gee_ARV_1 <- geeglm(Frailty ~ BMI_quantiles_ARV, 
                    id = mergeid, 
                    weights = iptw_weight_trim,  # 传入IPTW截尾权重（加权分析）
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
gee_ARV_2 <- geeglm(Frailty ~ BMI_quantiles_ARV + age + Gender, 
                    id = mergeid, 
                    weights = iptw_weight_trim,
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
gee_ARV_3 <- geeglm(Frailty ~ BMI_quantiles_ARV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                    id = mergeid, 
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


# ============================ 4. 长期变异性_VIM ============================
gee_VIM_1 <- geeglm(Frailty ~ BMI_quantiles_VIM, 
                    id = mergeid, 
                    weights = iptw_weight_trim,  # 传入IPTW截尾权重（加权分析）
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
gee_VIM_2 <- geeglm(Frailty ~ BMI_quantiles_VIM + age + Gender, 
                    id = mergeid, 
                    weights = iptw_weight_trim,
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
gee_VIM_3 <- geeglm(Frailty ~ BMI_quantiles_VIM + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                    id = mergeid, 
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


# 完成GEE和IPTW权重计算的数据集备份
saveRDS(dat_long, 
        file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\1. SHARE\\2. Backups of study participants\\完成GEE和IPTW权重计算_20260411.rds")
dat_long <- readRDS("D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\1. SHARE\\2. Backups of study participants\\完成GEE和IPTW权重计算_20260411.rds")



























