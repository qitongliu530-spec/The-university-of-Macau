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
library(survey)     # 处理复杂调查与加权数据
library(minpack.lm, lib.loc = "D:/MyRPackages")
#save.image("D:/0. 科研文件/2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong/my_workspace.RData")

data_KLoSA9 <- readRDS(file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\2. KLoSA\\2. Backups of study participants\\KLoSA_Variability + Cumulative_20260412_完成X定义.rds")
names(data_KLoSA9)
# ============= 基线表 =========================================================20260412
continuous_vars <- c("age", "r1bmi", "CV_bmi", "FI_w4")
categorical_vars <- c("Gender", "Rrbanicity", "marital_st", "educate", "employ_st", "physical_activity", "smoke", "drink_ever_3", "wealth")  
# ==================== 连续变量：中位数 (P25, P75) ====================
cont_desc <- lapply(data_KLoSA9[continuous_vars], function(x) {
  med <- round(median(x, na.rm=TRUE), 2)
  p25 <- round(quantile(x, 0.25, na.rm=TRUE), 2)
  p75 <- round(quantile(x, 0.75, na.rm=TRUE), 2)
  med_fmt <- sprintf("%.2f", med)
  p25_fmt <- sprintf("%.2f", p25)
  p75_fmt <- sprintf("%.2f", p75)
  paste0(med_fmt, " [", p25_fmt, ", ", p75_fmt, "]")
})
cont_df <- data.frame(Variable = names(cont_desc), Value = unlist(cont_desc))
# ==================== 分类变量：n (百分比 2 位) ====================
cat_list <- list()
for (var in categorical_vars) {
  tbl <- table(data_KLoSA9[[var]])
  prop <- round(prop.table(tbl) * 100, 2)  # 强制 2 位小数
  cat_list[[var]] <- data.frame(
    Variable = paste0(var, " (%)"),
    Level = names(tbl),
    Value = paste0(tbl, " (", sprintf("%.2f", prop), ")"))}
cat_df <- do.call(rbind, cat_list)

# ==================== 合并输出 ====================
cat("==================== 最终基线特征表 ====================\n")
cat("\n📊 连续变量（中位数 [P25, P75]）：\n")
print(cont_df, row.names = FALSE)
cat("\n分类变量（n (%)）：\n")
print(cat_df, row.names = FALSE)

write_xlsx(cat_df, path = "D:/0. 科研文件/3. 衰弱患病率_Qitong&Simu&Yuding/2. Backups of study participants/HRS/11111111111111111111111111.xlsx",
           col_names = TRUE)


# ==============================================================================
# (二) 数据分析 Table 2d GEE analysis with IPTW + Love Plot
# ==============================================================================
# 复制原始数据，避免修改原数据
data_KLoSA9_clean <- data_KLoSA9
names(data_KLoSA9_clean)

# 转换格式
dat_long <- data_KLoSA9_clean %>%
  select(pid,
         inw4, inw5, inw6, inw7, inw8, inw9, r4iwstat, r5iwstat, r6iwstat, r7iwstat, r8iwstat, r9iwstat,
         Frailty_w4, Frailty_w5, Frailty_w6, Frailty_w7, Frailty_w8, Frailty_w9,
         BMI_quantiles_CV, BMI_quantiles_SD, BMI_quantiles_ARV, BMI_quantiles_VIM,
         CV_bmi, 
         r1rxhibp, r1rxdiab, r1rxcancr, r1rxlung, r1rxheart, r1rxpsych, r1rxarthr, 
         age, Gender, Rrbanicity, educate, marital_st, employ_st, physical_activity, smoke, drink_ever_3, wealth, slope, CumBMI, n_chronic,
         cancer, stroke, heart
         ) %>%
  # 第一步拆分：仅处理随波次变化的结局变量（带_w后缀）
  pivot_longer(cols = starts_with("Frailty_w"),  # 仅拆分结局变量，精准指定
               names_to = c(".value", "wave"),
               names_pattern = "(.*)_w(\\d+)",
               values_drop_na = TRUE  # 剔除结局变量的缺失值，减少后续筛选
  ) %>%
  # 第二步拆分：仅处理随访变量，避免纳入非拆分列
  pivot_longer(
    cols = c(inw4, inw5, inw6, inw7, inw8, inw9, r4iwstat, r5iwstat, r6iwstat, r7iwstat, r8iwstat, r9iwstat), 
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
    pid = factor(pid),
    follow_up_wave = as.numeric(follow_up_wave)
  ) %>%
  # 筛选有效随访：同时满足“有随访计划”和“实际完成随访”（根据你的业务规则调整）
  filter(follow_flag == 1 & interview_status == 1) %>%
  # 剔除结局/暴露的缺失值
  drop_na(Frailty, BMI_quantiles_CV) %>%
  # 移除多余列
  select(-wave_num)

# 查看转换后的数据结构
head(dat_long)
table(dat_long$pid) %>% head()

# ============================ IPTW权重计算 ====================================
dat_baseline <- dat_long %>%
  group_by(pid) %>%
  filter(row_number() == 1) %>%  # 👈 替换 slice
  ungroup() %>%
  select(pid, BMI_quantiles_CV, age, Gender, Rrbanicity, educate, marital_st, 
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
dat_long <- dat_long %>% left_join(dat_baseline %>% select(pid, iptw_weight_trim), by = "pid")

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
smd_unweighted <- c(age = 0.199, Gender = 0.124, Rrbanicity = 0.163, educate = 0.181, marital_st = 0.107, employ_st = 0.080, physical_activity = 0.164, smoke = 0.098, drink_ever_3 = 0.118, wealth = 0.157, slope = 0.052, CumBMI = 0.125)
smd_weighted <- c(age = 0.016, Gender = 0.012, Rrbanicity = 0.009, educate = 0.013, marital_st = 0.010, employ_st = 0.008, physical_activity = 0.007, smoke = 0.012, drink_ever_3 = 0.010, wealth = 0.014, slope = 0.009, CumBMI = 0.003)

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

p <- ggplot(smd_clean, aes(y = reorder(协变量, 未加权SMD))) +
  geom_segment(aes(x = 未加权SMD, xend = 加权后SMD, yend = after_stat(y)), 
               color = "gray50", linetype = "solid", linewidth = 0.6) +
  geom_point(aes(x = 未加权SMD), color = "black", size = 4, shape = 16) +
  geom_point(aes(x = 加权后SMD), color = "black", size = 4, shape = 17) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  labs(x = "Standardized Mean Difference", y = "", title = "South Korea (N=5,330)") +
  theme_minimal() +
  theme(
    # 👇 把所有 margin 换成 ggplot2::margin 强制指定，彻底解决冲突
    axis.text.y = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 15, color = "black", 
                                margin = ggplot2::margin(t = 10)),  # 👈 修复
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    plot.margin = ggplot2::margin(t = 7.5, r = 5, b = 7, l = 2, "pt")  # 👈 修复
  ) + scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05))

print(p)
ggsave(
  filename = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\2. KLoSA\\2. Backups of study participants\\love_无网格线_20260412.png",
  plot = p,
  width = 7,
  height = 6,
  dpi = 350,
  device = "png")

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
                                    levels = c("1", "2", "3", "4"), 
                                    labels = c("Q1", "Q2", "Q3", "Q4"))
gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                   id = pid, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#2    BMI_quantiles_CVQ2 1.10 (0.91, 1.33)    0.32
#3    BMI_quantiles_CVQ3 1.40 (1.17, 1.68)    0.00
#4    BMI_quantiles_CVQ4 1.48 (1.24, 1.78)    0.00

# ============================ 1. 长期变异性_CV + 共病 =========================
gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI + n_chronic, 
                   id = pid, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#2    BMI_quantiles_CVQ2 1.09  (0.9, 1.31)    0.39
#3    BMI_quantiles_CVQ3 1.34 (1.12, 1.61)    0.00
#4    BMI_quantiles_CVQ4 1.46 (1.22, 1.75)    0.00

# ============================ 1. 长期变异性_CV + 排除用药史 =========================
names(dat_long)
table(dat_long$r1rxhibp, useNA = "always")      # 0-19981 1-6871 NA-0
table(dat_long$r1rxdiab, useNA = "always")      # 0-24259 1-2593 NA-0
table(dat_long$r1rxcancr, useNA = "always")     # 0-26672 1-180 NA-0
table(dat_long$r1rxlung, useNA = "always")      # 0-26622 1-230 NA-0
table(dat_long$r1rxheart, useNA = "always")     # 0-25993 1-859 NA-0
table(dat_long$r1rxpsych, useNA = "always")     # 0-26605 1-247 NA-0
table(dat_long$r1rxarthr, useNA = "always")     # 0-23934 1-2918 NA-0

dat_clean <- dat_long[
  dat_long$r1rxhibp   == 0 &
    dat_long$r1rxdiab   == 0 &
    dat_long$r1rxcancr  == 0 &
    dat_long$r1rxlung   == 0 &
    dat_long$r1rxheart  == 0 &
    dat_long$r1rxpsych  == 0,]   # & dat_long$r1rxarthr  == 0,]

cat("筛选前总样本数：", nrow(dat_long), "\n")     # 26852 
cat("剔除所有用药者后样本数：", nrow(dat_clean), "\n")     # 
cat("本次剔除人数：", nrow(dat_long) - nrow(dat_clean), "\n")     # 

gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI + n_chronic, 
                   id = pid, 
                   weights = iptw_weight_trim,
                   data = dat_clean, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#2    BMI_quantiles_CVQ2 1.12 (0.86, 1.45)    0.41
#3    BMI_quantiles_CVQ3 1.33 (1.03, 1.72)    0.03
#4    BMI_quantiles_CVQ4 1.54  (1.2, 1.98)    0.00

# =================== 在这里插入：Q1-Q4 样本量 + case 数统计 ===================
data_KLoSA9 <- readRDS(file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\2. KLoSA\\2. Backups of study participants\\KLoSA_Variability + Cumulative_20260412_完成X定义.rds")
data_KLoSA9_clean <- data_KLoSA9[
  data_KLoSA9$r1rxhibp   == 0 &
    data_KLoSA9$r1rxdiab   == 0 &
    data_KLoSA9$r1rxcancr  == 0 &
    data_KLoSA9$r1rxlung   == 0 &
    data_KLoSA9$r1rxheart  == 0 &
    data_KLoSA9$r1rxpsych  == 0,]   # & data_KLoSA9$r1rxarthr  == 0,]

#描述性统计
table(data_KLoSA9_clean$Frailty, data_KLoSA9_clean$BMI_quantiles_CV)
data_KLoSA9_clean %>% group_by(BMI_quantiles_CV) %>% summarise(total_ttime = sprintf("%.2f", sum(survival衰弱, na.rm = TRUE)))

surv_summary <- data_KLoSA9_clean %>% 
  group_by(BMI_quantiles_CV) %>% 
  summarise(总样本数 = n(),
            事件数 = sum(Frailty == 1, na.rm = TRUE),
            总暴露时间 = sum(survival衰弱, na.rm = TRUE),
            发病率 = sprintf("%.3f", 事件数/总暴露时间 * 1000))
print(surv_summary)
# A tibble: 4 × 5
#BMI_quantiles_CV 总样本数 事件数 总暴露时间 发病率
#<int>    <int>  <int>      <dbl> <chr> 
#               1      884    137      9098. 15.058
#               2      874    178      8574. 20.761
#               3      846    194      8096. 23.962
#               4      851    259      7543. 34.336

# 你原来的模型
gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI + n_chronic, 
                   id = pid, 
                   weights = iptw_weight_trim,
                   data = dat_clean, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)




# ============================ 2. 长期变异性_SD============================
dat_long$BMI_quantiles_SD <- factor(dat_long$BMI_quantiles_SD, 
                                    levels = c("1", "2", "3", "4"), 
                                    labels = c("Q1", "Q2", "Q3", "Q4"))
gee_SD_3 <- geeglm(Frailty ~ BMI_quantiles_SD + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                   id = pid, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_SD_3 <- get_gee_or(gee_SD_3, digits = 2)
print(or_result_SD_3)
#2    BMI_quantiles_SDQ2 1.06 (0.88, 1.28)    0.53
#3    BMI_quantiles_SDQ3 1.28 (1.07, 1.54)    0.01
#4    BMI_quantiles_SDQ4 1.40 (1.17, 1.68)    0.00

# ============================ 3. 长期变异性_ARV ============================
dat_long$BMI_quantiles_ARV <- factor(dat_long$BMI_quantiles_ARV, 
                                    levels = c("1", "2", "3", "4"), 
                                    labels = c("Q1", "Q2", "Q3", "Q4"))
gee_ARV_3 <- geeglm(Frailty ~ BMI_quantiles_ARV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                   id = pid, 
                   weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_ARV_3 <- get_gee_or(gee_ARV_3, digits = 2)
print(or_result_ARV_3)
#2   BMI_quantiles_ARVQ2 1.08 (0.89, 1.29)    0.44
#3   BMI_quantiles_ARVQ3 1.28 (1.07, 1.54)    0.01
#4   BMI_quantiles_ARVQ4 1.41 (1.18, 1.69)    0.00

# ============================ 4. 长期变异性_VIM ============================
dat_long$BMI_quantiles_VIM <- factor(dat_long$BMI_quantiles_VIM, 
                                     levels = c("1", "2", "3", "4"), 
                                     labels = c("Q1", "Q2", "Q3", "Q4"))
gee_VIM_3 <- geeglm(Frailty ~ BMI_quantiles_VIM + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                    id = pid, 
                    weights = iptw_weight_trim,
                    data = dat_long, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
or_result_VIM_3 <- get_gee_or(gee_VIM_3, digits = 2)
print(or_result_VIM_3)
#2   BMI_quantiles_VIMQ2 1.08   (0.9, 1.3)    0.42
#3   BMI_quantiles_VIMQ3 1.33  (1.11, 1.6)    0.00
#4   BMI_quantiles_VIMQ4 1.44  (1.2, 1.72)    0.00

# 完成GEE和IPTW权重计算的数据集备份
saveRDS(dat_long, file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\2. KLoSA\\2. Backups of study participants\\完成GEE和IPTW权重计算_20260412.rds")
dat_long <- readRDS("D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\2. KLoSA\\2. Backups of study participants\\完成GEE和IPTW权重计算_20260412.rds")



# 形成Case/Total 分组 --------------
data_KLoSA9 <- readRDS(file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\2. KLoSA\\2. Backups of study participants\\KLoSA_Variability + Cumulative_20260412_完成X定义.rds")
# 1. CV -----------
data_KLoSA9$BMI_quantiles_CV <- factor(data_KLoSA9$BMI_quantiles_CV)
table(data_KLoSA9$Frailty, data_KLoSA9$BMI_quantiles_CV)
data_KLoSA9 %>% group_by(BMI_quantiles_CV) %>% summarise(total_ttime = sprintf("%.2f", sum(survival衰弱, na.rm = TRUE)))
surv_summary <- data_KLoSA9 %>% group_by(BMI_quantiles_CV) %>% summarise(总样本数 = n(),
                                                                         事件数 = sum(Frailty == 1, na.rm = TRUE),
                                                                         总暴露时间 = sum(survival衰弱, na.rm = TRUE),
                                                                         发病率 = sprintf("%.3f", 事件数 / 总暴露时间 * 1000))
print(surv_summary)
#BMI_quantiles_CV 总样本数 事件数 总暴露时间 发病率
#<fct>               <int>  <int>      <dbl> <chr> 
#1 1                    1333    280     13069. 21.425
#2 2                    1333    357     12422. 28.740
#3 3                    1332    424     11757. 36.064
#4 4                    1332    480     10961. 43.793

# 2. SD -----------
table(data_KLoSA9$Frailty, data_KLoSA9$BMI_quantiles_SD)
data_KLoSA9 %>% group_by(BMI_quantiles_SD) %>% summarise(total_ttime = sprintf("%.2f", sum(survival衰弱, na.rm = TRUE)))
surv_summary <- data_KLoSA9 %>% group_by(BMI_quantiles_SD) %>% summarise(总样本数 = n(),
                                                                         事件数 = sum(Frailty == 1, na.rm = TRUE),
                                                                         总暴露时间 = sum(survival衰弱, na.rm = TRUE),
                                                                         发病率 = sprintf("%.3f", 事件数 / 总暴露时间 * 1000))
print(surv_summary)
#1                1     1333    286     13028. 21.953
#2                2     1333    366     12313. 29.724
#3                3     1332    411     11786. 34.872
#4                4     1332    478     11081. 43.136

# 3. ARV -----------
table(data_KLoSA9$Frailty, data_KLoSA9$BMI_quantiles_ARV)
data_KLoSA9 %>% group_by(BMI_quantiles_ARV) %>% summarise(total_ttime = sprintf("%.2f", sum(survival衰弱, na.rm = TRUE)))
surv_summary <- data_KLoSA9 %>% group_by(BMI_quantiles_ARV) %>% summarise(总样本数 = n(),
                                                                          事件数 = sum(Frailty == 1, na.rm = TRUE),
                                                                          总暴露时间 = sum(survival衰弱, na.rm = TRUE),
                                                                          发病率 = sprintf("%.3f", 事件数 / 总暴露时间 * 1000))
print(surv_summary)
#1                 1     1333    295     12901. 22.866
#2                 2     1333    354     12455. 28.422
#3                 3     1332    428     11597. 36.907
#4                 4     1332    464     11254. 41.228

# 4. VIM -----------
table(data_KLoSA9$Frailty, data_KLoSA9$BMI_quantiles_VIM)
data_KLoSA9 %>% group_by(BMI_quantiles_VIM) %>% summarise(total_ttime = sprintf("%.2f", sum(survival衰弱, na.rm = TRUE)))
surv_summary <- data_KLoSA9 %>% group_by(BMI_quantiles_VIM) %>% summarise(总样本数 = n(),
                                                                          事件数 = sum(Frailty == 1, na.rm = TRUE),
                                                                          总暴露时间 = sum(survival衰弱, na.rm = TRUE),
                                                                          发病率 = sprintf("%.3f", 事件数 / 总暴露时间 * 1000))
print(surv_summary)
#1                 1     1333    284     13047. 21.768
#2                 2     1333    366     12344. 29.651
#3                 3     1332    411     11774. 34.907
#4                 4     1332    480     11044. 43.464

# ---------------------------- 20260409 不IPTW加权 -----------------------------
#dat_long$BMI_quantiles_CV <- factor(dat_long$BMI_quantiles_CV, levels = c("1", "2", "3", "4"), labels = c("Q1", "Q2", "Q3", "Q4"))
gee_CV_3 <- geeglm(Frailty ~ BMI_quantiles_CV + age + Gender + Rrbanicity + educate + marital_st + employ_st + physical_activity + smoke + drink_ever_3 + wealth + slope + CumBMI, 
                   id = pid, 
                   #weights = iptw_weight_trim,
                   data = dat_long, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
or_result_CV_3 <- get_gee_or(gee_CV_3, digits = 2)
print(or_result_CV_3)
#2    BMI_quantiles_CVQ2 1.17  (0.97, 1.4)    0.10
#3    BMI_quantiles_CVQ3 1.49 (1.24, 1.79)    0.00
#4    BMI_quantiles_CVQ4 1.57 (1.32, 1.88)    0.00





