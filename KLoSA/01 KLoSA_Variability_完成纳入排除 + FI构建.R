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
library(minpack.lm, lib.loc = "D:/MyRPackages")

#rm(list = ls())
data_KLoSA <- readRDS("D:/0. 科研文件/其他/上传KLoSA数据/klosa/GH_KLoSA_f.rds")
names(data_KLoSA)

data_KLoSA <- subset(
  data_KLoSA, 
  select = c(
    # 唯一标识
    "pid",
    # 参与调查情况 (补齐 r9iwstat)
    "inw1", "inw2", "inw3", "inw4", "inw5", "inw6", "inw7", "inw8", "inw9",
    "r1iwstat", "r2iwstat", "r3iwstat", "r4iwstat", "r5iwstat", "r6iwstat", "r7iwstat", "r8iwstat", "r9iwstat",
    # 调查时间 (补齐 r9iwy, r9iwm)
    "r1iwy", "r2iwy", "r3iwy", "r4iwy", "r5iwy", "r6iwy", "r7iwy", "r8iwy", "r9iwy",
    "r1iwm", "r2iwm", "r3iwm", "r4iwm", "r5iwm", "r6iwm", "r7iwm", "r8iwm", "r9iwm",
    # 年龄 (补齐 r9agey)
    "r1agey", "r2agey", "r3agey", "r4agey", "r5agey", "r6agey", "r7agey", "r8agey", "r9agey",
    # BMI 变异+累积 (补齐 r4bmi ~ r9bmi)
    "r1bmi", "r2bmi", "r3bmi",
    # Self-rated health 1 item (补齐 r9shlt)
    "r1shlt", "r2shlt", "r3shlt", "r4shlt", "r5shlt", "r6shlt", "r7shlt", "r8shlt", "r9shlt",
    # Physical condition 7 items 去除BMI (全部补齐到 r9)
    "r1sighta", "r2sighta", "r3sighta", "r4sighta", "r5sighta", "r6sighta", "r7sighta", "r8sighta", "r9sighta", 
    "r1hearinga", "r2hearinga", "r3hearinga", "r4hearinga", "r5hearinga", "r6hearinga", "r7hearinga", "r8hearinga", "r9hearinga",
    "r1sleeprl", "r2sleeprl", "r3sleeprl", "r4sleeprl", "r5sleeprl", "r6sleeprl", "r7sleeprl", "r8sleeprl", "r9sleeprl",
    "r1hlthlm", "r2hlthlm", "r3hlthlm", "r4hlthlm", "r5hlthlm", "r6hlthlm", "r7hlthlm", "r8hlthlm", "r9hlthlm",
    #"r1weight", "r2weight", "r3weight", "r4weight", "r5weight", "r6weight", "r7weight", "r8weight", "r9weight",
    #"r1gripsum", "r2gripsum", "r3gripsum", "r4gripsum", "r5gripsum", "r6gripsum", "r7gripsum", "r8gripsum", "r9gripsum",
    # Mental health 4 items (补齐到 r9)
    "r1mindtsl", "r2mindtsl", "r3mindtsl", "r4mindtsl", # "r5mindtsl", "r6mindtsl", "r7mindtsl", "r8mindtsl", "r9mindtsl",
    "r1effortl", "r2effortl", "r3effortl", "r4effortl", "r5effortl", "r6effortl", "r7effortl", "r8effortl", "r9effortl",
    "r1flonel", "r2flonel", "r3flonel", "r4flonel", "r5flonel", "r6flonel", "r7flonel", "r8flonel", "r9flonel",
    "r1goingl", "r2goingl", "r3goingl", "r4goingl", "r5goingl", "r6goingl", "r7goingl", "r8goingl", "r9goingl",
    # ADL 4 items (补齐到 r9)
    "r1dressb", "r2dressb", "r3dressb", "r4dressb", "r5dressb", "r6dressb", "r7dressb", "r8dressb", "r9dressb",
    "r1brushb", "r2brushb", "r3brushb", "r4brushb", "r5brushb", "r6brushb", "r7brushb", "r8brushb", "r9brushb",
    "r1bathb", "r2bathb", "r3bathb", "r4bathb", "r5bathb", "r6bathb", "r7bathb", "r8bathb", "r9bathb",
    "r1bedb_k", "r2bedb_k", "r3bedb_k", "r4bedb_k", "r5bedb_k", "r6bedb_k", "r7bedb_k", "r8bedb_k", "r9bedb_k",
    # IADL 10 items (补齐到 r9)
    "r1groomb", "r2groomb", "r3groomb", "r4groomb", "r5groomb", "r6groomb", "r7groomb", "r8groomb", "r9groomb",
    "r1housewkb", "r2housewkb", "r3housewkb", "r4housewkb", "r5housewkb", "r6housewkb", "r7housewkb", "r8housewkb", "r9housewkb",
    "r1mealsb", "r2mealsb", "r3mealsb", "r4mealsb", "r5mealsb", "r6mealsb", "r7mealsb", "r8mealsb", "r9mealsb",
    "r1laundryb", "r2laundryb", "r3laundryb", "r4laundryb", "r5laundryb", "r6laundryb", "r7laundryb", "r8laundryb", "r9laundryb",
    "r1gooutb", "r2gooutb", "r3gooutb", "r4gooutb", "r5gooutb", "r6gooutb", "r7gooutb", "r8gooutb", "r9gooutb",
    "r1transb", "r2transb", "r3transb", "r4transb", "r5transb", "r6transb", "r7transb", "r8transb", "r9transb",
    "r1shopb", "r2shopb", "r3shopb", "r4shopb", "r5shopb", "r6shopb", "r7shopb", "r8shopb", "r9shopb",
    "r1moneyb", "r2moneyb", "r3moneyb", "r4moneyb", "r5moneyb", "r6moneyb", "r7moneyb", "r8moneyb", "r9moneyb",
    "r1phoneb", "r2phoneb", "r3phoneb", "r4phoneb", "r5phoneb", "r6phoneb", "r7phoneb", "r8phoneb", "r9phoneb",
    "r1medsb", "r2medsb", "r3medsb", "r4medsb", "r5medsb", "r6medsb", "r7medsb", "r8medsb", "r9medsb",
    # chronic diseases 8 items (补齐到 r9)
    "r1hibpe", "r2hibpe", "r3hibpe", "r4hibpe", "r5hibpe", "r6hibpe", "r7hibpe", "r8hibpe", "r9hibpe",
    "r1diabe", "r2diabe", "r3diabe", "r4diabe", "r5diabe", "r6diabe", "r7diabe", "r8diabe", "r9diabe",
    "r1lunge", "r2lunge", "r3lunge", "r4lunge", "r5lunge", "r6lunge", "r7lunge", "r8lunge", "r9lunge",
    "r1hearte", "r2hearte", "r3hearte", "r4hearte", "r5hearte", "r6hearte", "r7hearte", "r8hearte", "r9hearte",
    "r1stroke", "r2stroke", "r3stroke", "r4stroke", "r5stroke", "r6stroke", "r7stroke", "r8stroke", "r9stroke",
    "r1arthre", "r2arthre", "r3arthre", "r4arthre", "r5arthre", "r6arthre", "r7arthre", "r8arthre", "r9arthre",
    "r1urinai", "r2urinai", "r3urinai", "r4urinai", "r5urinai", "r6urinai", "r7urinai", "r8urinai", "r9urinai",
    # All-cause Mortality
    "radyear", "radmonth",

    # 协变量 (全部补齐到 r9)
    "ragender",
    "r1rural", 
    "r1mstath",
    "raeducl",
    "r1lbrf_k",
    "r1vigactf_k",
    "r1smokev", 
    "r1smoken",
    "r1drinkev", 
    "r1drinkx",
    "r1atotb",
    "hh1hhresp",
    
    # n_chronic -----------
    #"r1arthre", 
    "r1cancre",
    #"r1diabe"
    #"r1lunge",
    "r1psyche",
    #"r1stroke", 
    #"r1hearte", 
    #"r1hibpe", 
    
    # Medication -----------
    "r1rxhibp",
    "r1rxdiab",
    "r1rxcancr",
    "r1rxlung",
    "r1rxheart",
    "r1rxpsych",
    "r1rxarthr",
    
    "r1wtresp"))

# ============================================================================================================================================================
# (一) 纳入排除
# ============================================================================================================================================================
#1.inw1参与者
table(data_KLoSA$inw1, useNA = "always")
data_KLoSA1 <- data_KLoSA[data_KLoSA$inw1 == 1 & !is.na(data_KLoSA$inw1), ]

#2.年龄≥50
sum(is.na(data_KLoSA1$r1agey))
data_KLoSA2 <- data_KLoSA1[data_KLoSA1$r1agey >= 50 & !(is.na(data_KLoSA1$r1agey)), ]     # N = 8465

#3.没有Wave 4 5 6 的BMI测量值
summary(data_KLoSA2$r1bmi, useNA = "always")
summary(data_KLoSA2$r2bmi, useNA = "always")
summary(data_KLoSA2$r3bmi, useNA = "always")
data_KLoSA3 <- data_KLoSA2 %>% filter(!is.na(r1bmi) & !is.na(r2bmi) & !is.na(r3bmi))     #N = 6096

#5. 排除未参加随访者 Wave 4 5 6 7 8
data_KLoSA4 <- data_KLoSA3 %>% filter(r4iwstat == 1 | r5iwstat == 1 | r6iwstat == 1 | r7iwstat == 1 | r8iwstat == 1 | r9iwstat == 1)     #N = 5701
#6. 构建Frailty 并 排除基线FI≥0.25 的参与者 重头戏来咯！
##### (1) Select health-related variables 已筛选 [略]===========================
##### (2) Remove variables with >10% missing at baseline========================
all_vars <- c("pid",
              "r1sighta", "r2sighta", "r3sighta", "r4sighta", "r5sighta", "r6sighta", "r7sighta", "r8sighta", "r9sighta", 
              "r1hearinga", "r2hearinga", "r3hearinga", "r4hearinga", "r5hearinga", "r6hearinga", "r7hearinga", "r8hearinga", "r9hearinga",
              "r1sleeprl", "r2sleeprl", "r3sleeprl", "r4sleeprl", "r5sleeprl", "r6sleeprl", "r7sleeprl", "r8sleeprl", "r9sleeprl",
              "r1hlthlm", "r2hlthlm", "r3hlthlm", "r4hlthlm", "r5hlthlm", "r6hlthlm", "r7hlthlm", "r8hlthlm", "r9hlthlm",
              #"r1weight", "r2weight", "r3weight", "r4weight", "r5weight", "r6weight", "r7weight", "r8weight", "r9weight",
              #"r1gripsum", "r2gripsum", "r3gripsum", "r4gripsum", "r5gripsum", "r6gripsum", "r7gripsum", "r8gripsum", "r9gripsum",
              # Mental health 4 items (补齐到 r9)
              "r1mindtsl", "r2mindtsl", "r3mindtsl", "r4mindtsl", # "r5mindtsl", "r6mindtsl", "r7mindtsl", "r8mindtsl", "r9mindtsl",
              "r1effortl", "r2effortl", "r3effortl", "r4effortl", "r5effortl", "r6effortl", "r7effortl", "r8effortl", "r9effortl",
              "r1flonel", "r2flonel", "r3flonel", "r4flonel", "r5flonel", "r6flonel", "r7flonel", "r8flonel", "r9flonel",
              "r1goingl", "r2goingl", "r3goingl", "r4goingl", "r5goingl", "r6goingl", "r7goingl", "r8goingl", "r9goingl",
              # ADL 4 items (补齐到 r9)
              "r1dressb", "r2dressb", "r3dressb", "r4dressb", "r5dressb", "r6dressb", "r7dressb", "r8dressb", "r9dressb",
              "r1brushb", "r2brushb", "r3brushb", "r4brushb", "r5brushb", "r6brushb", "r7brushb", "r8brushb", "r9brushb",
              "r1bathb", "r2bathb", "r3bathb", "r4bathb", "r5bathb", "r6bathb", "r7bathb", "r8bathb", "r9bathb",
              "r1bedb_k", "r2bedb_k", "r3bedb_k", "r4bedb_k", "r5bedb_k", "r6bedb_k", "r7bedb_k", "r8bedb_k", "r9bedb_k",
              # IADL 10 items (补齐到 r9)
              "r1groomb", "r2groomb", "r3groomb", "r4groomb", "r5groomb", "r6groomb", "r7groomb", "r8groomb", "r9groomb",
              "r1housewkb", "r2housewkb", "r3housewkb", "r4housewkb", "r5housewkb", "r6housewkb", "r7housewkb", "r8housewkb", "r9housewkb",
              "r1mealsb", "r2mealsb", "r3mealsb", "r4mealsb", "r5mealsb", "r6mealsb", "r7mealsb", "r8mealsb", "r9mealsb",
              "r1laundryb", "r2laundryb", "r3laundryb", "r4laundryb", "r5laundryb", "r6laundryb", "r7laundryb", "r8laundryb", "r9laundryb",
              "r1gooutb", "r2gooutb", "r3gooutb", "r4gooutb", "r5gooutb", "r6gooutb", "r7gooutb", "r8gooutb", "r9gooutb",
              "r1transb", "r2transb", "r3transb", "r4transb", "r5transb", "r6transb", "r7transb", "r8transb", "r9transb",
              "r1shopb", "r2shopb", "r3shopb", "r4shopb", "r5shopb", "r6shopb", "r7shopb", "r8shopb", "r9shopb",
              "r1moneyb", "r2moneyb", "r3moneyb", "r4moneyb", "r5moneyb", "r6moneyb", "r7moneyb", "r8moneyb", "r9moneyb",
              "r1phoneb", "r2phoneb", "r3phoneb", "r4phoneb", "r5phoneb", "r6phoneb", "r7phoneb", "r8phoneb", "r9phoneb",
              "r1medsb", "r2medsb", "r3medsb", "r4medsb", "r5medsb", "r6medsb", "r7medsb", "r8medsb", "r9medsb",
              # chronic diseases 8 items (补齐到 r9)
              "r1hibpe", "r2hibpe", "r3hibpe", "r4hibpe", "r5hibpe", "r6hibpe", "r7hibpe", "r8hibpe", "r9hibpe",
              "r1diabe", "r2diabe", "r3diabe", "r4diabe", "r5diabe", "r6diabe", "r7diabe", "r8diabe", "r9diabe",
              "r1lunge", "r2lunge", "r3lunge", "r4lunge", "r5lunge", "r6lunge", "r7lunge", "r8lunge", "r9lunge",
              "r1hearte", "r2hearte", "r3hearte", "r4hearte", "r5hearte", "r6hearte", "r7hearte", "r8hearte", "r9hearte",
              "r1stroke", "r2stroke", "r3stroke", "r4stroke", "r5stroke", "r6stroke", "r7stroke", "r8stroke", "r9stroke",
              "r1arthre", "r2arthre", "r3arthre", "r4arthre", "r5arthre", "r6arthre", "r7arthre", "r8arthre", "r9arthre",
              "r1urinai", "r2urinai", "r3urinai", "r4urinai", "r5urinai", "r6urinai", "r7urinai", "r8urinai", "r9urinai")

vars_r1 <- all_vars[grepl("^r1", all_vars)]     #只看基线
data_missing_baseline <- data_KLoSA4[, vars_r1, drop = FALSE]

total_sample <- nrow(data_missing_baseline)
missing_count <- colSums(is.na(data_missing_baseline))  
missing_pct <- round((missing_count / total_sample) * 100, 2)  # 缺失值比例

missing_result <- data.frame(变量名 = names(missing_count),
                             总样本量 = total_sample,
                             缺失值数量 = missing_count,
                             缺失值百分比 = missing_pct)

missing_result <- missing_result[order(-missing_result$缺失值百分比), ]
print(missing_result)

##### (3) Recode all variables to a 0–1 scale(变量取值范围不为0-1的调整为0-1)===
for (v in all_vars) {
  # 跳过不存在的变量 + 全是缺失值的变量
  if (!v %in% colnames(data_KLoSA4)) next
  if (all(is.na(data_KLoSA4[[v]]))) next
  # 计算最大最小值
  min_val <- min(data_KLoSA4[[v]], na.rm = TRUE)
  max_val <- max(data_KLoSA4[[v]], na.rm = TRUE)
  # 判断是否在 0–1 之间
  in_range <- min_val >= 0 & max_val <= 1
  # 只输出【超出范围】的变量
  if (!in_range) {
    cat(sprintf("❌ %-15s  min: %.3f  max: %.3f\n", v, min_val, max_val))
  }
}
cat("\n✅ 输出完毕！以上都是【超出 0–1 范围】的变量！\n")
# 1. r1sighta 取值1-6
# 2. r1hearinga 取值1-5
# 3. r1sleeprl 取值1-9
# 4. r1mindtsl 取值1-4
# 5. r1effortl 取值1-4
# 6. r1flonel 取值1-4
# 7. r1goingl 取值1-4

# 1. Self-rated health (1-5 → 0-1)
table(data_KLoSA4$r1shlt)
vars_health <- paste0("r", 1:9, "shlt")
vars_health <- vars_health[vars_health %in% colnames(data_KLoSA4)]
new_vars_health <- paste0("w", sub("r(\\d+)shlt", "\\1", vars_health), "shlt")
data_KLoSA4 <- data_KLoSA4 %>% 
  mutate(across(.cols = all_of(vars_health),
                .fns = ~ ifelse(between(., 1, 5), (.-1)/4, NA_real_) # 1-5分制转0-1，异常值设为NA
                ))

# 2. r1sighta: 1-6 → r4nsight-r9nsight (1-5 -> 0, 0.25, 0.5, 0.75, 1)
table(data_KLoSA4$r1sighta)
vars_nearsight <- paste0("r", 1:9, "sighta")
vars_nearsight <- vars_nearsight[vars_nearsight %in% colnames(data_KLoSA4)]
new_vars_hearing <- paste0("w", sub("r(\\d+)sighta", "\\1", vars_nearsight), "sighta")
data_KLoSA4 <- data_KLoSA4 %>% mutate(across(.cols = all_of(vars_nearsight),
                                             .fns = ~ case_when(
                                               . == 1 ~ 0,                # 原始1分→0
                                               . == 6 ~ 1,                # 原始6分→1
                                               between(., 2, 5) ~ (.-1)/5, # 2-5分线性映射到0-1（(2-1)/5=0.2, (5-1)/5=0.8）
                                               TRUE ~ NA_real_            # 非1-6的异常值设为NA
                                               )))

# 3. r1hearinga (1-5 → 0-1)
table(data_KLoSA4$r3hearinga)
vars_hear <- paste0("r", 1:9, "hearinga")
vars_hear <- vars_hear[vars_hear %in% colnames(data_KLoSA4)]
new_vars_health <- paste0("w", sub("r(\\d+)hearinga", "\\1", vars_hear), "hearinga")
data_KLoSA4 <- data_KLoSA4 %>% 
  mutate(across(
    .cols = all_of(vars_hear),
    .fns = ~ ifelse(between(., 1, 5), (.-1)/4, NA_real_) # 1-5分制转0-1，异常值设为NA
  ))

# 4. r1sleeprl 1-4 
table(data_KLoSA4$r1sleeprl)
vars_sleep <- paste0("r", 1:9, "sleeprl")
vars_sleep <- vars_sleep[vars_sleep %in% colnames(data_KLoSA4)]
new_vars_health <- paste0("w", sub("r(\\d+)sleeprl", "\\1", vars_sleep), "sleeprl")
data_KLoSA4 <- data_KLoSA4 %>% 
  mutate(across(
    .cols = all_of(vars_sleep),
    .fns = ~ case_when(
      . == 1 ~ 0,                # 原始1分→0
      . == 4 ~ 1,                # 原始4分→1
      between(., 2, 3) ~ (.-1)/3, # 2-3分线性映射（(2-1)/3≈0.333，(3-1)/3≈0.667）
      TRUE ~ NA_real_)))

# 5. r1mindtsl 1-4 →0-1
table(data_KLoSA4$r1mindtsl)
vars_mind <- paste0("r", 1:9, "mindtsl")
vars_mind <- vars_mind[vars_mind %in% colnames(data_KLoSA4)]
new_vars_health <- paste0("w", sub("r(\\d+)mindtsl", "\\1", vars_mind), "mindtsl")
data_KLoSA4 <- data_KLoSA4 %>% 
  mutate(across(
    .cols = all_of(vars_mind),
    .fns = ~ case_when(
      . == 1 ~ 0,                # 原始1分→0
      . == 4 ~ 1,                # 原始4分→1
      between(., 2, 3) ~ (.-1)/3, # 2-3分线性映射（(2-1)/3≈0.333，(3-1)/3≈0.667）
      TRUE ~ NA_real_)))

# 6. r1effortl 1-4 →0-1
table(data_KLoSA4$r1effortl)
vars_effort <- paste0("r", 1:9, "effortl")
vars_effort <- vars_effort[vars_effort %in% colnames(data_KLoSA4)]
new_vars_health <- paste0("w", sub("r(\\d+)effortl", "\\1", vars_effort), "effortl")
data_KLoSA4 <- data_KLoSA4 %>% 
  mutate(across(
    .cols = all_of(vars_effort),
    .fns = ~ case_when(
      . == 1 ~ 0,                # 原始1分→0
      . == 4 ~ 1,                # 原始4分→1
      between(., 2, 3) ~ (.-1)/3, # 2-3分线性映射（(2-1)/3≈0.333，(3-1)/3≈0.667）
      TRUE ~ NA_real_)))

# 7. r1flonel 1-4
table(data_KLoSA4$r1flonel)
vars_lone <- paste0("r", 1:9, "flonel")
vars_lone <- vars_lone[vars_lone %in% colnames(data_KLoSA4)]
new_vars_health <- paste0("w", sub("r(\\d+)flonel", "\\1", vars_lone), "flonel")
data_KLoSA4 <- data_KLoSA4 %>% 
  mutate(across(
    .cols = all_of(vars_lone),
    .fns = ~ case_when(
      . == 1 ~ 0,                # 原始1分→0
      . == 4 ~ 1,                # 原始4分→1
      between(., 2, 3) ~ (.-1)/3, # 2-3分线性映射（(2-1)/3≈0.333，(3-1)/3≈0.667）
      TRUE ~ NA_real_)))

# 8. r1goingl 1-4
table(data_KLoSA4$r1goingl)
vars_going <- paste0("r", 1:9, "goingl")
vars_going <- vars_going[vars_going %in% colnames(data_KLoSA4)]
new_vars_health <- paste0("w", sub("r(\\d+)goingl", "\\1", vars_going), "goingl")
data_KLoSA4 <- data_KLoSA4 %>% 
  mutate(across(
    .cols = all_of(vars_going),
    .fns = ~ case_when(
      . == 1 ~ 0,                # 原始1分→0
      . == 4 ~ 1,                # 原始4分→1
      between(., 2, 3) ~ (.-1)/3, # 2-3分线性映射（(2-1)/3≈0.333，(3-1)/3≈0.667）
      TRUE ~ NA_real_)))

for (v in all_vars) {
  # 跳过不存在的变量 + 全是缺失值的变量
  if (!v %in% colnames(data_KLoSA4)) next
  if (all(is.na(data_KLoSA4[[v]]))) next
  # 计算最大最小值
  min_val <- min(data_KLoSA4[[v]], na.rm = TRUE)
  max_val <- max(data_KLoSA4[[v]], na.rm = TRUE)
  # 判断是否在 0–1 之间
  in_range <- min_val >= 0 & max_val <= 1
  # 只输出【超出范围】的变量
  if (!in_range) {
    cat(sprintf("❌ %-15s  min: %.3f  max: %.3f\n", v, min_val, max_val))}}
cat("\n✅ 输出完毕！以上都是【超出 0–1 范围】的变量！\n")

##### (4) Exclude rare (<1%) or saturated (>80%) deficits=======================
target_vars <- c("r1sighta", "r2sighta", "r3sighta", "r4sighta", "r5sighta", "r6sighta", "r7sighta", "r8sighta", "r9sighta", 
                 "r1hearinga", "r2hearinga", "r3hearinga", "r4hearinga", "r5hearinga", "r6hearinga", "r7hearinga", "r8hearinga", "r9hearinga",
                 "r1sleeprl", "r2sleeprl", "r3sleeprl", "r4sleeprl", "r5sleeprl", "r6sleeprl", "r7sleeprl", "r8sleeprl", "r9sleeprl",
                 "r1hlthlm", "r2hlthlm", "r3hlthlm", "r4hlthlm", "r5hlthlm", "r6hlthlm", "r7hlthlm", "r8hlthlm", "r9hlthlm",
                 "r1weight", "r2weight", "r3weight", "r4weight", "r5weight", "r6weight", "r7weight", "r8weight", "r9weight",
                 "r1gripsum", "r2gripsum", "r3gripsum", "r4gripsum", "r5gripsum", "r6gripsum", "r7gripsum", "r8gripsum", "r9gripsum",
                 # Mental health 4 items (补齐到 r9)
                 "r1mindtsl", "r2mindtsl", "r3mindtsl", "r4mindtsl", # "r5mindtsl", "r6mindtsl", "r7mindtsl", "r8mindtsl", "r9mindtsl",
                 "r1effortl", "r2effortl", "r3effortl", "r4effortl", "r5effortl", "r6effortl", "r7effortl", "r8effortl", "r9effortl",
                 "r1flonel", "r2flonel", "r3flonel", "r4flonel", "r5flonel", "r6flonel", "r7flonel", "r8flonel", "r9flonel",
                 "r1goingl", "r2goingl", "r3goingl", "r4goingl", "r5goingl", "r6goingl", "r7goingl", "r8goingl", "r9goingl",
                 # ADL 4 items (补齐到 r9)
                 "r1dressb", "r2dressb", "r3dressb", "r4dressb", "r5dressb", "r6dressb", "r7dressb", "r8dressb", "r9dressb",
                 "r1brushb", "r2brushb", "r3brushb", "r4brushb", "r5brushb", "r6brushb", "r7brushb", "r8brushb", "r9brushb",
                 "r1bathb", "r2bathb", "r3bathb", "r4bathb", "r5bathb", "r6bathb", "r7bathb", "r8bathb", "r9bathb",
                 "r1bedb_k", "r2bedb_k", "r3bedb_k", "r4bedb_k", "r5bedb_k", "r6bedb_k", "r7bedb_k", "r8bedb_k", "r9bedb_k",
                 # IADL 10 items (补齐到 r9)
                 "r1groomb", "r2groomb", "r3groomb", "r4groomb", "r5groomb", "r6groomb", "r7groomb", "r8groomb", "r9groomb",
                 "r1housewkb", "r2housewkb", "r3housewkb", "r4housewkb", "r5housewkb", "r6housewkb", "r7housewkb", "r8housewkb", "r9housewkb",
                 "r1mealsb", "r2mealsb", "r3mealsb", "r4mealsb", "r5mealsb", "r6mealsb", "r7mealsb", "r8mealsb", "r9mealsb",
                 "r1laundryb", "r2laundryb", "r3laundryb", "r4laundryb", "r5laundryb", "r6laundryb", "r7laundryb", "r8laundryb", "r9laundryb",
                 "r1gooutb", "r2gooutb", "r3gooutb", "r4gooutb", "r5gooutb", "r6gooutb", "r7gooutb", "r8gooutb", "r9gooutb",
                 "r1transb", "r2transb", "r3transb", "r4transb", "r5transb", "r6transb", "r7transb", "r8transb", "r9transb",
                 "r1shopb", "r2shopb", "r3shopb", "r4shopb", "r5shopb", "r6shopb", "r7shopb", "r8shopb", "r9shopb",
                 "r1moneyb", "r2moneyb", "r3moneyb", "r4moneyb", "r5moneyb", "r6moneyb", "r7moneyb", "r8moneyb", "r9moneyb",
                 "r1phoneb", "r2phoneb", "r3phoneb", "r4phoneb", "r5phoneb", "r6phoneb", "r7phoneb", "r8phoneb", "r9phoneb",
                 "r1medsb", "r2medsb", "r3medsb", "r4medsb", "r5medsb", "r6medsb", "r7medsb", "r8medsb", "r9medsb",
                 # chronic diseases 8 items (补齐到 r9)
                 "r1hibpe", "r2hibpe", "r3hibpe", "r4hibpe", "r5hibpe", "r6hibpe", "r7hibpe", "r8hibpe", "r9hibpe",
                 "r1diabe", "r2diabe", "r3diabe", "r4diabe", "r5diabe", "r6diabe", "r7diabe", "r8diabe", "r9diabe",
                 "r1lunge", "r2lunge", "r3lunge", "r4lunge", "r5lunge", "r6lunge", "r7lunge", "r8lunge", "r9lunge",
                 "r1hearte", "r2hearte", "r3hearte", "r4hearte", "r5hearte", "r6hearte", "r7hearte", "r8hearte", "r9hearte",
                 "r1stroke", "r2stroke", "r3stroke", "r4stroke", "r5stroke", "r6stroke", "r7stroke", "r8stroke", "r9stroke",
                 "r1arthre", "r2arthre", "r3arthre", "r4arthre", "r5arthre", "r6arthre", "r7arthre", "r8arthre", "r9arthre",
                 "r1urinai", "r2urinai", "r3urinai", "r4urinai", "r5urinai", "r6urinai", "r7urinai", "r8urinai", "r9urinai")

vars_r1_1_80 <- target_vars[grepl("^r1", target_vars)]
existing_targets <- intersect(vars_r1_1_80, names(data_KLoSA4))

means <- round(colMeans(data_KLoSA4[, existing_targets, drop = FALSE], na.rm = TRUE), 4) 

invalid_vars_r4 <- names(means)[means <= 0.01 | means >= 0.80]

## 筛选条件：均值≤0.01 或 ≥0.80 的变量为待删除变量
rare_saturated_vars <- unlist(lapply(invalid_vars_r4, function(var) {     ## 匹配所有轮次的对应变量（如r4walkra → r5walkra/r6walkra...）
  suffix <- sub("^r4", "", var)     # 提取变量名中除r4外的后缀（如walkra、eata）
  paste0("r", 4:9, suffix)     # 生成所有轮次的变量名（r4-r9 + 后缀）
}))

result_df <- data.frame(
  变量名 = names(means),
  均值 = means,
  是否符合条件 = ifelse(means > 0.01 & means < 0.80, "是", "否"),
  筛选条件 = "0.01 < 均值 < 0.80",
  row.names = NULL,
  stringsAsFactors = FALSE
) %>% arrange(desc(是否符合条件)) # 按是否符合条件降序排序

cat("===== data_KLoSA4 中目标变量的均值及筛选结果 =====\n")
print(result_df, row.names = FALSE)

existing_rare_vars <- intersect(rare_saturated_vars, names(data_KLoSA4))
data_KLoSA5 <- data_KLoSA4[, setdiff(names(data_KLoSA4), existing_rare_vars), drop = FALSE]

##### (5) Ensure age-related trend==============================================
vars_r1 <- target_vars[grepl("^r1", target_vars)]                               # 提取Wave4的目标变量（以r4开头）
valid_r1_vars <- result_df$变量名[result_df$是否符合条件 == "是"]               # 从result_df中筛选出"是否符合条件"为"是"的Wave4变量
final_r1_vars <- intersect(valid_r1_vars, names(data_KLoSA5))                   # 确保这些变量在data_SHARE6中存在

cat("===== Wave4符合条件的变量筛选结果 =====\n",
    "Wave4目标变量总数：", length(vars_r1), "\n",
    "符合0.01<均值<0.80的变量数：", length(valid_r1_vars), "\n",
    "data_SHARE6中实际存在的符合条件变量数：", length(final_r1_vars), "\n",
    "符合条件的Wave4变量列表：\n", sep = "")
print(final_r1_vars)

analysis_data <- data_KLoSA5 %>% select(r1agey, all_of(final_r1_vars)) %>% drop_na(r1agey) # 提取分析数据集：仅保留年龄变量（r4agey）和符合条件的Wave4变量 删除年龄缺失的行

# 相关性检验：Wave1变量与年龄的皮尔逊相关 
corr_results <- data.frame(变量名 = character(),
                           相关系数 = numeric(),
                           P值 = numeric(),
                           相关性方向 = character(),
                           显著性 = character(),
                           stringsAsFactors = FALSE)

for (var in final_r1_vars) {
  clean_data <- analysis_data %>% select(r1agey, all_of(var)) %>% drop_na()           # 去除变量缺失值
  corr_test <- cor.test(clean_data$r1agey, clean_data[[var]], method = "pearson")     # 皮尔逊相关检验（有序变量可改用method = "spearman"）
  corr_coef <- round(corr_test$estimate, 4)
  p_val <- round(corr_test$p.value, 4)
  
  direction <- ifelse(corr_coef > 0, "正相关", ifelse(corr_coef < 0, "负相关", "无相关"))  
  significance <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "ns")))

  corr_results <- rbind(corr_results, data.frame(变量名 = var, 
                                                 相关系数 = corr_coef,
                                                 P值 = p_val,
                                                 相关性方向 = direction,
                                                 显著性 = significance,
                                                 stringsAsFactors = FALSE))}

corr_results <- corr_results %>% arrange(相关性方向, desc(abs(相关系数)))
cat("\n===== Wave4变量与年龄的相关性检验结果 =====\n")
print(corr_results, row.names = FALSE)

# 删除"负相关"的变量形成data_KLoSA6
neg_corr_r4_vars <- corr_results$变量名[corr_results$相关性方向 == "负相关"]
existing_neg_vars <- intersect(unlist(lapply(neg_corr_r4_vars, function(var) paste0("r", 4:9, sub("^r4", "", var)))), names(data_SHARE6))
cat("删除的变量：", paste(existing_neg_vars, collapse = ", "), "\n")

data_KLoSA6 <- data_KLoSA5[, setdiff(names(data_KLoSA5), existing_neg_vars), drop = FALSE]


##### (6) Check for collinearity (r > 0.95)
calculate_wave1_correlation <- function(data, target_vars, method = "spearman", high_corr_thresh = 0.95) {
  w4_vars <- intersect(target_vars[grepl("^r1", target_vars)], names(data))     # 提取Wave4有效变量
  cat("===== Wave4 变量相关性分析（r >", high_corr_thresh, "）=====\n")
  
  # 变量/样本量验证
  if (length(w4_vars) < 2) return(cat("有效变量不足2个，跳过\n"))
  var_data <- data[complete.cases(data[, w4_vars]), w4_vars]
  if (nrow(var_data) < 5) return(cat("有效样本量不足，跳过\n"))
  
  # 计算相关性矩阵并筛选高相关对
  corr_mat <- round(cor(as.matrix(var_data), method = method), 3)
  high_corr <- which(corr_mat > high_corr_thresh & corr_mat < 1, arr.ind = TRUE)
  
  if (nrow(high_corr) > 0) {
    pairs <- data.frame(
      变量1 = rownames(corr_mat)[high_corr[, 1]],
      变量2 = colnames(corr_mat)[high_corr[, 2]],
      相关系数 = corr_mat[high_corr]
    )[with(pairs, 变量1 < 变量2), ] # 去重
    cat("极高相关变量对：\n"); print(pairs, row.names = FALSE)
  } else {
    cat("无极高相关变量对（r ≤", high_corr_thresh, "）\n")
  }
  invisible(corr_mat) # 返回相关性矩阵
}

calculate_wave1_correlation(data_KLoSA6, target_vars, "spearman", 0.95)         # 调用函数

# 形成高共线性变量数据集（虽然没有变量高共线性，但便于后续筛选）
calculate_wave1_correlation_with_return <- function(data, target_vars, method = "spearman", high_corr_thresh = 0.95) {
  w4_vars <- intersect(target_vars[grepl("^r1", target_vars)], names(data))
  if (length(w4_vars) < 2) return(NULL)
  var_data <- data[complete.cases(data[, w4_vars]), w4_vars]
  if (nrow(var_data) < 5) return(NULL)
  corr_mat <- round(cor(as.matrix(var_data), method = method), 3)
  high_corr <- which(corr_mat > high_corr_thresh & corr_mat < 1, arr.ind = TRUE)
  if (nrow(high_corr) == 0) return(NULL)
  pairs <- data.frame(
    变量1 = rownames(corr_mat)[high_corr[, 1]],
    变量2 = colnames(corr_mat)[high_corr[, 2]],
    相关系数 = corr_mat[high_corr]
  )[with(pairs, 变量1 < 变量2), ]
  return(pairs)
}

high_corr_pairs <- calculate_wave1_correlation_with_return(data_KLoSA6, target_vars, "spearman", 0.95)     # 提取Wave4高相关变量对
# 定义高共线性变量删除规则：每对中删除第二个变量（可根据需求调整）
if (!is.null(high_corr_pairs)) {
  del_w4_coll_vars <- unique(high_corr_pairs$变量2) # 提取Wave4需删除的高相关变量
  # 生成所有Wave的高相关待删除变量
  del_coll_vars <- unlist(lapply(del_w4_coll_vars, function(var) {
    suffix <- sub("^r1", "", var)
    paste0("r", 1:9, suffix)
  }))
  cat("需删除的高共线性变量（所有Wave）：", paste(del_coll_vars, collapse = ", "), "\n")
} else {
  del_coll_vars <- c() # 无高相关变量时，待删除列表为空
  cat("无高共线性变量需删除\n")
}

##### (7) Calculate individual FI===============================================
# 从data_SHARE7中提取最终符合所有条件的target_vars子集，生成用于 FI 构建的数据集
final_fi_target_vars <- setdiff(target_vars, c(rare_saturated_vars, existing_neg_vars, del_coll_vars))       # 步骤1：从target_vars中排除稀有/饱和变量、负相关变量、高共线性变量
final_fi_vars <- intersect(final_fi_target_vars, names(data_KLoSA6))            # 步骤2：筛选data_SHARE7中实际存在的最终变量（避免变量不存在）
key_cols <- c("pid")                                                        # 步骤3：保留关键标识列（mergeid）+ 最终FI变量，生成数据集
fi_data <- data_KLoSA6[, c(key_cols, final_fi_vars), drop = FALSE]

# 按Wave拆分最终FI变量
fi_var_wave <- data.frame(var_name = final_fi_vars,
                          wave = gsub("r", "", str_extract(final_fi_vars, "^r\\d+")), # 提取Wave数字
                          stringsAsFactors = FALSE)
fi_var_wave <- fi_var_wave[!is.na(fi_var_wave$wave) & fi_var_wave$wave != "", ] # 过滤无效Wave
wave_fi_vars <- split(fi_var_wave$var_name, fi_var_wave$wave)                   # 按Wave分组FI变量
min_var_count <- 19      #阈值 KLoSA

fi_result_list <- list()
i <- 1 # 列表索引

# 循环每个Wave计算FI值（核心：向量化计算）
for (wave in names(wave_fi_vars)) {     
  cat("正在计算Wave", wave, "的FI值...\n")
  current_vars <- wave_fi_vars[[wave]]
  if (length(current_vars) == 0) {
    cat("Wave", wave, "无FI变量，跳过\n")
    next
  }
  
  # 提取数据：仅保留mergeid和当前Wave变量
  wave_data <- fi_data %>% select(pid, all_of(current_vars))

    # 核心优化：向量化计算非缺失值个数和总和
  var_matrix <- as.matrix(wave_data[, current_vars, drop = FALSE])              # 转为矩阵
  non_missing_count <- rowSums(!is.na(var_matrix))                              # 向量化计算每行非缺失值个数
  var_sum <- rowSums(var_matrix, na.rm = TRUE)                                  # 向量化计算每行总和
  
  # 计算FI值（向量化判断，无循环）
  fi_value <- ifelse(non_missing_count >= min_var_count, var_sum / non_missing_count, NA)
  
  # 组装结果并加入列表
  wave_fi <- data.frame(
    pid = wave_data$pid,
    non_missing_count = non_missing_count,
    fi_value = fi_value,
    wave = wave,
    stringsAsFactors = FALSE
  )
  fi_result_list[[i]] <- wave_fi
  i <- i + 1
}

fi_result <- do.call(rbind, fi_result_list)                                     # 合并列表为数据框

# 整理结果
fi_final <- fi_result %>% select(pid, wave, non_missing_count, fi_value) %>% arrange(pid, wave) %>%
  rename(波次 = wave,
         非缺失值变量数 = non_missing_count,
         FI值 = fi_value)
cat("\n===== 各mergeid各Wave的FI值（前250行）=====\n")
print(head(fi_final, 250), row.names = FALSE)

# 统计各Wave的FI值有效情况
wave_fi_summary <- fi_final %>% group_by(波次) %>%
  summarise(总参与者数 = dplyr::n_distinct(pid),
            FI有效数 = sum(!is.na(FI值)),
            FI缺失数 = sum(is.na(FI值)),
            平均FI值 = round(mean(FI值, na.rm = TRUE), 4)) %>% ungroup()
cat("\n===== 各Wave FI值统计汇总 =====\n")
print(wave_fi_summary, row.names = FALSE)

# 将FI值按Wave合并到data_SHARE7中
fi_wide <- fi_final %>% select(pid, 波次, FI值) %>%
  # 重命名波次为FI_w+数字（如4→FI_w4）
  mutate(FI列名 = paste0("FI_w", 波次)) %>%
  # 转换为宽格式：每个Wave的FI值作为单独列，无数据则为NA
  pivot_wider(id_cols = pid, names_from = FI列名, values_from = FI值, values_fill = NA)

all_fi_cols <- paste0("FI_w", 1:9)
for (col in all_fi_cols) {
  if (!col %in% colnames(fi_wide)) {
    fi_wide[[col]] <- NA
  }
}

data_KLoSA7 <- data_KLoSA6 %>% left_join(fi_wide, by = "pid") # 按mergeid将FI宽格式数据合并到data_SHARE7中 左连接，保留data_SHARE7所有行
cat("\n===== 合并FI值后的data_SHARE7数据集（前20行，展示mergeid和FI_w4~FI_w9）=====\n")
print(head(data_KLoSA7[, c("pid", all_fi_cols)], 20), row.names = FALSE)     # N = 5701
#pid  FI_w1  FI_w2  FI_w3  FI_w4  FI_w5   FI_w6   FI_w7   FI_w8   FI_w9
#<dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#1    11 0.110  0.0569 0.110  0.0730 0.139   0.192   0.191   0.496  NA     
#2    21 0.05   0.137  0.0224 0.0569 0.142   0.135   0.139   0.194   0.166 
#3    22 0.0161 0.0143 0.0232 0.0232 0.0167  0.0290  0.0519  0.0457  0.0704
#4    41 0.0839 0.0655 0.0770 0.0787 0.175   0.0951  0.151   0.151  NA     
#5    42 0.0518 0.0589 0.123  0.116  0.144   0.110   0.164   0.169  NA     
#6    51 0.118  0.0518 0.0827 0.0679 0.0907  0.120   0.198   0.157   0.160 
#7    52 0.141  0.0845 0.126  0.126  0.154   0.181   0.151   0.158   0.163 
#8    61 0.0720 0.0958 0.129  0.0339 0.104   0.169   0.169  NA      NA     
#9    62 0.05   0.0569 0.116  0.0931 0.130   0.0827  0.154   0.0679  0.115 
#10    81 0.130  0.161  0.190  0.216  0.148  NA      NA      NA      NA     
#11    82 0.0161 0.0232 0.0589 0.0589 0.110   0.0907  0.107   0.139  NA     
#12    91 0.0759 0.210  0.0845 0.0931 0.194  NA      NA      NA      NA     
#13   101 0.0155 0.103  0.05   0.0569 0.151   0.127   0.0917 NA      NA     
#14   111 0.0911 0.0839 0.101  0.112  0.0537  0.0611  0.107   0.0704  0.144 
#15   112 0.0954 0.177  0.134  0.119  0.199   0.0981  0.142   0.187   0.154 
#16   131 0.0161 0.0440 0.0232 0.0440 0.0735  0.0611  0.0704  0.102   0.122 
#17   132 0.0155 0.106  0.0914 0.0914 0.0994  0.0946  0.111   0.139   0.126 
#18   151 0.0839 0.0589 0.130  0.130  0.107   0.103   0.107   0.152   0.157 
#19   152 0.107  0.0569 0.0569 0.137  0.0708  0.0611  0.0679  0.0518  0.0970
#20   161 0.149  0.0983 0.137  0.0914 0.104   0.115   0.0679  0.148   0.120

# 在data_SHARE4中生成Frailty变量
data_KLoSA7 <- data_KLoSA7 %>% mutate(Frailty_w1 = case_when(FI_w1 >= 0.25 ~ 1, FI_w1 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w2 = case_when(FI_w2 >= 0.25 ~ 1, FI_w2 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w3 = case_when(FI_w3 >= 0.25 ~ 1, FI_w3 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w4 = case_when(FI_w4 >= 0.25 ~ 1, FI_w4 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w5 = case_when(FI_w5 >= 0.25 ~ 1, FI_w5 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w6 = case_when(FI_w6 >= 0.25 ~ 1, FI_w6 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w7 = case_when(FI_w7 >= 0.25 ~ 1, FI_w7 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w8 = case_when(FI_w8 >= 0.25 ~ 1, FI_w8 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w9 = case_when(FI_w9 >= 0.25 ~ 1, FI_w9 < 0.25 ~ 0, TRUE ~ NA_real_))
# N_KLoSA7 = 5701

# 保留 Frailty_w4 不等于1的参与者
table(data_KLoSA7$Frailty_w1, useNA = "always")
#0     1      <NA> 
#5330  371    0

data_KLoSA8 <- data_KLoSA7 %>% filter(Frailty_w1 != 1)
# N_KLoSA8 = 5330

#先备份
saveRDS(data_KLoSA8, 
        file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\2. KLoSA\\2. Backups of study participants\\KLoSA_Variability + Cumulative_20260411.rds")




