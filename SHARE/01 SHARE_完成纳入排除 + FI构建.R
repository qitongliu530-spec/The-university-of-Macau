#所需包&数据
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
library(survey)
library(minpack.lm, lib.loc = "D:/MyRPackages")

#rm(list = ls())
#data_SHARE <- read_dta ("C:\\Users\\liuqi\\Desktop\\Original Data\\SHARE\\GH_SHARE_g_rel9-0-0_ALL_datasets_stata\\GH_SHARE_g.dta")
#saveRDS(data_SHARE, file = "C:\\Users\\liuqi\\Desktop\\Original Data\\SHARE\\GH_SHARE_g_rel9-0-0_ALL_datasets_stata\\GH_SHARE_g.rds")
data_SHARE <- readRDS(file = "C:\\Users\\liuqi\\Desktop\\Original Data\\SHARE\\GH_SHARE_g_rel9-0-0_ALL_datasets_stata\\GH_SHARE_g.rds")
table(data_SHARE$country)
#选取SHARE变量
data_SHARE <- subset(data_SHARE, select = c(mergeid,
                                              #参与调查
                                              inw4, inw5, inw6, inw7, inw8, inw9,     #inw8: =1 if respondent w8
                                              r4iwstat, r5iwstat, r6iwstat, r7iwstat, r8iwstat, r9iwstat,     #wx r interview status
                                              #调查时间
                                              r4iwy, r5iwy, r6iwy, r7iwy, r8iwy, r9iwy,     #r4iwy:w4 r interview year
                                              r4iwm, r5iwm, r6iwm, r7iwm, r8iwm, r9iwm,     #r4iwm:w4 r interview month
                                              #年龄
                                              r4agey, r5agey, r6agey, r7agey, r8agey, r9agey,    #r8agey_m:w8 r age (years) at ivw midmon
                                              #BMI 变异+累积
                                              r4bmi, r5bmi, r6bmi,
                                              #Frailty相关变量
                                              # 一、日常活动能力相关变量
                                              r4dressa, r5dressa, r6dressa, r7dressa, r8dressa, r9dressa,                # 1. 穿衣（Dressing）
                                              r4walkra, r5walkra, r6walkra, r7walkra, r8walkra, r9walkra,                # 2. 室内行走（Walking in house）
                                              r4batha, r5batha, r6batha, r7batha, r8batha, r9batha,                      # 3. 洗澡（Bathing/showering）
                                              r4eata, r5eata, r6eata, r7eata, r8eata, r9eata,                            # 4. 进食（Eating）
                                              r4beda, r5beda, r6beda, r7beda, r8beda, r9beda,                            # 5. 上下床（Getting in and out of bed）
                                              r4toilta, r5toilta, r6toilta, r7toilta, r8toilta, r9toilta,                # 6. 使用厕所（Use toilet）
                                              r4mealsa, r5mealsa, r6mealsa, r7mealsa, r8mealsa, r9mealsa,                # 7. 准备热餐（Cook/prepare warm meal）
                                              r4shopa, r5shopa, r6shopa, r7shopa, r8shopa, r9shopa,                      # 8. 购买杂货（Shop groceries）
                                              r4phonea, r5phonea, r6phonea, r7phonea, r8phonea, r9phonea,                # 9. 使用电话（Use telephone）
                                              r4mapa, r5mapa, r6mapa, r7mapa, r8mapa, r9mapa,                            # 10. 使用地图（Use map）
                                              r4medsa, r5medsa, r6medsa, r7medsa, r8medsa, r9medsa,                      # 11. 服药（Take medication）
                                              r4housewka, r5housewka, r6housewka, r7housewka, r8housewka, r9housewka,    # 12. 做家务（Do housework）
                                              r4moneya, r5moneya, r6moneya, r7moneya, r8moneya, r9moneya,                # 13. 管理财务（Manage money）
                                              r4walk100a, r5walk100a, r6walk100a, r7walk100a, r8walk100a, r9walk100a,    # 14. 步行 100 米（Walking 100 meters/1 block）
                                              r4sita, r5sita, r6sita, r7sita, r8sita, r9sita,                            # 15. 久坐 2 小时（Sitting for two hours）
                                              r4chaira, r5chaira, r6chaira, r7chaira, r8chaira, r9chaira,                # 16. 从椅子站起（Getting up from chair）
                                              r4clim1a, r5clim1a, r6clim1a, r7clim1a, r8clim1a, r9clim1a,                # 17. 爬一层楼梯（Climb one flight of stairs）
                                              r4climsa, r5climsa, r6climsa, r7climsa, r8climsa, r9climsa,                # 18. 爬多层楼梯（Climb several flights of stairs）
                                              r4stoopa, r5stoopa, r6stoopa, r7stoopa, r8stoopa, r9stoopa,                # 19. 弯腰 / 屈膝（Stooping）
                                              r4armsa, r5armsa, r6armsa, r7armsa, r8armsa, r9armsa,                      # 20. 伸展手臂（Reaching）
                                              r4pusha, r5pusha, r6pusha, r7pusha, r8pusha, r9pusha,                      # 21. 推拉重物（Pulling/pushing）
                                              r4lifta, r5lifta, r6lifta, r7lifta, r8lifta, r9lifta,                      # 22. 提举重物（Lifting）
                                              r4dimea, r5dimea, r6dimea, r7dimea, r8dimea, r9dimea,                      # 23. 捡拾物品（Picking）
                                              # 二、健康状况相关变量
                                              r4hearte, r5hearte, r6hearte, r7hearte, r8hearte, r9hearte,                # 24. 心脏病（Heart attack）
                                              r4hibpe, r5hibpe, r6hibpe, r7hibpe, r8hibpe, r9hibpe,                      # 25. 高血压（High blood pressure）
                                              r4stroke, r5stroke, r6stroke, r7stroke, r8stroke, r9stroke,                # 26. 中风（Stroke）
                                              r4diabe, r5diabe, r6diabe, r7diabe, r8diabe, r9diabe,                      # 27. 糖尿病（Diabetes）
                                              r4lunge, r5lunge, r6lunge, r7lunge, r8lunge, r9lunge,                      # 28. 慢性肺病（Chronic lung disease）
                                              r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre,                # 29. 关节炎（Arthritis）
                                              r4cancre, r5cancre, r6cancre, r7cancre, r8cancre, r9cancre,                # 30. 癌症（Cancer）
                                              r4parkine, r5parkine, r6parkine, r7parkine, r8parkine, r9parkine,          # 31. 帕金森病（Parkinson’s disease）
                                              r4hipe, r5hipe, r6hipe, r7hipe, r8hipe, r9hipe,                            # 32. 髋部骨折（Hip fracture）
                                              r4fall_s, r5fall_s, r6fall_s, r7fall_s, r8fall_s, r9fall_s,                # 33. 跌倒（Falls）
                                              r4fatig, r5fatig, r6fatig, r7fatig, r8fatig, r9fatig,                      # 34. 疲劳（Fatigue）
                                              r4sleep, r5sleep, r6sleep, r7sleep, r8sleep, r9sleep,                      # 35. 睡眠问题（Sleep problems）
                                              r4depress, r5depress, r6depress, r7depress, r8depress, r9depress,          # 36. 抑郁（Depressed）
                                              r4pessim, r5pessim, r6pessim, r7pessim, r8pessim, r9pessim,                # 37. 绝望感（Hopelessness）
                                              r4appett, r5appett, r6appett, r7appett, r8appett, r9appett,                # 38. 食欲减退（Diminished appetite）
                                              # 三、健康评估相关变量（原始 + 新生成）
                                              r4shlt, r5shlt, r6shlt, r7shlt, r8shlt, r9shlt,                            # 39. 自评健康（Self-rated health）
                                              #r4bmicat, r5bmicat, r6bmicat, r7bmicat, r8bmicat, r9bmicat,               #     BMI 异常（BMI deficit）
                                              r4hearing, r5hearing, r6hearing, r7hearing, r8hearing, r9hearing,          # 40. 听力障碍（Hearing impairment）
                                              r4nsight, r5nsight, r6nsight, r7nsight, r8nsight, r9nsight,                # 41. 近视力障碍（Visual impairment - close）
                                              r4dsight, r5dsight, r6dsight, r7dsight, r8dsight, r9dsight,                # 42. 远视力障碍（Visual impairment - distant）
                                              # 四、体力活动相关变量
                                              #r4vgactx, r5vgactx, r6vgactx, r7vgactx, r8vgactx, r9vgactx,                # 43. 体力活动（Physical activity）
                                              #r4mdactx, r5mdactx, r6mdactx, r7mdactx, r8mdactx, r9mdactx,
                                              # 五、认知功能相关变量
                                              r4imrc, r5imrc, r6imrc, r7imrc, r8imrc, r9imrc,                            # 44. 即时词语回忆（Immediate word recall）
                                              r4dlrc, r5dlrc, r6dlrc, r7dlrc, r8dlrc, r9dlrc,                            # 45. 延迟词语回忆（Delayed word recall）
                                              r4verbf, r5verbf, r6verbf, r7verbf, r8verbf, r9verbf,                      # 46. 语言流畅性（Verbal fluency）
                                              #All-cause Mortality
                                              radyear,      #radyear: r death year
                                              radmonth,     #radmonth: r death month
                                              #协变量
                                              country,
                                              ragender,     #ragender: r gender
                                              h4rural,     #h4rural:w8 lives in rural or urban
                                              r4mstath,     #r4mstath:w8 r marstat-w/o part,filled
                                              raeducl,     #raeducl: r harmonized education level
                                              r4lbrf_s,     #r4lbrf_s:w8 R labor force status
                                              #r4vgactx,     #r4vgactx:w8 r freq vigorous phys activ {finer scale}
                                              #r4mdactx,     #r4mdactx:w9 r freq moderate phys activ {finer scale}
                                              r4smokev,     #r4smokev:w9 r smoke ever
                                              r4smoken,     #r4smoken:w9 r smokes now
                                              r4drinkev,     #r4drinkev:w4 R ever drank alcohol
                                              r4drink3m,     #r4drink3m:w4 R drinks any alcohol last 3 months
                                              r4drinkx,     #r4drinkx:w4 R Frequency of drinking
                                              r4drinkxw,     #r4drinkxw:w4 R drinks alcohol weekly
                                              h4atotb,     #h4atotb:w9 total of all assets--cross-wave
                                              hh4hhresp,     #hh8hhresp:w8 # core respondents in hh
                                              #r4arthre, #w8 R ever had arthritis
                                              #r4cancre, #w8 R ever had cancer
                                              #r4hibpe, #w8 R ever had high blood pressure
                                              #r4diabe, #w8 R ever had diabetes
                                              #r4lunge, #w8 R ever had lung disease
                                              #r4hearte, #w8 R ever had heart problems
                                              #r4stroke, #w8 R ever had stroke
                                              r4psyche, #w8 R ever had psych disorder
                                              
                                              # Medication ---------------------
                                              r4rxhibp,
                                              r4rxdiab,
                                              r4rxlung,
                                              r4rxheart,
                                              r4rxpsych,
                                              r5rxinflm,

                                              r4wtresp          #r4wtresp:w1 R person-level cross-sectional weight
))

# ============================================================================================================================================================
# (一) 纳入排除
# ============================================================================================================================================================
#1.inw4参与者
table(data_SHARE$inw4, useNA = "always")
data_SHARE1 <- data_SHARE[data_SHARE$inw4 == 1 & !is.na(data_SHARE$inw4), ]

#2.年龄≥50
sum(is.na(data_SHARE1$r4agey))
data_SHARE2 <- data_SHARE1[data_SHARE1$r4agey >= 50 & !(is.na(data_SHARE1$r4agey)), ]

#3.来自欧洲
table(data_SHARE2$country, useNA = "always")
data_SHARE3 <- data_SHARE2[data_SHARE2$country != 25, ]     #N = 56600

#4.没有Wave 4 5 6 的BMI测量值
summary(data_SHARE3$r4bmi, useNA = "always")
summary(data_SHARE3$r5bmi, useNA = "always")
summary(data_SHARE3$r6bmi, useNA = "always")
data_SHARE4 <- data_SHARE3 %>% filter(!is.na(r4bmi) & !is.na(r5bmi) & !is.na(r6bmi))     #N = 26814

#5. 排除未参加随访者 Wave 7 8 9
data_SHARE5 <- data_SHARE4 %>% filter(r7iwstat == 1 | r8iwstat == 1 | r9iwstat == 1)     #N = 23311

#先备份
saveRDS(data_SHARE5, file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\1. SHARE\\2. Backups of study participants\\SHARE_Variability + Cumulative_20260601.rds")

#6. 构建Frailty 并 排除基线FI≥0.25 的参与者 重头戏来咯！
##### (1) Select health-related variables 已筛选 [略]===========================
##### (2) Remove variables with >10% missing at baseline========================
all_vars <- c("mergeid",
              # ADLs/IADLs 1-13
              "r4dressa", "r5dressa", "r6dressa", "r7dressa", "r8dressa", "r9dressa",
              "r4walkra", "r5walkra", "r6walkra", "r7walkra", "r8walkra", "r9walkra",
              "r4batha", "r5batha", "r6batha", "r7batha", "r8batha", "r9batha",
              "r4eata", "r5eata", "r6eata", "r7eata", "r8eata", "r9eata",
              "r4beda", "r5beda", "r6beda", "r7beda", "r8beda", "r9beda",
              "r4toilta", "r5toilta", "r6toilta", "r7toilta", "r8toilta", "r9toilta",
              "r4mealsa", "r5mealsa", "r6mealsa", "r7mealsa", "r8mealsa", "r9mealsa",
              "r4shopa", "r5shopa", "r6shopa", "r7shopa", "r8shopa", "r9shopa",
              "r4phonea", "r5phonea", "r6phonea", "r7phonea", "r8phonea", "r9phonea",
              "r4mapa", "r5mapa", "r6mapa", "r7mapa", "r8mapa", "r9mapa",
              "r4medsa", "r5medsa", "r6medsa", "r7medsa", "r8medsa", "r9medsa",
              "r4housewka", "r5housewka", "r6housewka", "r7housewka", "r8housewka", "r9housewka",
              "r4moneya", "r5moneya", "r6moneya", "r7moneya", "r8moneya", "r9moneya",
              # MOBILITY LIMITATIONS 14-23
              "r4walk100a", "r5walk100a", "r6walk100a", "r7walk100a", "r8walk100a", "r9walk100a",
              "r4sita", "r5sita", "r6sita", "r7sita", "r8sita", "r9sita",
              "r4chaira", "r5chaira", "r6chaira", "r7chaira", "r8chaira", "r9chaira",
              "r4clim1a", "r5clim1a", "r6clim1a", "r7clim1a", "r8clim1a", "r9clim1a",
              "r4climsa", "r5climsa", "r6climsa", "r7climsa", "r8climsa", "r9climsa",
              "r4stoopa", "r5stoopa", "r6stoopa", "r7stoopa", "r8stoopa", "r9stoopa",
              "r4armsa", "r5armsa", "r6armsa", "r7armsa", "r8armsa", "r9armsa",
              "r4pusha", "r5pusha", "r6pusha", "r7pusha", "r8pusha", "r9pusha",
              "r4lifta", "r5lifta", "r6lifta", "r7lifta", "r8lifta", "r9lifta",
              "r4dimea", "r5dimea", "r6dimea", "r7dimea", "r8dimea", "r9dimea",
              # CHRONIC DISEASES 24-32
              "r4hearte", "r5hearte", "r6hearte", "r7hearte", "r8hearte", "r9hearte",
              "r4hibpe", "r5hibpe", "r6hibpe", "r7hibpe", "r8hibpe", "r9hibpe",
              "r4stroke", "r5stroke", "r6stroke", "r7stroke", "r8stroke", "r9stroke",
              "r4diabe", "r5diabe", "r6diabe", "r7diabe", "r8diabe", "r9diabe",
              "r4lunge", "r5lunge", "r6lunge", "r7lunge", "r8lunge", "r9lunge",
              "r4arthre", "r5arthre", "r6arthre", "r7arthre", "r8arthre", "r9arthre",
              "r4cancre", "r5cancre", "r6cancre", "r7cancre", "r8cancre", "r9cancre",
              "r4parkine", "r5parkine", "r6parkine", "r7parkine", "r8parkine", "r9parkine",
              "r4hipe", "r5hipe", "r6hipe", "r7hipe", "r8hipe", "r9hipe",
              # SELF-RATED HEALTH & SYMPTOMS 33-41
              "r4shlt", "r5shlt", "r6shlt", "r7shlt", "r8shlt", "r9shlt",
              #"r4vgactx", "r5vgactx", "r6vgactx", "r7vgactx", "r8vgactx", "r9vgactx",
              #"r4mdactx", "r5mdactx", "r6mdactx", "r7mdactx", "r8mdactx", "r9mdactx",
              "r4fall_s", "r5fall_s", "r6fall_s", "r7fall_s", "r8fall_s", "r9fall_s",
              "r4hearing", "r5hearing", "r6hearing", "r7hearing", "r8hearing", "r9hearing",
              "r4nsight", "r5nsight", "r6nsight", "r7nsight", "r8nsight", "r9nsight",
              "r4dsight", "r5dsight", "r6dsight", "r7dsight", "r8dsight", "r9dsight",
              "r4fatig", "r5fatig", "r6fatig", "r7fatig", "r8fatig", "r9fatig",
              "r4sleep", "r5sleep", "r6sleep", "r7sleep", "r8sleep", "r9sleep",
              # PSYCHOLOGICAL SYMPTOMS 42-44
              "r4depress", "r5depress", "r6depress", "r7depress", "r8depress", "r9depress",
              "r4pessim", "r5pessim", "r6pessim", "r7pessim", "r8pessim", "r9pessim",
              "r4appett", "r5appett", "r6appett", "r7appett", "r8appett", "r9appett",
              # COGNITIVE FUNCTIONING 45-46
              "r4imrc", "r5imrc", "r6imrc", "r7imrc", "r8imrc", "r9imrc",
              "r4dlrc", "r5dlrc", "r6dlrc", "r7dlrc", "r8dlrc", "r9dlrc",
              "r4verbf", "r5verbf", "r6verbf", "r7verbf", "r8verbf", "r9verbf")

vars_r4 <- all_vars[grepl("^r4", all_vars)]     #只看基线
data_missing_baseline <- data_SHARE5[, vars_r4, drop = FALSE]

total_sample <- nrow(data_missing_baseline)
missing_count <- colSums(is.na(data_missing_baseline))  
missing_pct <- round((missing_count / total_sample) * 100, 2)  # 缺失值比例

missing_result <- data.frame(变量名 = names(missing_count),
                             总样本量 = total_sample,
                             缺失值数量 = missing_count,
                             缺失值百分比 = missing_pct)

missing_result <- missing_result[order(-missing_result$缺失值百分比), ]
print(missing_result)

##### (3) Recode all variables to a 0–1 scale===================================
# 1. Self-rated health (1-5 → 0-1)
table(data_SHARE5$r4shlt)
vars_health <- paste0("r", 4:9, "shlt")
vars_health <- vars_health[vars_health %in% colnames(data_SHARE5)]
new_vars_health <- paste0("w", sub("r(\\d+)shlt", "\\1", vars_health), "shlt")
data_SHARE5 <- data_SHARE5 %>% 
  mutate(across(
    .cols = all_of(vars_health),
    .fns = ~ ifelse(between(., 1, 5), (.-1)/4, NA_real_) # 1-5分制转0-1，异常值设为NA
  ))

# 2. Immediate word recall (0-10 → 1-0)
table(data_SHARE5$r4imrc)
vars_wordrecord <- paste0("r", 4:9, "imrc")
vars_wordrecord <- vars_wordrecord[vars_wordrecord %in% colnames(data_SHARE5)]
new_vars_wordrecord <- paste0("w", sub("r(\\d+)imrc", "\\1", vars_wordrecord), "imrc")
data_SHARE5 <- data_SHARE5 %>% 
  mutate(across(
    .cols = all_of(vars_wordrecord),
    .fns = ~ ifelse(between(., 0, 10), . / 10, NA_real_) # 0-10分制转1-0，异常值设为NA
  ))

# 3. Delayed word recall（r4dlrc-r9dlrc → r4Delayedword-r9Delayedword）
table(data_SHARE5$r4dlrc)
vars_dlrc <- paste0("r", 4:9, "dlrc")
vars_dlrc <- vars_dlrc[vars_dlrc %in% colnames(data_SHARE5)]
new_vars_hearing <- paste0("w", sub("r(\\d+)dlrc", "\\1", vars_dlrc), "dlrc")
data_SHARE5 <- data_SHARE5 %>% 
  mutate(across(
    .cols = all_of(vars_dlrc),
    .fns = ~ ifelse(between(., 0, 10), . / 10, NA_real_) # 0-10分制转1-0，异常值设为NA
  ))

# 4. r4verbf:w4 r verbal fluency score (0-100 → 1-0)
table(data_SHARE5$r4verbf)
vars_verf <- paste0("r", 4:9, "verbf")
vars_verf <- vars_verf[vars_verf %in% colnames(data_SHARE5)]
new_vars_hearing <- paste0("w", sub("r(\\d+)verbf", "\\1", vars_verf), "verbf")
data_SHARE5 <- data_SHARE5 %>% 
  mutate(across(
    .cols = all_of(vars_verf),
    .fns = ~ ifelse(between(., 0, 100), (100 - .) / 100, NA_real_) # 0-100分制转1-0，异常值设为NA
  ))

# 5. r4hearing:w4 R self-rated hearing (1-5 -> 0, 0.25, 0.5, 0.75, 1)
table(data_SHARE5$r4hearing)
vars_verf <- paste0("r", 4:9, "hearing")
vars_verf <- vars_verf[vars_verf %in% colnames(data_SHARE5)]
new_vars_hearing <- paste0("w", sub("r(\\d+)hearing", "\\1", vars_verf), "hearing")
data_SHARE5 <- data_SHARE5 %>% 
  mutate(across(
    .cols = all_of(vars_verf),
    .fns = ~ ifelse(between(., 1, 5), (.-1)/4, NA_real_) # 0-100分制转1-0，异常值设为NA
  ))

# 6. r4nsight-r9nsight (1-5 -> 0, 0.25, 0.5, 0.75, 1)
table(data_SHARE5$r4nsight)
vars_nearsight <- paste0("r", 4:9, "nsight")
vars_nearsight <- vars_nearsight[vars_nearsight %in% colnames(data_SHARE5)]
new_vars_hearing <- paste0("w", sub("r(\\d+)nsight", "\\1", vars_nearsight), "nsight")
data_SHARE5 <- data_SHARE5 %>% 
  mutate(across(
    .cols = all_of(vars_nearsight),
    .fns = ~ ifelse(between(., 1, 5), (.-1)/4, NA_real_) # 1-5分制转0-1，异常值设为NA
  ))

# 7. r4dsight-r9dsight (1-5 -> 0, 0.25, 0.5, 0.75, 1)
table(data_SHARE5$r4dsight)
vars_distantsight <- paste0("r", 4:9, "dsight")
vars_distantsight <- vars_distantsight[vars_distantsight %in% colnames(data_SHARE5)]
new_vars_hearing <- paste0("w", sub("r(\\d+)dsight", "\\1", vars_distantsight), "dsight")
data_SHARE5 <- data_SHARE5 %>% 
  mutate(across(
    .cols = all_of(vars_distantsight),
    .fns = ~ ifelse(between(., 1, 5), (.-1)/4, NA_real_) # 1-5分制转0-1，异常值设为NA
  ))

# 8. r4vgactx-r9vgactx r4mdactx-r9mdactx
# 2.> 1 per week     3.1 per week     4.1-3 per mon(0.25-0.75 per week)     5.hardly ever or never
waves <- 4:9
for (w in waves) {
  data_SHARE5[[paste0("r", w, "phyact")]] <- ifelse(
    data_SHARE5[[paste0("r", w, "vgactx")]] + data_SHARE5[[paste0("r", w, "mdactx")]] <= 5,
    1, 0)}
for (w in waves) {
  var <- paste0("r", w, "phyact")
  cat("=== 变量", var, "的频数表 ===\n")
  print(table(data_SHARE5[[var]], useNA = "ifany"))
  cat("\n")
}

##### (4) Exclude rare (<1%) or saturated (>80%) deficits=======================
target_vars <- c(
  # ADLs/IADLs 1-13
  "r4dressa", "r5dressa", "r6dressa", "r7dressa", "r8dressa", "r9dressa",
  "r4walkra", "r5walkra", "r6walkra", "r7walkra", "r8walkra", "r9walkra",
  "r4batha", "r5batha", "r6batha", "r7batha", "r8batha", "r9batha",
  "r4eata", "r5eata", "r6eata", "r7eata", "r8eata", "r9eata",
  "r4beda", "r5beda", "r6beda", "r7beda", "r8beda", "r9beda",
  "r4toilta", "r5toilta", "r6toilta", "r7toilta", "r8toilta", "r9toilta",
  "r4mealsa", "r5mealsa", "r6mealsa", "r7mealsa", "r8mealsa", "r9mealsa",
  "r4shopa", "r5shopa", "r6shopa", "r7shopa", "r8shopa", "r9shopa",
  "r4phonea", "r5phonea", "r6phonea", "r7phonea", "r8phonea", "r9phonea",
  "r4mapa", "r5mapa", "r6mapa", "r7mapa", "r8mapa", "r9mapa",
  "r4medsa", "r5medsa", "r6medsa", "r7medsa", "r8medsa", "r9medsa",
  "r4housewka", "r5housewka", "r6housewka", "r7housewka", "r8housewka", "r9housewka",
  "r4moneya", "r5moneya", "r6moneya", "r7moneya", "r8moneya", "r9moneya",
  # MOBILITY LIMITATIONS 14-23
  "r4walk100a", "r5walk100a", "r6walk100a", "r7walk100a", "r8walk100a", "r9walk100a",
  "r4sita", "r5sita", "r6sita", "r7sita", "r8sita", "r9sita",
  "r4chaira", "r5chaira", "r6chaira", "r7chaira", "r8chaira", "r9chaira",
  "r4clim1a", "r5clim1a", "r6clim1a", "r7clim1a", "r8clim1a", "r9clim1a",
  "r4climsa", "r5climsa", "r6climsa", "r7climsa", "r8climsa", "r9climsa",
  "r4stoopa", "r5stoopa", "r6stoopa", "r7stoopa", "r8stoopa", "r9stoopa",
  "r4armsa", "r5armsa", "r6armsa", "r7armsa", "r8armsa", "r9armsa",
  "r4pusha", "r5pusha", "r6pusha", "r7pusha", "r8pusha", "r9pusha",
  "r4lifta", "r5lifta", "r6lifta", "r7lifta", "r8lifta", "r9lifta",
  "r4dimea", "r5dimea", "r6dimea", "r7dimea", "r8dimea", "r9dimea",
  # CHRONIC DISEASES 24-32
  "r4hearte", "r5hearte", "r6hearte", "r7hearte", "r8hearte", "r9hearte",
  "r4hibpe", "r5hibpe", "r6hibpe", "r7hibpe", "r8hibpe", "r9hibpe",
  "r4stroke", "r5stroke", "r6stroke", "r7stroke", "r8stroke", "r9stroke",
  "r4diabe", "r5diabe", "r6diabe", "r7diabe", "r8diabe", "r9diabe",
  "r4lunge", "r5lunge", "r6lunge", "r7lunge", "r8lunge", "r9lunge",
  "r4arthre", "r5arthre", "r6arthre", "r7arthre", "r8arthre", "r9arthre",
  "r4cancre", "r5cancre", "r6cancre", "r7cancre", "r8cancre", "r9cancre",
  "r4parkine", "r5parkine", "r6parkine", "r7parkine", "r8parkine", "r9parkine",
  "r4hipe", "r5hipe", "r6hipe", "r7hipe", "r8hipe", "r9hipe",
  # SELF-RATED HEALTH & SYMPTOMS 33-41
  "r4shlt", "r5shlt", "r6shlt", "r7shlt", "r8shlt", "r9shlt",
  #"r4phyact", "r5phyact", "r6phyact", "r7phyact", "r8phyact", "r9phyact",
  #"r4vgactx", "r5vgactx", "r6vgactx", "r7vgactx", "r8vgactx", "r9vgactx",       
  #"r4mdactx", "r5mdactx", "r6mdactx", "r7mdactx", "r8mdactx", "r9mdactx",
  "r4fall_s", "r5fall_s", "r6fall_s", "r7fall_s", "r8fall_s", "r9fall_s",
  "r4hearing", "r5hearing", "r6hearing", "r7hearing", "r8hearing", "r9hearing",
  "r4nsight", "r5nsight", "r6nsight", "r7nsight", "r8nsight", "r9nsight",
  "r4dsight", "r5dsight", "r6dsight", "r7dsight", "r8dsight", "r9dsight",
  "r4fatig", "r5fatig", "r6fatig", "r7fatig", "r8fatig", "r9fatig",
  "r4sleep", "r5sleep", "r6sleep", "r7sleep", "r8sleep", "r9sleep",
  # PSYCHOLOGICAL SYMPTOMS 42-44
  "r4depress", "r5depress", "r6depress", "r7depress", "r8depress", "r9depress",
  "r4pessim", "r5pessim", "r6pessim", "r7pessim", "r8pessim", "r9pessim",
  "r4appett", "r5appett", "r6appett", "r7appett", "r8appett", "r9appett",
  # COGNITIVE FUNCTIONING 45-46
  "r4imrc", "r5imrc", "r6imrc", "r7imrc", "r8imrc", "r9imrc",
  "r4dlrc", "r5dlrc", "r6dlrc", "r7dlrc", "r8dlrc", "r9dlrc",
  "r4verbf", "r5verbf", "r6verbf", "r7verbf", "r8verbf", "r9verbf")

vars_r4_1_80 <- target_vars[grepl("^r4", target_vars)]
existing_targets <- intersect(vars_r4_1_80, names(data_SHARE5))

means <- round(colMeans(data_SHARE5[, existing_targets, drop = FALSE], na.rm = TRUE), 4) 

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

cat("===== data_SHARE5 中目标变量的均值及筛选结果 =====\n")
print(result_df, row.names = FALSE)
#不满足条件的变量如下
#r4walkra 0.0096           否 0.01 < 均值 < 0.80
#r4eata 0.0097           否 0.01 < 均值 < 0.80
#r4phonea 0.0086           否 0.01 < 均值 < 0.80
#r4medsa 0.0072           否 0.01 < 均值 < 0.80
#r4parkine 0.0055           否 0.01 < 均值 < 0.80

existing_rare_vars <- intersect(rare_saturated_vars, names(data_SHARE5))
data_SHARE6 <- data_SHARE5[, setdiff(names(data_SHARE5), existing_rare_vars), drop = FALSE]

##### (5) Ensure age-related trend==============================================
vars_r4 <- target_vars[grepl("^r4", target_vars)]                               # 提取Wave4的目标变量（以r4开头）
valid_r4_vars <- result_df$变量名[result_df$是否符合条件 == "是"]               # 从result_df中筛选出"是否符合条件"为"是"的Wave4变量
final_r4_vars <- intersect(valid_r4_vars, names(data_SHARE6))                   # 确保这些变量在data_SHARE6中存在

cat("===== Wave4符合条件的变量筛选结果 =====\n",
    "Wave4目标变量总数：", length(vars_r4), "\n",
    "符合0.01<均值<0.80的变量数：", length(valid_r4_vars), "\n",
    "data_SHARE6中实际存在的符合条件变量数：", length(final_r4_vars), "\n",
    "符合条件的Wave4变量列表：\n", sep = "")
print(final_r4_vars)

analysis_data <- data_SHARE6 %>% select(r4agey, all_of(final_r4_vars)) %>% drop_na(r4agey) # 提取分析数据集：仅保留年龄变量（r4agey）和符合条件的Wave4变量 删除年龄缺失的行

# 相关性检验：Wave4变量与年龄的皮尔逊相关 
corr_results <- data.frame(变量名 = character(),
                           相关系数 = numeric(),
                           P值 = numeric(),
                           相关性方向 = character(),
                           显著性 = character(),
                           stringsAsFactors = FALSE)

for (var in final_r4_vars) {
  clean_data <- analysis_data %>% select(r4agey, all_of(var)) %>% drop_na()           # 去除变量缺失值
  corr_test <- cor.test(clean_data$r4agey, clean_data[[var]], method = "pearson")     # 皮尔逊相关检验（有序变量可改用method = "spearman"）
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

# 删除"负相关"的变量形成data_SHARE7
neg_corr_r4_vars <- corr_results$变量名[corr_results$相关性方向 == "负相关"]
existing_neg_vars <- intersect(unlist(lapply(neg_corr_r4_vars, function(var) paste0("r", 4:9, sub("^r4", "", var)))), names(data_SHARE6))
cat("删除的变量：", paste(existing_neg_vars, collapse = ", "), "\n")

data_SHARE7 <- data_SHARE6[, setdiff(names(data_SHARE6), existing_neg_vars), drop = FALSE]


##### (6) Check for collinearity (r > 0.95)
calculate_wave4_correlation <- function(data, target_vars, method = "spearman", high_corr_thresh = 0.95) {
  w4_vars <- intersect(target_vars[grepl("^r4", target_vars)], names(data))     # 提取Wave4有效变量
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

calculate_wave4_correlation(data_SHARE7, target_vars, "spearman", 0.95)         # 调用函数

# 形成高共线性变量数据集（虽然没有变量高共线性，但便于后续筛选）
calculate_wave4_correlation_with_return <- function(data, target_vars, method = "spearman", high_corr_thresh = 0.95) {
  w4_vars <- intersect(target_vars[grepl("^r4", target_vars)], names(data))
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

high_corr_pairs <- calculate_wave4_correlation_with_return(data_SHARE7, target_vars, "spearman", 0.95)     # 提取Wave4高相关变量对
# 定义高共线性变量删除规则：每对中删除第二个变量（可根据需求调整）
if (!is.null(high_corr_pairs)) {
  del_w4_coll_vars <- unique(high_corr_pairs$变量2) # 提取Wave4需删除的高相关变量
  # 生成所有Wave的高相关待删除变量
  del_coll_vars <- unlist(lapply(del_w4_coll_vars, function(var) {
    suffix <- sub("^r4", "", var)
    paste0("r", 4:9, suffix)
  }))
  cat("需删除的高共线性变量（所有Wave）：", paste(del_coll_vars, collapse = ", "), "\n")
} else {
  del_coll_vars <- c() # 无高相关变量时，待删除列表为空
  cat("无高共线性变量需删除\n")
}

##### (7) Calculate individual FI===============================================
# 从data_SHARE7中提取最终符合所有条件的target_vars子集，生成用于 FI 构建的数据集
final_fi_target_vars <- setdiff(target_vars, c(rare_saturated_vars, existing_neg_vars, del_coll_vars))       # 步骤1：从target_vars中排除稀有/饱和变量、负相关变量、高共线性变量
final_fi_vars <- intersect(final_fi_target_vars, names(data_SHARE7))            # 步骤2：筛选data_SHARE7中实际存在的最终变量（避免变量不存在）
key_cols <- c("mergeid")                                                        # 步骤3：保留关键标识列（mergeid）+ 最终FI变量，生成数据集
fi_data <- data_SHARE7[, c(key_cols, final_fi_vars), drop = FALSE]

# 按Wave拆分最终FI变量
fi_var_wave <- data.frame(var_name = final_fi_vars,
                          wave = gsub("r", "", str_extract(final_fi_vars, "^r\\d+")), # 提取Wave数字
                          stringsAsFactors = FALSE)
fi_var_wave <- fi_var_wave[!is.na(fi_var_wave$wave) & fi_var_wave$wave != "", ] # 过滤无效Wave
wave_fi_vars <- split(fi_var_wave$var_name, fi_var_wave$wave)                   # 按Wave分组FI变量
min_var_count <- 25      #SHARE的阈值是25

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
  wave_data <- fi_data %>% select(mergeid, all_of(current_vars))

    # 核心优化：向量化计算非缺失值个数和总和
  var_matrix <- as.matrix(wave_data[, current_vars, drop = FALSE])              # 转为矩阵
  non_missing_count <- rowSums(!is.na(var_matrix))                              # 向量化计算每行非缺失值个数
  var_sum <- rowSums(var_matrix, na.rm = TRUE)                                  # 向量化计算每行总和
  
  # 计算FI值（向量化判断，无循环）
  fi_value <- ifelse(non_missing_count >= min_var_count, var_sum / non_missing_count, NA)
  
  # 组装结果并加入列表
  wave_fi <- data.frame(
    mergeid = wave_data$mergeid,
    non_missing_count = non_missing_count,
    fi_value = fi_value,
    wave = wave,
    stringsAsFactors = FALSE
  )
  fi_result_list[[i]] <- wave_fi
  i <- i + 1
}

fi_result <- do.call(rbind, fi_result_list)                                     # 合并列表为数据框

# 整理结果 ---------------------------------------------------------------------20260601修改
fi_final <- fi_result %>% 
  # select 内部直接完成列选取 + 重命名，不再单独使用 rename
  select(
    mergeid,
    波次 = wave,
    非缺失值变量数 = non_missing_count,
    FI值 = fi_value
  ) %>% 
  arrange(mergeid, 波次)
cat("\n===== 各mergeid各Wave的FI值（前250行）=====\n")
print(head(fi_final, 250), row.names = FALSE)

# 统计各Wave的FI值有效情况
wave_fi_summary <- fi_final %>% group_by(波次) %>%
  summarise(总参与者数 = dplyr::n_distinct(mergeid),
            FI有效数 = sum(!is.na(FI值)),
            FI缺失数 = sum(is.na(FI值)),
            平均FI值 = round(mean(FI值, na.rm = TRUE), 4)) %>% ungroup()
cat("\n===== 各Wave FI值统计汇总 =====\n")
print(wave_fi_summary, row.names = FALSE)

# 将FI值按Wave合并到data_SHARE7中
fi_wide <- fi_final %>% select(mergeid, 波次, FI值) %>%
  # 重命名波次为FI_w+数字（如4→FI_w4）
  mutate(FI列名 = paste0("FI_w", 波次)) %>%
  # 转换为宽格式：每个Wave的FI值作为单独列，无数据则为NA
  pivot_wider(id_cols = mergeid, names_from = FI列名, values_from = FI值, values_fill = NA)

all_fi_cols <- paste0("FI_w", 4:9)
for (col in all_fi_cols) {
  if (!col %in% colnames(fi_wide)) {
    fi_wide[[col]] <- NA
  }
}

data_SHARE8 <- data_SHARE7 %>% left_join(fi_wide, by = "mergeid") # 按mergeid将FI宽格式数据合并到data_SHARE7中 左连接，保留data_SHARE7所有行
cat("\n===== 合并FI值后的data_SHARE7数据集（前20行，展示mergeid和FI_w4~FI_w9）=====\n")
print(head(data_SHARE8[, c("mergeid", all_fi_cols)], 20), row.names = FALSE)

# 在data_SHARE4中生成Frailty变量
data_SHARE8 <- data_SHARE8 %>% mutate(Frailty_w4 = case_when(FI_w4 >= 0.25 ~ 1, FI_w4 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w5 = case_when(FI_w5 >= 0.25 ~ 1, FI_w5 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w6 = case_when(FI_w6 >= 0.25 ~ 1, FI_w6 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w7 = case_when(FI_w7 >= 0.25 ~ 1, FI_w7 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w8 = case_when(FI_w8 >= 0.25 ~ 1, FI_w8 < 0.25 ~ 0, TRUE ~ NA_real_),
                                      Frailty_w9 = case_when(FI_w9 >= 0.25 ~ 1, FI_w9 < 0.25 ~ 0, TRUE ~ NA_real_))
# 保留 Frailty_w4 不等于1的参与者
table(data_SHARE8$Frailty_w4, useNA = "always")
data_SHARE8 <- data_SHARE8 %>% filter(Frailty_w4 != 1)

#先备份
saveRDS(data_SHARE8, file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\1. SHARE\\2. Backups of study participants\\SHARE_Variability + Cumulative_20251128.rds")
