library(tidyverse) #data tidying
library(leaps) #data tidying
library(ggplot2) #visualizing data
library(corrplot) #visualizing data
library(plyr) #data tidying
library(dplyr) 
library(glmnet) #for modeling
library(caret) #for modeling
library(xgboost) #for modeling
library(randomForest) #for modeling
library(pander) #data collation
library(mice) #check missing values
library(ggalluvial)
library(haven)
library(MCM)
library(ggsci)
library(reshape2)

setwd('D:\\R project\\Frailty\\HRS')

data_r <- read_dta('C:\\Users\\liuqi\\Desktop\\Original Data\\HRS\\randhrs1992_2022v1_STATA\\randhrs1992_2022v1.dta')
data_h <- read_dta('C:\\Users\\liuqi\\Desktop\\Original Data\\HRS\\HarmonizedHRSvC_STATA\\H_HRS_c.dta')

subdat_r <- subset(data_r,
                   select = c(inw7,inw8,inw9,inw10,inw11,inw12,inw13,inw14,inw15,inw16,
                              r11iwstat,r12iwstat,r13iwstat,r14iwstat,r10wtresp,r15iwstat,r16iwstat,
                              hhid,hhidpn,
                              r1iwmid,r2iwmid,r3iwmid,r4iwmid,r5iwmid,r6iwmid,r7iwmid,r8iwmid,
                              r9iwmid,r10iwmid,r11iwmid,r12iwmid,r13iwmid,r14iwmid,r15iwmid,r16iwmid,#interview time
                              #demographics
                              rabyear,   #birth year
                              r7agey_e, r10agey_e, #age
                              #socioeconomic
                              h10atotb, #household wealth
                              h10hhres, #people living with
                              r10lbrf, #labour force status
                              r10mstath, #marital status
                              #health related
                              r10conde, # number of chronic diseases (0-8)
                              r10tr20, # word recall
                              #behavioral
                              r10smokev, r10smoken, # ever and current smoking
                              r10drink, r10drinkd,  # currently drinks, and number of day/week drinks
                              r10vgactx, # vigorous activity
                              r10mdactx, # moderate activity
                              #exposre
                              r7walkra, r8walkra, r9walkra, r10walkra, r11walkra, r12walkra, r13walkra, r14walkra, r15walkra, r16walkra,
                              r7dressa, r8dressa, r9dressa, r10dressa, r11dressa, r12dressa, r13dressa, r14dressa, r15dressa, r16dressa,
                              r7batha, r8batha, r9batha, r10batha, r11batha, r12batha, r13batha, r14batha, r15batha, r16batha,
                              r7eata, r8eata, r9eata, r10eata, r11eata, r12eata, r13eata, r14eata, r15eata, r16eata,
                              r7beda, r8beda, r9beda, r10beda, r11beda, r12beda, r13beda, r14beda, r15beda, r16beda,
                              r7toilta, r8toilta, r9toilta, r10toilta, r11toilta, r12toilta, r13toilta, r14toilta, r15toilta, r16toilta,
                              
                              # ========== IADL ==========
                              r7shopa, r8shopa, r9shopa, r10shopa, r11shopa, r12shopa, r13shopa, r14shopa, r15shopa, r16shopa,
                              r7phonea, r8phonea, r9phonea, r10phonea, r11phonea, r12phonea, r13phonea, r14phonea, r15phonea, r16phonea,
                              r7moneya, r8moneya, r9moneya, r10moneya, r11moneya, r12moneya, r13moneya, r14moneya, r15moneya, r16moneya,
                              
                              # ========== Mobility ==========
                              r7clim1a, r8clim1a, r9clim1a, r10clim1a, r11clim1a, r12clim1a, r13clim1a, r14clim1a, r15clim1a, r16clim1a,
                              r7clims, r8clims, r9clims, r10clims, r11clims, r12clims, r13clims, r14clims, r15clims, r16clims,
                              r7walk1a, r8walk1a, r9walk1a, r10walk1a, r11walk1a, r12walk1a, r13walk1a, r14walk1a, r15walk1a, r16walk1a,
                              r7sita, r8sita, r9sita, r10sita, r11sita, r12sita, r13sita, r14sita, r15sita, r16sita,
                              r7chaira, r8chaira, r9chaira, r10chaira, r11chaira, r12chaira, r13chaira, r14chaira, r15chaira, r16chaira,
                              r7stoopa, r8stoopa, r9stoopa, r10stoopa, r11stoopa, r12stoopa, r13stoopa, r14stoopa, r15stoopa, r16stoopa,
                              r7armsa, r8armsa, r9armsa, r10armsa, r11armsa, r12armsa, r13armsa, r14armsa, r15armsa, r16armsa,
                              r7dimea, r8dimea, r9dimea, r10dimea, r11dimea, r12dimea, r13dimea, r14dimea, r15dimea, r16dimea,
                              
                              # ========== Chronic Disease ==========
                              r7hearte, r8hearte, r9hearte, r10hearte, r11hearte, r12hearte, r13hearte, r14hearte, r15hearte, r16hearte,
                              r7hibpe, r8hibpe, r9hibpe, r10hibpe, r11hibpe, r12hibpe, r13hibpe, r14hibpe, r15hibpe, r16hibpe,
                              r7stroke, r8stroke, r9stroke, r10stroke, r11stroke, r12stroke, r13stroke, r14stroke, r15stroke, r16stroke,
                              r7diabe, r8diabe, r9diabe, r10diabe, r11diabe, r12diabe, r13diabe, r14diabe, r15diabe, r16diabe,
                              r7arthre, r8arthre, r9arthre, r10arthre, r11arthre, r12arthre, r13arthre, r14arthre, r15arthre, r16arthre,
                              r7cancre, r8cancre, r9cancre, r10cancre, r11cancre, r12cancre, r13cancre, r14cancre, r15cancre, r16cancre,
                              r7psyche, r8psyche, r9psyche, r10psyche, r11psyche, r12psyche, r13psyche, r14psyche, r15psyche, r16psyche,
                              
                              # ========== Self-rated Health ==========
                              r7shlt, r8shlt, r9shlt, r10shlt, r11shlt, r12shlt, r13shlt, r14shlt, r15shlt, r16shlt,
                              #r7pmbmi, r8pmbmi, r9pmbmi, r10pmbmi, r11pmbmi, r12pmbmi, r13pmbmi, r14pmbmi, r16pmbmi,
                              r7sleepr, r8sleepr, r9sleepr, r10sleepr, r11sleepr, r12sleepr, r13sleepr, r14sleepr, r15sleepr, r16sleepr,
                              r7depres, r8depres, r9depres, r10depres, r11depres, r12depres, r13depres, r14depres, r15depres, r16depres,
                              r7fsad, r8fsad, r9fsad, r10fsad, r11fsad, r12fsad, r13fsad, r14fsad, r15fsad, r16fsad,
                              r7effort, r8effort, r9effort, r10effort, r11effort, r12effort, r13effort, r14effort, r15effort, r16effort,
                              r7whappy, r8whappy, r9whappy, r10whappy, r11whappy, r12whappy, r13whappy, r14whappy, r15whappy, r16whappy,
                              r7flone, r8flone, r9flone, r10flone, r11flone, r12flone, r13flone, r14flone, r15flone, r16flone,
                              r7going, r8going, r9going, r10going, r11going, r12going, r13going, r14going, r15going, r16going,
                              r7enlife, r8enlife, r9enlife, r10enlife, r11enlife, r12enlife, r13enlife, r14enlife, r15enlife, r16enlife,
                              # ========== Cognitive ==========
                              r7imrc, r8imrc, r9imrc, r10imrc, r11imrc, r12imrc, r13imrc, r14imrcp, r15imrcp,
                              r7dlrc, r8dlrc, r9dlrc, r10dlrc, r11dlrc, r12dlrc, r13dlrc, r14dlrcp, r15dlrcp,
                              r7slfmem, r8slfmem, r9slfmem, r10slfmem, r11slfmem, r12slfmem, r13slfmem, r14slfmem, r15slfmem,
                              r10alzhee, r11alzhee, r12alzhee, r13alzhee, r14alzhee, r15alzhee, r16alzhee,   # r7alzhee, r8alzhee, r9alzhee, 
                              r10demene, r11demene, r12demene, r13demene, r14demene, r15demene, r16demene,   # r7demene, r8demene, r9demene, 
                              # ========== Others ==========
                              r7heart, r8heart, r9heart, r10heart, r11heart, r12heart, r13heart, r14heart, r15heart, r16heart,
                              r7hlthlm, r8hlthlm, r9hlthlm, r10hlthlm, r11hlthlm, r12hlthlm, r13hlthlm, r14hlthlm, r15hlthlm, r16hlthlm, 
                              # covariates
                              r7agey_m,ragender,raracem,rahispan,r7lbrf,r7vgactx,r7mdactx,r7mstath, 
                              r7smokev, r7smoken,
                              r7drink, r7drinkd, 
                              h7atotb, #household wealth
                              h7hhres, #people living with
                              r7lunge,
                              # exposure
                              # bmi
                              r1bmi,r2bmi,r3bmi,r4bmi,r5bmi,r6bmi,r7bmi,r8bmi,r9bmi))

subdat_h <- subset(data_h,
                   select = c(hhid,hhidpn,
                              r7mbmi,r8mbmi,r9mbmi,
                              ## Medication
                              r7rxhibp, r7rxdiab, r7cncrchem, r7rxlung, r7rxangina, r7rxchf, r7rxhrtat, r7rxheart, r7rxpsych, r7rxarthr,
                              ## disease
                              r7urinai,r8urinai,r9urinai,r10urinai,r11urinai,r12urinai,r13urinai,r14urinai,#r15urinai,r16urinai,
                              r7fallinj,r8fallinj,r9fallinj,r10fallinj,r11fallinj,r12fallinj,r13fallinj,r14fallinj,#r15fallinj,r16fallinj,
                              r7hipe,r8hipe,r9hipe,r10hipe,r11hipe,r12hipe,r13hipe,r14hipe,#r15hipe,r16hipe,
                              ## self rated health
                              #r10hearing,r11hearing,r12hearing,r13hearing,r14hearing,#r15hearing,r16hearing,
                              r7dsight,r8dsight,r9dsight,r10dsight,r11dsight,r12dsight,r13dsight,r14dsight,#r15dsight,r16dsight,
                              r7nsight,r8nsight,r9nsight,r10nsight,r11nsight,r12nsight,r13nsight,r14nsight,#r15nsight,r16nsight,
                              r7painfr,r8painfr,r9painfr,r10painfr,r11painfr,r12painfr,r13painfr,r14painfr,#r15painfr,r16painfr,
                              ## education
                              raeducl,h7rural))  

data01 <- merge(subdat_r, subdat_h, by = c("hhid","hhidpn"))
write.csv(data01,'D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\3. HRS\\2. Backups of study participants\\hrs_20260401.csv')



# ============================================================================================================================================================
# (一) 纳入排除
# ============================================================================================================================================================
data_HRS <- read.csv(file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\3. HRS\\2. Backups of study participants\\hrs_20260401.csv")
# N =42232

#1.参加wave7
data_HRS <- data_HRS[data_HRS$inw7 == 1 & !is.na(data_HRS$inw7), ]

#2.年龄≥50
sum(is.na(data_HRS$r7agey))
data_HRS <- data_HRS[data_HRS$r7agey_e >= 50 & !(is.na(data_HRS$r7agey_e)), ]     # N = 19280

#4.没有Wave 789的BMI测量值
summary(data_HRS$r7bmi, useNA = "always")
summary(data_HRS$r8bmi, useNA = "always")
summary(data_HRS$r9bmi, useNA = "always")
data_HRS <- data_HRS %>% filter(!is.na(r7bmi) & !is.na(r8bmi) & !is.na(r9bmi))     #N = 14796

#5. 排除未参加随访者 Wave 10-14
data_HRS <- data_HRS %>% filter(inw10 == 1 | inw11 == 1 | inw12 == 1| inw13 == 1| inw14 == 1| inw15 == 1| inw16 == 1)     #N = 13237
#data_HRS <- data_HRS %>% filter(inw10 == 1) 


#6. 构建Frailty 并 排除基线FI≥0.25 的参与者 重头戏来咯！
##### (1) Select health-related variables 已筛选 [略]===========================
##### (2) Remove variables with >10% missing at baseline========================
cat("=====================================\n")
cat("STEP 1: Select health-related variables\n")
cat("=====================================\n\n")

health_vars <- c(
  "r7walkra", "r8walkra", "r9walkra", "r10walkra", "r11walkra", "r12walkra", "r13walkra", "r14walkra", "r15walkra", "r16walkra",
  "r7dressa", "r8dressa", "r9dressa", "r10dressa", "r11dressa", "r12dressa", "r13dressa", "r14dressa", "r15dressa", "r16dressa",
  "r7batha", "r8batha", "r9batha", "r10batha", "r11batha", "r12batha", "r13batha", "r14batha", "r15batha", "r16batha",
  "r7eata", "r8eata", "r9eata", "r10eata", "r11eata", "r12eata", "r13eata", "r14eata", "r15eata", "r16eata",
  "r7beda", "r8beda", "r9beda", "r10beda", "r11beda", "r12beda", "r13beda", "r14beda", "r15beda", "r16beda",
  "r7toilta", "r8toilta", "r9toilta", "r10toilta", "r11toilta", "r12toilta", "r13toilta", "r14toilta", "r15toilta", "r16toilta",
  
  # ========== IADL ==========
  "r7shopa", "r8shopa", "r9shopa", "r10shopa", "r11shopa", "r12shopa", "r13shopa", "r14shopa", "r15shopa", "r16shopa",
  "r7phonea", "r8phonea", "r9phonea", "r10phonea", "r11phonea", "r12phonea", "r13phonea", "r14phonea", "r15phonea", "r16phonea",
  "r7moneya", "r8moneya", "r9moneya", "r10moneya", "r11moneya", "r12moneya", "r13moneya", "r14moneya", "r15moneya", "r16moneya",
  
  # ========== Mobility ==========
  "r7clim1a", "r8clim1a", "r9clim1a", "r10clim1a", "r11clim1a", "r12clim1a", "r13clim1a", "r14clim1a", "r15clim1a", "r16clim1a",
  "r7clims", "r8clims", "r9clims", "r10clims", "r11clims", "r12clims", "r13clims", "r14clims", "r15clims", "r16clims",
  "r7walk1a", "r8walk1a", "r9walk1a", "r10walk1a", "r11walk1a", "r12walk1a", "r13walk1a", "r14walk1a", "r15walk1a", "r16walk1a",
  "r7sita", "r8sita", "r9sita", "r10sita", "r11sita", "r12sita", "r13sita", "r14sita", "r15sita", "r16sita",
  "r7chaira", "r8chaira", "r9chaira", "r10chaira", "r11chaira", "r12chaira", "r13chaira", "r14chaira", "r15chaira", "r16chaira",
  "r7stoopa", "r8stoopa", "r9stoopa", "r10stoopa", "r11stoopa", "r12stoopa", "r13stoopa", "r14stoopa", "r15stoopa", "r16stoopa",
  "r7armsa", "r8armsa", "r9armsa", "r10armsa", "r11armsa", "r12armsa", "r13armsa", "r14armsa", "r15armsa", "r16armsa",
  "r7dimea", "r8dimea", "r9dimea", "r10dimea", "r11dimea", "r12dimea", "r13dimea", "r14dimea", "r15dimea", "r16dimea",
  
  # ========== Chronic Disease ==========
  "r7hearte", "r8hearte", "r9hearte", "r10hearte", "r11hearte", "r12hearte", "r13hearte", "r14hearte", "r15hearte", "r16hearte",
  "r7hibpe", "r8hibpe", "r9hibpe", "r10hibpe", "r11hibpe", "r12hibpe", "r13hibpe", "r14hibpe", "r15hibpe", "r16hibpe",
  "r7stroke", "r8stroke", "r9stroke", "r10stroke", "r11stroke", "r12stroke", "r13stroke", "r14stroke", "r15stroke", "r16stroke",
  "r7diabe", "r8diabe", "r9diabe", "r10diabe", "r11diabe", "r12diabe", "r13diabe", "r14diabe", "r15diabe", "r16diabe",
  "r7arthre", "r8arthre", "r9arthre", "r10arthre", "r11arthre", "r12arthre", "r13arthre", "r14arthre", "r15arthre", "r16arthre",
  "r7cancre", "r8cancre", "r9cancre", "r10cancre", "r11cancre", "r12cancre", "r13cancre", "r14cancre", "r15cancre", "r16cancre",
  "r7psyche", "r8psyche", "r9psyche", "r10psyche", "r11psyche", "r12psyche", "r13psyche", "r14psyche", "r15psyche", "r16psyche",
  
  # ========== Self-rated Health ==========
  "r7shlt", "r8shlt", "r9shlt", "r10shlt", "r11shlt", "r12shlt", "r13shlt", "r14shlt", "r15shlt", "r16shlt",
  #"r7pmbmi", "r8pmbmi", "r9pmbmi", "r10pmbmi", "r11pmbmi", "r12pmbmi", "r13pmbmi", "r14pmbmi", "r16pmbmi",
  "r7sleepr", "r8sleepr", "r9sleepr", "r10sleepr", "r11sleepr", "r12sleepr", "r13sleepr", "r14sleepr", "r15sleepr", "r16sleepr",
  "r7depres", "r8depres", "r9depres", "r10depres", "r11depres", "r12depres", "r13depres", "r14depres", "r15depres", "r16depres",
  "r7fsad", "r8fsad", "r9fsad", "r10fsad", "r11fsad", "r12fsad", "r13fsad", "r14fsad", "r15fsad", "r16fsad",
  "r7effort", "r8effort", "r9effort", "r10effort", "r11effort", "r12effort", "r13effort", "r14effort", "r15effort", "r16effort",
  "r7whappy", "r8whappy", "r9whappy", "r10whappy", "r11whappy", "r12whappy", "r13whappy", "r14whappy", "r15whappy", "r16whappy",
  "r7flone", "r8flone", "r9flone", "r10flone", "r11flone", "r12flone", "r13flone", "r14flone", "r15flone", "r16flone",
  "r7going", "r8going", "r9going", "r10going", "r11going", "r12going", "r13going", "r14going", "r15going", "r16going",
  "r7enlife", "r8enlife", "r9enlife", "r10enlife", "r11enlife", "r12enlife", "r13enlife", "r14enlife", "r15enlife", "r16enlife",
  
  # ========== Cognitive ==========
  "r7imrc", "r8imrc", "r9imrc", "r10imrc", "r11imrc", "r12imrc", "r13imrc", "r14imrcp", "r15imrcp",
  "r7dlrc", "r8dlrc", "r9dlrc", "r10dlrc", "r11dlrc", "r12dlrc", "r13dlrc", "r14dlrcp", "r15dlrcp",
  "r7slfmem", "r8slfmem", "r9slfmem", "r10slfmem", "r11slfmem", "r12slfmem", "r13slfmem", "r14slfmem", "r15slfmem",
  "r10alzhee", "r11alzhee", "r12alzhee", "r13alzhee", "r14alzhee", "r15alzhee", "r16alzhee",   # "r7alzhee", "r8alzhee", "r9alzhee", 
  "r10demene", "r11demene", "r12demene", "r13demene", "r14demene", "r15demene", "r16demene",   # "r7demene", "r8demene", "r9demene", 
  
  # ========== Others ==========
  "r7heart", "r8heart", "r9heart", "r10heart", "r11heart", "r12heart", "r13heart", "r14heart", "r15heart", "r16heart",
  "r7hlthlm", "r8hlthlm", "r9hlthlm", "r10hlthlm", "r11hlthlm", "r12hlthlm", "r13hlthlm", "r14hlthlm", "r15hlthlm", "r16hlthlm",
  
  "r7urinai", "r8urinai", "r9urinai", "r10urinai", "r11urinai", "r12urinai", "r13urinai", "r14urinai",
  "r7fallinj", "r8fallinj", "r9fallinj", "r10fallinj", "r11fallinj", "r12fallinj", "r13fallinj", "r14fallinj",
  "r7hipe", "r8hipe", "r9hipe", "r10hipe", "r11hipe", "r12hipe", "r13hipe", "r14hipe",
  "r7dsight", "r8dsight", "r9dsight", "r10dsight", "r11dsight", "r12dsight", "r13dsight", "r14dsight",
  "r7nsight", "r8nsight", "r9nsight", "r10nsight", "r11nsight", "r12nsight", "r13nsight", "r14nsight",
  "r7painfr", "r8painfr", "r9painfr", "r10painfr", "r11painfr", "r12painfr", "r13painfr", "r14painfr")
wave10_vars <- grep("^r7", health_vars, value = TRUE)
wave10_vars <- intersect(wave10_vars, colnames(data_HRS))

cat("Initial variables count:", length(wave10_vars), "\n")
cat("Variables included in analysis\n\n")

step1_vars <- wave10_vars

# ============================================================================
# STEP 2: Remove variables with >10% missing at baseline
# ============================================================================
cat("=====================================\n")
cat("STEP 2: Remove variables with >10% missing\n")
cat("=====================================\n\n")

missing_rates_w10 <- colMeans(is.na(data_HRS[, wave10_vars, drop = FALSE]))
vars_to_remove_s2 <- names(missing_rates_w10)[missing_rates_w10 > 0.10]
wave10_vars <- wave10_vars[missing_rates_w10 <= 0.10]

cat("Removed variables (>10% missing):", length(vars_to_remove_s2), "\n")
if(length(vars_to_remove_s2) > 0) {
  for(var in vars_to_remove_s2) {
    cat("  -", var, "missing rate:", round(missing_rates_w10[var]*100, 2), "%\n")
  }
}
cat("Remaining variables:", length(wave10_vars), "\n\n")

step2_vars <- wave10_vars

# ============================================================================
# STEP 3: Recode all variables to 0-1 scale
# ============================================================================

cat("=====================================\n")
cat("STEP 3: Recode all variables to 0-1 scale\n")
cat("=====================================\n\n")

table(data_HRS$r10whappy, useNA = "always")
table(data_HRS$r16whappy, useNA = "always")
for(wave in 7:16) {
  var <- paste0("r", wave, "whappy")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- ifelse(data_HRS[[var]] == 0, 1, 
                            ifelse(data_HRS[[var]] == 1, 0, NA))
  }
}

table(data_HRS$r10shlt, useNA = "always")
table(data_HRS$r16shlt, useNA = "always")
for(wave in 7:16) {
  var <- paste0("r", wave, "shlt")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- ifelse(data_HRS[[var]] %in% 1:3, 0,
                            ifelse(data_HRS[[var]] %in% 4:5, 1, NA))
  }
}

table(data_HRS$r10slfmem, useNA = "always")
table(data_HRS$r16slfmem, useNA = "always")
for(wave in 7:16) {
  var <- paste0("r", wave, "slfmem")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- case_when(
      data_HRS[[var]] == 1 ~ 0, data_HRS[[var]] == 2 ~ 0.25, data_HRS[[var]] == 3 ~ 0.5,
      data_HRS[[var]] == 4 ~ 0.75, data_HRS[[var]] == 5 ~ 1, TRUE ~ NA_real_
    )
  }
}

table(data_HRS$r10hearing, useNA = "always")
table(data_HRS$r16hearing, useNA = "always")
for(wave in 7:16) {
  var <- paste0("r", wave, "hearing")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- case_when(
      data_HRS[[var]] == 1 ~ 0, data_HRS[[var]] == 2 ~ 0.25, data_HRS[[var]] == 3 ~ 0.5,
      data_HRS[[var]] == 4 ~ 0.75, data_HRS[[var]] == 5 ~ 1, TRUE ~ NA_real_
    )
  }
}

table(data_HRS$r10dsight, useNA = "always")
table(data_HRS$r16dsight, useNA = "always")
for(wave in 7:16) {
  var <- paste0("r", wave, "dsight")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- case_when(
      data_HRS[[var]] == 1 ~ 0, data_HRS[[var]] == 2 ~ 0.2, data_HRS[[var]] == 3 ~ 0.4,
      data_HRS[[var]] == 4 ~ 0.6, data_HRS[[var]] == 5 ~ 0.8, data_HRS[[var]] == 6 ~ 1,
      TRUE ~ NA_real_
    )
  }
}

table(data_HRS$r10nsight, useNA = "always")
table(data_HRS$r16nsight, useNA = "always")
for(wave in 7:16) {
  var <- paste0("r", wave, "nsight")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- case_when(
      data_HRS[[var]] == 1 ~ 0, data_HRS[[var]] == 2 ~ 0.2, data_HRS[[var]] == 3 ~ 0.4,
      data_HRS[[var]] == 4 ~ 0.6, data_HRS[[var]] == 5 ~ 0.8, data_HRS[[var]] == 6 ~ 1,
      TRUE ~ NA_real_
    )
  }
}

# Immediate word recall (0-10 → 0-1)
table(data_HRS$r10imrc, useNA = "always")
table(data_HRS$r16imrc, useNA = "always")
for(wave in 7:16) {
  var <- paste0("r", wave, "imrc")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- ifelse(between(data_HRS[[var]], 0, 10), data_HRS[[var]] / 10, NA_real_)
  }
}

# Delayed word recall (0-10 → 0-1)
table(data_HRS$r10dlrc, useNA = "always")
table(data_HRS$r16dlrc, useNA = "always")
for(wave in 7:16) {
  var <- paste0("r", wave, "dlrc")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- ifelse(between(data_HRS[[var]], 0, 10), data_HRS[[var]] / 10, NA_real_)
  }
}

cat("All variables recoded to 0-1 scale\n")
cat("Remaining variables:", length(wave10_vars), "\n\n")

step3_vars <- wave10_vars

# ============================================================================
# 🧪 检查所有波次（r10 ~ r16）所有变量是否在 0–1
# ============================================================================
cat("=====================================\n")
cat("检查全部波次所有变量 0–1 范围\n")
cat("=====================================\n\n")

for (v in health_vars) {
  if (all(is.na(data_HRS[[v]]))) next
  
  min_val <- min(data_HRS[[v]], na.rm = TRUE)
  max_val <- max(data_HRS[[v]], na.rm = TRUE)
  in_range <- min_val >= 0 & max_val <= 1
  
  if (!in_range) {
    cat(sprintf("❌ %-15s  min: %.3f  max: %.3f\n", v, min_val, max_val))
  }
}
cat("\n✅ 输出的都是【超出 0–1 范围】的变量！\n")
#❌ r7clims          min: 0.000  max: 7.000
#❌ r8clims          min: 0.000  max: 7.000
#❌ r9clims          min: 0.000  max: 7.000
#❌ r10clims         min: 0.000  max: 7.000
#❌ r11clims         min: 0.000  max: 7.000
#❌ r12clims         min: 0.000  max: 7.000
#❌ r13clims         min: 0.000  max: 7.000
#❌ r14clims         min: 0.000  max: 7.000
#❌ r15clims         min: 0.000  max: 7.000
#❌ r16clims         min: 0.000  max: 7.000
#❌ r14imrcp         min: 0.000  max: 10.000
#❌ r15imrcp         min: 0.000  max: 10.000
#❌ r14dlrcp         min: 0.000  max: 10.000
#❌ r15dlrcp         min: 0.000  max: 10.000
#❌ r7heart          min: 0.000  max: 4.000
#❌ r8heart          min: 0.000  max: 4.000
#❌ r9heart          min: 0.000  max: 4.000
#❌ r10heart         min: 0.000  max: 4.000
#❌ r11heart         min: 0.000  max: 4.000
#❌ r12heart         min: 0.000  max: 4.000
#❌ r13heart         min: 0.000  max: 4.000
#❌ r14heart         min: 0.000  max: 6.000
#❌ r15heart         min: 0.000  max: 6.000
#❌ r16heart         min: 0.000  max: 6.000

for(wave in 7:16) {
  var <- paste0("r", wave, "imrcp")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- ifelse(between(data_HRS[[var]], 0, 10), data_HRS[[var]] / 10, NA_real_)
  }
}
for(wave in 7:16) {
  var <- paste0("r", wave, "dlrcp")
  if(var %in% colnames(data_HRS)) {
    data_HRS[[var]] <- as.numeric(as.character(data_HRS[[var]]))
    data_HRS[[var]] <- ifelse(between(data_HRS[[var]], 0, 10), data_HRS[[var]] / 10, NA_real_)
  }
}
for (v in health_vars) {
  if (all(is.na(data_HRS[[v]]))) next
  
  min_val <- min(data_HRS[[v]], na.rm = TRUE)
  max_val <- max(data_HRS[[v]], na.rm = TRUE)
  in_range <- min_val >= 0 & max_val <= 1
  
  if (!in_range) {
    cat(sprintf("❌ %-15s  min: %.3f  max: %.3f\n", v, min_val, max_val))
  }
}
cat("\n✅ 输出的都是【超出 0–1 范围】的变量！\n")

# 取值不是 0 也不是 1 → 设为缺失值,需要处理的变量列表
fix_vars <- c(paste0("r", 7:16, "clims"), 
              paste0("r", 7:16, "heart"))

for (v in fix_vars) {
  if (v %in% colnames(data_HRS)) {
    data_HRS[[v]] <- ifelse(data_HRS[[v]] %in% c(0, 1), data_HRS[[v]], NA)
  }
}
cat("✅ 所有非 0/1 的值已全部转为缺失值！\n")

# ============================================================================
# STEP 4: Exclude rare (<1%) or saturated (>80%) deficits
# ============================================================================

cat("=====================================\n")
cat("STEP 4: Exclude rare (<1%) or saturated (>80%)\n")
cat("=====================================\n\n")

means_w10 <- colMeans(data_HRS[, wave10_vars, drop = FALSE], na.rm = TRUE)
rare_vars <- names(means_w10)[means_w10 <= 0.01]
saturated_vars <- names(means_w10)[means_w10 >= 0.80]
vars_to_remove_s4 <- c(rare_vars, saturated_vars)
wave10_vars <- wave10_vars[means_w10 > 0.01 & means_w10 < 0.80]

cat("Removed variables:\n")
if(length(rare_vars) > 0) {
  cat("  Rare (<1%):", length(rare_vars), "\n")
  for(var in rare_vars) {
    cat("    -", var, "mean:", round(means_w10[var], 4), "\n")
  }
}
if(length(saturated_vars) > 0) {
  cat("  Saturated (>80%):", length(saturated_vars), "\n")
  for(var in saturated_vars) {
    cat("    -", var, "mean:", round(means_w10[var], 4), "\n")
  }
}
cat("Total removed:", length(vars_to_remove_s4), "\n")
cat("Remaining variables:", length(wave10_vars), "\n\n")

step4_vars <- wave10_vars

# ============================================================================
# STEP 5: Ensure age-related trend
# ============================================================================
cat("=====================================\n")
cat("STEP 5: Ensure age-related trend\n")
cat("=====================================\n\n")

analysis_data <- data_HRS %>% select(r7agey_e, all_of(wave10_vars)) %>% drop_na(r7agey_e)

corr_results <- data.frame(
  variable = character(),
  correlation = numeric(),
  p_value = numeric(),
  direction = character(),
  stringsAsFactors = FALSE)

for(var in wave10_vars) {
  clean_data <- analysis_data %>% 
    select(r7agey_e, all_of(var)) %>% 
    drop_na()
  
  if(nrow(clean_data) > 0) {
    corr_test <- cor.test(clean_data$r7agey_e, clean_data[[var]], method = "pearson")
    corr_results <- rbind(corr_results, data.frame(
      variable = var,
      correlation = corr_test$estimate,
      p_value = corr_test$p.value,
      direction = ifelse(corr_test$estimate > 0, "positive", "negative"),
      stringsAsFactors = FALSE
    ))
  }
}

neg_corr_vars <- corr_results$variable[corr_results$direction == "negative"]
wave10_vars <- wave10_vars[!(wave10_vars %in% neg_corr_vars)]

cat("Variables with negative correlation with age (excluded):", length(neg_corr_vars), "\n")
if(length(neg_corr_vars) > 0) {
  for(var in neg_corr_vars) {
    cor_val <- corr_results$correlation[corr_results$variable == var]
    cat("  -", var, "correlation:", round(cor_val, 4), "\n")
  }
}
cat("Remaining variables:", length(wave10_vars), "\n\n")
#- r7beda correlation: -0.0145 
#- r7sita correlation: -0.0242 
#- r7psyche correlation: -0.0669 
#- r7sleepr correlation: -0.0588 
#- r7depres correlation: -0.0098 
#- r7fsad correlation: -0.0117 
#- r7effort correlation: -0.0438 
#- r7whappy correlation: -0.075 
#- r7imrc correlation: -0.2661 
#- r7dlrc correlation: -0.2643 
#- r7painfr correlation: -0.0396

step5_vars <- wave10_vars

# ============================================================================
# STEP 6: Check for collinearity (r > 0.95)
# ============================================================================

cat("=====================================\n")
cat("STEP 6: Check for collinearity (r > 0.95)\n")
cat("=====================================\n\n")

var_data <- data_HRS[complete.cases(data_HRS[, wave10_vars]), wave10_vars]

if(nrow(var_data) > 0 & length(wave10_vars) > 1) {
  corr_mat <- cor(as.matrix(var_data), method = "spearman")
  high_corr <- which(corr_mat > 0.95 & corr_mat < 1, arr.ind = TRUE)
  
  if(nrow(high_corr) > 0) {
    high_corr_pairs <- data.frame(
      var1 = rownames(corr_mat)[high_corr[, 1]],
      var2 = colnames(corr_mat)[high_corr[, 2]],
      correlation = corr_mat[high_corr]
    )
    high_corr_pairs <- high_corr_pairs[high_corr_pairs$var1 < high_corr_pairs$var2, ]
    
    # Keep first, remove second
    vars_to_remove_s6 <- unique(high_corr_pairs$var2)
    wave10_vars <- wave10_vars[!(wave10_vars %in% vars_to_remove_s6)]
    
    cat("High collinearity pairs found:", nrow(high_corr_pairs), "\n")
    for(i in 1:nrow(high_corr_pairs)) {
      cat("  -", high_corr_pairs$var1[i], "vs", high_corr_pairs$var2[i], 
          "r =", round(high_corr_pairs$correlation[i], 4), "\n")
    }
    cat("Removed:", paste(vars_to_remove_s6, collapse = ", "), "\n")
  } else {
    cat("No high collinearity found\n")
  }
}
cat("Remaining variables:", length(wave10_vars), "\n\n")

step6_vars <- wave10_vars

# ============================================================================
# STEP 7: Ensure at least 30-40 diverse items
# ============================================================================

cat("=====================================\n")
cat("STEP 7: Ensure at least 30 diverse items\n")
cat("=====================================\n\n")

cat("Final variable count:", length(wave10_vars), "\n")
if(length(wave10_vars) >= 30) {
  cat("✓ Meets minimum threshold (30 variables)\n")
} else {
  cat("✗ Below minimum threshold (30 variables)\n")
}
cat("\n")

baseline_var_count <- length(wave10_vars)
min_var_threshold <- round(baseline_var_count * 0.70)

cat("Baseline variable count:", baseline_var_count, "\n")
cat("Minimum threshold (70%):", min_var_threshold, "\n\n")

step7_vars <- wave10_vars

# ============================================================================
# STEP 8: Calculate individual FI scores
# ============================================================================
cat("=====================================\n")
cat("STEP 8: Calculate individual FI scores\n")
cat("=====================================\n\n")

fi_cols <- paste0("r", 7:16, "_fi")
for(col in fi_cols) {
  data_HRS[[col]] <- NA_real_
}

for(wave in 7:16) {
  wave_vars <- gsub("^r7", paste0("r", wave), step7_vars)
  wave_vars <- intersect(wave_vars, colnames(data_HRS))
  
  # 仅在该wave参加的参与者中计算缺失率
  inw_col <- paste0("inw", wave)
  if(inw_col %in% colnames(data_HRS)) {
    wave_participants <- data_HRS[[inw_col]] == 1 & !is.na(data_HRS[[inw_col]])
    
    if(sum(wave_participants) > 0) {
      # 在参加该wave的参与者中计算缺失率
      missing_in_wave <- colMeans(is.na(data_HRS[wave_participants, wave_vars, drop = FALSE]))
      wave_vars_use <- wave_vars[missing_in_wave <= 0.10]
      
      if(length(wave_vars_use) >= min_var_threshold) {
        fi_col <- paste0("r", wave, "_fi")
        non_missing_count <- rowSums(!is.na(data_HRS[, wave_vars_use, drop = FALSE]))
        data_HRS[[fi_col]] <- ifelse(
          non_missing_count > 0,
          rowSums(data_HRS[, wave_vars_use, drop = FALSE], na.rm = TRUE) / non_missing_count,
          NA_real_
        )
      }
    }
  }
}

for(wave in 7:16) {
  fi_var <- paste0("r", wave, "_fi")
  n_valid <- sum(!is.na(data_HRS[[fi_var]]))
  mean_fi <- mean(data_HRS[[fi_var]], na.rm = TRUE)
  cat("Wave", wave, "- Valid FI:", n_valid, ", Mean FI:", round(mean_fi, 3), "\n")
}
cat("\n")



# ============================================================================
# STEP 10: Apply in research models
# ============================================================================
cat("=====================================\n")
cat("STEP 10: Apply in research models\n")
cat("=====================================\n\n")

# N = 11835

# 查看 r10_fi 到 r16_fi 的缺失情况
fi_vars <- paste0("r", 7:16, "_fi")
for (v in fi_vars) {
  if (v %in% colnames(data_HRS)) {
    total <- nrow(data_HRS)
    not_na <- sum(!is.na(data_HRS[[v]]))
    na_num <- sum(is.na(data_HRS[[v]]))
    na_pct <- round(na_num / total * 100, 2)
    cat(sprintf("📊 %-10s | 总数：%4d | 有效值：%4d | 缺失值：%4d | 缺失率：%5.2f%%\n",
                v, total, not_na, na_num, na_pct))
  }}
#📊 r7_fi      | 总数：13237 | 有效值：13237 | 缺失值：   0 | 缺失率： 0.00%
#📊 r8_fi      | 总数：13237 | 有效值：13237 | 缺失值：   0 | 缺失率： 0.00%
#📊 r9_fi      | 总数：13237 | 有效值：13237 | 缺失值：   0 | 缺失率： 0.00%
#📊 r10_fi     | 总数：13237 | 有效值：12984 | 缺失值： 253 | 缺失率： 1.91%
#📊 r11_fi     | 总数：13237 | 有效值：11860 | 缺失值：1377 | 缺失率：10.40%
#📊 r12_fi     | 总数：13237 | 有效值：10539 | 缺失值：2698 | 缺失率：20.38%
#📊 r13_fi     | 总数：13237 | 有效值：9020 | 缺失值：4217 | 缺失率：31.86%
#📊 r14_fi     | 总数：13237 | 有效值：7296 | 缺失值：5941 | 缺失率：44.88%
#📊 r15_fi     | 总数：13237 | 有效值：6365 | 缺失值：6872 | 缺失率：51.92%
#📊 r16_fi     | 总数：13237 | 有效值：5095 | 缺失值：8142 | 缺失率：61.51%

has_w10_fi <- !is.na(data_HRS$r10_fi)
has_followup_fi <- (
  !is.na(data_HRS$r11_fi) | 
    !is.na(data_HRS$r12_fi) | 
    !is.na(data_HRS$r13_fi) | 
    !is.na(data_HRS$r14_fi) | 
    !is.na(data_HRS$r15_fi) | 
    !is.na(data_HRS$r16_fi)
)

data_HRS7 <- data_HRS[has_w10_fi & has_followup_fi, ]     # 11835

cat("Initial sample: N =", nrow(data_HRS), "\n")
cat("With Wave 10 FI: N =", sum(has_w10_fi), "\n")
cat("With followup FI:", sum(has_followup_fi), "\n")
cat("Final analytical sample: N =", nrow(data_HRS7), "\n\n")

#exclude participants had frailty at wave 10
target_vars <- c("r7_fi", "r8_fi", "r9_fi", "r10_fi", "r11_fi", "r12_fi", "r13_fi", "r14_fi", "r15_fi", "r16_fi")

data_HRS8 <- data_HRS7 %>%
  mutate(r7_fi_binary = case_when(r7_fi >= 0.25 ~ 1L,
                                  r7_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r8_fi_binary = case_when(r8_fi >= 0.25 ~ 1L,
                                  r8_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r9_fi_binary = case_when(r9_fi >= 0.25 ~ 1L,
                                  r9_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r10_fi_binary = case_when(r10_fi >= 0.25 ~ 1L,
                                   r10_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r11_fi_binary = case_when(r11_fi >= 0.25 ~ 1L,
                                   r11_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r12_fi_binary = case_when(r12_fi >= 0.25 ~ 1L,
                                   r12_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r13_fi_binary = case_when(r13_fi >= 0.25 ~ 1L,
                                   r13_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r14_fi_binary = case_when(r14_fi >= 0.25 ~ 1L,
                                   r14_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r15_fi_binary = case_when(r15_fi >= 0.25 ~ 1L,
                                   r15_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_),
         r16_fi_binary = case_when(r16_fi >= 0.25 ~ 1L,
                                   r16_fi < 0.25  ~ 0L,
                                   TRUE ~ NA_integer_)) %>% filter(r7_fi_binary != 1)
# N = 9096

#先备份
saveRDS(data_HRS8, file = "D:\\0. 科研文件\\2. Obesity trajectories and frailty_Zhiyue&Simu&Qitong\\3. HRS\\2. Backups of study participants\\HRS_Variability + Cumulative_20260401.rds")













