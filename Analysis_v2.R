##Load packages and code
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Tables.R")
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Figures.R")
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Functions.R")
library(tidyr)
library(robustlmm)
library(lme4)
library(lmerTest)
library(multcomp)

##Define contrasts to be used later in the hypothesis testing
contr1 = rbind("A" =  c(1, 0, 0, 0),
    "B" =  c(1, 1, 0, 0),
    "C" = c(1, 0, 1, 0),
    "D" = c(1, 0, 0, 1))

contr2 = matrix(c(0, 1), 1, 2)

contrast.matrix3 = rbind("diffA" =  c(0, 0, 0, 0, 1, 0, 0, 0),
    "diffB" =  c(0, 0, 0, 0, 1, 1, 0, 0),
    "diffC" = c(0, 0, 0, 0, 1, 0, 1, 0),
    "diffD" = c(0, 0, 0, 0, 1, 0, 0, 1))

contrast.matrix4 = rbind("weekly" =  c(0, 0, 1, 1),
    "monthly" =  c(0, 0, 1, 0),
    "wMm" = c(0, 0, 0, 1))

contrast.matrix5 = rbind("targeted" =  c(0, 0, 1, 1),
    "untargeted" =  c(0, 0, 1, 0),
    "tMu" = c(0, 0, 0, 1))

##Define functions to be used later in the table construction and hypothesis testing
flip = function(x) {
x2 = -1*x + 2
}

getCI = function(varname) {
    r4$var1 = r4[, which(names(r4)==varname)] #data frame must be called r4
    mm = lmer(var1 ~ group*time + (1 | chives_id), data=r4)
    jj = round(confint(glht(mm, contrast.matrix3), calpha = univariate_calpha())$confint, 4)
    con = summary(glht(mm, contrast.matrix3), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
    pval = round(con$test$pvalues, 4)
    cbind(rbind(paste(paste(paste(paste(jj[1,1], " (", sep=""), jj[1,2], sep=""), jj[1,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[2,1], " (", sep=""), jj[2,2], sep=""), jj[2,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[3,1], " (", sep=""), jj[3,2], sep=""), jj[3,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[4,1], " (", sep=""), jj[4,2], sep=""), jj[4,3], sep=", "), ")", sep="")), pval) #change back for original table
   }

getCI2 = function(varname) {
    r4$var1 = r4[, which(names(r4)==varname)]
    mm = lmer(var1 ~ freqW*time + (1 | chives_id), data=r4)
    jj = round(confint(glht(mm, contrast.matrix4), calpha = univariate_calpha())$confint, 2)
    con = summary(glht(mm, contrast.matrix4), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
    pval = round(con$test$pvalues, 4)
    cbind(t(rbind(paste(paste(paste(paste(jj[1,1], " (", sep=""), jj[1,2], sep=""), jj[1,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[2,1], " (", sep=""), jj[2,2], sep=""), jj[2,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[3,1], " (", sep=""), jj[3,2], sep=""), jj[3,3], sep=", "), ")", sep=""))), pval[3])
   }

getCI3 = function(varname) {
    r4$var1 = r4[, which(names(r4)==varname)]
    mm = lmer(var1 ~ targetFV*time + (1 | chives_id), data=r4)
    jj = round(confint(glht(mm, contrast.matrix5), calpha = univariate_calpha())$confint, 2)
    con = summary(glht(mm, contrast.matrix5), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
    pval = round(con$test$pvalues, 4)
    cbind(t(rbind(paste(paste(paste(paste(jj[1,1], " (", sep=""), jj[1,2], sep=""), jj[1,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[2,1], " (", sep=""), jj[2,2], sep=""), jj[2,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[3,1], " (", sep=""), jj[3,2], sep=""), jj[3,3], sep=", "), ")", sep=""))), pval[3])
   }

#############################
##Descriptive data analysis##
#############################

##Load data
d1 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/DATA_2018-03-22.csv", header=TRUE)

##Merge with randomization groups
d2 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Groups_2018-03-22.csv", header=TRUE)

d3 = merge(d1, d2[, names(d2) %in% c("record_id", "randomize_group")], by="record_id")

##Redefine variables as necessary
##Collapse education into <HS grad, HS grad, some college, college or higher
table(d3$education_level, exclude=NULL)
d3$educ2 = NA
d3$educ2[d3$education_level %in% c(1, 2, 3)] = "a.<HS"
d3$educ2[d3$education_level %in% c(4, 5)] = "b.HS"
d3$educ2[d3$education_level == 6] = "c.College"
table(d3$educ2, d3$education_level, exclude=NULL)

##Collapse number of people in HH to 1, 2-3, >=4
table(d3$ppl_in_hh, exclude=NULL)
d3$ppl2 = NA
d3$ppl2[d3$ppl_in_hh == 1] = "a.1"
d3$ppl2[d3$ppl_in_hh %in% c(2, 3)] = "b.2-3"
d3$ppl2[d3$ppl_in_hh >= 4] = "c.>=4"
table(d3$ppl2, d3$ppl_in_hh, exclude=NULL)

##Add in food security data
fs = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/FS_2018-05-15.csv", header=TRUE)
fs2 = fs[fs$redcap_event_name=="prerandomization_arm_1", names(fs) %in% c("record_id", "fs_recall_1_worried", "fs_recall_1_last")]
d4 = merge(d3, fs2, by="record_id", all.x=TRUE)

##Add in %FPL data
elig = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Elig_2018-05-15.csv", header=TRUE)
elig2 = elig[, names(elig) %in% c("record_id", "income_meet")]
d5 = merge(d4, elig2, by="record_id", all.x=TRUE)
d5$FPL = as.character(d5$income_meet)

##After de-identification, create variables for group identification
table(d5$randomize_group) #1=monthly F/V, 2=monthly unrest, 3=weekly unrest, 4=weekly F/V
d5$group = NA
d5$group[d5$randomize_group==4] = "a.FVweekly"
d5$group[d5$randomize_group==1] = "b.FVmonthly"
d5$group[d5$randomize_group==3] = "c.UNweekly"
d5$group[d5$randomize_group==2] = "d.UNmonthly"
table(d5$group, d5$randomize_group, exclude=NULL)

##And for later statistical model
d5$targetFV = ifelse(d5$group %in% c("a.FVweekly", "b.FVmonthly"), 1, 0)
d5$freqW = ifelse(d5$group %in% c("a.FVweekly", "c.UNweekly"), 1, 0)
table(d5$targetFV, d5$group, exclude=NULL)
table(d5$freqW, d5$group, exclude=NULL)


##Deriving variables involving NDSR data, both weekly and monthly

#####################
##Consumption cycle##
#####################

##Need calories, F/V outcome, HEI, AHEI at weeks 1 and 4
##Read in HEI and AHEI (derived in separate file)
hei5 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/HEI_weekly_2018-08-22.csv", header=TRUE)
ahei5 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/AHEI_weekly_2018-08-22.csv", header=TRUE)

##Read in F/V by week data (derived in separate file)
n5 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_weekly_2018-08-22.csv", header=TRUE)

##Descriptive stats for paper
##Calories
summary(n5$calories[n5$time=="a.BL" & n5$week=="week1"])
summary(n5$calories[n5$time=="a.BL" & n5$week=="week4"])
summary(n5$calories[n5$time=="b.6m" & n5$week=="week1"])
summary(n5$calories[n5$time=="b.6m" & n5$week=="week4"])

##F/V
summary(n5$fvC[n5$time=="a.BL" & n5$week=="week1"])
summary(n5$fvC[n5$time=="a.BL" & n5$week=="week4"])
summary(n5$fvC[n5$time=="b.6m" & n5$week=="week1"])
summary(n5$fvC[n5$time=="b.6m" & n5$week=="week4"])

##HEI
summary(hei5$HEI[hei5$time=="a.BL" & hei5$week=="week1"])
summary(hei5$HEI[hei5$time=="a.BL" & hei5$week=="week4"])
summary(hei5$HEI[hei5$time=="b.6m" & hei5$week=="week1"])
summary(hei5$HEI[hei5$time=="b.6m" & hei5$week=="week4"])


##Ratios of consumption
cw1 = n5[n5$time=="a.BL" & n5$week=="week1", ][, c(1, 4)]
names(cw1)[2] = paste(names(cw1)[2], "m0w1", sep="_")
cw4 = n5[n5$time=="a.BL" & n5$week=="week4", ][, c(1, 4)]
names(cw4)[2] = paste(names(cw4)[2], "m0w4", sep="_")

ww1 = n5[n5$time=="b.6m" & n5$week=="week1", ][, c(1, 4)]
names(ww1)[2] = paste(names(ww1)[2], "m6w1", sep="_")
ww4 = n5[n5$time=="b.6m" & n5$week=="week4", ][, c(1, 4)]
names(ww4)[2] = paste(names(ww4)[2], "m6w4", sep="_")

cc1 = merge(cw4, cw1, by="chives_id", all.x=TRUE)
cc2 = merge(cc1, ww1, by="chives_id", all.x=TRUE)
cc3 = merge(cc2, ww4, by="chives_id", all.x=TRUE)

cc3$ratio_m0 = cc3$calories_m0w4 / cc3$calories_m0w1
cc3$ratio_m6 = cc3$calories_m6w4 / cc3$calories_m6w1

##Save data and make table
#write.csv(cc3, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Data_consumption_cycle_2018-08-22.csv", row.names=FALSE)
#cc3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Data_consumption_cycle_2018-08-15_v2.csv", header=TRUE)
cc3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Data_consumption_cycle_2018-08-22.csv", header=TRUE)

cc4 = merge(cc3, d5[, names(d5) %in% c("chives_id", "group")], all.x=TRUE)

tabC2 = mktab(data=cc4, var.names=c("calories_m0w1", "calories_m0w4", "ratio_m0", "calories_m6w1", "calories_m6w4", "ratio_m6"), ind.cat=rep(0, 6), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=2)

word.doc(obj.list=list(tabC2), obj.title="Appendix Exhibit 1: Consumption cycle of calories", dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/AppendixExhibit1_2018-08-22.docx", ftype="Arial", col.odd="white")


##Deriving ease of use outcome (only at 6 months)
##vouch_understood You have been in our voucher program for several months. Do you feel you understood how to use the vouchers? 1=Yes, 0=No
##vouch_easy Was it easy to decide which foods you are allowed to spend the voucher on? 1=Yes, 0=No
##vouch_trouble Did you have any trouble using the vouchers at any stores; that is, did any cashiers or store employees give you a hard time or not allow you to use the vouchers? 1=Yes, 0=No
##Disaggregated and composite score (for mediator analysis)
sec = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Secondary_2018-08-08.csv", header=TRUE)
table(sec$vouch_understood, sec$redcap_event_name, exclude=NULL)
ease = sec[sec$redcap_event_name %in% c("m6w1_arm_1", "m6w4_arm_1"), names(sec) %in% c("record_id", "redcap_event_name", "vouch_understood", "vouch_easy", "vouch_trouble")]
ease2 = ease[ease$record_id %in% d5$record_id, ]
head(ease2)
ease3 = merge(ease2, d5[, names(d5) %in% c("record_id", "group")], by="record_id", all.x=TRUE)
table(by(ease3$vouch_understood, ease3$record_id, function(x) sum(!is.na(x))))
#  0   1   2
# 30 320   9

##Merge to n=359
e1 = ease2[ease2$redcap_event_name=="m6w1_arm_1", ]
e4 = ease2[ease2$redcap_event_name=="m6w4_arm_1", ]
names(e1)[3:5] = paste(names(e1)[3:5], "w1", sep="_")
names(e4)[3:5] = paste(names(e4)[3:5], "w4", sep="_")
ee = merge(e1[, names(e1) %in% c("record_id", "vouch_understood_w1", "vouch_easy_w1", "vouch_trouble_w1")], e4[, names(e4) %in% c("record_id", "vouch_understood_w4", "vouch_easy_w4", "vouch_trouble_w4")], by="record_id", all.x=TRUE)

##Cross-tabs by variable
table(ee$vouch_understood_w1, ee$vouch_understood_w4, exclude=NULL) #there are 9 individuals who took it both times; what to do about them?
table(ee$vouch_easy_w1, ee$vouch_easy_w4, exclude=NULL) #there are 9 individuals who took it both times; what to do about them?
table(ee$vouch_trouble_w1, ee$vouch_trouble_w4, exclude=NULL) #there are 9 individuals who took it both times; what to do about them?

##Save and make table
ee2 = merge(ee, d5[, names(d5) %in% c("record_id", "group")], by="record_id", all.x=TRUE)

##Only keep earliest 6 month measurement
ee2$vouch_understood = ee2$vouch_understood_w1
ee2$vouch_understood[is.na(ee2$vouch_understood_w1)] = ee2$vouch_understood_w4[is.na(ee2$vouch_understood_w1)]

ee2$vouch_easy = ee2$vouch_easy_w1
ee2$vouch_easy[is.na(ee2$vouch_easy_w1)] = ee2$vouch_easy_w4[is.na(ee2$vouch_easy_w1)]

ee2$vouch_trouble = ee2$vouch_trouble_w1
ee2$vouch_trouble[is.na(ee2$vouch_trouble_w1)] = ee2$vouch_trouble_w4[is.na(ee2$vouch_trouble_w1)]

##Composite score
ee2$vouch = (ee2$vouch_understood==1) + (ee2$vouch_easy==1) + (ee2$vouch_trouble==0)
table(ee2$vouch, exclude=NULL)

tabEase = mktab(data=ee2, var.names=c("vouch_understood", "vouch_easy", "vouch_trouble", "vouch"), ind.cat=rep(1, 4), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)

word.doc(obj.list=list(tabEase), obj.title="Appendix Exhibit 1: Ease of voucher use variables at 6 months", dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/AppendixExhibit2_2018-08-22.docx", ftype="Arial", col.odd="white")


############################
##Voucher utilization rate##
############################
##Read in Mandy's data
v = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/VoucherRedemption_2018-08-08.csv", header=TRUE)

##Number of uses per person
nper = by(v$CHIVES.ID, v$CHIVES.ID, length)
v2 = data.frame(chives_id = names(nper), uses=as.numeric(nper))
v3 = merge(d5[, names(d5) %in% c("chives_id", "group")], v2, by="chives_id", all.x=TRUE)
v3$cohort = substr(v3$chives_id, 3, 3)
table(v3$cohort, exclude=NULL)
v3$cohort[v3$cohort==0] = 1

by(v3$uses, v3$cohort, function(x) sum(is.na(x)))
by(v3$uses, v3$cohort, function(x) max(x, na.rm=TRUE))

##Calculating proportion used (fix this?)
v3$total = 24
v3$total[v3$cohort %in% c(1, 6)] = 28 #C1 and C6 got 28 vouchers
#v3[(v3$uses>24 & !v3$cohort %in% c(1, 6)), ]

##4 other individuals got 28 vouchers (CP8066, CP8926, CP7210, CP8036)
v3$total[v3$chives_id %in% c("CP8926", "CP8066", "CP7210", "CP8036")] = 28
v3$pct = 100 * (v3$uses / v3$total)

##Make table and save
tabV = mktab(data=v3, var.names=c("uses", "pct"), ind.cat=c(0, 0), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)

word.doc(obj.list=list(tabV), obj.title="Appendix Exhibit 4: Voucher redemption in CHIVES", dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/AppendixExhibit3_2018-08-23.docx", ftype="Arial", col.odd="white")


###########################
##Food insecurity outcome##
###########################
table(sec$fs_last, sec$redcap_event_name, exclude=NULL)
fs = sec[sec$record_id %in% d5$record_id, names(sec) %in% c("record_id", "redcap_event_name", "fs_last", "fs_balanced", "fs_cut", "fs_cut_freq", "fs_eat_less", "fs_hungry")]

##Follow Hilary's instructions for deriving variable
##fs_last (HH3) The first statement is, "The food that (I/we) bought just didn't last, and (I/we) didn't have money to get more."
##Was that often, sometimes, or never true for (you or your household) in the last 12 months? 	radio, Required
#1	Often true (+1)
#2	Sometimes true (+1)
#3	Never true
fs$cutHH3 = ifelse(fs$fs_last %in% c(1, 2), 1, 0)
fs$cutHH3[is.na(fs$fs_last)] = NA
table(fs$cutHH3, fs$fs_last, exclude=NULL)

##fs_balanced (HH4) "(I/we) couldn't afford to eat balanced meals."
##Was that often, sometimes, or never true for (you or your household) in the last 12 months? 	radio, Required
#1	Often true (+1)
#2	Sometimes true (+1)
#3	Never true
fs$cutHH4 = ifelse(fs$fs_balanced %in% c(1, 2), 1, 0)
fs$cutHH4[is.na(fs$fs_balanced)] = NA
table(fs$cutHH4, fs$fs_balanced, exclude=NULL)

##fs_cut (AD1) In the last 12 months, since last [name of current month], did you (or other adults in your household) ever cut the size of your meals or skip meals because there wasn't enough money for food? 	yesno, Required
#1	Yes (+1)
#0	No
fs$cutAD1 = ifelse(fs$fs_cut==1, 1, 0)
fs$cutAD1[is.na(fs$fs_cut)] = NA
table(fs$cutAD1, fs$fs_cut, exclude=NULL)

##fs_cut_freq (AD1a) Show the field ONLY if:[fs_cut] = '1'
##How often did this happen-almost every month, some months but not every month, or in only 1 or 2 months? 	radio, Required
#1	Almost every month (+1)
#2	Some months but not every month (+1)
#3	Only 1 or 2 months
fs$cutAD1a = ifelse(fs$fs_cut_freq %in% c(1, 2), 1, 0)
fs$cutAD1a[is.na(fs$fs_cut_freq)] = NA
table(fs$cutAD1a, fs$fs_cut_freq, exclude=NULL)

##fs_eat_less (AD2) In the last 12 months, did you ever eat less than you felt you should because there wasn't enough money for food? 	yesno, Required
#1	Yes (+1)
#0	No
fs$cutAD2 = ifelse(fs$fs_eat_less==1, 1, 0)
fs$cutAD2[is.na(fs$fs_eat_less)] = NA
table(fs$cutAD2, fs$fs_eat_less, exclude=NULL)

##fs_hungry (AD3) In the last 12 months, were you ever hungry but didn't eat because there wasn't enough money for food? 	yesno, Required
#1	Yes (+1)
#0	No
fs$cutAD3 = ifelse(fs$fs_hungry==1, 1, 0)
fs$cutAD3[is.na(fs$fs_hungry)] = NA
table(fs$cutAD3, fs$fs_hungry, exclude=NULL)

##Total score
fs$fs_total = apply(fs[, names(fs) %in% c("cutHH3", "cutHH4", "cutAD1", "cutAD1a", "cutAD2", "cutAD3")], 1, function(x) sum(x, na.rm=TRUE))
fs$fs_total[is.na(fs$cutHH3) & is.na(fs$cutHH4) & is.na(fs$cutAD1) & is.na(fs$cutAD1a) & is.na(fs$cutAD2) & is.na(fs$cutAD3)] = NA
table(fs$fs_total, exclude=NULL)

##Categorize fs_total
fs$fs_total_cat = NA
fs$fs_total_cat[fs$fs_total %in% c(0, 1)] = "a.HighSecurity"
fs$fs_total_cat[fs$fs_total %in% c(2, 3, 4)] = "b.LowSecurity"
fs$fs_total_cat[fs$fs_total %in% c(5, 6)] = "c.VeryLowSecurity"
table(fs$fs_total_cat, fs$fs_total, exclude=NULL)


##Merge to see cross tabs at 6 months
fsB = fs[fs$redcap_event_name=="prerandomization_arm_1", c(1, 3:8, 15, 16)]
names(fsB)[2:9] = paste(names(fsB)[2:9], "BL", sep="_")

fs1 = fs[fs$redcap_event_name=="m6w1_arm_1", c(1, 3:8, 15, 16)]
names(fs1)[2:9] = paste(names(fs1)[2:9], "m6w1", sep="_")

fs4 = fs[fs$redcap_event_name=="m6w4_arm_1", c(1, 3:8, 15, 16)]
names(fs4)[2:9] = paste(names(fs4)[2:9], "m6w4", sep="_")

fsJ1 = merge(fsB, fs1, by="record_id")
fsJ2 = merge(fsJ1, fs4, by="record_id")
fsJ3 = merge(fsJ2, d5[, names(d5) %in% c("record_id", "group")], by="record_id", all.x=TRUE)

##Keep earliest available measurement at month 6
fsJ3$fs_last_m6 = fsJ3$fs_last_m6w1
fsJ3$fs_last_m6[is.na(fsJ3$fs_last_m6w1)] = fsJ3$fs_last_m6w4[is.na(fsJ3$fs_last_m6w1)]

fsJ3$fs_balanced_m6 = fsJ3$fs_balanced_m6w1
fsJ3$fs_balanced_m6[is.na(fsJ3$fs_balanced_m6w1)] = fsJ3$fs_balanced_m6w4[is.na(fsJ3$fs_balanced_m6w1)]

fsJ3$fs_cut_m6 = fsJ3$fs_cut_m6w1
fsJ3$fs_cut_m6[is.na(fsJ3$fs_cut_m6w1)] = fsJ3$fs_cut_m6w4[is.na(fsJ3$fs_cut_m6w1)]

fsJ3$fs_cut_freq_m6 = fsJ3$fs_cut_freq_m6w1
fsJ3$fs_cut_freq_m6[is.na(fsJ3$fs_cut_freq_m6w1)] = fsJ3$fs_cut_freq_m6w4[is.na(fsJ3$fs_cut_freq_m6w1)]

fsJ3$fs_eat_less_m6 = fsJ3$fs_eat_less_m6w1
fsJ3$fs_eat_less_m6[is.na(fsJ3$fs_eat_less_m6w1)] = fsJ3$fs_eat_less_m6w4[is.na(fsJ3$fs_eat_less_m6w1)]

fsJ3$fs_hungry_m6 = fsJ3$fs_hungry_m6w1
fsJ3$fs_hungry_m6[is.na(fsJ3$fs_hungry_m6w1)] = fsJ3$fs_hungry_m6w4[is.na(fsJ3$fs_hungry_m6w1)]

fsJ3$fs_total_m6 = fsJ3$fs_total_m6w1
fsJ3$fs_total_m6[is.na(fsJ3$fs_total_m6w1)] = fsJ3$fs_total_m6w4[is.na(fsJ3$fs_total_m6w1)]

fsJ3$fs_total_cat_m6 = fsJ3$fs_total_cat_m6w1
fsJ3$fs_total_cat_m6[is.na(fsJ3$fs_total_cat_m6w1)] = fsJ3$fs_total_cat_m6w4[is.na(fsJ3$fs_total_cat_m6w1)]

##Make table and send
tabFS = mktab(data=fsJ3, var.names=c("fs_last_BL", "fs_last_m6", "fs_balanced_BL", "fs_balanced_m6", "fs_cut_BL", "fs_cut_m6", "fs_cut_freq_BL", "fs_cut_freq_m6", "fs_eat_less_BL", "fs_eat_less_m6", "fs_hungry_BL", "fs_hungry_m6", "fs_total_BL", "fs_total_m6", "fs_total_cat_BL", "fs_total_cat_m6"), ind.cat=rep(1, 16), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)

word.doc(obj.list=list(tabFS), obj.title="Appendix Exhibit 3: Summary of food security variables", dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/AppendixExhibit4_2018-08-22.docx", ftype="Arial", col.odd="white")

#table(fsJ3$fs_total_cat_BL, fsJ3$fs_total_cat_m6w1, exclude=NULL)
#table(fsJ3$fs_total_cat_m6w1, fsJ3$fs_total_cat_m6w4, exclude=NULL) #there are n=8 that took both



###################
##PRIMARY OUTCOME##
###################

##Read in wide nutrition data
nutr = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_wide_2018-08-22.csv", header=TRUE)

##Merge demographics and nutrition data
d6 = merge(d5, nutr, by="chives_id", all.x=TRUE)
names(d6)

summary(d6$fvC)
summary(d6$fvC_6)


##Table 1: Baseline demographics by group (with p-values)
##Currently omitted: "income_meet", "fs_recall_1_worried", "fs_recall_1_last"
##Per Hilary's comment: I think the SNAP participation rates for someone else in the household would look higher and better reflect what we are trying to communicate if we limit that variable to people living in 2+ person households?  That is, if you are in a 1 person household it doesn’t make sense to keep you in the denominator of people without someone else in the household with SNAP.
table(d6$ppl_in_hh, exclude=NULL)
table(d6$fa_snap_hh, exclude=NULL)
table(d6$fa_wic_hh, exclude=NULL)

d6$fa_snap_hh2 = d6$fa_snap_hh
d6$fa_snap_hh2[d6$ppl_in_hh==1] = NA
table(d6$fa_snap_hh2, exclude=NULL)

d6$fa_wic_hh2 = d6$fa_wic_hh
d6$fa_wic_hh2[d6$ppl_in_hh==1] = NA
table(d6$fa_wic_hh2, exclude=NULL)

##Recode a few variables for table 1
table(d6$race___3, exclude=NULL)
d6$race2 = flip(d6$race___3)
table(d6$race2, d6$race___3)
d6$ethnicity2 = flip(d6$ethnicity)
d6$fa_snap_ppt2 = flip(d6$fa_snap_ppt)
d6$fa_wic_ppt2 = flip(d6$fa_wic_ppt)

##Merge in food security data
d6t = merge(d6, fsJ3[, names(fsJ3) %in% c("record_id", "fs_total_cat_BL")], by="record_id", all.x=TRUE)

##Pull in HEI/AHEI (data read in later)
hei = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/HEI_2018-07-20.csv", header=TRUE)
hei_bl = hei[hei$time=="a.Baseline", ]
hei_6 = hei[hei$time=="b.6M", ]
names(hei_6)[2:15] = paste(names(hei_6)[2:15], "6", sep="_")
hei_all = merge(hei_bl[, names(hei_bl) %in% c("chives_id", "HEI")], hei_6[, names(hei_6) %in% c("chives_id", "HEI_6")], by="chives_id", all.x=TRUE)

ahei = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/AHEI_2018-07-20.csv", header=TRUE)
ahei_bl = ahei[ahei$time=="a.Baseline", ]
ahei_6 = ahei[ahei$time=="b.6M", ]
names(ahei_6)[2:13] = paste(names(ahei_6)[2:13], "6", sep="_")
ahei_all = merge(ahei_bl[, names(ahei_bl) %in% c("chives_id", "AHEI")], ahei_6[, names(ahei_6) %in% c("chives_id", "AHEI_6")], by="chives_id", all.x=TRUE)

d7 = merge(d6t, hei_all, by="chives_id")
d8 = merge(d7, ahei_all, by="chives_id")

##Calculation on calories
quantile(d8$calories, c(.025,.975), na.rm=T)

tab1 = mktab(data=d8, var.names=c("age_in_years", "sex", "race2", "ethnicity2", "educ2", "hh_monthly_income", "ppl2", "fa_snap_ppt2", "fa_snap_hh2", "fa_wic_ppt2", "fa_wic_hh2", "fs_total_cat_BL", "calories", "fvC", "HEI", "AHEI"), ind.cat=c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0), group.name="group", cfn=describeMean, miss="always", pval=TRUE, tot="last", digit=2)

##Save Table 1
word.doc(obj.list=list(tab1), obj.title=c("Table 1: Baseline Demographics for CHIVES study"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/Table1_2018-08-22.docx", ftype="Arial", col.odd="white")


##Figure 1: Boxplots (horizontal) of outcomes (FV, AHEI, HEI) by group
d8$FVchange = d8$fvC_6-d8$fvC
d8$HEIchange = d8$HEI_6-d8$HEI
d8$AHEIchange = d8$AHEI_6-d8$AHEI
summary(d8$FVchange)
summary(d8$HEIchange)
summary(d8$AHEIchange)

listF = list(d8$FVchange[d8$group=="a.FVweekly"], d8$FVchange[d8$group=="b.FVmonthly"], d8$FVchange[d8$group=="c.UNweekly"], d8$FVchange[d8$group=="d.UNmonthly"])
listH = list(d8$HEIchange[d8$group=="a.FVweekly"], d8$HEIchange[d8$group=="b.FVmonthly"], d8$HEIchange[d8$group=="c.UNweekly"], d8$HEIchange[d8$group=="d.UNmonthly"])
listA = list(d8$AHEIchange[d8$group=="a.FVweekly"], d8$AHEIchange[d8$group=="b.FVmonthly"], d8$AHEIchange[d8$group=="c.UNweekly"], d8$AHEIchange[d8$group=="d.UNmonthly"])

##Horizontal boxplots
pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Figures/Exhibit2_2018-08-23_v2.pdf")
par(mfrow=c(2,2))
boxp(obj.list=listF, pos=c(1,2,3,4)-0.5, cols=c("white", "white", "white", "white"), atx=c(1,2,3,4)-0.5, labs=c("FW, weekly", "FV, monthly", "UN, weekly", "UN, monthly"), ytitle="", xtitle="F/V 0-6 month change (cup equivs)", mtitle="A", ylim=c(-4, 4), add.n=FALSE, rotx=0, xcex=0.65, ad=FALSE, horz=TRUE, rangex=1)

boxp(obj.list=listH, pos=c(1,2,3,4)-0.5, cols=c("white", "white", "white", "white"), atx=c(1,2,3,4)-0.5, labs=c("FW, weekly", "FV, monthly", "UN, weekly", "UN, monthly"), ytitle="", xtitle="HEI 0-6 month change", mtitle="B", ylim=c(-40, 40), add.n=FALSE, rotx=0, xcex=0.65, ad=FALSE, horz=TRUE, rangex=1)

boxp(obj.list=listA, pos=c(1,2,3,4)-0.5, cols=c("white", "white", "white", "white"), atx=c(1,2,3,4)-0.5, labs=c("FW, weekly", "FV, monthly", "UN, weekly", "UN, monthly"), ytitle="", xtitle="AHEI 0-6 month change", mtitle="C", ylim=c(-30, 30), add.n=FALSE, rotx=0, xcex=0.65, ad=FALSE, horz=TRUE, rangex=1)
dev.off()


#########################
##Statistical Inference##
#########################

##Primary outcomes
r3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_long_2018-08-22.csv", header=TRUE)

##Merge to get groups of interest
r4 = merge(r3, d5[, names(d5) %in% c("chives_id", "group", "targetFV", "freqW")], by="chives_id", all.x=TRUE)

##Get HEI and AHEI in there
h1 = hei_all[, names(hei_all) %in% c("chives_id", "HEI")]
h1$time = "a.BL"
h2 = hei_all[, names(hei_all) %in% c("chives_id", "HEI_6")]
names(h2)[2] = "HEI"
h2$time = "b.6m"
h3 = rbind(h1, h2)
h3$unique = paste(h3$chives_id, h3$time, sep="_")

a1 = ahei_all[, names(ahei_all) %in% c("chives_id", "AHEI")]
a1$time = "a.BL"
a2 = ahei_all[, names(ahei_all) %in% c("chives_id", "AHEI_6")]
names(a2)[2] = "AHEI"
a2$time = "b.6m"
a3 = rbind(a1, a2)
a3$unique = paste(a3$chives_id, a3$time, sep="_")

h4 = merge(h3[, names(h3) %in% c("unique", "HEI")], a3[, names(a3) %in% c("unique", "AHEI")], by="unique", all.x=TRUE)

r4$unique = paste(r4$chives_id, r4$time, sep="_")
r4 = merge(r4, h4, by="unique", all.x=TRUE)

##0-6 month changes by group
tab2m = t(cbind(getCI("fvC"),
getCI("HEI"),
getCI("AHEI"),
getCI("fruitsC"),
getCI("vegC"),
getCI("calories"),
getCI("fat_g"),
getCI("carb_g"),
getCI("protein_g"),
getCI("fruitsO"),
getCI("vegO"),
getCI("grains"),
getCI("protein"),
getCI("dairy"),
getCI("ssb")))

rownames(tab2m)[seq(1, 29, 2)] = c("fvC","HEI", "AHEI", "fruitsC", "vegC", "calories", "fat_g", "carb_g", "protein_g", "fruitsO", "vegO", "grains", "protein", "dairy", "ssb")
colnames(tab2m) = c("FVweekly", "FVmonthly", "UNweekly", "UNmonthly")

word.doc(obj.list=list(tab2m), obj.title=c("Table 3: Preliminary estimation of baseline to 6 month changes for selected variables in CHIVES study"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/Primary_2018-08-22.docx", ftype="Arial", col.odd="white")

##Interaction plot to display estimates
##x-axis, UN to FV
##y-axis, F/V, HEI, AHEI change from mixed effects models
##solid line for weekly, dotted for monthly (C-A, D-B)
##can you pull out the legend (weekly_____ monthly - - - - ) from each box and make one central legend for all three?
##also we have to put in some legend descriptions of what “A”, “B” and “C” are and how they differ.
mmF = lmer(fvC ~ group*time + (1 | chives_id), data=r4)
jjF = round(confint(glht(mmF, contrast.matrix3), calpha = univariate_calpha())$confint, 4)

mmH = lmer(HEI ~ group*time + (1 | chives_id), data=r4)
jjH = round(confint(glht(mmH, contrast.matrix3), calpha = univariate_calpha())$confint, 4)

mmA = lmer(AHEI ~ group*time + (1 | chives_id), data=r4)
jjA = round(confint(glht(mmA, contrast.matrix3), calpha = univariate_calpha())$confint, 4)

dfF = data.frame(bar=jjF[, 1], error = (jjF[, 3]-jjF[, 2])/2)
dfH = data.frame(bar=jjH[, 1], error = (jjH[, 3]-jjH[, 2])/2)
dfA = data.frame(bar=jjA[, 1], error = (jjA[, 3]-jjA[, 2])/2)

##Interaction plot
pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Figures/Interaction_Plot_2018-09-07.pdf")
par(mfrow=c(2,2))
plot(c(0.5, 1.5), dfF$bar[c(3, 1)], xlim=c(0, 2), ylim=c(-0.2, 0.4), main="A", ylab="0-6 month change (cup equivs)", type="b", xaxt="n", xlab="F/V targeting")
arrows(x0=c(0.5, 1.5), y0=dfF$bar[c(3, 1)]+dfF$error[c(3, 1)], y1=dfF$bar[c(3, 1)]-dfF$error[c(3, 1)], angle=90,code=3, length=0.1)

lines(c(0.4, 1.4), dfF$bar[c(4, 2)], type="b", lty=2)
arrows(x0=c(0.4, 1.4), y0=dfF$bar[c(4, 2)]+dfF$error[c(4, 2)], y1=dfF$bar[c(4, 2)]-dfF$error[c(4, 2)], angle=90,code=3, length=0.1, lty=2)
axis(1, at=c(0.45, 1.45), c("UN", "FV"))

plot(c(0.5, 1.5), dfH$bar[c(3, 1)], xlim=c(0, 2), ylim=c(-2.5, 4.3), main="B", ylab="0-6 month change", type="b", xaxt="n", xlab="F/V targeting")
arrows(x0=c(0.5, 1.5), y0=dfH$bar[c(3, 1)]+dfH$error[c(3, 1)], y1=dfH$bar[c(3, 1)]-dfH$error[c(3, 1)], angle=90,code=3, length=0.1)

lines(c(0.4, 1.4), dfH$bar[c(4, 2)], type="b", lty=2)
arrows(x0=c(0.4, 1.4), y0=dfH$bar[c(4, 2)]+dfH$error[c(4, 2)], y1=dfH$bar[c(4, 2)]-dfH$error[c(4, 2)], angle=90,code=3, length=0.1, lty=2)
axis(1, at=c(0.45, 1.45), c("UN", "FV"))

plot(c(0.5, 1.5), dfA$bar[c(3, 1)], xlim=c(0, 2), ylim=c(-2.5, 3), main="C", ylab="0-6 month change", type="b", xaxt="n", xlab="F/V targeting")
arrows(x0=c(0.5, 1.5), y0=dfA$bar[c(3, 1)]+dfA$error[c(3, 1)], y1=dfA$bar[c(3, 1)]-dfA$error[c(3, 1)], angle=90,code=3, length=0.1)

lines(c(0.4, 1.4), dfA$bar[c(4, 2)], type="b", lty=2)
arrows(x0=c(0.4, 1.4), y0=dfA$bar[c(4, 2)]+dfA$error[c(4, 2)], y1=dfA$bar[c(4, 2)]-dfA$error[c(4, 2)], angle=90,code=3, length=0.1, lty=2)
axis(1, at=c(0.45, 1.45), c("UN", "FV"))

plot.new()
legend('topleft', lty=c(1, 2), c("Weekly", "Monthly"), bty="n", cex=1.2)
legend('bottomleft', c('A: F/V intake', 'B: HEI', 'C: AHEI'), cex=1.2, bty="n")
dev.off()


##Statistical testing for two main effects
##Monthly to weekly
tab2w = rbind(
getCI2("fvC"),
getCI2("HEI"),
getCI2("AHEI"),
getCI2("fruitsC"),
getCI2("vegC"),
getCI2("calories"),
getCI2("fat_g"),
getCI2("carb_g"),
getCI2("protein_g"),
getCI2("fruitsO"),
getCI2("vegO"),
getCI2("grains"),
getCI2("protein"),
getCI2("dairy"),
getCI2("ssb"))

rownames(tab2w) = c("fvC","HEI", "AHEI", "fruitsC", "vegC", "calories", "fat_g", "carb_g", "protein_g", "fruitsO", "vegO", "grains", "protein", "dairy", "ssb")
colnames(tab2w) = c("Weekly", "Monthly", "Weekly-Monthly", "P-value")

word.doc(obj.list=list(tab2w), obj.title=c("Table 4: Preliminary estimation of baseline to 6 month changes for selected variables in CHIVES study: weekly vs. monthly"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/Weekly_v_monthly_2018-08-22.docx", ftype="Arial", col.odd="white")


##Targeted vs. unrestricted
tab2t = rbind(
getCI3("fvC"),
getCI3("HEI"),
getCI3("AHEI"),
getCI3("fruitsC"),
getCI3("vegC"),
getCI3("calories"),
getCI3("fat_g"),
getCI3("carb_g"),
getCI3("protein_g"),
getCI3("fruitsO"),
getCI3("vegO"),
getCI3("grains"),
getCI3("protein"),
getCI3("dairy"),
getCI3("ssb"))

rownames(tab2t) = c("fvC","HEI", "AHEI", "fruitsC", "vegC", "calories", "fat_g", "carb_g", "protein_g", "fruitsO", "vegO", "grains", "protein", "dairy", "ssb")
colnames(tab2t) = c("Targeted", "Untargeted", "Targeted-Untargeted", "P-value")

word.doc(obj.list=list(tab2t), obj.title=c("Table 4: Preliminary estimation of baseline to 6 month changes for selected variables in CHIVES study: targeted vs. untargeted"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/Targeted_v_unrestricted_2018-08-22.docx", ftype="Arial", col.odd="white")


##Secondary outcomes

###############
##Ease of use##
###############

##Define variables for statistical model
ee2$targetFV = ifelse(ee2$group %in% c("a.FVweekly", "b.FVmonthly"), 1, 0)
ee2$freqW = ifelse(ee2$group %in% c("a.FVweekly", "c.UNweekly"), 1, 0)
table(ee2$targetFV, ee2$group, exclude=NULL)
table(ee2$freqW, ee2$group, exclude=NULL)

##Inference by group via linear regression
eReg = lm(vouch ~ group, data=ee2)
round(confint(glht(eReg, contr1), calpha = univariate_calpha())$confint, 2)

##Weekly vs. monthly
eReg1 = lm(vouch ~ freqW, data=ee2)
round(confint(glht(eReg1, contr2), calpha = univariate_calpha())$confint, 2)
con = summary(glht(eReg1, contr2), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Targeted vs. untargeted
eReg2 = lm(vouch ~ targetFV, data=ee2)
round(confint(glht(eReg2, contr2), calpha = univariate_calpha())$confint, 2)
con = summary(glht(eReg2, contr2), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

#######################
##Voucher utilization##
#######################
##Define variables for statistical model
v3$targetFV = ifelse(v3$group %in% c("a.FVweekly", "b.FVmonthly"), 1, 0)
v3$freqW = ifelse(v3$group %in% c("a.FVweekly", "c.UNweekly"), 1, 0)
table(v3$targetFV, v3$group, exclude=NULL)
table(v3$freqW, v3$group, exclude=NULL)

##Inference via linear regression
vReg = lm(pct ~ group, data=v3)
round(confint(glht(vReg, contr1), calpha = univariate_calpha())$confint, 1)

##Weekly vs. monthly
vReg1 = lm(pct ~ freqW, data=v3)
round(confint(glht(vReg1, contr2), calpha = univariate_calpha())$confint, 1)
con = summary(glht(vReg1, contr2), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Targeted vs. untargeted
vReg2 = lm(pct ~ targetFV, data=v3)
round(confint(glht(vReg2, contr2), calpha = univariate_calpha())$confint, 2)
con = summary(glht(vReg2, contr2), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

###################
##Food insecurity##
###################
##Inference on food security change from baseline to m6
f1 = fsJ3[, names(fsJ3) %in% c("record_id", "group", "fs_total_cat_BL")][, c(1, 3, 2)]
names(f1)[3] = "fs"
f1$time = "a.BL"

f2 = fsJ3[, names(fsJ3) %in% c("record_id", "group", "fs_total_cat_m6")]
names(f2)[3] = "fs"
f2$time = "b.6M"

f3 = rbind(f1, f2)
f3$insecure = ifelse(f3$fs=="a.HighSecurity", 0, 1)
table(f3$insecure, f3$fs, exclude=NULL)
f4 = f3[order(f3$record_id, f3$time), ]

##Set up contrasts of interest
f4$targetFV = ifelse(f4$group %in% c("a.FVweekly", "b.FVmonthly"), 1, 0)
f4$freqW = ifelse(f4$group %in% c("a.FVweekly", "c.UNweekly"), 1, 0)
table(f4$targetFV, f4$group, exclude=NULL)
table(f4$freqW, f4$group, exclude=NULL)

##Generalized linear mixed effects model for probability of food insecure
mf1 = glmer(insecure ~ group*time + (1 | record_id), family=binomial, data=f4)
mf2 = glmer(insecure ~ freqW*time + (1 | record_id), family=binomial, data=f4)
mf3 = glmer(insecure ~ targetFV*time + (1 | record_id), family=binomial, data=f4)

##Within group odds ratios and p-values
round(exp(confint(glht(mf1, contrast.matrix3), calpha = univariate_calpha())$confint), 2)
con = summary(glht(mf1, contrast.matrix3), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Weekly vs. monthly
round(exp(confint(glht(mf2, contrast.matrix4), calpha = univariate_calpha())$confint), 2)
con = summary(glht(mf2, contrast.matrix4), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Targeted vs. untargeted
round(exp(confint(glht(mf3, contrast.matrix5), calpha = univariate_calpha())$confint), 2)
con = summary(glht(mf3, contrast.matrix5), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Try GEE
library(geepack)

mf1 = geeglm(insecure ~ group*time, corstr="exchangeable", id=record_id, family=binomial, data=f4)

mf2 = geeglm(insecure ~ freqW*time, corstr="exchangeable", id=record_id, family=binomial, data=f4)

mf3 = geeglm(insecure ~ targetFV*time, corstr="exchangeable", id=record_id, family=binomial, data=f4)


##Within group odds ratios and p-values
##A
comp.CI2(contr=contrast.matrix3[1, ], beta=mf1$geese$beta, vcov=mf1$geese$vbeta, ex=TRUE)$pres
WaldTest(L=contrast.matrix3[1, ], betahat=mf1$geese$beta, Vn=mf1$geese$vbeta, h=0)[3]

##B
comp.CI2(contr=contrast.matrix3[2, ], beta=mf1$geese$beta, vcov=mf1$geese$vbeta, ex=TRUE)$pres
WaldTest(L=contrast.matrix3[2, ], betahat=mf1$geese$beta, Vn=mf1$geese$vbeta, h=0)[3]

##C
comp.CI2(contr=contrast.matrix3[3, ], beta=mf1$geese$beta, vcov=mf1$geese$vbeta, ex=TRUE)$pres
WaldTest(L=contrast.matrix3[3, ], betahat=mf1$geese$beta, Vn=mf1$geese$vbeta, h=0)[3]

##D
comp.CI2(contr=contrast.matrix3[4, ], beta=mf1$geese$beta, vcov=mf1$geese$vbeta, ex=TRUE)$pres
WaldTest(L=contrast.matrix3[4, ], betahat=mf1$geese$beta, Vn=mf1$geese$vbeta, h=0)[3]


##Weekly vs. monthly
##Weekly
comp.CI2(contr=contrast.matrix4[1, ], beta=mf2$geese$beta, vcov=mf2$geese$vbeta, ex=TRUE)$pres

##Monthly
comp.CI2(contr=contrast.matrix4[2, ], beta=mf2$geese$beta, vcov=mf2$geese$vbeta, ex=TRUE)$pres

##Comparison
WaldTest(L=contrast.matrix4[3, ], betahat=mf2$geese$beta, Vn=mf2$geese$vbeta, h=0)[3]


##Targeted vs. untargeted
##Targeted
comp.CI2(contr=contrast.matrix5[1, ], beta=mf3$geese$beta, vcov=mf3$geese$vbeta, ex=TRUE)$pres

##Untargeted
comp.CI2(contr=contrast.matrix5[2, ], beta=mf3$geese$beta, vcov=mf3$geese$vbeta, ex=TRUE)$pres

##Comparison
WaldTest(L=contrast.matrix5[3, ], betahat=mf3$geese$beta, Vn=mf3$geese$vbeta, h=0)[3]


######################
##Mediation analyses##
######################
##Consumption cycle mediated the effectiveness of weekly versus monthly vouchers,
mod1 = merge(d6[, names(d6) %in% c("chives_id", "fvC", "fvC_6", "freqW", "group")], cc3, by="chives_id", all.x=TRUE)
mod1$FVchange = mod1$fvC_6-mod1$fvC
mod1$ratio_6_0 = mod1$ratio_m6 / mod1$ratio_m0
mod1$ratioZ = (mod1$ratio_6_0-mean(mod1$ratio_6_0, na.rm=TRUE)) / sd(mod1$ratio_6_0, na.rm=TRUE)
summary(mod1$ratioZ)

##Model
lm1 = lm(FVchange ~ freqW*ratioZ, data=mod1)
confint(lm1)
summary(lm1)$coeff

##Ease of use mediated the effectiveness of the targeted versus untargeted vouchers
mod2 = merge(ee2, d6[, names(d6) %in% c("record_id", "fvC", "fvC_6")], by="record_id", all.x=TRUE)
mod2$FVchange = mod2$fvC_6-mod2$fvC
summary(mod2$vouch)
mod2$vouch2 = mod2$vouch-2
summary(mod2$vouch2)

##Model
lm2 = lm(FVchange ~ targetFV*vouch2, data=mod2)
confint(lm2)
summary(lm2)$coeff


#################################
##Exploratory adjusted analyses##
#################################

##Repeat Exhibit 3: Primary - F/V; Secondary - HEI, AHEI, Voucher utilization rate, food insecurity odds, ease of use of vouchers

##In all models, adjust for age_in_years, sex, race___3, ethnicity," hh_monthly_income, ppl_in_hh, snap_you
names(r4)
dim(r4)
names(d6)

r5 = merge(r4, d6[, names(d6) %in% c("chives_id", "sex", "race___3", "ethnicity", "hh_monthly_income", "ppl_in_hh", "fa_snap_ppt")], by="chives_id", all.x=TRUE)

##Clean and center some variables
##Sex (race and ethnicity OK)
r5$sex[r5$sex==99] = 3
r5$sex2 = NA
r5$sex2[r5$sex==1] = "a.Male"
r5$sex2[r5$sex==2] = "b.Female"
r5$sex2[r5$sex==3] = "c.Other"
table(r5$sex2, r5$sex, exclude=NULL)

##Income
r5$income2 = r5$hh_monthly_income-median(r5$hh_monthly_income, na.rm=TRUE)

##People in HH
r5$hh = r5$ppl_in_hh-median(r5$ppl_in_hh, na.rm=TRUE)

##Define contrasts for adjusted model
mat0 = matrix(0, 4, 7)
contrast.matrix3a = cbind(contrast.matrix3[, 1:5], mat0, contrast.matrix3[, 6:8])

mat1 = matrix(0, 3, 7)
contrast.matrix4a = cbind(contrast.matrix4[, 1:3], mat1, contrast.matrix4[, 4])

contrast.matrix5a = cbind(contrast.matrix5[, 1:3], mat1, contrast.matrix5[, 4])

##Functions for adjusted models
getCIa = function(varname) {
    r5$var1 = r5[, which(names(r5)==varname)] #data frame must be called r5
    mm = lmer(var1 ~ group*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt +(1 | chives_id), data=r5)
    jj = round(confint(glht(mm, contrast.matrix3a), calpha = univariate_calpha())$confint, 4)
    con = summary(glht(mm, contrast.matrix3a), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
    pval = round(con$test$pvalues, 4)
    cbind(rbind(paste(paste(paste(paste(jj[1,1], " (", sep=""), jj[1,2], sep=""), jj[1,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[2,1], " (", sep=""), jj[2,2], sep=""), jj[2,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[3,1], " (", sep=""), jj[3,2], sep=""), jj[3,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[4,1], " (", sep=""), jj[4,2], sep=""), jj[4,3], sep=", "), ")", sep="")), pval) #change back for original table
   }

getCI2a = function(varname) {
    r5$var1 = r5[, which(names(r5)==varname)]
    mm = lmer(var1 ~ freqW*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt + (1 | chives_id), data=r5)
    jj = round(confint(glht(mm, contrast.matrix4a), calpha = univariate_calpha())$confint, 4)
    con = summary(glht(mm, contrast.matrix4a), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
    pval = round(con$test$pvalues, 4)
    cbind(t(rbind(paste(paste(paste(paste(jj[1,1], " (", sep=""), jj[1,2], sep=""), jj[1,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[2,1], " (", sep=""), jj[2,2], sep=""), jj[2,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[3,1], " (", sep=""), jj[3,2], sep=""), jj[3,3], sep=", "), ")", sep=""))), pval[3])
   }

getCI3a = function(varname) {
    r5$var1 = r5[, which(names(r5)==varname)]
    mm = lmer(var1 ~ targetFV*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt + (1 | chives_id), data=r5)
    jj = round(confint(glht(mm, contrast.matrix5a), calpha = univariate_calpha())$confint, 4)
    con = summary(glht(mm, contrast.matrix5a), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
    pval = round(con$test$pvalues, 4)
    cbind(t(rbind(paste(paste(paste(paste(jj[1,1], " (", sep=""), jj[1,2], sep=""), jj[1,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[2,1], " (", sep=""), jj[2,2], sep=""), jj[2,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[3,1], " (", sep=""), jj[3,2], sep=""), jj[3,3], sep=", "), ")", sep=""))), pval[3])
   }

##F/V
getCIa("fvC")
getCI2a("fvC") #week vs. month
getCI3a("fvC") #FV vs. UN

##HEI
getCIa("HEI")
getCI2a("HEI") #week vs. month
getCI3a("HEI") #FV vs. UN

##AHEI
getCIa("AHEI")
getCI2a("AHEI") #week vs. month
getCI3a("AHEI") #FV vs. UN


##Voucher utilization rate (% used over 6 months)
v4 = merge(v3, r5[r5$time=="a.BL", names(r5) %in% c("chives_id", "sex2", "race___3", "ethnicity", "income2", "hh", "fa_snap_ppt")], by="chives_id", all.x=TRUE)

##Inference via linear regression
vRegA = lm(pct ~ group + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, data=v4)
round(confint(glht(vRegA, cbind(contr1, mat0)), calpha = univariate_calpha())$confint, 1)

##Weekly vs. monthly
vReg1A = lm(pct ~ freqW + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, data=v4)
round(confint(glht(vReg1A, cbind(contr2, matrix(rep(0, 7), 1, 7))), calpha = univariate_calpha())$confint, 1)
con = summary(glht(vReg1A, cbind(contr2, matrix(rep(0, 7), 1, 7))), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Targeted vs. untargeted
vReg2A = lm(pct ~ targetFV + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, data=v4)
round(confint(glht(vReg2A, cbind(contr2, matrix(rep(0, 7), 1, 7))), calpha = univariate_calpha())$confint, 2)
con = summary(glht(vReg2A, cbind(contr2, matrix(rep(0, 7), 1, 7))), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)



##Food insecurity (month 6 vs month 0 odds ratio of food insecurity)
r6 = merge(r5, d5[, names(d5) %in% c("record_id", "chives_id" )], by="chives_id", all.x=TRUE)
f5 = merge(f4, r6[r6$time=="a.BL", names(r6) %in% c("record_id", "sex2", "race___3", "ethnicity", "income2", "hh", "fa_snap_ppt")], by="record_id", all.x=TRUE)

##Generalized linear mixed effects model for probability of food insecure
mf1A = glmer(insecure ~ group*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt + (1 | record_id), family=binomial, data=f5)
mf2A = glmer(insecure ~ freqW*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt + (1 | record_id), family=binomial, data=f5)
mf3A = glmer(insecure ~ targetFV*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt + (1 | record_id), family=binomial, data=f5)

##Within group odds ratios and p-values
round(exp(confint(glht(mf1A, contrast.matrix3a), calpha = univariate_calpha())$confint), 2)
con = summary(glht(mf1A, contrast.matrix3a), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Weekly vs. monthly
round(exp(confint(glht(mf2A, contrast.matrix4a), calpha = univariate_calpha())$confint), 2)
con = summary(glht(mf2A, contrast.matrix4a), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Targeted vs. untargeted
round(exp(confint(glht(mf3A, contrast.matrix5a), calpha = univariate_calpha())$confint), 2)
con = summary(glht(mf3A, contrast.matrix5a), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)


##Try GEE
mf1a = geeglm(insecure ~ group*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, corstr="exchangeable", id=record_id, family=binomial, data=f5)

mf2a = geeglm(insecure ~ freqW*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, corstr="exchangeable", id=record_id, family=binomial, data=f5)

mf3a = geeglm(insecure ~ targetFV*time + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, corstr="exchangeable", id=record_id, family=binomial, data=f5)


##Within group odds ratios and p-values
##A
comp.CI2(contr=contrast.matrix3a[1, ], beta=mf1a$geese$beta, vcov=mf1a$geese$vbeta, ex=TRUE)$pres
WaldTest(L=contrast.matrix3a[1, ], betahat=mf1a$geese$beta, Vn=mf1a$geese$vbeta, h=0)[3]

##B
comp.CI2(contr=contrast.matrix3a[2, ], beta=mf1a$geese$beta, vcov=mf1a$geese$vbeta, ex=TRUE)$pres
WaldTest(L=contrast.matrix3a[2, ], betahat=mf1a$geese$beta, Vn=mf1a$geese$vbeta, h=0)[3]

##C
comp.CI2(contr=contrast.matrix3a[3, ], beta=mf1a$geese$beta, vcov=mf1a$geese$vbeta, ex=TRUE)$pres
WaldTest(L=contrast.matrix3a[3, ], betahat=mf1a$geese$beta, Vn=mf1a$geese$vbeta, h=0)[3]

##D
comp.CI2(contr=contrast.matrix3a[4, ], beta=mf1a$geese$beta, vcov=mf1a$geese$vbeta, ex=TRUE)$pres
WaldTest(L=contrast.matrix3a[4, ], betahat=mf1a$geese$beta, Vn=mf1a$geese$vbeta, h=0)[3]

##Weekly vs. monthly
##Weekly
comp.CI2(contr=contrast.matrix4a[1, ], beta=mf2a$geese$beta, vcov=mf2a$geese$vbeta, ex=TRUE)$pres

##Monthly
comp.CI2(contr=contrast.matrix4a[2, ], beta=mf2a$geese$beta, vcov=mf2a$geese$vbeta, ex=TRUE)$pres

##Comparison
WaldTest(L=contrast.matrix4a[3, ], betahat=mf2a$geese$beta, Vn=mf2a$geese$vbeta, h=0)[3]

##Targeted vs. untargeted
##Targeted
comp.CI2(contr=contrast.matrix5a[1, ], beta=mf3a$geese$beta, vcov=mf3a$geese$vbeta, ex=TRUE)$pres

##Untargeted
comp.CI2(contr=contrast.matrix5a[2, ], beta=mf3a$geese$beta, vcov=mf3a$geese$vbeta, ex=TRUE)$pres

##Comparison
WaldTest(L=contrast.matrix5a[3, ], betahat=mf3a$geese$beta, Vn=mf3a$geese$vbeta, h=0)[3]


##Ease of use of vouchers
ee3 = merge(ee2, r6[r6$time=="a.BL", names(r6) %in% c("record_id", "sex2", "race___3", "ethnicity", "income2", "hh", "fa_snap_ppt")], by="record_id", all.x=TRUE)

##Inference by group via linear regression
eRegA = lm(vouch ~ group + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, data=ee3)
round(confint(glht(eRegA, cbind(contr1, mat0)), calpha = univariate_calpha())$confint, 2)

##Weekly vs. monthly
eReg1A = lm(vouch ~ freqW + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, data=ee3)
round(confint(glht(eReg1A, cbind(contr2, matrix(rep(0, 7), 1, 7))), calpha = univariate_calpha())$confint, 2)
con = summary(glht(eReg1A, cbind(contr2, matrix(rep(0, 7), 1, 7))), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##Targeted vs. untargeted
eReg2A = lm(vouch ~ targetFV + sex2 + race___3 + ethnicity + income2 + hh + fa_snap_ppt, data=ee3)
round(confint(glht(eReg2A, cbind(contr2, matrix(rep(0, 7), 1, 7))), calpha = univariate_calpha())$confint, 2)
con = summary(glht(eReg2A, cbind(contr2, matrix(rep(0, 7), 1, 7))), test=adjusted("none")) #substitute "Westfall" for "none" if want adjusted p-values
round(con$test$pvalues, 4)

##2SLS models
library(AER)
library(ivpack)

##My comment included two half-thoughts of different approaches for addressing nonadherence. Really. the latter approach I describe is the recommended approach. That would entail using the random assignment as an IV for voucher utilization to get an unbiased effect of treatment (voucher usage) on F&V consumption. The estimated corresponds to the complier average treatment effect, aka local average treatment effect, which in this case is equivalent to the average effect of treatment on the treated. We observe some differential utilization by axis, which means that the CATE/LATE could differ from the main treatment effect estimates.

##A basic two-stage least squares model should work, running separate models for F&V vs. unrestricted (reference category) and weekly vs. monthly (reference category).

##A good medical paper that reports IV estimates in a factorial-like RCT is here, in which the IV results (Table 2) actually became their headline finding. Here’s a BMJ paper that promotes this approach for related reasons. It’s a standard procedure in econ experiments.

v5 = merge(v4, d6[, names(d6) %in% c("chives_id", "fvC_6")], by="chives_id", all.x=TRUE)

##FV vs. UN
st1 = lm(pct ~ targetFV, data=v5)
summary(st1)
confint(st1)

iv1 = ivreg(fvC_6 ~ pct | targetFV, data=v5)
summary(iv1, vcov=sandwich, diagnostics=TRUE)
confint(iv1)

##Weekly vs. monthly
st2 = lm(pct ~ freqW, data=v5)
summary(st2)
confint(st2)

iv2 = ivreg(fvC_6 ~ pct | freqW, data=v5)
summary(iv2, vcov=sandwich, diagnostics=TRUE)
confint(iv2)

##Table of frequency data
x1 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/M6Survey_2018-09-07.csv", header=TRUE)
x2 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/M6Aux_2018-09-07.csv", header=TRUE)

x1a = x1[, names(x1) %in% c("record_id", "free_groceries_6mos", "free_meal_6mos")]
x3 = merge(d5[, names(d5) %in% c("record_id", "group")], x1a, by="record_id", all.x=TRUE)
x4 = merge(x3, x2, by="record_id", all.x=TRUE)


x5 = x4[, names(x4) %in% c("record_id", "group", "free_groceries_6mos", "free_meal_6mos", "aux_shop_often___1", "aux_shop_often___2", "aux_shop_often___3", "aux_shop_often___99", "aux_travel_how___1", "aux_travel_how___2", "aux_travel_how___3", "aux_travel_how___4", "aux_travel_how___5", "aux_travel_how___6", "aux_travel_how___7", "aux_travel_how___8", "aux_travel_how___99")]


tabSupp = mktab(data=x4, var.names=c("free_groceries_6mos", "free_meal_6mos", "aux_shop_often___1", "aux_shop_often___2", "aux_shop_often___3", "per_day", "per_week", "per_month", "aux_travel_how___1", "aux_travel_how___2", "aux_travel_how___3", "aux_travel_how___4", "aux_travel_how___5", "aux_travel_how___6", "aux_travel_how___7", "aux_travel_how___8", "aux_travel_how___99"), ind.cat=c(rep(1, 5), rep(0, 3), rep(1, 9)), group.name="group", cfn=describeMean, miss="always", pval=TRUE, tot="last", digit=1)

word.doc(obj.list=list(tabSupp), obj.title=c("Appendix Exhibit 10: Shopping habits data"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/AppendixExhibit10_2018-09-07.docx", ftype="Arial", col.odd="white")

##For the grocery shopping frequency data [appendix exhibit 10]: is it possible to consolidate the rows that begin with ‘grocery shop per’ and just show the distribution between ‘per day’, ‘per week’, and ‘per month’, and similarly consolidate the rows ‘how many times grocery shop’ into the same units [per month]. These were just two questions: how often do you shop? [and the respondent chooses per day or per week or per month].

##Similarly, could we consolidate ‘grocery shop travel with’ to just one item where it just shows the percentage who go by eat form [drive/bus/etc], knowing it will add up to more than 100% if people use more than one mode?


##After this set of edits, do you mind running a simple CART or mCART to see if we find substantial HTE?
##Simple function
NC = function(x) {
    as.numeric(as.character(x))
}

source('/Users/jrigdon/Box sync/Rigdon/Sanjay/HTE/HTE_v1.R')
source('/Users/jrigdon/Box sync/Rigdon/Useful Functions/Tables.R')
source('/Users/jrigdon/Box sync/Rigdon/Sanjay/HTE/TablesASD.R')


##match on the six randomization variables for UN vs. FV and then do the CART and look at 0-6 month F/V change (plus AHEI/HEI also?)
names(d8)
d8$fvChange = d8$fvC_6-d8$fvC
d8$heiChange = d8$HEI_6-d8$HEI
d8$aheiChange = d8$AHEI_6-d8$AHEI

summary(d8$fvChange)
summary(d8$heiChange)
summary(d8$aheiChange)

##Matching variables
covs1 = c("age_in_years", "sex", "race___3", "ethnicity", "hh_monthly_income", "ppl_in_hh", "fa_snap_ppt")

d9 = d8[, names(d8) %in% c("chives_id", "targetFV", "freqW", "fvChange", "heiChange", "aheiChange", covs1)]

#Income per person in hh
d9$inc = d9$hh_monthly_income/d9$ppl_in_hh
names(d9)
d10 = d9[, c(1, 2, 3, 4, 5, 14, 8, 9:13)]
names(d10)[2:7] = c("sex", "age", "raceblack", "eth", "inc", "snap")
d11 = d10[d10$sex!=99, ]

##Test set
newdata = d11[, names(d11) %in% c("sex", "age", "raceblack", "eth", "inc", "snap")]

##UN vs. FV##
table(d11$targetFV)

##FV change
ufF = matchd(dta=d11, id='chives_id', trt='targetFV', outc='fvChange', covs=c("sex", "age", "raceblack", "eth", "inc", "snap"), ind.cat=c(1, 0, 1, 1, 0, 1))
jj1 = inf_delta(delt=ufF[!is.na(ufF$delta), ], newd=newdata, cont=TRUE)
jj1$fit

##HEI change
ufH = matchd(dta=d11, id='chives_id', trt='targetFV', outc='heiChange', covs=c("sex", "age", "raceblack", "eth", "inc", "snap"), ind.cat=c(1, 0, 1, 1, 0, 1))
jj2 = inf_delta(delt=ufH[!is.na(ufH$delta), ], newd=newdata, cont=TRUE)
jj2$fit

##AHEI change
ufA = matchd(dta=d11, id='chives_id', trt='targetFV', outc='aheiChange', covs=c("sex", "age", "raceblack", "eth", "inc", "snap"), ind.cat=c(1, 0, 1, 1, 0, 1))
jj3 = inf_delta(delt=ufA[!is.na(ufA$delta), ], newd=newdata, cont=TRUE)
jj3$fit


##weekly vs. monthly##
table(d11$freqW)

##FV change
mwF = matchd(dta=d11, id='chives_id', trt='freqW', outc='fvChange', covs=c("sex", "age", "raceblack", "eth", "inc", "snap"), ind.cat=c(1, 0, 1, 1, 0, 1))
kk1 = inf_delta(delt=mwF[!is.na(mwF$delta), ], newd=newdata, cont=TRUE)
kk1$fit

##HEI change
mwH = matchd(dta=d11, id='chives_id', trt='freqW', outc='heiChange', covs=c("sex", "age", "raceblack", "eth", "inc", "snap"), ind.cat=c(1, 0, 1, 1, 0, 1))
kk2 = inf_delta(delt=mwH[!is.na(mwH$delta), ], newd=newdata, cont=TRUE)
kk2$fit

##AHEI change
mwA = matchd(dta=d11, id='chives_id', trt='freqW', outc='aheiChange', covs=c("sex", "age", "raceblack", "eth", "inc", "snap"), ind.cat=c(1, 0, 1, 1, 0, 1))
kk3 = inf_delta(delt=mwA[!is.na(mwA$delta), ], newd=newdata, cont=TRUE)
kk3$fit



##For regular CART, would it be that we would do a CART on 0-6 month change as a function of randomization to the FOUR groups, along with the six randomization variables?


