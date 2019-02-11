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
hei5 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/HEI_weekly_12m_2018-12-03.csv", header=TRUE)
ahei5 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/AHEI_weekly_12m_2018-12-03.csv", header=TRUE)

##Read in F/V by week data (derived in separate file)
n5 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_weekly_12m_2018-12-03.csv", header=TRUE)

##Descriptive stats for paper
##Calories
summary(n5$calories[n5$time=="a.BL" & n5$week=="week1"])
summary(n5$calories[n5$time=="a.BL" & n5$week=="week4"])
summary(n5$calories[n5$time=="b.12" & n5$week=="week1"])
summary(n5$calories[n5$time=="b.12" & n5$week=="week4"])

##F/V
summary(n5$fvC[n5$time=="a.BL" & n5$week=="week1"])
summary(n5$fvC[n5$time=="a.BL" & n5$week=="week4"])
summary(n5$fvC[n5$time=="b.12" & n5$week=="week1"])
summary(n5$fvC[n5$time=="b.12" & n5$week=="week4"])

##HEI
summary(hei5$HEI[hei5$time=="a.BL" & hei5$week=="week1"])
summary(hei5$HEI[hei5$time=="a.BL" & hei5$week=="week4"])
summary(hei5$HEI[hei5$time=="b.12" & hei5$week=="week1"])
summary(hei5$HEI[hei5$time=="b.12" & hei5$week=="week4"])


##Ratios of consumption
cw1 = n5[n5$time=="a.BL" & n5$week=="week1", ][, c(1, 4)]
names(cw1)[2] = paste(names(cw1)[2], "m0w1", sep="_")
cw4 = n5[n5$time=="a.BL" & n5$week=="week4", ][, c(1, 4)]
names(cw4)[2] = paste(names(cw4)[2], "m0w4", sep="_")

ww1 = n5[n5$time=="b.12" & n5$week=="week1", ][, c(1, 4)]
names(ww1)[2] = paste(names(ww1)[2], "m12w1", sep="_")
ww4 = n5[n5$time=="b.12" & n5$week=="week4", ][, c(1, 4)]
names(ww4)[2] = paste(names(ww4)[2], "m12w4", sep="_")

cc1 = merge(cw4, cw1, by="chives_id", all.x=TRUE)
cc2 = merge(cc1, ww1, by="chives_id", all.x=TRUE)
cc3 = merge(cc2, ww4, by="chives_id", all.x=TRUE)

cc3$ratio_m0 = cc3$calories_m0w4 / cc3$calories_m0w1
cc3$ratio_m12 = cc3$calories_m12w4 / cc3$calories_m12w1

##Save data and make table
#write.csv(cc3, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Data_consumption_cycle_12m_2018-12-03.csv", row.names=FALSE)
#cc3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Data_consumption_cycle_2018-08-15_v2.csv", header=TRUE)
cc3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Data_consumption_cycle_12m_2018-12-03.csv", header=TRUE)

cc4 = merge(cc3, d5[, names(d5) %in% c("chives_id", "group")], all.x=TRUE)

tabC2 = mktab(data=cc4, var.names=c("calories_m0w1", "calories_m0w4", "ratio_m0", "calories_m12w1", "calories_m12w4", "ratio_m12"), ind.cat=rep(0, 6), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=2)

word.doc(obj.list=list(tabC2), obj.title="Appendix Exhibit 1: Consumption cycle of calories", dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/AppendixExhibit1_12m_2018-12-04.docx", ftype="Arial", col.odd="white")

###########################
##Food insecurity outcome##
###########################
sec = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Secondary_2018-12-04.csv", header=TRUE)
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


##Merge to see cross tabs at 12 months
fsB = fs[fs$redcap_event_name=="prerandomization_arm_1", c(1, 3:8, 15, 16)]
names(fsB)[2:9] = paste(names(fsB)[2:9], "BL", sep="_")

fs12 = fs[fs$redcap_event_name=="m12w1_arm_1", c(1, 3:8, 15, 16)]
names(fs12)[2:9] = paste(names(fs12)[2:9], "m12", sep="_")

fsJ1 = merge(fsB, fs12, by="record_id")
fsJ3 = merge(fsJ1, d5[, names(d5) %in% c("record_id", "group")], by="record_id", all.x=TRUE)


##Make table and send
tabFS = mktab(data=fsJ3, var.names=c("fs_last_BL", "fs_last_m12", "fs_balanced_BL", "fs_balanced_m12", "fs_cut_BL", "fs_cut_m12", "fs_cut_freq_BL", "fs_cut_freq_m12", "fs_eat_less_BL", "fs_eat_less_m12", "fs_hungry_BL", "fs_hungry_m12", "fs_total_BL", "fs_total_m12", "fs_total_cat_BL", "fs_total_cat_m12"), ind.cat=rep(1, 16), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)

word.doc(obj.list=list(tabFS), obj.title="Appendix Exhibit 3: Summary of food security variables", dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/AppendixExhibit4_2018-12-04.docx", ftype="Arial", col.odd="white")

#table(fsJ3$fs_total_cat_BL, fsJ3$fs_total_cat_m6w1, exclude=NULL)
#table(fsJ3$fs_total_cat_m6w1, fsJ3$fs_total_cat_m6w4, exclude=NULL) #there are n=8 that took both



###################
##PRIMARY OUTCOME##
###################

##Read in wide nutrition data
nutr = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_wide_12m_2018-12-03.csv", header=TRUE)

##Merge demographics and nutrition data
d6 = merge(d5, nutr, by="chives_id", all.x=TRUE)
names(d6)

summary(d6$fvC)
summary(d6$fvC_12)


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
d6t = merge(d6, fsJ3[, names(fsJ3) %in% c("record_id", "fs_total_BL", "fs_total_m12", "fs_total_cat_BL", "fs_total_cat_m12")], by="record_id", all.x=TRUE)

##Pull in HEI/AHEI (data read in later)
hei = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/HEI_12m_2018-12-03.csv", header=TRUE)
hei_bl = hei[hei$time=="a.Baseline", ]
hei_12 = hei[hei$time=="b.12", ]
names(hei_12)[2:15] = paste(names(hei_12)[2:15], "12", sep="_")
hei_all = merge(hei_bl[, names(hei_bl) %in% c("chives_id", "HEI")], hei_12[, names(hei_12) %in% c("chives_id", "HEI_12")], by="chives_id", all.x=TRUE)

ahei = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/AHEI_12m_2018-12-03.csv", header=TRUE)
ahei_bl = ahei[ahei$time=="a.Baseline", ]
ahei_12 = ahei[ahei$time=="b.12", ]
names(ahei_12)[2:13] = paste(names(ahei_12)[2:13], "12", sep="_")
ahei_all = merge(ahei_bl[, names(ahei_bl) %in% c("chives_id", "AHEI")], ahei_12[, names(ahei_12) %in% c("chives_id", "AHEI_12")], by="chives_id", all.x=TRUE)

d7 = merge(d6t, hei_all, by="chives_id")
d8 = merge(d7, ahei_all, by="chives_id") #actually don't really need this ...

##Look at descriptives
d8$insecure_BL = ifelse(d8$fs_total_cat_BL=="a.HighSecurity", 0, 1)
table(d8$insecure_BL, d8$fs_total_cat_BL, exclude=NULL)
d8$insecure_12 = ifelse(d8$fs_total_cat_m12=="a.HighSecurity", 0, 1)
table(d8$insecure_12, d8$fs_total_cat_m12, exclude=NULL)

tabDesc = mktab(data=d8, var.names=c("fvC", "fvC_12", "HEI", "HEI_12", "AHEI", "AHEI_12", "insecure_BL", "insecure_12", "fs_total_cat_BL", "fs_total_cat_m12", "fs_total_BL", "fs_total_m12"), ind.cat=c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)

word.doc(obj.list=list(tabDesc), obj.title=c("Table 1: Descriptive table of primary and secondary outcomes at baseline and 12 months"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/Outcomes_2018-12-05.docx", ftype="Arial", col.odd="white")

##Save d8 for CT.gov stats
write.csv(d8, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/CT_data_2018-12-14.csv", row.names=FALSE)

#########################
##Statistical Inference##
#########################

##Primary outcomes
r3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_long_12m_2018-12-03.csv", header=TRUE)

##Merge to get groups of interest
r4 = merge(r3, d5[, names(d5) %in% c("chives_id", "group", "targetFV", "freqW")], by="chives_id", all.x=TRUE)

##Get HEI and AHEI in there
h1 = hei_all[, names(hei_all) %in% c("chives_id", "HEI")]
h1$time = "a.BL"
h2 = hei_all[, names(hei_all) %in% c("chives_id", "HEI_12")]
names(h2)[2] = "HEI"
h2$time = "b.12"
h3 = rbind(h1, h2)
h3$unique = paste(h3$chives_id, h3$time, sep="_")

a1 = ahei_all[, names(ahei_all) %in% c("chives_id", "AHEI")]
a1$time = "a.BL"
a2 = ahei_all[, names(ahei_all) %in% c("chives_id", "AHEI_12")]
names(a2)[2] = "AHEI"
a2$time = "b.12"
a3 = rbind(a1, a2)
a3$unique = paste(a3$chives_id, a3$time, sep="_")

h4 = merge(h3[, names(h3) %in% c("unique", "HEI")], a3[, names(a3) %in% c("unique", "AHEI")], by="unique", all.x=TRUE)

r4$unique = paste(r4$chives_id, r4$time, sep="_")
r4 = merge(r4, h4, by="unique", all.x=TRUE)

##0-6 month changes by group
tab2m = t(cbind(getCI("fvC"), getCI("HEI"), getCI("AHEI")))

rownames(tab2m)[seq(1, 5, 2)] = c("fvC","HEI", "AHEI")
colnames(tab2m) = c("FVweekly", "FVmonthly", "UNweekly", "UNmonthly")

word.doc(obj.list=list(tab2m), obj.title=c("Exhibit 2: Baseline to 12 month changes for selected variables in CHIVES study"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/Exhibit2_12m_2018-12-04.docx", ftype="Arial", col.odd="white")



##Statistical testing for two main effects
##Monthly to weekly
tab2w = rbind(getCI2("fvC"), getCI2("HEI"), getCI2("AHEI"))

rownames(tab2w) = c("fvC","HEI", "AHEI")
colnames(tab2w) = c("Weekly", "Monthly", "Weekly-Monthly", "P-value")

word.doc(obj.list=list(tab2w), obj.title=c("Exhibit 2a: Baseline to 12 month changes for selected variables in CHIVES study: weekly vs. monthly"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/Weekly_v_monthly_12m_2018-12-04.docx", ftype="Arial", col.odd="white")


##Targeted vs. unrestricted
tab2t = rbind(getCI3("fvC"), getCI3("HEI"), getCI3("AHEI"))

rownames(tab2t) = c("fvC","HEI", "AHEI")
colnames(tab2t) = c("Targeted", "Untargeted", "Targeted-Untargeted", "P-value")

word.doc(obj.list=list(tab2t), obj.title=c("Exhibit 2b: Baseline to 12 month changes for selected variables in CHIVES study: targeted vs. untargeted"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/Targeted_v_unrestricted_12m_2018-12-04.docx.docx", ftype="Arial", col.odd="white")


##Secondary outcomes


###################
##Food insecurity##
###################
##Inference on food security change from baseline to m6
f1 = fsJ3[, names(fsJ3) %in% c("record_id", "group", "fs_total_cat_BL")][, c(1, 3, 2)]
names(f1)[3] = "fs"
f1$time = "a.BL"

f2 = fsJ3[, names(fsJ3) %in% c("record_id", "group", "fs_total_cat_m12")][, c(1, 3, 2)]
names(f2)[3] = "fs"
f2$time = "b.12"

f3 = rbind(f1, f2)
f3$insecure = ifelse(f3$fs=="a.HighSecurity", 0, 1)
table(f3$insecure, f3$fs, exclude=NULL)
f4 = f3[order(f3$record_id, f3$time), ]

##Set up contrasts of interest
f4$targetFV = ifelse(f4$group %in% c("a.FVweekly", "b.FVmonthly"), 1, 0)
f4$freqW = ifelse(f4$group %in% c("a.FVweekly", "c.UNweekly"), 1, 0)
table(f4$targetFV, f4$group, exclude=NULL)
table(f4$freqW, f4$group, exclude=NULL)

##Generalized linear mixed effects model for probability of food insecure (look into this - check descriptives on 12/5/2018)
##Double check with descriptive table
##table(f4$insecure, f4$group, f4$time)
mf1 = glmer(insecure ~ group*time + (1 | record_id), family=binomial, data=f4, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
mf2 = glmer(insecure ~ freqW*time + (1 | record_id), family=binomial, data=f4, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
mf3 = glmer(insecure ~ targetFV*time + (1 | record_id), family=binomial, data=f4, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

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


