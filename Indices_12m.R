##Load functions and code
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Tables.R")
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Figures.R")
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Functions.R")
source("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Code/NDSR_hei.R")
source("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/AHEI/Code/NDSR_ahei.R")
foods = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Code/foods.csv", header=TRUE)

##Average recalls within person, merge w/ diet, and summarize
perID = function(dta) {
    dta2 = aggregate(dta[, 3:16], by=list(dta$ID), function(x) mean(x, na.rm=TRUE)) #change to 3:16 for HEI
    names(dta2)[1] = "ID"
    dta2
}

perID2 = function(dta) {
    dta2 = aggregate(dta[, 3:14], by=list(dta$ID), function(x) mean(x, na.rm=TRUE)) #change to 3:16 for HEI
    names(dta2)[1] = "ID"
    dta2
}

##Read in NDSR data
bl4 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_NDSR_4_2018-07-02.csv", header=TRUE)
bl9 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_NDSR_9_2018-07-02.csv", header=TRUE)

tt4 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/12_Month_NDSR_4_2018-12-03.csv", header=TRUE)
tt9 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/12_Month_NDSR_9_2018-12-03.csv", header=TRUE)

##Calculate HEI scores
blh = perID(hei_ndsr(r4=bl4, r9=bl9))
blh$time = "a.Baseline"
m12h = perID(hei_ndsr(r4=tt4, r9=tt9))
m12h$time = "b.12"
hei = rbind(blh, m12h)

##Calculate AHEI scores
bla = perID2(ahei_ndsr(r4=bl4, r9=bl9))
bla$time = "a.Baseline"
m12a = perID2(ahei_ndsr(r4=tt4, r9=tt9))
m12a$time = "b.12"
ahei = rbind(bla, m12a)

##Merge with randomization groups
d1 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/DATA_2018-03-22.csv", header=TRUE)
d2 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Groups_2018-03-22.csv", header=TRUE)
d3 = merge(d1, d2[, names(d2) %in% c("record_id", "randomize_group")], by="record_id")

hei2 = hei[hei$ID %in% d3$chives_id, ]
names(hei2)[1] = "chives_id"
ahei2 = ahei[ahei$ID %in% d3$chives_id, ]
names(ahei2)[1] = "chives_id"

hei3 = merge(hei2, d3[, names(d3) %in% c("chives_id", "randomize_group")], by="chives_id", all.x=TRUE)
ahei3 = merge(ahei2, d3[, names(d3) %in% c("chives_id", "randomize_group")], by="chives_id", all.x=TRUE)

##Re-order groups
table(hei3$randomize_group)
hei3$group = NA
hei3$group[hei3$randomize_group==4] = "a.FVweekly"
hei3$group[hei3$randomize_group==1] = "b.FVmonthly"
hei3$group[hei3$randomize_group==3] = "c.UNweekly"
hei3$group[hei3$randomize_group==2] = "d.UNmonthly"
table(hei3$group, hei3$randomize_group, exclude=NULL)

table(ahei3$randomize_group)
ahei3$group = NA
ahei3$group[ahei3$randomize_group==4] = "a.FVweekly"
ahei3$group[ahei3$randomize_group==1] = "b.FVmonthly"
ahei3$group[ahei3$randomize_group==3] = "c.UNweekly"
ahei3$group[ahei3$randomize_group==2] = "d.UNmonthly"
table(ahei3$group, ahei3$randomize_group, exclude=NULL)


##Save data
write.csv(hei3, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/HEI_12m_2018-12-03.csv", row.names=FALSE)
write.csv(ahei3, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/AHEI_12m_2018-12-03.csv", row.names=FALSE)


##Using data loaded in above and date ranges, calculate by week (1 and 4) scores for HEI and AHEI
##HEI
blh2 = hei_ndsr(r4=bl4, r9=bl9)
blh2$date = as.Date(blh2[, names(blh2)=="date"], format="%m/%d/%y")
blh2$week = "week4"
blh2$week[substr(blh2$date, 9, 9)=="0"] = "week1"
blh2$time = "a.BL"

m12h2 = hei_ndsr(r4=tt4, r9=tt9)
m12h2$date = as.Date(m12h2[, names(m12h2)=="date"], format="%m/%d/%y")
m12h2$week = "week4"
m12h2$week[substr(m12h2$date, 9, 9)=="0"] = "week1"
m12h2$time = "b.12"

##Take averages within time and week
hei2 = rbind(blh2, m12h2)
hei2$unique = paste(hei2$ID, paste(hei2$time, hei2$week, sep="_"), sep="_")
hei3 = aggregate(hei2[, 3:16], by=list(hei2$unique), function(x) mean(x, na.rm=TRUE))

##Save for studying consumption cycle (fix CP8917`_b.12_week1)
hei3$chives_id = substr(hei3$Group.1, 1, 6)
hei3$time = substr(hei3$Group.1, 8, 11)
hei3$week = substr(hei3$Group.1, 13, 17)
hei4 = hei3[, c(16:18, 2:15)]
hei5 = hei4[hei4$chives_id %in% d3$chives_id, ]

write.csv(hei5, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/HEI_weekly_12m_2018-12-03.csv", row.names=FALSE)

##AHEI
source("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/AHEI/Code/NDSR_ahei.R")
bla2 = ahei_ndsr(r4=bl4, r9=bl9)
bla2$date = as.Date(bla2[, names(bla2)=="date"], format="%m/%d/%y")
bla2$week = "week4"
bla2$week[substr(bla2$date, 9, 9)=="0"] = "week1"
bla2$time = "a.BL"

m12a2 = ahei_ndsr(r4=tt4, r9=tt9)
m12a2$date = as.Date(m12a2[, names(m12a2)=="date"], format="%m/%d/%y")
m12a2$week = "week4"
m12a2$week[substr(m12a2$date, 9, 9)=="0"] = "week1"
m12a2$time = "b.12"

##Take averages within time and week
ahei2 = rbind(bla2, m12a2)
ahei2$unique = paste(ahei2$ID, paste(ahei2$time, ahei2$week, sep="_"), sep="_")
ahei3 = aggregate(ahei2[, 3:14], by=list(ahei2$unique), function(x) mean(x, na.rm=TRUE))

##Save for studying consumption cycle
ahei3$chives_id = substr(ahei3$Group.1, 1, 6)
ahei3$time = substr(ahei3$Group.1, 8, 11)
ahei3$week = substr(ahei3$Group.1, 13, 17)
ahei4 = ahei3[, c(14:16, 2:13)]
ahei5 = ahei4[ahei4$chives_id %in% d3$chives_id, ]

write.csv(ahei5, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/AHEI_weekly_12m_2018-12-03.csv", row.names=FALSE)






##OLD WORK
bla = perID2(ahei_ndsr(r4=bl4, r9=bl9))
bla$time = "a.Baseline"
m6a = perID2(ahei_ndsr(r4=s4, r9=s9))
m6a$time = "b.6M"
ahei = rbind(bla, m6a)

bl$date = as.Date(bl[, names(bl)=="Date.of.Intake"], format="%m/%d/%y")
s$date = as.Date(s[, names(s)=="Date.of.Intake"], format="%m/%d/%y")

bl$week = "week4"
bl$week[substr(bl$date, 9, 9)=="0"] = "week1"
table(bl$week, exclude=NULL)

s$week = "week4"
s$week[substr(s$date, 9, 9)=="0"] = "week1"
table(s$week, exclude=NULL)
