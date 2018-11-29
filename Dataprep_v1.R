##Add in total calories, baseline cup-equivalents of F/V, baseline HEI score, baseline AHEI score, and baseline oz-equivalents of major USDA food categories (refined grains, whole grains, etc.) ... just the big categories

##Load NDSR data
##Connect to server
#Path: Y:\Sanjay Projects\CHIVES\Chives NDSR Data
#Server: 171.64.154.67
#U: jrigdon
#P: pcor-584

##Add in new data from Sarah
##ALL DONE! 😊I have outputted the missing folder including 22 recalls.
##Here is the path: smb://171.64.154.67/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Missing/C1-8 BL & 6M
m_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Missing/C1-8 BL & 6M/ChivesMissind/ChivesMissind/ChivesMissin04.csv', header=TRUE)
m_4_bl = m_4[m_4$Participant.ID %in% c("CP1058", "CP7003"), ]
m_4_s = m_4[!m_4$Participant.ID %in% c("CP1058", "CP7003"), ]

m_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Missing/C1-8 BL & 6M/ChivesMissind/ChivesMissind/ChivesMissin09.csv', header=TRUE)
m_9_bl = m_9[m_9$Participant.ID %in% c("CP1058", "CP7003"), ]
m_9_s = m_9[!m_9$Participant.ID %in% c("CP1058", "CP7003"), ]

#Baseline data
bl1_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/Baseline Data/ChivesC1BL04.csv', header=TRUE)
bl2_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/Baseline Data/ChiveC2BLalld/ChiveC2BLall04.csv', header=TRUE)
bl3_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/Baseline Data/ChiveC3BLalld/ChiveC3BLall04.csv', header=TRUE)
bl4_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 4/Baseline/ChiveC4BLalld/ChiveC4BLall04.csv', header=TRUE)
bl5_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 5/Baseline/ChiveC5BLalld/ChiveC5BLall04.csv', header=TRUE)
bl6_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 6/Baseline/ChiveC6BLalld/ChiveC6BLall04.csv', header=TRUE)
bl7_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 7/Baseline/ChiveC7BLalld/ChiveC7BLall04.csv', header=TRUE)
bl8_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 8/Baseline/ChiveC8BLalld/ChiveC8BLall04.csv', header=TRUE)
bl_4 = rbind(m_4_bl, bl1_4, bl2_4, bl3_4, bl4_4, bl5_4, bl6_4, bl7_4, bl8_4)
write.csv(bl_4, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_NDSR_4_2018-07-02.csv", row.names=FALSE)

bl1_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/Baseline Data/ChivesC1BL09.csv', header=TRUE)
bl2_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/Baseline Data/ChiveC2BLalld/ChiveC2BLall09.csv', header=TRUE)
bl3_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/Baseline Data/ChiveC3BLalld/ChiveC3BLall09.csv', header=TRUE)
bl4_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 4/Baseline/ChiveC4BLalld/ChiveC4BLall09.csv', header=TRUE)
bl5_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 5/Baseline/ChiveC5BLalld/ChiveC5BLall09.csv', header=TRUE)
bl6_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 6/Baseline/ChiveC6BLalld/ChiveC6BLall09.csv', header=TRUE)
bl7_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 7/Baseline/ChiveC7BLalld/ChiveC7BLall09.csv', header=TRUE)
bl8_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 8/Baseline/ChiveC8BLalld/ChiveC8BLall09.csv', header=TRUE)
bl_9 = rbind(m_9_bl, bl1_9, bl2_9, bl3_9, bl4_9, bl5_9, bl6_9, bl7_9, bl8_9)
write.csv(bl_9, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_NDSR_9_2018-07-02.csv", row.names=FALSE)

##6 month data
##4 of recalls from cohort 7 (Participate number 7210) was in cohort 8 folder
s1_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/6 Month Data/ChiveC16Malld/ChiveC16Mall04.csv', header=TRUE)
s2_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/6 Month Data/ChiveC26Malld/ChiveC26Mall04.csv', header=TRUE)
s3_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/6 Month Data/ChiveC36Malld/ChiveC36Mall04.csv', header=TRUE)
s4_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 4/6 Month/ChiveC46Malld/ChiveC46Mall04.csv', header=TRUE)
s5_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 5/6 Month/ChiveC56Malld/ChiveC56Mall04.csv', header=TRUE)
s6_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 6/Month 6/ChiveC6M6alld/ChiveC6M6all04.csv', header=TRUE)
s7_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 7/Month 6/ChiveC7M6alld/ChiveC7M6all04.csv', header=TRUE)
s8_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 8/Month 6/ChiveC8M6alld/ChiveC8M6all04.csv', header=TRUE)
s_4 = rbind(m_4_s, s1_4, s2_4, s3_4, s4_4, s5_4, s6_4[, 1:215], s7_4[, 1:215], s8_4[, 1:215])
write.csv(s_4, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/6_Month_NDSR_4_2018-07-02.csv", row.names=FALSE)

s1_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/6 Month Data/ChiveC16Malld/ChiveC16Mall09.csv', header=TRUE)
s2_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/6 Month Data/ChiveC26Malld/ChiveC26Mall09.csv', header=TRUE)
s3_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/6 Month Data/ChiveC36Malld/ChiveC36Mall09.csv', header=TRUE)
s4_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 4/6 Month/ChiveC46Malld/ChiveC46Mall09.csv', header=TRUE)
s5_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 5/6 Month/ChiveC56Malld/ChiveC56Mall09.csv', header=TRUE)
s6_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 6/Month 6/ChiveC6M6alld/ChiveC6M6all09.csv', header=TRUE)
s7_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 7/Month 6/ChiveC7M6alld/ChiveC7M6all09.csv', header=TRUE)
s8_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 8/Month 6/ChiveC8M6alld/ChiveC8M6all09.csv', header=TRUE)

s_9 = rbind(m_9_s, s1_9, s2_9, s3_9, s4_9, s5_9, s6_9, s7_9, s8_9)
write.csv(s_9, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/6_Month_NDSR_9_2018-07-02.csv", row.names=FALSE)


##Look at number of recalls per participant and file
bl4 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_NDSR_4_2018-07-02.csv", header=TRUE)
bl9 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_NDSR_9_2018-07-02.csv", header=TRUE)

s4 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/6_Month_NDSR_4_2018-07-02.csv", header=TRUE)
s9 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/6_Month_NDSR_9_2018-07-02.csv", header=TRUE)

##Remove duplicate recalls and subset to Mandy final spreadsheet
final = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Mandy_final.csv", header=TRUE)

#Remove duplicates
#r3 = r2[!duplicated(r2),]
bl = cbind(bl4[, c(2:3, 14:215)], bl9[, 4:171])
bl2 = bl[!duplicated(bl[, 205:372]), ]
bl3 = bl2[bl2$Participant.ID %in% final$chives_id, ]

s = cbind(s4[, c(2:3, 14:215)], s9[, 4:171])
s2 = s[!duplicated(s[, 205:372]), ]
s3 = s2[s2$Participant.ID %in% final$chives_id, ]

##Remove recalls not found in google sheets
##Baseline removals
##CP2034 3/6/17 4956.297, CP2065 2/21/17 2685.655, CP4106 5/1/17
bl_remove1 = which(bl3$Participant.ID=="CP2034" & bl3$Date.of.Intake=="3/6/17" & bl3$Total.Grams==4956.297)
bl_remove2 = which(bl3$Participant.ID=="CP2065" & bl3$Date.of.Intake=="2/21/17" & bl3$Total.Grams==2685.655)
bl_remove3 = which(bl3$Participant.ID=="CP4106" & bl3$Date.of.Intake=="5/1/17")
bl4 = bl3[-c(bl_remove1, bl_remove2, bl_remove3), ]

##6 month removals (don't have this data)
##CP6130 12/4/17, CP7194 2/25/18
#s_remove1 = which(s3$Participant.ID=="CP6130" & s3$Date.of.Intake=="12/4/17")
#s_remove2 = which(s3$Participant.ID=="CP7194" & s3$Date.of.Intake=="2/25/18")
#s4 = s3[-c(s_remove1, s_remove2), ]
#s3[s3$Participant.ID=="CP6130", 1:5]
#s3[s3$Participant.ID=="CP7194", 1:5]

##Get number of recalls per person
blr = by(bl4$Participant.ID, bl4$Participant.ID, length)
data_bl = data.frame(chives_id=names(blr), nrecall=as.numeric(blr))
data_bl2 = data_bl[!is.na(data_bl$nrecall), ]
table(data_bl2$nrecall)
write.csv(data_bl2, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_nrecall_2018-07-02.csv", row.names=FALSE)

s = by(s3$Participant.ID, s3$Participant.ID, length)
data_s = data.frame(chives_id=names(s), nrecall=as.numeric(s))
table(data_s$nrecall)
write.csv(data_s, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_nrecall_2018-07-02.csv", row.names=FALSE)

##Closely examine those not matched with Mandy's sheet
unver = final$chives_id[1:16] #for ordering later
names(data_bl2)[2] = "joe_nrecall_bl"
names(data_s)[2] = "joe_nrecall_6"
nr = merge(data_bl2, data_s, by="chives_id", all.x=TRUE)

final2 = final[, c(1, 3, 5)]
names(final2)[2:3] = c("mandy_nrecall_bl", "mandy_nrecall_6")

nr2 = merge(nr, final2, by="chives_id", all.x=TRUE)
nr3 = nr2[, c(1:2, 4, 3, 5)]
nr3[is.na(nr3)] = 0

nr3$matchBL = ifelse(nr3$joe_nrecall_bl==nr3$mandy_nrecall_bl, "Match", "Mismatch")
nr3$match6 = ifelse(nr3$joe_nrecall_6==nr3$mandy_nrecall_6, "Match", "Mismatch")

nr3$anyMismatch = ifelse(nr3$matchBL=="Mismatch" | nr3$match6=="Mismatch", 1, 0)

nr4 = nr3[order(-nr3$anyMismatch), ]
write.csv(nr4, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nrecall_check_2018-07-02.csv", row.names=FALSE)


##Save these versions of the nutrition data
write.csv(bl4, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-02.csv", row.names=FALSE)
write.csv(s3, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_2018-07-02.csv", row.names=FALSE)


##3 final orders of business before proceeding to analysis
##CP3058 (1 extra BL for Joe), CP6179 (1 extra M6 for Joe), CP8123 (1 extra M6 for Mandy) were the problematic ones from yesterday
##CP3058 - 1 extra BL recall **delete the recall for 4/5/2017, this was sent over e-mail (not protocol) and the ppt never verified the amts
##CP6179 - 1 extra M6 recall **delete the recall for 1/4/2018
##CP8123 – I actually should also only have 4 recalls at M6 (not 5 recalls), as the true 2nd recall on 3/27/2018 was deleted due to low kcal intake. There should only be 2 M6W4 recalls on 3/26 and 3/28.

bl4 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-02.csv", header=TRUE)
bl4_remove1 = which(bl4$Participant.ID=="CP3058" & bl4$Date.of.Intake=="4/5/17")
bl5 = bl4[-c(bl4_remove1), ]

s3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_2018-07-02.csv", header=TRUE)
s3_remove1 = which(s3$Participant.ID=="CP6179" & s3$Date.of.Intake=="1/4/18")
#s3_remove2 = which(s3$Participant.ID=="CP8123" & s3$Date.of.Intake=="1/4/18")
s4 = s3[-c(s3_remove1), ]

##Save (7/3/2018 is the most up-to-date)
write.csv(bl5, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-03.csv", row.names=FALSE)
write.csv(s4, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_2018-07-03.csv", row.names=FALSE)

##Send final numbers of recalls per person to Mandy
bl4 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-03.csv", header=TRUE)
s3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_2018-07-03.csv", header=TRUE)


##Get number of recalls per person
blr = by(bl4$Participant.ID, bl4$Participant.ID, length)
data_bl = data.frame(chives_id=names(blr), nrecall=as.numeric(blr))
data_bl2 = data_bl[!is.na(data_bl$nrecall), ]
table(data_bl2$nrecall)
names(data_bl2)[2] = "nrecall_bl"

s = by(s3$Participant.ID, s3$Participant.ID, length)
data_s = data.frame(chives_id=names(s), nrecall=as.numeric(s))
table(data_s$nrecall)
names(data_s)[2] = "nrecall_m6"

data_r = merge(data_bl2, data_s, by="chives_id", all.x=TRUE)
data_r[is.na(data_r)] = 0
table(data_r$nrecall_bl)
table(data_r$nrecall_m6)

write.csv(data_r, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nrecalls_2018-07-05.csv", row.names=FALSE)


#####################
##Consumption cycle##
#####################
##Read in NDSR data
bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-03.csv", header=TRUE)
s = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_2018-07-03.csv", header=TRUE)

##aggregate over week1 and week4 recall within person
rec = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Recalls_2018-07-27.csv", header=TRUE) #merge by chives_id!

blw1r1 = rec[rec$redcap_event_name=="m1w1_arm_1", names(rec) %in% c("record_id", "recall_1_when")]
blw1r1$week = "week1"
names(blw1r1)[2] = "date"
blw1r2 = rec[rec$redcap_event_name=="m1w1_arm_1", names(rec) %in% c("record_id", "recall_2_when")]
blw1r2$week = "week1"
names(blw1r2)[2] = "date"
blw1r3 = rec[rec$redcap_event_name=="m1w1_arm_1", names(rec) %in% c("record_id", "recall_3_when")]
blw1r3$week = "week1"
names(blw1r3)[2] = "date"

blw1 = rbind(blw1r1, blw1r2, blw1r3)
blw1$date = substr(blw1$date, 1, 7)

blw4r1 = rec[rec$redcap_event_name=="prerandomization_arm_1", names(rec) %in% c("record_id", "recall_1_when")]
blw4r1$week = "week4"
names(blw4r1)[2] = "date"
blw4r2 = rec[rec$redcap_event_name=="prerandomization_arm_1", names(rec) %in% c("record_id", "recall_2_when")]
blw4r2$week = "week4"
names(blw4r2)[2] = "date"
blw4r3 = rec[rec$redcap_event_name=="prerandomization_arm_1", names(rec) %in% c("record_id", "recall_3_when")]
blw4r3$week = "week4"
names(blw4r3)[2] = "date"

blw4 = rbind(blw4r1, blw4r2, blw4r3)
blw4$date = substr(blw4$date, 1, 7)

blw = rbind(blw1, blw4)
blw = blw[blw$date!="", ]

#bl$ID = paste(bl$Participant.ID, bl$Date.of.Intake, sep="_")

blw2 = merge(blw, d3[, names(d3) %in% c("record_id", "chives_id")], by="record_id", all.x=TRUE)

#blw2$ID = paste(blw2$chives_id, blw2$date, sep="_")
blw2 = blw2[order(blw2$chives_id, blw2$date), ]
bl = bl[order(bl$Participant.ID, bl$Date.of.Intake), ]

#bll = merge(bl, blw2[, names(blw2) %in% c("ID", "week")], by="ID", all.x=TRUE)

##Save data sets for baseline and manually record week 1 or 4
##Convert to dates and order
bl$Date.of.Intake = as.Date(bl$Date.of.Intake, format="%m/%d/%y")
bl = bl[order(bl$Participant.ID, bl$Date.of.Intake), ]

blw2$date = as.Date(blw2$date, format="%m/%d/%y")
blw2 = blw2[order(blw2$chives_id, blw2$date), ]

#write.csv(blw2[, c(4, 2, 3)], "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_merge_2018-07-28.csv", row.names=FALSE)
#write.csv(bl, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-28.csv", row.names=FALSE)

##Same thing for 6 months
s = s[order(s$Participant.ID, s$Date.of.Intake), ]
sw1r1 = rec[rec$redcap_event_name=="m6w1_arm_1", names(rec) %in% c("record_id", "recall_1_when")]
sw1r1$week = "week1"
names(sw1r1)[2] = "date"
sw1r2 = rec[rec$redcap_event_name=="m6w1_arm_1", names(rec) %in% c("record_id", "recall_2_when")]
sw1r2$week = "week1"
names(sw1r2)[2] = "date"
sw1r3 = rec[rec$redcap_event_name=="m6w1_arm_1", names(rec) %in% c("record_id", "recall_3_when")]
sw1r3$week = "week1"
names(sw1r3)[2] = "date"

sw1 = rbind(sw1r1, sw1r2, sw1r3)
sw1$date = substr(sw1$date, 1, 7)

sw4r1 = rec[rec$redcap_event_name=="m6w4_arm_1", names(rec) %in% c("record_id", "recall_1_when")]
sw4r1$week = "week4"
names(sw4r1)[2] = "date"
sw4r2 = rec[rec$redcap_event_name=="m6w4_arm_1", names(rec) %in% c("record_id", "recall_2_when")]
sw4r2$week = "week4"
names(sw4r2)[2] = "date"
sw4r3 = rec[rec$redcap_event_name=="m6w4_arm_1", names(rec) %in% c("record_id", "recall_3_when")]
sw4r3$week = "week4"
names(sw4r3)[2] = "date"

sw4 = rbind(sw4r1, sw4r2, sw4r3)
sw4$date = substr(sw4$date, 1, 7)

sw = rbind(sw1, sw4)
sw = sw[sw$date!="", ]

sw2 = merge(sw, d3[, names(d3) %in% c("record_id", "chives_id")], by="record_id", all.x=TRUE)

sw2 = sw2[order(sw2$chives_id, sw2$date), ]

##Save data sets for 6 months and manually record week 1 or 4
##Convert to dates and order
s$Date.of.Intake = as.Date(s$Date.of.Intake, format="%m/%d/%y")
s = s[order(s$Participant.ID, s$Date.of.Intake), ]

sw2$date = as.Date(sw2$date, format="%m/%d/%y")

##Convert 2001 to 2017
sw2$date = gsub("2001", "2017", sw2$date)
sw2 = sw2[order(sw2$chives_id, sw2$date), ]

#write.csv(sw2[, c(4, 2, 3)], "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_merge_2018-08-06.csv", row.names=FALSE)
#write.csv(s, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_2018-08-06.csv", row.names=FALSE)

##Re-read in data after figuring out weeks
bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-28.csv", header=TRUE) #copy/paste weeks
s = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_6_2018-08-06.csv", header=TRUE) #go through carefully here

##Derive consumption cycle data for calories
bl$ID = paste(bl$Participant.ID, bl$Week, sep="_")
blC = aggregate(bl[, 10], list(bl$ID), function(x) mean(x, na.rm=TRUE))
blC$study_id = substr(blC$Group.1, 1, 6)
blC$week = substr(blC$Group.1, 8, 12)
blC2 = blC[, c(3,4,2)]
names(blC2)[3] = "calories"
blC2$time = "a.Baseline"

s$ID = paste(s$Participant.ID, s$Week, sep="_")
sC = aggregate(s[, 10], list(s$ID), function(x) mean(x, na.rm=TRUE))
sC$study_id = substr(sC$Group.1, 1, 6)
sC$week = substr(sC$Group.1, 8, 12)
sC2 = sC[, c(3,4,2)]
names(sC2)[3] = "calories"
sC2$time = "b.6Months"

allC = rbind(blC2, sC2)
names(allC)[1] = "chives_id"
allC2 = merge(allC, d5[, names(d5) %in% c("chives_id", "group")], by="chives_id", all.x=TRUE)

#cyB1 = mktab(data=allC2[allC2$time=="a.Baseline" & allC2$week=="week1", ], var.names=c("calories"), ind.cat=c(0), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)
#cyB4 = mktab(data=allC2[allC2$time=="a.Baseline" & allC2$week=="week4", ], var.names=c("calories"), ind.cat=c(0), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)

#cyS1 = mktab(data=allC2[allC2$time=="b.6Months" & allC2$week=="week1", ], var.names=c("calories"), ind.cat=c(0), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)
#cyS4 = mktab(data=allC2[allC2$time=="b.6Months" & allC2$week=="week4", ], var.names=c("calories"), ind.cat=c(0), group.name="group", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=1)

#tabC = rbind(cyB1, cyB4, cyS1, cyS4)

#word.doc(obj.list=list(tabC), obj.title="Appendix Exhibit 1: Consumption cycle of calories", dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Tables/AppendixExhibit1_2018-08-08.docx", ftype="Arial", col.odd="white")

##Send Un, weekly M6 W4 IDs to Mandy
#write.csv(allC2[allC2$time=="b.6Months" & allC2$week=="week4" & allC2$group=="c.UNweekly", ], "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Data_consumption_cycle_2018-08-07.csv", row.names=FALSE)
