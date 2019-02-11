##Add in total calories, baseline cup-equivalents of F/V, baseline HEI score, baseline AHEI score, and baseline oz-equivalents of major USDA food categories (refined grains, whole grains, etc.) ... just the big categories

##Load NDSR data
##Connect to server
#Path: Y:\Sanjay Projects\CHIVES\Chives NDSR Data
#Server: 171.64.154.67
#U: jrigdon
#P: pcor-584


#Baseline data
t1_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/12 Month Data/ChivC1M12alld/ChivC1M12all04.csv', header=TRUE)
t2_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/12 Month Data/ChivC2M12alld/ChivC2M12all04.csv', header=TRUE)
t3_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/12 Month Data/ChivC3M12alld/ChivC3M12all04.csv', header=TRUE)
t4_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 4/12 Month/ChivC4M12alld/ChivC4M12all04.csv', header=TRUE)
t5_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 5/Month 12/ChivC5M12alld/ChivC5M12all04.csv', header=TRUE)
t6_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 6/Month 12/ChivC6M12alld/ChivC6M12all04.csv', header=TRUE)
t7_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 7/Month 12/ChivC7M12alld/ChivC7M12all04.csv', header=TRUE)
t8_4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 8/Month 12/ChivC8M12alld/ChivC8M12all04.csv', header=TRUE)
names(t4_4)[1] = "Project.Abbreviation" #fix one mismatch
t_4 = rbind(t1_4, t2_4, t3_4, t4_4, t5_4, t6_4, t7_4, t8_4)

##Names don't match; check them
#nn = cbind(names(t1_4), names(t2_4), names(t3_4), names(t4_4), names(t5_4), names(t6_4), names(t7_4), names(t8_4))
#apply(nn, 1, function(x) length(table(x))) #it is ONLY the first variable that is problematic
#write.csv(nn, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/M12_NDSR_4_names.csv", row.names=FALSE)
write.csv(t_4, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/12_Month_NDSR_4_2018-12-03.csv", row.names=FALSE) #save for Indices_12m.R

t1_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/12 Month Data/ChivC1M12alld/ChivC1M12all09.csv', header=TRUE)
t2_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/12 Month Data/ChivC2M12alld/ChivC2M12all09.csv', header=TRUE)
t3_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/12 Month Data/ChivC3M12alld/ChivC3M12all09.csv', header=TRUE)
t4_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 4/12 Month/ChivC4M12alld/ChivC4M12all09.csv', header=TRUE)
t5_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 5/Month 12/ChivC5M12alld/ChivC5M12all09.csv', header=TRUE)
t6_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 6/Month 12/ChivC6M12alld/ChivC6M12all09.csv', header=TRUE)
t7_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 7/Month 12/ChivC7M12alld/ChivC7M12all09.csv', header=TRUE)
t8_9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 8/Month 12/ChivC8M12alld/ChivC8M12all09.csv', header=TRUE)
t_9 = rbind(t1_9, t2_9, t3_9, t4_9, t5_9, t6_9, t7_9, t8_9)
write.csv(t_9, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/12_Month_NDSR_9_2018-12-03.csv", row.names=FALSE)


##Remove duplicate recalls and subset to Mandy final spreadsheet
final = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Mandy_final.csv", header=TRUE)

##Check dates
#cc = cbind(t_4$Date.of.Intake, t_9$Date.of.Intake)
#sum(apply(cc, 1, function(x) x[1]==x[2]))

tt = cbind(t_4[, c(2:3, 14:215)], t_9[, 4:171]) #may need to add gluten to baseline, 6, and 12 month files - missed this
t2 = tt[!duplicated(tt[, 205:372]), ]
t3 = t2[t2$Participant.ID %in% final$chives_id, ]
t2[, 1][!t2[, 1] %in% t3[, 1]]

##Save (12/3/2018 is the most up-to-date)
write.csv(t3, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_12_2018-12-03.csv", row.names=FALSE)


