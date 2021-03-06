##Derive F/V outcome by week
bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-03.csv", header=TRUE)
tt = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_12_2018-12-03.csv", header=TRUE)

##List of final CHIVES IDs from Mandy
d3 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Mandy_final.csv", header=TRUE)

##Check that names match
#cd = cbind(names(bl), names(tt))
#apply(cd, 1, function(x) sum(x[1]==x[2])) #all match

##Rename a few variables
foods = read.csv('/Users/jrigdon/Box sync/Rigdon/Sanjay/EVIDENCE/Data/foods.csv', header=TRUE)
##Baseline
sum(names(bl)[205:372]!=foods$Food_group)
names(bl)[205:372] = as.character(foods$New_name)
##12 Months
sum(names(tt)[205:372]!=foods$Food_group)
names(tt)[205:372] = as.character(foods$New_name)

##Fruits
bl$citrus_juiceC = bl$citrus_juice / 2
bl$fruit_juice_nocitrusC = bl$fruit_juice_nocitrus / 2
bl$citrus_fruitC = bl$citrus_fruit * (3/8)
bl$fruit_nocitrusC = bl$fruit_nocitrus * (3/8)
bl$avocadoC = bl$avocado / 2
bl$fried_fruitC = bl$fried_fruit / 2
bl$fruit_savory_snackC = bl$fruit_savory_snack / 8

tt$citrus_juiceC = tt$citrus_juice / 2
tt$fruit_juice_nocitrusC = tt$fruit_juice_nocitrus / 2
tt$citrus_fruitC = tt$citrus_fruit * (3/8)
tt$fruit_nocitrusC = tt$fruit_nocitrus * (3/8)
tt$avocadoC = tt$avocado / 2
tt$fried_fruitC = tt$fried_fruit / 2
tt$fruit_savory_snackC = tt$fruit_savory_snack / 8

##Vegetables
bl$darkgreen_vegC = bl$darkgreen_veg / 2
bl$deepyellow_vegC = bl$deepyellow_veg / 2
bl$tomatoC = bl$tomato * (3/8)
bl$white_potatoesC = bl$white_potatoes / 2
bl$fried_potatoesC = bl$fried_potatoes / 2
bl$starchy_vegC = bl$starchy_veg / 2
bl$legumesC = bl$legumes / 2
bl$other_vegC = bl$other_veg / 2
bl$fried_vegC = bl$fried_veg / 2
bl$veg_juiceC = bl$veg_juice / 2

tt$darkgreen_vegC = tt$darkgreen_veg / 2
tt$deepyellow_vegC = tt$deepyellow_veg / 2
tt$tomatoC = tt$tomato * (3/8)
tt$white_potatoesC = tt$white_potatoes / 2
tt$fried_potatoesC = tt$fried_potatoes / 2
tt$starchy_vegC = tt$starchy_veg / 2
tt$legumesC = tt$legumes / 2
tt$other_vegC = tt$other_veg / 2
tt$fried_vegC = tt$fried_veg / 2
tt$veg_juiceC = tt$veg_juice / 2

##Fruits (only citrus_fruit and fruit_nocitrus)
fruitsC = c("citrus_fruitC", "fruit_nocitrusC")
bl$fruitsC = apply(bl[, names(bl) %in% fruitsC], 1, function(x) sum(x, na.rm=TRUE))
summary(bl$fruitsC)

tt$fruitsC = apply(tt[, names(tt) %in% fruitsC], 1, function(x) sum(x, na.rm=TRUE))
summary(tt$fruitsC)

##Vegetables (only darkgreen_veg, deepyellow_veg, tomato, and starchy_veg)
vegC = c("darkgreen_vegC", "deepyellow_vegC", "tomatoC", "starchy_vegC")
bl$vegC = apply(bl[, names(bl) %in% vegC], 1, function(x) sum(x, na.rm=TRUE))
summary(bl$vegC)

tt$vegC = apply(tt[, names(tt) %in% vegC], 1, function(x) sum(x, na.rm=TRUE))
summary(tt$vegC)

##Fruits and Vegetables in cup equivalents
bl$fvC = bl$fruitsC + bl$vegC
tt$fvC = tt$fruitsC + tt$vegC
summary(bl$fvC)
summary(tt$fvC)


#########################
##Groupings of interest##
#########################
##Move to cup equivalents
##Fruits other (already have CHIVES fruits)
fruitsO = c("citrus_juiceC", "fruit_juice_nocitrusC", "avocadoC", "fried_fruitC", "fruit_savory_snackC")
bl$fruitsO = apply(bl[, names(bl) %in% fruitsO], 1, function(x) sum(x, na.rm=TRUE))
summary(bl$fruitsO)

tt$fruitsO = apply(tt[, names(tt) %in% fruitsO], 1, function(x) sum(x, na.rm=TRUE))
summary(tt$fruitsO)


##Vegetables other (already have CHIVES veg)
vegO = c("white_potatoesC", "fried_potatoesC", "legumesC", "other_vegC", "fried_vegC", "veg_juiceC")
bl$vegO = apply(bl[, names(bl) %in% vegO], 1, function(x) sum(x, na.rm=TRUE))
summary(bl$vegO)

tt$vegO = apply(tt[, names(tt) %in% vegO], 1, function(x) sum(x, na.rm=TRUE))
summary(tt$vegO)

##SSBs
##Convert from servings to ounces by multiplying by 8
ssb = c('sweetened_soda', 'artificially_sweetened_soda', 'sweetened_fruit_drink', 'artificially_sweetened_fruit_drink', 'sweetened_tea', 'artificially_sweetened_tea', 'sweetened_coffee', 'artificially_sweetened_coffee', 'sweetened_coffee_substitute', 'artificially_sweetened_coffee_substitute', 'sweetened_water', 'artificially_sweetened_water', 'nondairy_sweetened_meal_replacement', 'nondairy_artificially_sweetened_meal_replacement') #remove unsweetened water!  And all other unsweetened ones
bl$ssb = apply(bl[, names(bl) %in% ssb], 1, function(x) sum(x, na.rm=TRUE)) * 8
summary(bl$ssb)
tt$ssb = apply(tt[, names(tt) %in% ssb], 1, function(x) sum(x, na.rm=TRUE)) * 8
summary(tt$ssb)


##Grains
grains = c("whole_grain_mix","some_whole_grain_mix","refined_grain_mix","whole_grain_bread","some_whole_grain_bread","refined_grain_bread","whole_grain_other","some_whole_grain_other","refined_grain_other","whole_grain_cracker","some_whole_grain_cracker","refined_grain_cracker","whole_grain_pasta","some_whole_grain_pasta","refined_grain_pasta","whole_grain_cereal_nosweet","some_whole_grain_cereal_nosweet","refined_grain_cereal_nosweet","whole_grain_cereal_sweet","some_whole_grain_cereal_sweet","refined_grain_cereal_sweet","whole_grain_cookie","some_whole_grain_cookie","refined_grain_cookie","whole_grain_bar","some_whole_grain_bar","refined_grain_bar","whole_grain_chip","some_whole_grain_chip","refined_grain_chip","popcorn","popcorn_flavored","refined_grain_baby","baby_food_whole_grain","baby_food_some_whole_grain")

##Cuq equivalents (1/2 serving size usually)
weightGR = c(1/2, 1/2, 1/2, 1/8, 1/8, 1/8, 1/5, 1/5, 1/5, 1/8, 1/8, 1/8, 1/2, 1/2, 1/2, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/2, 1/2, 1/2, 1/5.67, 1/5.67, 1/5.67, 1/8, 1/8, 1/8, 1/8, 1/8, 1/2, 1/2, 1/2)
bl$grains = apply(t(t(bl[, names(bl) %in% grains]) * weightGR), 1, function(x) sum(x, na.rm=TRUE))
summary(bl$grains)
tt$grains = apply(t(t(tt[, names(tt) %in% grains]) * weightGR), 1, function(x) sum(x, na.rm=TRUE))
summary(tt$grains)


##Protein
meat_fish_eggs_nuts = c("beef","lean_beef","veal","lean_veal","lamb","lean_lamb","fresh_pork","lean_fresh_pork","cured_pork","lean_cured_pork","game","poultry","lean_poultry","fried_chicken","fresh_fish","lean_fish","fried_fish","shellfish","fried_shellfish","coldcuts_sausage","lean_coldcuts_sausage","organ_meat","babyfood_meat","meat_savory_snack","eggs","eggs_substitute","nuts_seeds","nut_seed_butters","meat_substitute")

##Cup equivalents
weightPR = c(1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/2, 1/8, 1/4, 1/4, 1/16, 1/16, 1/12)
bl$protein = apply(t(t(bl[, names(bl) %in% meat_fish_eggs_nuts]) * weightPR), 1, function(x) sum(x, na.rm=TRUE))
summary(bl$protein)
tt$protein = apply(t(t(tt[, names(tt) %in% meat_fish_eggs_nuts]) * weightPR), 1, function(x) sum(x, na.rm=TRUE))
summary(tt$protein)


##Dairy
dairy = c("milk_whole","milk_reduced_fat","milk_low_fat","milk_nondairy","milk_flavored_whole","milk_flavored_reduced_fat","milk_flavored_low_fat","sweetened_nonfat_dry_milk","artificially_sweetened_nonfat_dry_milk","sweetened_no_nonfat_dry_milk","artificially_sweetened_no_nonfat_dry_milk","cheese_full_fat","cheese_reduced_fat","cheese_low_fat","cheese_nondairy","yogurt_sweetened_whole","yogurt_sweetened_low","yogurt_sweetened_free","yogurt_artificially_sweetened_whole","yogurt_artificially_sweetened_low","yogurt_artificially_sweetened_free","yogurt_nondairy","frozen_dairy","frozen_nondairy","pudding_other","artificially_sweetened_pudding","cream","cream_reduced","cream_low","cream_nondairy","dairy_sweetened_replacement","dairy_artificially_sweetened_replacement")

##Cup equivalents
weightD = c(3/4, 3/4, 3/4, 1, 1, 1, 1, 1, 1, 1, 1, 1/4, 1/4, 1/4, 1/4, 1, 1, 1, 1, 1, 1, 1, 1/2, 1/2, 3/4, 3/4, 1/16, 1/16, 1/16, 1/16, 1, 1)
bl$dairy = apply(t(t(bl[, names(bl) %in% dairy]) * weightD), 1, function(x) sum(x, na.rm=TRUE))
summary(bl$dairy)
tt$dairy = apply(t(t(tt[, names(tt) %in% dairy]) * weightD), 1, function(x) sum(x, na.rm=TRUE))
summary(tt$dairy)

##Fix nutrients (baseline and 6 months)
bl3 = bl[, names(bl) %in% c("Participant.ID", "Date.of.Intake", "Energy..kcal.", "Total.Fat..g.", "Total.Carbohydrate..g.", "Total.Protein..g.", "X..Calories.from.Fat", "X..Calories.from.Carbohydrate", "X..Calories.from.Protein", "fruitsC", "vegC", "fvC", "fruitsO", "vegO", "ssb", "grains", "protein", "dairy")]

names(bl3)[1:9] = c("chives_id", "date", "calories", "fat_g", "carb_g", "protein_g", "fat_%", "carb_%", "protein_%")
names(bl3)
bl3$time = "a.BL"

tt3 = tt[, names(tt) %in% c("Participant.ID", "Date.of.Intake", "Energy..kcal.", "Total.Fat..g.", "Total.Carbohydrate..g.", "Total.Protein..g.", "X..Calories.from.Fat", "X..Calories.from.Carbohydrate", "X..Calories.from.Protein", "fruitsC", "vegC", "fvC", "fruitsO", "vegO", "ssb", "grains", "protein", "dairy")]

names(tt3)[1:9] = c("chives_id", "date", "calories", "fat_g", "carb_g", "protein_g", "fat_%", "carb_%", "protein_%")
names(tt3)
tt3$time = "b.12"

##Aggregate by week
n2 = rbind(bl3, tt3)
n2$date = as.Date(n2[, names(n2)=="date"], format="%m/%d/%y")
n2$week = "week4"
n2$week[substr(n2$date, 9, 9)=="0"] = "week1"

n2$unique = paste(n2$chives_id, paste(n2$time, n2$week, sep="_"), sep="_")
n3 = aggregate(n2[, 3:18], by=list(n2$unique), function(x) mean(x, na.rm=TRUE))

##Save for studying consumption cycle
n3$chives_id = substr(n3$Group.1, 1, 6)
n3$time = substr(n3$Group.1, 8, 11)
n3$week = substr(n3$Group.1, 13, 17)
n4 = n3[, c(18:20, 2:17)]
n5 = n4[n4$chives_id %in% d3$chives_id, ] #all check out

write.csv(n5, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_weekly_12m_2018-12-03.csv", row.names=FALSE)



##Derive F/V outcome by time point
##Read in NDSR data
bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Baseline_2018-07-03.csv", header=TRUE)
tt = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Month_12_2018-12-03.csv", header=TRUE)

##Take average over the recalls by person
##Baseline
bl2 = aggregate(bl[, 8:372], list(bl$Participant.ID), function(x) mean(x, na.rm=TRUE))
names(bl2)[1] = "chives_id"

##12 Months
tt2 = aggregate(tt[, 8:372], list(tt$Participant.ID), function(x) mean(x, na.rm=TRUE))
names(tt2)[1] = "chives_id"

##Quick check that IDs in nutrition data are same as final list from Mandy
sum(names(table(bl2$chives_id))==names(table(d3$chives_id))) #all 359 match!

##Rename a few variables
foods = read.csv('/Users/jrigdon/Box sync/Rigdon/Sanjay/EVIDENCE/Data/foods.csv', header=TRUE)
##Baseline
sum(names(bl2)[199:366]!=foods$Food_group)
names(bl2)[199:366] = as.character(foods$New_name)
##12 Months
sum(names(tt2)[199:366]!=foods$Food_group)
names(tt2)[199:366] = as.character(foods$New_name)

##Fruits
bl2$citrus_juiceC = bl2$citrus_juice / 2
bl2$fruit_juice_nocitrusC = bl2$fruit_juice_nocitrus / 2
bl2$citrus_fruitC = bl2$citrus_fruit * (3/8)
bl2$fruit_nocitrusC = bl2$fruit_nocitrus * (3/8)
bl2$avocadoC = bl2$avocado / 2
bl2$fried_fruitC = bl2$fried_fruit / 2
bl2$fruit_savory_snackC = bl2$fruit_savory_snack / 8

tt2$citrus_juiceC = tt2$citrus_juice / 2
tt2$fruit_juice_nocitrusC = tt2$fruit_juice_nocitrus / 2
tt2$citrus_fruitC = tt2$citrus_fruit * (3/8)
tt2$fruit_nocitrusC = tt2$fruit_nocitrus * (3/8)
tt2$avocadoC = tt2$avocado / 2
tt2$fried_fruitC = tt2$fried_fruit / 2
tt2$fruit_savory_snackC = tt2$fruit_savory_snack / 8

##Vegetables
bl2$darkgreen_vegC = bl2$darkgreen_veg / 2
bl2$deepyellow_vegC = bl2$deepyellow_veg / 2
bl2$tomatoC = bl2$tomato * (3/8)
bl2$white_potatoesC = bl2$white_potatoes / 2
bl2$fried_potatoesC = bl2$fried_potatoes / 2
bl2$starchy_vegC = bl2$starchy_veg / 2
bl2$legumesC = bl2$legumes / 2
bl2$other_vegC = bl2$other_veg / 2
bl2$fried_vegC = bl2$fried_veg / 2
bl2$veg_juiceC = bl2$veg_juice / 2

tt2$darkgreen_vegC = tt2$darkgreen_veg / 2
tt2$deepyellow_vegC = tt2$deepyellow_veg / 2
tt2$tomatoC = tt2$tomato * (3/8)
tt2$white_potatoesC = tt2$white_potatoes / 2
tt2$fried_potatoesC = tt2$fried_potatoes / 2
tt2$starchy_vegC = tt2$starchy_veg / 2
tt2$legumesC = tt2$legumes / 2
tt2$other_vegC = tt2$other_veg / 2
tt2$fried_vegC = tt2$fried_veg / 2
tt2$veg_juiceC = tt2$veg_juice / 2

##Fruits (only citrus_fruit and fruit_nocitrus)
fruitsC = c("citrus_fruitC", "fruit_nocitrusC")
bl2$fruitsC = apply(bl2[, names(bl2) %in% fruitsC], 1, function(x) sum(x, na.rm=TRUE))
summary(bl2$fruitsC)

tt2$fruitsC = apply(tt2[, names(tt2) %in% fruitsC], 1, function(x) sum(x, na.rm=TRUE))
summary(tt2$fruitsC)

##Vegetables (only darkgreen_veg, deepyellow_veg, tomato, and starchy_veg)
vegC = c("darkgreen_vegC", "deepyellow_vegC", "tomatoC", "starchy_vegC")
bl2$vegC = apply(bl2[, names(bl2) %in% vegC], 1, function(x) sum(x, na.rm=TRUE))
summary(bl2$vegC)

tt2$vegC = apply(tt2[, names(tt2) %in% vegC], 1, function(x) sum(x, na.rm=TRUE))
summary(tt2$vegC)

##Fruits and Vegetables in cup equivalents
bl2$fvC = bl2$fruitsC + bl2$vegC
tt2$fvC = tt2$fruitsC + tt2$vegC
summary(bl2$fvC)
summary(tt2$fvC)


#########################
##Groupings of interest##
#########################
##Move to cup equivalents
##Fruits other (already have CHIVES fruits)
fruitsO = c("citrus_juiceC", "fruit_juice_nocitrusC", "avocadoC", "fried_fruitC", "fruit_savory_snackC")
bl2$fruitsO = apply(bl2[, names(bl2) %in% fruitsO], 1, function(x) sum(x, na.rm=TRUE))
summary(bl2$fruitsO)

tt2$fruitsO = apply(tt2[, names(tt2) %in% fruitsO], 1, function(x) sum(x, na.rm=TRUE))
summary(tt2$fruitsO)


##Vegetables other (already have CHIVES veg)
vegO = c("white_potatoesC", "fried_potatoesC", "legumesC", "other_vegC", "fried_vegC", "veg_juiceC")
bl2$vegO = apply(bl2[, names(bl2) %in% vegO], 1, function(x) sum(x, na.rm=TRUE))
summary(bl2$vegO)

tt2$vegO = apply(tt2[, names(tt2) %in% vegO], 1, function(x) sum(x, na.rm=TRUE))
summary(tt2$vegO)

##SSBs
##Convert from servings to ounces by multiplying by 8
ssb = c('sweetened_soda', 'artificially_sweetened_soda', 'sweetened_fruit_drink', 'artificially_sweetened_fruit_drink', 'sweetened_tea', 'artificially_sweetened_tea', 'sweetened_coffee', 'artificially_sweetened_coffee', 'sweetened_coffee_substitute', 'artificially_sweetened_coffee_substitute', 'sweetened_water', 'artificially_sweetened_water', 'nondairy_sweetened_meal_replacement', 'nondairy_artificially_sweetened_meal_replacement') #remove unsweetened water!  And all other unsweetened ones
bl2$ssb = apply(bl2[, names(bl2) %in% ssb], 1, function(x) sum(x, na.rm=TRUE)) * 8
summary(bl2$ssb)
tt2$ssb = apply(tt2[, names(tt2) %in% ssb], 1, function(x) sum(x, na.rm=TRUE)) * 8
summary(tt2$ssb)


##Grains
grains = c("whole_grain_mix","some_whole_grain_mix","refined_grain_mix","whole_grain_bread","some_whole_grain_bread","refined_grain_bread","whole_grain_other","some_whole_grain_other","refined_grain_other","whole_grain_cracker","some_whole_grain_cracker","refined_grain_cracker","whole_grain_pasta","some_whole_grain_pasta","refined_grain_pasta","whole_grain_cereal_nosweet","some_whole_grain_cereal_nosweet","refined_grain_cereal_nosweet","whole_grain_cereal_sweet","some_whole_grain_cereal_sweet","refined_grain_cereal_sweet","whole_grain_cookie","some_whole_grain_cookie","refined_grain_cookie","whole_grain_bar","some_whole_grain_bar","refined_grain_bar","whole_grain_chip","some_whole_grain_chip","refined_grain_chip","popcorn","popcorn_flavored","refined_grain_baby","baby_food_whole_grain","baby_food_some_whole_grain")

##Cuq equivalents (1/2 serving size usually)
weightGR = c(1/2, 1/2, 1/2, 1/8, 1/8, 1/8, 1/5, 1/5, 1/5, 1/8, 1/8, 1/8, 1/2, 1/2, 1/2, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/2, 1/2, 1/2, 1/5.67, 1/5.67, 1/5.67, 1/8, 1/8, 1/8, 1/8, 1/8, 1/2, 1/2, 1/2)
bl2$grains = apply(t(t(bl2[, names(bl2) %in% grains]) * weightGR), 1, function(x) sum(x, na.rm=TRUE))
summary(bl2$grains)
tt2$grains = apply(t(t(tt2[, names(tt2) %in% grains]) * weightGR), 1, function(x) sum(x, na.rm=TRUE))
summary(tt2$grains)


##Protein
meat_fish_eggs_nuts = c("beef","lean_beef","veal","lean_veal","lamb","lean_lamb","fresh_pork","lean_fresh_pork","cured_pork","lean_cured_pork","game","poultry","lean_poultry","fried_chicken","fresh_fish","lean_fish","fried_fish","shellfish","fried_shellfish","coldcuts_sausage","lean_coldcuts_sausage","organ_meat","babyfood_meat","meat_savory_snack","eggs","eggs_substitute","nuts_seeds","nut_seed_butters","meat_substitute")

##Cup equivalents
weightPR = c(1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/2, 1/8, 1/4, 1/4, 1/16, 1/16, 1/12)
bl2$protein = apply(t(t(bl2[, names(bl2) %in% meat_fish_eggs_nuts]) * weightPR), 1, function(x) sum(x, na.rm=TRUE))
summary(bl2$protein)
tt2$protein = apply(t(t(tt2[, names(tt2) %in% meat_fish_eggs_nuts]) * weightPR), 1, function(x) sum(x, na.rm=TRUE))
summary(tt2$protein)


##Dairy
dairy = c("milk_whole","milk_reduced_fat","milk_low_fat","milk_nondairy","milk_flavored_whole","milk_flavored_reduced_fat","milk_flavored_low_fat","sweetened_nonfat_dry_milk","artificially_sweetened_nonfat_dry_milk","sweetened_no_nonfat_dry_milk","artificially_sweetened_no_nonfat_dry_milk","cheese_full_fat","cheese_reduced_fat","cheese_low_fat","cheese_nondairy","yogurt_sweetened_whole","yogurt_sweetened_low","yogurt_sweetened_free","yogurt_artificially_sweetened_whole","yogurt_artificially_sweetened_low","yogurt_artificially_sweetened_free","yogurt_nondairy","frozen_dairy","frozen_nondairy","pudding_other","artificially_sweetened_pudding","cream","cream_reduced","cream_low","cream_nondairy","dairy_sweetened_replacement","dairy_artificially_sweetened_replacement")

##Cup equivalents
weightD = c(3/4, 3/4, 3/4, 1, 1, 1, 1, 1, 1, 1, 1, 1/4, 1/4, 1/4, 1/4, 1, 1, 1, 1, 1, 1, 1, 1/2, 1/2, 3/4, 3/4, 1/16, 1/16, 1/16, 1/16, 1, 1)
bl2$dairy = apply(t(t(bl2[, names(bl2) %in% dairy]) * weightD), 1, function(x) sum(x, na.rm=TRUE))
summary(bl2$dairy)
tt2$dairy = apply(t(t(tt2[, names(tt2) %in% dairy]) * weightD), 1, function(x) sum(x, na.rm=TRUE))
summary(tt2$dairy)

##Fix nutrients (baseline and 6 months)
bl3 = bl2[, names(bl2) %in% c("chives_id", "Energy..kcal.", "Total.Fat..g.", "Total.Carbohydrate..g.", "Total.Protein..g.", "X..Calories.from.Fat", "X..Calories.from.Carbohydrate", "X..Calories.from.Protein", "fruitsC", "vegC", "fvC", "fruitsO", "vegO", "ssb", "grains", "protein", "dairy")]

names(bl3)[1:8] = c("chives_id", "calories", "fat_g", "carb_g", "protein_g", "fat_%", "carb_%", "protein_%")
names(bl3)

tt3 = tt2[, names(tt2) %in% c("chives_id", "Energy..kcal.", "Total.Fat..g.", "Total.Carbohydrate..g.", "Total.Protein..g.", "X..Calories.from.Fat", "X..Calories.from.Carbohydrate", "X..Calories.from.Protein", "fruitsC", "vegC", "fvC", "fruitsO", "vegO", "ssb", "grains", "protein", "dairy")]

names(tt3)[1:8] = c("chives_id", "calories", "fat_g", "carb_g", "protein_g", "fat_%", "carb_%", "protein_%")
names(tt3)

##Long data set
tt3a = tt3
bl3a = bl3

bl3a$time = "a.BL"
tt3a$time = "b.12"

r3 = rbind(bl3a, tt3a)
write.csv(r3, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_long_12m_2018-12-03.csv", row.names=FALSE)


##Wide data set
names(tt3)[-1] = paste(names(tt3)[-1], "12", sep="_")
names(tt3)
nutr = merge(bl3, tt3, by="chives_id", all.x=TRUE)

write.csv(nutr, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/Nutrition_wide_12m_2018-12-03.csv", row.names=FALSE)





