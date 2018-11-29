##Preliminary HEI function

##Interpolation function
interp = function(x, x1, y1, x2, y2) {
    m = (y2-y1) / (x2-x1)
    b = y2-m*x2
    y = NA
if (!is.na(x) & x<=x1) {
    y = y1
} else if (!is.na(x) & x>x1 & x<x2) {
    y = m*x + b
} else if (!is.na(x) & x>=x2) {
    y = y2
}
    y
}

##HEI calculation
hei_ndsr = function(r4, r9) {
##Define categories of foods
##Adequacy
total_fruits = c("citrus_juice", "fruit_juice_nocitrus", "citrus_fruit", "fruit_nocitrus", "avocado", "fried_fruit", "fruit_savory_snack")
whole_fruits = c("citrus_fruit", "fruit_nocitrus", "avocado")
total_veg = c("darkgreen_veg", "deepyellow_veg", "tomato", "white_potatoes", "fried_potatoes", "starchy_veg", "legumes", "other_veg", "fried_veg", "veg_juice")
greens_beans = c("darkgreen_veg", "legumes")
whole_grains = c("whole_grain_mix","whole_grain_bread","whole_grain_other","whole_grain_cracker","whole_grain_pasta","whole_grain_cereal_nosweet","whole_grain_cereal_sweet","whole_grain_cookie","whole_grain_chip","whole_grain_bar")
dairy = c("milk_whole", "milk_reduced_fat", "milk_low_fat", "milk_nondairy", "milk_flavored_whole", "milk_flavored_reduced_fat", "milk_flavored_low_fat", "sweetened_nonfat_dry_milk", "artificially_sweetened_nonfat_dry_milk", "cheese_full_fat", "cheese_reduced_fat", "cheese_low_fat", "cheese_nondairy", "yogurt_sweetened_whole", "yogurt_sweetened_low", "yogurt_sweetened_free", "yogurt_artificially_sweetened_whole", "yogurt_artificially_sweetened_low", "yogurt_artificially_sweetened_free", "yogurt_nondairy", "frozen_dairy", "frozen_nondairy", "pudding_other", "artificially_sweetened_pudding", "dairy_sweetened_replacement", "dairy_artificially_sweetened_replacement")
total_protein = c("legumes", "beef", "lean_beef", "veal", "lean_veal", "lamb", "lean_lamb", "fresh_pork", "lean_fresh_pork", "cured_pork", "lean_cured_pork", "game", "poultry", "lean_poultry", "fried_chicken", "fresh_fish", "lean_fish", "fried_fish", "shellfish", "fried_shellfish", "coldcuts_sausage", "lean_coldcuts_sausage", "organ_meat", "eggs", "eggs_substitute", "nuts_seeds", "nut_seed_butters", "meat_substitute")
sea_plant_protein = c("legumes", "fresh_fish", "lean_fish", "fried_fish", "shellfish", "fried_shellfish")
#fatty_acids #(PUFAs + MUFAs)/SFAs

##Moderation
refined_grains = c("refined_grain_mix","refined_grain_bread","refined_grain_other","refined_grain_cracker","refined_grain_pasta","refined_grain_cereal_nosweet","refined_grain_cereal_sweet","refined_grain_cookie","refined_grain_bar","refined_grain_chip")
#sodium #convert to g;
#added_sugars #convert to %energy
#saturated_fats #convert to %energy

##Look at reports 4 and 9 and pull out necessary components
names(r4)
r4a = r4[, names(r4) %in% c("Participant.ID", "Date.of.Intake", "Energy..kcal.", "Total.Fat..g.", "Total.Carbohydrate..g.", "Total.Protein..g.", "Total.Saturated.Fatty.Acids..SFA...g.", "Total.Monounsaturated.Fatty.Acids..MUFA...g.", "Total.Polyunsaturated.Fatty.Acids..PUFA...g.", "Added.Sugars..by.Total.Sugars...g.", "Sodium..mg.")]
names(r4a) = c("ID", "date", "calories", "fat_g", "carb_g", "protein_g", "sfa_g", "mufa_g", "pufa_g", "sodium_mg", "added_sugars_g")

names(r9)
pos = match(foods$Food_group, names(r9))
names(r9)[pos] = as.character(foods$New_name)
r9a = r9[, names(r9) %in% c("Participant.ID", "Date.of.Intake", as.character(foods$New_name))]
names(r9a)[1:2] = c("ID", "date")

##Merge into one file
#r4a$unique = paste(r4a$ID, r4a$date, sep="_")
#r9a$unique = paste(r9a$ID, r9a$date, sep="_")

#names(r9a)[1:5}
all = cbind(r4a, r9a[, -c(1:2)])

#summary(all$calories)

##Aggregate within categories
all$total_fruits = 1000*(apply(all[, names(all) %in% total_fruits], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$total_fruits)

all$whole_fruits = 1000*(apply(all[, names(all) %in% whole_fruits], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$whole_fruits)

all$total_veg = 1000*(apply(all[, names(all) %in% total_veg], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$total_veg)

all$greens_beans = 1000*(apply(all[, names(all) %in% greens_beans], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$greens_beans)

all$whole_grains = 1000*(apply(all[, names(all) %in% whole_grains], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$whole_grains)

all$dairy = 1000*(apply(all[, names(all) %in% dairy], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$dairy)

all$total_protein = 1000*(apply(all[, names(all) %in% total_protein], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$total_protein)

all$sea_plant_protein = 1000*(apply(all[, names(all) %in% sea_plant_protein], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$sea_plant_protein)

all$fatty_acids = (all$mufa_g+all$pufa_g) / all$sfa_g
summary(all$fatty_acids)

all$refined_grains = 1000*(apply(all[, names(all) %in% refined_grains], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
summary(all$refined_grains)

all$sodium = all$sodium_mg / 1000
summary(all$sodium)

all$added_sugars = all$added_sugars_g / (all$fat_g + all$carb_g + all$protein_g)
summary(all$added_sugars)

all$saturated_fats = all$sfa_g / (all$fat_g + all$carb_g + all$protein_g)
summary(all$saturated_fats)

##Add component scores
##Adequacy
##Total fruits: MIN (0) to ≥0.8 cup equiv. per 1,000 kcal MAX (5)
all$ScTotalFruits = sapply(all$total_fruits, function(x) interp(x, 0, 0, 0.8, 5))
#plot(all$total_fruits, all$ScTotalFruits)

##Whole fruits: MIN (0) to ≥0.4 cup equiv. per 1,000 kcal MAX (5)
all$ScWholeFruits = sapply(all$whole_fruits, function(x) interp(x, 0, 0, 0.4, 5))
#plot(all$whole_fruits, all$ScWholeFruits)

##Total veg: MIN (0) to ≥1.1 cup equiv. per 1,000 kcal MAX (5)
all$ScTotalVeg = sapply(all$total_veg, function(x) interp(x, 0, 0, 1.1, 5))
#plot(all$total_veg, all$ScTotalVeg)

##Greens beans: MIN (0) to ≥0.2 cup equiv. per 1,000 kcal MAX (5)
all$ScGreensBeans = sapply(all$greens_beans, function(x) interp(x, 0, 0, 0.2, 5))
#plot(all$greens_beans, all$ScGreensBeans)

##Whole grains: MIN (0) to ≥1.5 oz equiv. per 1,000 kcal MAX (10)
all$ScWholeGrains = sapply(all$whole_grains, function(x) interp(x, 0, 0, 1.5, 10))
#plot(all$whole_grains, all$ScWholeGrains)

##Dairy: MIN (0) to ≥1.3 cup equiv. per 1,000 kcal MAX (10)
all$ScDairy = sapply(all$dairy, function(x) interp(x, 0, 0, 1.3, 10))
#plot(all$dairy, all$ScDairy)

##Total protein: MIN (0) to ≥2.5 oz equiv. per 1,000 kcal MAX (5)
all$ScTotalProtein = sapply(all$total_protein, function(x) interp(x, 0, 0, 2.5, 5))
#plot(all$total_protein, all$ScTotalProtein)

##Sea food / plant protein: MIN (0) to ≥0.8 oz equiv. per 1,000 kcal MAX (5)
all$ScSeaPlantProtein = sapply(all$sea_plant_protein, function(x) interp(x, 0, 0, 0.8, 5))
#plot(all$sea_plant_protein, all$ScSeaPlantProtein)

##Fatty acids: ≥2.5 MAX (10) to ≤1.2 MIN (0)
all$ScFattyAcids = sapply(all$fatty_acids, function(x) interp(x, 1.2, 0, 2.5, 10))
#plot(all$fatty_acids, all$ScFattyAcids)

##Moderation
##Refined grains: ≤1.8 oz equiv. per 1,000 kcal MAX (10) to ≥4.3 oz equiv. per 1,000 kcal MIN (0)
all$ScRefinedGrains = sapply(all$refined_grains, function(x) interp(x, 1.8, 10, 4.3, 0))
#plot(all$refined_grains, all$ScRefinedGrains)

##Sodium: ≤1.1 gram per 1,000 kcal MAX (10) to ≥2.0 grams per 1,000 kcal MIN (0)
all$ScSodium = sapply(all$sodium, function(x) interp(x, 1.1, 10, 2.0, 0))
#plot(all$sodium, all$ScSodium)

##Added sugars: ≤6.5% of energy MAX (10) to ≥26% of energy MIN (0)
all$ScAddedSugars = sapply(all$added_sugars, function(x) interp(x, 0.065, 10, 0.26, 0))
#plot(all$added_sugars, all$ScAddedSugars)

##Saturated fats: ≤8% of energy MAX (10) to ≥16% of energy MIN (0)
all$ScSaturatedFats = sapply(all$saturated_fats, function(x) interp(x, 0.08, 10, 0.16, 0))
#plot(all$saturated_fats, all$ScSaturatedFats)

##Calculate HEI (and radar plot later)
all$HEI = apply(all[, names(all) %in% c("ScTotalFruits", "ScWholeFruits", "ScTotalVeg", "ScGreensBeans", "ScWholeGrains", "ScDairy", "ScTotalProtein", "ScSeaPlantProtein", "ScFattyAcids", "ScRefinedGrains", "ScSodium", "ScAddedSugars", "ScSaturatedFats")], 1, function(x) sum(x, na.rm=TRUE))
all2 = all[, names(all) %in% c("ID", "date", "ScTotalFruits", "ScWholeFruits", "ScTotalVeg", "ScGreensBeans", "ScWholeGrains", "ScDairy", "ScTotalProtein", "ScSeaPlantProtein", "ScFattyAcids", "ScRefinedGrains", "ScSodium", "ScAddedSugars", "ScSaturatedFats", "HEI")]
all2
}


##Read in data (reports 4 and 9, plus food names)
#r4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/Baseline Data/ChivesC1BL04.csv', header=TRUE)
#r9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/Baseline Data/ChivesC1BL09.csv', header=TRUE)
#foods = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Code/foods.csv", header=TRUE)
#ex = hei_ndsr(r4, r9)

