##Preliminary AHEI function
##Read in data (reports 4 and 9, plus food names)
foods = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Code/foods.csv", header=TRUE)

##Read in demo data
d1 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/Data/DATA_2018-03-22.csv", header=TRUE)
d2 = d1[, names(d1) %in% c("chives_id", "sex")] #sex: 1=male, 2=female, 99=other
d2$sex2 = NA
d2$sex2[d2$sex==2] = "a.Female"
d2$sex2[d2$sex==1] = "b.Male"
d2$sex2[d2$sex==99] = "c.Other"
table(d2$sex2, d2$sex, exclude=NULL)

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

#ch = double()
#for (i in 10:15) {
#    ch = c(ch, choose(42, i))
#}
#sum(ch)


##AHEI calculation
ahei_ndsr = function(r4, r9) {
##Define categories of foods
total_veg = c("darkgreen_veg", "deepyellow_veg", "tomato", "white_potatoes", "fried_potatoes", "starchy_veg", "other_veg", "fried_veg", "veg_juice")

total_fruits = c("citrus_fruit", "fruit_nocitrus", "avocado", "fried_fruit", "fruit_savory_snack")

nuts_legumes = c("legumes", "nuts_seeds", "nut_seed_butters")

red_processed_meat = c("organ_meat", "game", "beef", "lean_beef", "veal", "lean_veal", "lamb", "lean_lamb", "fresh_pork", "lean_fresh_pork", "cured_pork", "lean_cured_pork")

##Long-chain (n-3) fats (EPA + DHA) (mg/d) - one serving of fish? 250mg/d is about 8 oz of fish (0-8 score)
long_chain_oz = c("fresh_fish", "lean_fish")
#long_chain_g = c("PUFA.20.5..eicosapentaenoic.acid..EPA....g.", "PUFA.22.6..docosahexaenoic.acid..DHA....g.")

#PUFA (% of energy)
#pufa = "X..Calories.from.PUFA"

##Moderation
ssb = c('sweetened_soda', 'artificially_sweetened_soda', 'sweetened_fruit_drink', 'artificially_sweetened_fruit_drink', 'sweetened_tea', 'artificially_sweetened_tea', 'sweetened_coffee', 'artificially_sweetened_coffee', 'sweetened_coffee_substitute', 'artificially_sweetened_coffee_substitute', 'sweetened_water', 'artificially_sweetened_water', 'nondairy_sweetened_meal_replacement', 'nondairy_artificially_sweetened_meal_replacement')

alcohol = c("beer", "liqueur", "distilled_liquor", "wine")

#sodium = "Sodium..mg."

#trans_fat_g = "Total.Trans.Fatty.Acids..TRANS...g."
#calories_g = "Total.Grams"

##Look at reports 4 and 9 and pull out necessary components
r4a = r4[, names(r4) %in% c("Participant.ID", "Date.of.Intake", "Total.Fat..g.", "Total.Carbohydrate..g.", "Total.Protein..g.", "Sodium..mg.", "X..Calories.from.PUFA", "Total.Trans.Fatty.Acids..TRANS...g.", "PUFA.20.5..eicosapentaenoic.acid..EPA....g.", "PUFA.22.6..docosahexaenoic.acid..DHA....g.", "Whole.Grains..ounce.equivalents.")]
names(r4a) = c("ID", "date", "fat_g", "carb_g", "protein_g", "sodium_mg", "pufa_epa_g", "pufa_dha_g", "pufa_pct", "trans_g", "whole_grains_oz")
r4a$total_g = r4a$fat_g + r4a$carb_g + r4a$protein_g
r4a$trans_pct = 100 * (r4a$trans_g / r4a$total_g)
r4a$whole_grains_g = r4a$whole_grains_oz*28.34952

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

##Aggregate within categories
all$total_veg = apply(all[, names(all) %in% total_veg], 1, function(x) sum(x, na.rm=TRUE))

all$total_fruits = apply(all[, names(all) %in% total_fruits], 1, function(x) sum(x, na.rm=TRUE))

#summary(all$whole_grains_g)

all$ssb = apply(all[, names(all) %in% ssb], 1, function(x) sum(x, na.rm=TRUE))

all$nuts_legumes = apply(all[, names(all) %in% nuts_legumes], 1, function(x) sum(x, na.rm=TRUE))

all$red_processed_meat = apply(all[, names(all) %in% red_processed_meat], 1, function(x) sum(x, na.rm=TRUE))

#summary(all$trans_pct)

#all$long_chain_oz = apply(all[, names(all) %in% long_chain_oz], 1, function(x) sum(x, na.rm=TRUE))
#summary(all$long_chain_oz)
#all$long_chain_mg1 = all$long_chain_oz * 28.34952
#summary(all$long_chain_mg1)
all$long_chain_g = all$pufa_epa_g + all$pufa_dha_g
all$long_chain_mg = all$long_chain_g * 1000
#summary(all$long_chain_mg)

#summary(all$pufa_pct)

#summary(all$sodium_mg)

all$alcohol = apply(all[, names(all) %in% alcohol], 1, function(x) sum(x, na.rm=TRUE))
#summary(all$alcohol)


##Add component scores
##Need sex/gender
names(d2)[1] = "ID"
all2 = merge(all, d2, by="ID")

##Vegetables (servings/d)
all2$ScVegetables = sapply(all2$total_veg, function(x) interp(x, 0, 0, 5, 10))
#plot(all2$total_veg, all2$ScVegetables)

##Fruits (servings/d)
all2$ScFruits = sapply(all2$total_fruits, function(x) interp(x, 0, 0, 4, 10))
#plot(all2$total_fruits, all2$ScFruits)

##Whole Grains (g/d) - women/men
all2$ScWholeGrains[all2$sex2=="a.Female"] = sapply(all2$whole_grains_g[all2$sex2=="a.Female"], function(x) interp(x, 0, 0, 75, 10))
all2$ScWholeGrains[all2$sex2=="b.Male"] = sapply(all2$whole_grains_g[all2$sex2=="b.Male"], function(x) interp(x, 0, 0, 90, 10))
all2$ScWholeGrains[all2$sex2=="c.Other"] = sapply(all2$whole_grains_g[all2$sex2=="c.Other"], function(x) interp(x, 0, 0, 82.5, 10))
#plot(all2$whole_grains_g[all2$sex2=="a.Female"], all2$ScWholeGrains[all2$sex2=="a.Female"])
#plot(all2$whole_grains_g[all2$sex2=="b.Male"], all2$ScWholeGrains[all2$sex2=="b.Male"])
all2$ScWholeGrains = as.numeric(all2$ScWholeGrains)

##Sugar Sweetened Beverages (servings/d)
all2$ScSSB = sapply(all2$ssb, function(x) interp(x, 0, 10, 1, 0))
#plot(all2$ssb, all2$ScSSB)

##Nuts/legumes (servings/d)
all2$ScNutsLegumes = sapply(all2$nuts_legumes, function(x) interp(x, 0, 0, 1, 10))
#plot(all2$nuts_legumes, all2$ScNutsLegumes)

##Red/processed meat (servings/d)
all2$ScRedProcessedMeat = sapply(all2$red_processed_meat, function(x) interp(x, 0, 10, 1.5, 0))
#plot(all2$red_processed_meat, all2$ScRedProcessedMeat)

##Trans fat (%energy)
all2$ScTransFat = sapply(all2$trans_pct, function(x) interp(x, 0.5, 10, 4, 0))
#plot(all2$trans_pct, all2$ScTransFat)

##Long chain fats (mg/d)
all2$ScLongChainFats = sapply(all2$long_chain_mg, function(x) interp(x, 0, 0, 250, 10))
#plot(all2$long_chain_mg, all2$ScLongChainFats)

##PUFA (%energy)
all2$ScPUFA = sapply(all2$pufa_pct, function(x) interp(x, 2, 0, 10, 10))
#plot(all2$pufa_pct, all2$ScPUFA)

##Sodium (mg/d) - women/men
all2$ScSodium[all2$sex2=="a.Female"] = sapply(all2$sodium_mg[all2$sex2=="a.Female"], function(x) interp(x, 1112, 10, 3337, 0))
all2$ScSodium[all2$sex2=="b.Male"] = sapply(all2$sodium_mg[all2$sex2=="b.Male"], function(x) interp(x, 1612, 10, 5271, 0))
all2$ScSodium[all2$sex2=="c.Other"] = sapply(all2$sodium_mg[all2$sex2=="c.Other"], function(x) interp(x, 1362, 10, 4304, 0))
#plot(all2$sodium_mg, all2$ScSodium)
all2$ScSodium = as.numeric(all2$ScSodium)

##Alcohol (drinks/d) - women/men
all2$ScAlcohol[all2$sex2=="a.Female"] = sapply(all2$alcohol[all2$sex2=="a.Female"], function(x) interp(x, 1.5, 10, 2.5, 0))
all2$ScAlcohol[all2$sex2=="a.Female" & all2$alcohol>=0.5 & all2$alcohol<=1.5] = 10
all2$ScAlcohol[all2$sex2=="b.Male"] = sapply(all2$alcohol[all2$sex2=="b.Male"], function(x) interp(x, 2, 10, 3.5, 0))
all2$ScAlcohol[all2$sex2=="b.Male" & all2$alcohol>=0.5 & all2$alcohol<=2] = 10
all2$ScAlcohol[all2$sex2=="c.Other"] = sapply(all2$alcohol[all2$sex2=="c.Other"], function(x) interp(x, 1.75, 10, 3, 0))
all2$ScAlcohol[all2$sex2=="c.Other" & all2$alcohol>=0.5 & all2$alcohol<=1.75] = 10
all2$ScAlcohol[all2$alcohol<0.5] = 2.5
#plot(all2$alcohol, all2$ScAlcohol)
all2$ScAlcohol = as.numeric(all2$ScAlcohol)

##Calculate AHEI (and radar plot later)
all2$AHEI = apply(all2[, names(all2) %in% c("ScVegetables", "ScFruits", "ScWholeGrains", "ScSSB", "ScNutsLegumes", "ScRedProcessedMeat", "ScTransFat", "ScLongChainFats", "ScPUFA", "ScSodium", "ScAlcohol")], 1, function(x) sum(as.numeric(x), na.rm=TRUE))

all3 = all2[, names(all2) %in% c("ID", "date", "ScVegetables", "ScFruits", "ScWholeGrains", "ScSSB", "ScNutsLegumes", "ScRedProcessedMeat", "ScTransFat", "ScLongChainFats", "ScPUFA", "ScSodium", "ScAlcohol", "AHEI")]
#apply(all3, 2, function(x) summary(as.numeric(x)))

all3
}


