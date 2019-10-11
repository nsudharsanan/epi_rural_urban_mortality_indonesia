***************************************************
*Project: Rural-urban differences in mortality in Indonesia
*Purpose: Describe missingness
*Last modified: 9 October 2019 by NSud
***************************************************

***************************************************
*00. Preamble
***************************************************
	
	set more off
	clear all

	*Set user and the directory 
	local user "user1" /*"user2"*/
		
		if "`user'"=="user1" {
			global dir /*Set user path here*/
		}
		if "`user'"=="user2" {
			global dir /*Set user path here*/
		}

***************************************************
*01. Precleaning
***************************************************
	
	*Load data
	use "$dir/build/output/cleaned_data.dta", clear

	*Make an age group var
	cap drop age_group
	egen age_group = cut(ageint), at(30(5)85)
	replace age_group = 85 if ageint >= 85

	*Keep only ppl within age range
	keep if age_group != .

***************************************************
*02. Missingness
***************************************************

	*How many age-eligible ppl? - 19,243

	*How many missing info on sex, death, date of censoring? - 0
 
	*How many missing info on sysbp, smoking at present - 2053

	*How many people missing prior BMI - 2869

	*Finally control vars - 1

	*End: 14,329 - 74%
	
