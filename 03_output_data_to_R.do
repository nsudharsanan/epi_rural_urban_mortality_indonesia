***************************************************
*Project: Rural-urban differences in mortality in Indonesia
*Purpose: Convert and output the cleaned data for R analyses
*Last modified: 10 September 2019 by NSud
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
*01. Pre cleaning
***************************************************

	*Load data
	use "$dir/build/output/cleaned_data.dta", clear

	*Drop ppl below 30
	drop if ageint < 30

	*Drop missing on baseline info that will not be imputed
	egen nmiss_dem = rowmiss(ageint female urban marit job schooling_abs died date_exit wealth_quintile wt)
	keep if nmiss_dem == 0

***************************************************
*02. Convert data to person-age format
***************************************************
	
	*Age at entrance
	g enter_age = floor(ageint)

	*Age at exit
	g exit_age = (date_exit - birth_date) / 365.25

	*Split into person-age observations using the st commands

		*Gen an id
		gen id = _n

		*stset
		stset exit_age, failure(died) enter(enter_age) id(id)

		*stsplit
		stsplit tsplit, every(1)

		*Exposure time
		g exposure = _t - _t0

	*Number observations per individual
	sort id _t0
	by id: g obs_num = _n

	*Make an age-group variable (using the time varying age)
	cap drop age_group
	egen age_group=cut(_t0), at(30(5)85)	
	replace age_group=85 if _t0>=85 & _t0!=.

***************************************************
*03. Export for R
***************************************************

	*Keep only needed vars for analysis
	keep id urban province female marit schooling_abs job ///
	ever_smoker bmi_w3 sysbp wealth_quintile wt _d exposure age_group obs_num wt
	order id obs_num age_group _d exposure female province marit ///
	schooling_abs job wealth_quintile urban ///
	ever_smoker bmi_w3 sysbp wt

	*Rename death and time-varying age
	rename _d died

	*Save as csv for R
	export delimited using "$dir/build/output/data_for_R.csv", replace
	
