***************************************************
*Rural-urban differences in mortality in Indonesia
*Purpose: Create a dataset with all the variables
*needed for the analyses.
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
*01. Merge IFLS 4 Data
***************************************************

	*ptrack in IFLS 4: interview status, sex, age, birthdate, sample weight
	use "$dir/raw_datasets/ifls_4/ptrack.dta"
	keep pidlink hhid07 pid07 ar01i_07 ar01a_07 sex age_07 bth_mnth bth_day bth_year pw07usxa pwt07xa member07
	tempfile ptrack
	save `ptrack', replace
	clear
	
	*Interview date
	use "$dir/raw_datasets/ifls_4/bk_cov.dta"
	keep hhid07 ivwday1 ivwmth1 ivwyr1
	tempfile ivwdate
	save `ivwdate', replace
	clear
	
	*Geographic variables
	use "$dir/raw_datasets/ifls_4/bk_sc.dta"
	keep hhid07 sc05 sc010707 sc020707 sc030707
	tempfile geo
	save `geo', replace
	clear

	*Additional birth date info
	use "$dir/raw_datasets/ifls_4/bk_ar1.dta"
	keep hhid07 pid07 pidlink ar08day ar08mth ar08yr ar13
	tempfile bdate 
	save `bdate', replace
	clear

	*Self-reported age from the adult sample
	use "$dir/raw_datasets/ifls_4/b3a_cov.dta"
	keep hhid07* pid07 pidlink age marstat
	tempfile srage
	save `srage', replace
	clear
	
	*Schooling from the adult sample
	use "$dir/raw_datasets/ifls_4/b3a_dl1.dta"
	keep hhid07 pid07 pidlink dl04 dl06 dl07
	tempfile schooling1
	save `schooling1', replace
	clear

	*Schooling from the HH cover book
	use "$dir/raw_datasets/ifls_4/bk_ar1.dta"
	keep hhid07 pid07 pidlink ar09 ar16 ar17
	tempfile schooling2
	save `schooling2', replace
	clear

	*Occupation and work data (part 1)
	use "$dir/raw_datasets/ifls_4/b3a_tk1.dta", clear
	keep hhid07 pid07 pidlink tk01
	tempfile workp1
	save `workp1', replace
	clear

	*Occupation and work data (part 2)
	use "$dir/raw_datasets/ifls_4/b3a_tk2.dta", clear
	keep hhid07 pid07 pidlink tk19ab
	tempfile workp2
	save `workp2', replace
	clear

	*Anthropometry part 1
	use "$dir/raw_datasets/ifls_4/bus1_1.dta", clear
	keep hhid07 pid07 pidlink  us06 us07a1 us07a2 us07b1 us07b2
	tempfile anthrop1
	save `anthrop1', replace
	clear

	*Anthropometry part 2
	use "$dir/raw_datasets/ifls_4/bus1_2.dta", clear
	keep hhid07 pid07 pidlink us04 us07c1 us07c2
	tempfile anthrop2
	save `anthrop2', replace
	clear

	*Tobacco use
	use "$dir/raw_datasets/ifls_4/b3b_km.dta", clear
	keep hhid07 pid07 pidlink km01* km04 km05aa km08 km10
	tempfile tobacco
	save `tobacco', replace
	clear

	*Merge files
	use `ptrack'
	count
	merge m:1 hhid07 using `ivwdate'
	drop if _merge==2
	drop _merge
	merge m:1 hhid07 using `geo'
	drop if _merge==2
	drop _merge
	merge 1:1 hhid07 pid07 pidlink using `bdate'
	drop if _merge==2
	drop _merge
	merge 1:1 hhid07 pid07 pidlink using `schooling1'
	drop if _merge==2
	drop _merge
	merge 1:1 hhid07 pid07 pidlink using `schooling2'
	drop if _merge==2
	drop _merge
	merge m:m hhid07 using $dir/raw_datasets/ifls_4/wealth_score.dta
	drop _merge
	merge 1:1 hhid07 pid07 pidlink using `srage'
	drop if _merge==2
	drop _merge	
	merge 1:1 hhid07 pid07 pidlink using `workp1'
	drop if _merge==2
	drop _merge	
	merge 1:1 hhid07 pid07 pidlink using `workp2'
	drop if _merge==2
	drop _merge
	merge 1:1 hhid07 pid07 pidlink using `anthrop1'
	drop if _merge==2
	drop _merge
	merge 1:1 hhid07 pid07 pidlink using `anthrop2'
	drop if _merge==2
	drop _merge
	merge 1:1 hhid07 pid07 pidlink using `tobacco'
	drop if _merge==2
	drop _merge	
	count
	isid pidlink
	tempfile ifls4
	save `ifls4', replace
	clear

***************************************************
*02. Merge IFLS 5 Data
***************************************************

	*ptrack
	use "$dir/raw_datasets/ifls_5/ptrack.dta", clear

		* drop duplicates
		bysort pidlink: g id=[_n]
		drop if id==2
		drop id

	keep pidlink hhid14 hhid07 pid07 bg_dob bth_month bth_day bth_year ar01a*
	rename bth* bth*14
	rename ar01a* ar01a*ifls5
	tempfile ifls5
	save `ifls5', replace
	clear

	*Interview date: earliest
	use "$dir/raw_datasets/ifls_5/bk_time.dta", clear
	g interview_date14=mdy(ivwmthbk,ivwdaybk,ivwyrbk)
	format interview_date14 %td
	sort hhid14 interview_date14
		
	*Keep only first date
	by hhid14: g id=[_n]
	keep if id==1
	keep hhid14 interview_date14
	tempfile ivw15
	save `ivw15', replace
	clear

	*Mortality
	use "$dir/raw_datasets/ifls_5/fe_cov.dta", clear
	merge 1:1 pidlink using "$dir/raw_datasets/ifls_5/fe_1.dta"
	drop _merge
	keep pidlink pidfe ef01 ef02_x ef02 ef03_x ef03_mth ef03_yr
	tempfile mort
	save `mort', replace
	clear

	*Merge IFLS 5 components
	use `ifls5'
	merge 1:1 pidlink using `mort'
	drop _merge
	merge m:1 hhid14 using `ivw15'
	drop _merge

	*Drop the 1 obs without a pidlink
	drop if pidlink == ""
	save `ifls5', replace
	clear

***************************************************
*03. Prepare IFLS 3 Data
***************************************************

	*Rural or urban in 2000
	use "$dir/raw_datasets/ifls_3/bk_sc.dta", clear
	keep hhid00 sc05
	rename sc05 sc05_w3
	tempfile geo_w3
	save `geo_w3', replace
	clear

	*Systolic BP from IFLS 3
	use "$dir/raw_datasets/ifls_3/bus1_1", clear
	keep hhid00 pid00 us07a
	rename us07a us07a_w3
	tempfile bp_w3
	save `bp_w3', replace
	clear

	*Anthro from IFLS 3
	use "$dir/raw_datasets/ifls_3/bus2_1.dta", clear
	keep hhid00 pid00 us04 us06
	rename us04 us04_w3
	rename us06 us06_w3
	tempfile anthro_w3
	save `anthro_w3', replace
	clear

	*Tobacco use in IFLS 3 (for now just ever and cigs/day)
	use "$dir/raw_datasets/ifls_3/b3b_km.dta", clear
	keep hhid00 pid00 km01a km04 km08
	foreach var of varlist km01a km04 km08 {
		rename `var' `var'_w3
	}
	tempfile tobacco_w3
	save `tobacco_w3', replace
	clear

	*Ptrack to match hhid00 and pid00 to pidlink
	use "$dir/raw_datasets/ifls_3/ptrack.dta", clear
	keep pidlink hhid00 pid00 
	drop if pid00 == .
	tempfile pidlink_w3
	save `pidlink_w3', replace
	clear

	*Merge ifls 3 info
	use `geo_w3', clear
	merge 1:m hhid00 using `bp_w3', nogen
	merge 1:1 hhid00 pid00 using `anthro_w3', nogen
	merge 1:1 hhid00 pid00 using `tobacco_w3'
	drop if _merge == 2
	drop _merge
	merge 1:1 hhid00 pid00 using `pidlink_w3' // there are obs that are not matched but they are the ones that don't have values for any of the anthropometrics
	keep if _merge == 3
	drop _merge hhid00 pid00
	tempfile ifls3
	save `ifls3', replace
	clear

***************************************************
*04. Merge IFLS 3, 4, 5
***************************************************

	*Start with IFLS 4
	use `ifls4', clear

	*Merge IFLS 5 follow up
	merge 1:1 pidlink using `ifls5'
	tab ar01a_14ifls5 if _merge==2, m
	* 99% of these are new HH members in 2014
	keep if _merge==3
	drop _merge

	*Merge IFLS 3
	merge 1:1 pidlink using `ifls3'
	drop _merge

***************************************************
*05. Subset and save
***************************************************

	*Follow-up mortality information was only asked for
	*target respondents -> restrict down
	tab ar01i_07, m
	keep if ar01i_07 == 1
	
	*Drop dead in 2007, not in HH, and duplicates
	tab ar01a_07, m
	drop if ar01a_07==0
	drop if ar01a_07==3
	drop if ar01a_07==6

	*Save
	save "$dir/build/temp/merged_data_uncleaned.dta", replace
