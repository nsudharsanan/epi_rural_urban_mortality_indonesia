***************************************************
*Rural-urban differences in mortality in Indonesia
*Purpose: Clean and prepare the dataset for analyses
*Last modified: 10 October 2019 by NSud
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

	*Load merged dataset
	use "$dir/build/temp/merged_data_uncleaned.dta", clear

***************************************************
*01. Set up key dates for mortality analyses
***************************************************

	*Date of birth
		*Fill in birth dates for 7 individuals who are missing using '07 data but have birth dates based on '14 data
		*3 additional cases have birth date information based on ar08 and us02 variables from 2007
		g birth_date = mdy(bth_mnth, bth_day, bth_year)
		count if birth_date == .
		count if birth_date == . & bg_dob != .
		replace birth_date = bg_dob if birth_date == .
		replace birth_date = mdy(ar08mth,ar08day,ar08yr) if birth_date == .
		format birth_date %td
		count if birth_date == . // 1 case

	*Date of interview in IFLS 4
		*2008 was a leap year, all Feb. 29 dates were in 2008 - OK
		*Replace Feb. 30 dates with Feb. 29
		tab ivwyr1 if ivwmth1 == 2 & ivwday1 == 29, m
		tab ivwyr1 if ivwmth1 == 2 & ivwday1 == 30, m // 4 cases
		replace ivwday1 = 29 if ivwmth1 == 2 & ivwday1 == 30
		g interview_date = mdy(ivwmth1,ivwday1,2000+ivwyr1)
		format interview_date %td	
		count if interview_date == . // no missing	

	*Date of interview in IFLS 5 (already made in the merging)
		*Some cases are missing on IFLS interview date --> use median IFLS 5 interview date = 16jan2015
		count if interview_date14 == . // 2302. Mention this missingness in paper
		g interview_date14_flag = (interview_date14 == . )
		replace interview_date14 = mdy(1,16,2015) if interview_date14 == .
		
	*Date of death

		*Make an indicator for who died
		g died=(pidfe!=.)

		*Death year
			*There are some implausble death years but without
			*death date its hard to know if they happened
			*in the same year of interview but at a later
			*month. Deal with them after making full dates
			g death_year = ef03_yr
			replace death_year = . if ef03_yr == 9998

		*Death month
		g death_month = ef03_mth
		replace death_month = . if ef03_mth == 98

		*Date date (set to midpoint of month)
		g death_date = mdy(death_month,15,death_year)
		format death_date %td

	*Fix issues with date of death

		*Death date precedes interview date -> set to midpoint between IFLS interviews (next step)
		count if death_date < interview_date // 36 ppl
		replace death_year = . if death_date < interview_date
		replace death_month = . if death_date < interview_date
		replace death_date = . if death_date < interview_date

		*Death year preceeds inteview year
		replace death_year = . if death_year < ivwyr1 + 2000 // Another 36 ppl

		*Case 1: Missing on year and month of death
			*Set to midpoint of IFLS 4 and IFLS 5 interview dates if available
			*Some cases are missing on IFLS interview date --> use median IFLS 5 interview date = 16jan2015
			g midpoint = interview_date + (interview_date14 - interview_date)/2
			replace death_date = midpoint if death_month == . & death_year == . & died == 1

		*Case 2: Missing on month but not year of death
			*If year of death is not same as IFLS 4 interview year, set to 01jul of year of death.
			*If year of death is same as IFLS 4 interview year, set to midpoint between interview date and end of the year.
			g intyear = ivwyr1+2000
			replace death_date = mdy(7,1,death_year) if death_month == . & death_year != . & died == 1 & death_year != intyear
			replace death_date = interview_date+(mdy(12,31,death_year) - interview_date)/2 if death_date == . & death_month == . & death_year != . & died == 1 & death_year == intyear

	*Exit date
	g date_exit = death_date if died == 1
	replace date_exit = interview_date14 if died == 0
	format date_exit %td

***************************************************
*02. Clean main variables
***************************************************

	*Age at first interview
	g ageint = (interview_date - birth_date) / 365.25

	*Urban
	g urban = (sc05 == 1)

		*Urban in 2000
		g urban_w3 = (sc05_w3 == 1)

	*Province
	g province = sc010707
	
		*Recode the people who are from
		*the non-main provinces into an other gruop (very few observations)
		replace province = 999 if province == 14 | province == 19 | ///
		province == 21 | province == 62 | province == 64 | ///
		province == 71 | province == 76

	*Female
	g female = (sex == 3)

	*Marital status
		*privilege Book 3, then use HH roster
		*codes are the same
		g marital = .
		replace marital = marstat
		replace marital = ar13 if marital == .
		replace marital = . if marital == 9
		g marit = marital
		replace marit = 3 if inlist(marital,4,5)
		tab marital marit, m

	*Schooling in three categories (none, some primary, more than primary)
		*privilege Book 3, then use HH roster
		*codes are the same except that the dl06 var does not contain
		*the no education category. This is contained in dl04.
		cap drop schooling_abs
	
			*Book 3 info
			g schooling_abs = 0 if dl04 == 3
			replace schooling_abs = 1 if inlist(dl06,2,3,4,5,6,11,12,13,14,15,72,73,74,90)
			replace schooling_abs = 2 if inlist(dl06,60,61,62,63)

			*Cover book data
			replace schooling_abs = 0 if ar16 == 1 & schooling_abs == .
			replace schooling_abs = 1 if inlist(ar16,2,3,4,5,6,11,12,13,14,15,72,73,74,90) & schooling_abs == .
			replace schooling_abs = 2 if inlist(ar16,60,61,62,63) & schooling_abs == .

	*Job
	destring tk19ab, replace
	cap drop job
	g job = 7 // Other or missing
	replace job = 6 if tk01 == 2 | tk01 == 3 | tk01 == 6 | tk01 == 7 | tk01 == 8 | tk01 == 9 | tk01 == 10 | tk01 == 11 | tk01 == 95 | tk01 == 99 // Not working
	replace job = 1 if tk01 == 4 // Housewife
	replace job = 2 if tk01 == 5 // Retired
	replace job = 3 if tk19ab == 1 // Agriculture
	replace job = 4 if tk19ab == 3 // Manufacturing  
	replace job = 0 if tk19ab == 6 // Wholesale/retail - reference
	replace job = 5 if tk19ab == 9 // Service

	*Smoking variables

		*Ever regular smoker
		g ever_smoker = (km01a == 1) if km01a < .

		
		*Ever regular smoker in w3
		g ever_smoker_w3 = (km01a_w3 == 1) if km01a_w3 < .

	*BMI from wave 3

		*Clean weight: set missing codes to missing
		replace us06 = . if us06 > 150

		*Height looks clean

		*Make BMI
		cap drop bmi_w3
		g bmi_w3 = us06_w3 / ((us04_w3/100)^2)

			*Set extreme BMIs to missing
			replace bmi_w3 = . if bmi_w3 > 50
			replace bmi_w3 = . if bmi_w3 < 15

	*Blood pressure

		*In this wave 3 measurements were taken - first set missing to .
		foreach var in us07a1 us07a2 us07b1 us07b2 us07c1 us07c2 {
			replace `var' = . if `var' == 999
		}

		*Average the second two measurements
		cap drop sysbp
		egen sysbp = rowmean(us07b1 us07c1)
		cap drop diabp
		egen diabp = rowmean(us07b2 us07c2)

		*Systolic Blood pressure in w3 (there was just one measurement)
		g sysbp_w3 = us07a_w3

	*Sample weight
	rename pwt07xa wt

***************************************************
*03. Subset and save
***************************************************

	*Keep only needed vars
	keep hhid07 pid07 pidlink interview_date interview_date14* date_exit died ///
	birth_date death_date ageint urban* province female marit ///
	schooling_abs job ever_smoker* smoker bmi* sysbp* diabp ///
	wealth_quintile wt ar01a_07ifls5 ar01a_14ifls5 
	order hhid07 pid07 pidlink interview_date interview_date14* date_exit died ///
	birth_date death_date ageint urban* province female marit ///
	schooling_abs job ever_smoker* smoker bmi* sysbp* diabp ///
	wealth_quintile wt ar01a_07ifls5 ar01a_14ifls5
	
	*Label unlabeled vars
	label var birth_date "Birth date"
	label var interview_date "Interview date, IFLS 4"
	label var interview_date14 "Interview date, IFLS 5"
	label var interview_date14_flag "Missing Interview date, IFLS 5"
	label var died "Died"
	label var death_date "Date of death"
	label var date_exit "Date of exit (dead or censored)"
	label var ageint "Age at interview, years"
	label var urban "Urban"
	label var urban_w3 "Urban in 2000"
	label var province "Province"
	label var female "Female"
	label var marit "Marital status (3-cat)"
	label var schooling_abs "Schooling (3-cat)"
	label var job "Occupation"
	label var ever_smoker "Ever smoked cigarettes"
	label var ever_smoker_w3 "Ever smoked cigarettes in IFLS 3"
	label var bmi_w3 "BMI from IFLS 3"
	label var sysbp "Systolic BP"
	label var sysbp_w3 "Systolic BP from IFLS 3"
	label var diabp "Diastolic BP"
	label var wealth_quintile "Asset-based wealth quintile"
	
	*Save
	save "$dir/build/output/cleaned_data.dta", replace
