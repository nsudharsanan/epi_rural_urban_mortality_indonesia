***************************************************
*Project: Rural-urban differences in mortality in Indonesia
*Purpose: Table 1 Descriptive Table
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
*01. Set up the data
***************************************************

	*Load data
	use "$dir/build/output/cleaned_data.dta", clear

	*Drop ppl below 30
	drop if ageint < 30

	*Drop missing on baseline info
	egen nmiss_dem = rowmiss(ageint female urban marit job schooling_abs died date_exit wealth_quintile wt)
	keep if nmiss_dem == 0
	
***************************************************
*02. Make the table
***************************************************

	*With the exception of age, BMI, and systolic BP
	*all the descriptive variables are categorical.
	*Tabout the categorical and then add the
	*three continuous variables.

	*Categorical
	tabout urban province marit schooling_abs job ever_smoker female ///
	using "$dir/analysis/temp/table_1_cat.xls", ///
	replace cells(freq col) ptotal(none)

	*Continuous
	tabstat ageint bmi_w3 sysbp, stats(mean sd) ///
	column(statistics) by(female) save

		*Save the matrix
		matrix continuous = r(Stat1) \ r(Stat2)
		putexcel set "$dir/analysis/temp/table_1_cont.xlsx", replace
		putexcel A1=matrix(continuous)
