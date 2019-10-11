***************************************************
*Project: Rural-urban differences in mortality in Indonesia
*Purpose: Graph risk factor distributions by urban-rural
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
*02. Estimate weighted age-patterns
***************************************************

	*Regression types
	local regsysbp = "regress"
	local regbmi_w3 = "regress"
	local regever_smoker = "logit"

	*Estimate weighted age-patterns
	* Loop over outcomes, sex, and urban rural
	forval u=0(1)1 {

		forval f = 0(1)1 {

			foreach outcome in sysbp bmi_w3 ever_smoker {

				`reg`outcome'' `outcome' i.age_group [pweight = wt] ///
				if urban == `u' & female == `f'
				margins, at(age_group=(30(5)80)) saving(urban`u'_f`f'_`outcome', replace)

			}
		}
	}

	*Graph
	local titlesysbp = "systolic BP (mmHg)"
	local titlebmi_w3 = "BMI in 2000"
	local titleever_smoker = "ever regular smoker"

	*Sex titles
	local sex0 = "Men"
	local sex1 = "Women"

	*Loop over outcomes and sex
	forval f = 0(1)1 {

		foreach outcome in sysbp bmi_w3 ever_smoker {
		
			combomarginsplot urban0_f`f'_`outcome' urban1_f`f'_`outcome', ///
			graphregion(color(white)) ///
			legend(order(3 "Rural" 4 "Urban") region(lcolor(white))) ///
			xtitle("Age group") xlabel(30(10)80) ///
			ytitle("Mean `title`outcome''") ylabel(,nogrid) ///
			title("`sex`f''", size(medium) color(black)) ///
			name(age_f`f'_`outcome', replace)


		}

	}

***************************************************
*03. Make Figure 4
***************************************************

	*Combine graphs
	grc1leg age_f0_sysbp age_f0_bmi_w3 age_f0_ever_smoker ///
	age_f1_sysbp age_f1_bmi_w3 age_f1_ever_smoker, ///
	rows(2) graphregion(color(white)) name(graph, replace)
		
	graph export "$dir/analysis/output/age_patterns.pdf", as(pdf) replace
