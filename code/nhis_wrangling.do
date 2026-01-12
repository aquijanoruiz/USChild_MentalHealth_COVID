clear all
capture log close

* Define file paths
global myProject    = "/Users/elocarinista/Library/CloudStorage/GoogleDrive-quija026@umn.edu/My Drive/UMN Drive/RA Peter Huckfeldt/USChild_MentalHealth_COVID"
global myDoFiles 	= "${myProject}/code"
global myLogs 		= "${myProject}/logs"
global myNHISRaw    = "${myProject}/rawdata/nhis"
global myOutputs 	= "${myProject}/outputs"
log using "${myLogs}/nhis_wrangling.log", replace text

*===============================================
* Load NHIS child files (2019–2023)
*===============================================

use "${myNHISRaw}/nhis_child19_raw.dta"
quietly append using "${myNHISRaw}/nhis_child20_raw.dta"
quietly append using "${myNHISRaw}/nhis_child21_raw.dta"
quietly append using "${myNHISRaw}/nhis_child22_raw.dta"
quietly append using "${myNHISRaw}/nhis_child23_raw.dta"

*===============================================
* SES variables
*===============================================

* Sex
tab sex_c srvy_yr, missing
tab sex_c srvy_yr, missing nolabel
gen sex = .
replace sex = 1   if sex_c == 1
replace sex = 2   if sex_c == 2
label define sex_lbl 1 "Male" 2 "Female"
label values sex sex_lbl
tab sex srvy_yr, missing

* Race & ethnicity
tab hispallp_c srvy_yr, missing
tab hispallp_c srvy_yr, missing nolabel
gen race = .
replace race = 1   if hispallp_c == 2
replace race = 2   if hispallp_c == 3
replace race = 3   if hispallp_c == 4
replace race = 4   if hispallp_c == 5
replace race = 5   if inlist(hispallp_c, 6, 7)
replace race = 6   if hispallp_c == 1
label define race_lbl 1 "White NH" 2 "Black NH" 3 "Asian NH" 4 "AIAN NH" 5 "Other NH" 6 "Hispanic"
label values race race_lbl
tab race srvy_yr, missing

* Number of parents in the household
tab pcntparnts_c srvy_yr, missing
tab pcntparnts_c srvy_yr, missing nolabel
gen cntparnts = .
replace cntparnts = 1 if pcntparnts_c == 0 | pcntparnts_c == 1 // 0 parents has very few observations
replace cntparnts = 2 if pcntparnts_c == 2
label define cntparnts_lbl 1 "1 parent or none" 2 "2+ parents"
label values cntparnts cntparnts_lbl
tab cntparnts srvy_yr, missing

* Highest level of education of all parents
tab maxpareduc_c srvy_yr, missing
tab maxpareduc_c srvy_yr, missing nolabel
gen lvledu = .
replace lvledu = 1   if inlist(maxpareduc_c, 1, 2) | inlist(maxparedup_c, 1, 2)
replace lvledu = 2   if inlist(maxpareduc_c, 3, 4) | inlist(maxparedup_c, 3, 4)
replace lvledu = 3   if inlist(maxpareduc_c, 5, 6) | inlist(maxparedup_c, 5, 6)
replace lvledu = 4   if inrange(maxpareduc_c, 7, 11) | inrange(maxparedup_c, 7, 10)
label define lvledu_lbl 1 "<HS" 2 "HS graduate" 3 "Some college/Assoc" 4 "Bachelor's+"
label values lvledu lvledu_lbl
tab lvledu srvy_yr, missing

* Ratio of family income to poverty threshold
tab ratcat_c srvy_yr, missing
tab ratcat_c srvy_yr, missing nolabel
gen income = .
replace income = 1 if inrange(ratcat_c, 1, 3)
replace income = 2 if inrange(ratcat_c, 4, 7)
replace income = 3 if inlist(ratcat_c, 8, 9)
replace income = 4 if inlist(ratcat_c, 10, 11)
replace income = 5 if inrange(ratcat_c, 12, 14)
label define income_lbl 1 "less than 100% FPL" 2 "100% to 199% FPL" 3 "200% to 299% FPL" 4 "300% to 399% FPL" 5 "400% FPL or greater"
label values income income_lbl
tab income srvy_yr, missing

*===============================================
* Cohort variables
*===============================================

* Year or birth: survey year - age
gen yob = srvy_yr - agep_c
tab agep_c srvy_yr, missing
* What cohorts are part of each survey
tab yob srvy_yr, missing

* Birth cohort
gen cohort = .
replace cohort = 1   if inrange(yob, 2012, 2014)
replace cohort = 2   if inrange(yob, 2009, 2011)
replace cohort = 3   if inrange(yob, 2006, 2008)
label define cohort_lbl 1 "5–7 in 2019 (2012–2014)" 2 "8–10 in 2019 (2009–2011)" ///
3 "11–13 in 2019 (2006–2008)"
label values cohort cohort_lbl
tab cohort srvy_yr, missing

* Combine cohort and sex variables
gen cohort_sex = .
replace cohort_sex = 1 if cohort == 1 & sex == 1
replace cohort_sex = 2 if cohort == 1 & sex == 2
replace cohort_sex = 3 if cohort == 2 & sex == 1
replace cohort_sex = 4 if cohort == 2 & sex == 2
replace cohort_sex = 5 if cohort == 3 & sex == 1
replace cohort_sex = 6 if cohort == 3 & sex == 2
label define cohort_sex_lbl 1 "5-7 in 2019 (2012-2014) male" 2 "5-7 in 2019 (2012-2014) female" ///
3 "8-10 in 2019 (2009-2011) male" 4 "8-10 in 2019 (2009-2011) female" ///
5 "11-13 in 2019 (2006-2008) male" 6 "11-13 in 2019 (2006-2008) female"
label values cohort_sex cohort_sex_lbl
tab cohort_sex srvy_yr, missing

*===============================================
* Outcome variables (sample children 5-17)
*===============================================

* Depression
* Reported feeling depressed daily, weekly, monthly, or a few times a year
tab depfreq_c srvy_yr, missing
tab depfreq_c srvy_yr, missing nolabel
gen dep = .
replace dep = 1 if depfreq_c == 1 | depfreq_c == 2 | depfreq_c == 3 | depfreq_c == 4
replace dep = 0 if depfreq_c == 5
tab dep srvy_yr, missing

* Anxiety
* Reported feeling anxious daily, weekly, monthly, or a few times a year
tab anxfreq_c srvy_yr, missing
tab anxfreq_c srvy_yr, missing nolabel
gen anx = .
replace anx = 1 if anxfreq_c == 1 | anxfreq_c == 2 | anxfreq_c == 3 | anxfreq_c == 4
replace anx = 0 if anxfreq_c == 5
tab anx srvy_yr, missing

*===============================================
* Export data
*===============================================

* Attach variable labels
do "${myDoFiles}/nhis_labels.do"

keep srvy_yr intv_qrt agep_c yob cohort cohort_sex sex race cntparnts lvledu income dep anx ///
ppsu wtfa_c pstrat
order srvy_yr intv_qrt agep_c yob cohort cohort_sex sex race cntparnts lvledu income dep anx ///
ppsu wtfa_c pstrat
save "${myOutputs}/nhis_clean_indv.dta", replace

*===============================================
* Survey design
*===============================================

table srvy_yr, statistic(sum wtfa_c)
gen wtfa_c5 = wtfa_c/5
svyset ppsu [pweight = wtfa_c5], strata(pstrat)

*===============================================
* Survey-weighted prevalence: Year × cohort × sex
*===============================================

* Create results container
tempname results
postfile `results' srvy_yr cohort sex mean_Depression mean_Anxiety ///
	using "${myOutputs}/nhis_MH_prev_yr.dta", replace

* Get levels
levelsof srvy_yr, local(srvy_yrs)
levelsof cohort, local(cohorts)
levelsof sex, local(sexes)

* Nested loop over year, cohort, and sex
foreach y in `srvy_yrs'{
	
	foreach c in `cohorts' {
		
		foreach s in `sexes' {
			
			* Skip if no observations
			count if srvy_yr==`y' & cohort==`c' & sex==`s'
			if (r(N)==0) continue
			
			* Weighted means
			svy, subpop(if srvy_yr == `y' & cohort == `c' & sex == `s'): mean dep anx	
			matrix M = r(table)
            local mean_dep = M[1,1]
            local mean_anx = M[1,2]
				
			post `results' (`y') (`c') (`s') (`mean_dep') (`mean_anx')
		}
	}
}

* Close the results file
postclose `results'

*===============================================
* Survey-weighted prevalence: Year × quarter × cohort_sex
*===============================================

* Create results container
tempname results
postfile `results' srvy_yr intv_qrt cohort_sex mean_Depression mean_Anxiety ///
	using "${myOutputs}/nhis_MH_prev_qrt.dta", replace

* Get levels
levelsof srvy_yr, local(srvy_yrs)
levelsof intv_qrt, local(quarters)
levelsof cohort_sex, local(cohort_sexes)

* Nested loop over year, quarter, and cohort_sex
foreach y in `srvy_yrs' {

	foreach q in `quarters' {

		foreach cs in `cohort_sexes' {

			* Skip if no observations
			count if srvy_yr==`y' & intv_qrt==`q' & cohort_sex==`cs'
			if (r(N)==0) continue

			* Weighted means
			svy, subpop(if srvy_yr==`y' & intv_qrt==`q' & cohort_sex==`cs'): mean dep anx
			matrix M = r(table)
			local mean_dep = M[1,1]
			local mean_anx = M[1,2]

			post `results' (`y') (`q') (`cs') (`mean_dep') (`mean_anx')
		}
	}
}

* Close the results file
postclose `results'

log close
