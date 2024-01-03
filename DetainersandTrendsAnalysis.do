// Replication Package for "Deportation Threat Predicts Latino U.S. Citizens and Noncitizens' Psychological Distress, 2011-2018"
// Authors: Amy L. Johnson, Christopher Levesque, Neil A. Lewis, Jr., Asad L. Asad

// Analysis of ICE Detainers and Google Trends

***
* CLEANING
***

global addit "WORKINGDIRECTORY"

* Detainer data

use "$addit/TRAC/detainerall.dta", clear

keep if prepare_date_year >= 2011 & prepare_date_year <= 2018

gen detain_monthyr = ym(prepare_date_year, prepare_date_month)
format detain_monthyr %tm
label var detain_monthyr "Month-Year for Detainers"

gen id = _n

collapse (count) detain_count=id , by(detain_monthyr)

gen intervwdate = detain_monthyr + 1 // interview date matched to detainer trends for preceding month

egen detain_std = std(detain_count)

save "$addit/TRAC/detainer_collapsed.dta", replace

* News search data

import delimited "$addit/Google Trends/immigrationnewssearch.csv", clear varnames(2)
rename immigrationunitedstates gtimmig_news
egen gtimmig_news_std = std(gtimmig_news)

gen gt_monthyr = ym(real(substr(month,1,4)), real(substr(month,6,7)))
format gt_monthyr %tm
label var gt_monthyr "Month-Year for Google Trends"

drop month

gen intervwdate = gt_monthyr + 1 // interview date matched to google trends for preceding month

save "$addit/Google Trends/immigrationnewssearch.dta", replace


* Google Trends data

// importing google trends data
import delimited "$addit/Google Trends/immigration_only.csv", clear varnames(2)

rename immigrationunitedstates gtimmig
egen gtimmig_std = std(gtimmig)

gen gt_monthyr = ym(real(substr(month,1,4)), real(substr(month,6,7)))
format gt_monthyr %tm
label var gt_monthyr "Month-Year for Google Trends"

drop month

gen intervwdate = gt_monthyr + 1 // interview date matched to google trends for preceding month

merge 1:1 intervwdate using "$addit/Google Trends/immigrationnewssearch.dta"

drop _merge

merge 1:m intervwdate using "nhis_2011-18_clean.dta"

drop _merge

merge m:1 intervwdate using "$addit/TRAC/detainer_collapsed.dta"

drop _merge

gen sampweight2=sampweight/8
label variable sampweight2 "Sampling Weights Adjusted by dividing by the number of years"
svyset psu [pweight = sampweight2], strata(strata)

label var detain_std "Count of detainers (std)"
label var gtimmig_std "Google search for immigration (std)"

label var female "Female"
label var age "Age"
label var marstcohab2 "Married or cohabiting"
label var intervlang2_b "Interview Language other than English"

replace income2_b = . if income2_b == 2
label var income2_b "Household income $50k or over"

recode educ (102/116 = 1) (201/202 = 2) (301/303 = 3) (400/503 = 4) (0 997/999 = .), gen(educ_4cat)
label var educ_4cat " Education"
label def educ_4cat_lbl 1 "Less than HS" 2 "High school" 3 "Some college" 4 "Bachelor's"
label values educ_4cat educ_4cat_lbl

gen employed = 0
replace employed = . if empstat == . | empstat >=997
replace employed = 1 if empstat == 111 | empstat == 112 | empstat == 120  //working for pay, working w/o pay, or w/ job but not at work
label var employed "Employed"


label var detain_std "Detainer Count (std)"
label var gtimmig_std "Google Trends for Immigration (std)"
label var gtimmig_news_std "Google News Trends for Immigration (std)"


tab race2, gen(race_)
gen nh_blackwhite = 0
replace nh_blackwhite = 1 if (race_1 == 1 | race_2 == 1) & citizen2==2

rename nh_blackwhite bw

gen nh_white = 0
replace nh_white = 1 if race_1 == 1 & citizen2==2

gen nh_black = 0
replace nh_black = 1 if race_2 == 1 & citizen2==2

rename nh_white white
rename nh_black black

gen belowpov = .
replace belowpov = 1 if pooryn == 2
replace belowpov = 0 if pooryn == 1

corr detain_std gtimmig_std

***
* MODELS
***

* TABLE S4

foreach var of varlist k6 k6_bin k6_anx k6_depr {
	
	foreach j of varlist usb nat nczn {
	
		reg `var' detain_std gtimmig_std if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		outreg2 using "$addit/Results/GTandDet_Reg.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					title("OLS Model") ///
					ctitle ("`var' - `j'") 
		
		reg `var' detain_std gtimmig_std female age employed income2_b parent marstcohab2 i.educ_4cat i.region intervlang2_b if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		outreg2 using "$addit/Results/GTandDet_Reg.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					ctitle ("`var' - `j' - ctrls") 
					
		reg `var' detain_std gtimmig_std female age employed income2_b parent marstcohab2 i.educ_4cat i.region intervlang2_b i.intervwmo i.intervwyr if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		estimates store m_`j'`var'
		
		outreg2 using "$addit/Results/GTandDet_Reg.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					ctitle ("`var' - `j' - FEs") 
	
	}

}

* TABLE S5

foreach var of varlist k6 k6_bin k6_anx k6_depr {
	
	foreach j of varlist bw black white {
	
		reg `var' detain_std gtimmig_std if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		outreg2 using "$addit/Results/GTandDet_Reg_NHWBsep.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					title("OLS Model") ///
					ctitle ("`var' - `j'") 
		
		reg `var' detain_std gtimmig_std female age employed income2_b parent marstcohab2 i.educ_4cat i.region intervlang2_b if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		outreg2 using "$addit/Results/GTandDet_Reg_NHWBsep.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar  ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					ctitle ("`var' - `j' - ctrls") 
					
		reg `var' detain_std gtimmig_std female age employed income2_b parent marstcohab2 i.educ_4cat i.region intervlang2_b i.intervwmo i.intervwyr if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
							
		estimates store m_`j'`var'
		
		outreg2 using "$addit/Results/GTandDet_Reg_NHWBsep.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					ctitle ("`var' - `j' - FEs") 
	
	}

}


* TABLE S6

foreach var of varlist k6 k6_bin k6_anx k6_depr {
	
	foreach j of varlist usb nat nczn bw {
	
	
	// Get rid of the household income measure and replace with POORYN and see if main associations hold.
					
		reg `var' detain_std gtimmig_std female age employed belowpov parent marstcohab2 i.educ_4cat i.region intervlang2_b i.intervwmo i.intervwyr if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		outreg2 using "$addit/Results/GTandDet_Reg_PovertySupp.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					ctitle ("`var' - `j' - replace") 
					
	// Add POORYN to existing regressions and see if observed associations hold.
	
		reg `var' detain_std gtimmig_std female age employed income2_b belowpov parent marstcohab2 i.educ_4cat i.region intervlang2_b i.intervwmo i.intervwyr if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		
		outreg2 using "$addit/Results/GTandDet_Reg_PovertySupp.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					ctitle ("`var' - `j' - add") 

	// Interact household income and/or POORYN with measures of deportation threat and see results by income or poverty status.
	
		reg `var' c.detain_std##belowpov c.gtimmig_std##belowpov female age employed income2_b parent marstcohab2 i.educ_4cat i.region intervlang2_b i.intervwmo i.intervwyr if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		outreg2 using "$addit/Results/GTandDet_Reg_PovertySupp.xls", excel ///
					label append stats(coef se ci) dec(3) nodepvar ///
					alpha(0.001, 0.01, 0.05, 0.1) symbol(***, **, *, +) ///
					ctitle ("`var' - `j' - interact") 
	}

}



* FIGURE 4

local var "k6 k6_bin k6_anx k6_depr"
display "`var'"
tokenize "`var'"  

foreach v of local var {

	if(strpos("`v'", "k6_bin") > 0) {
	
		local axistog = "off"
		local title = "B. Proportion K6 >= 5"
		local fxsize = 40
	
	}
	
	else if(strpos("`v'", "anx") > 0) {
	
		local axistog = ""
		local title = "C. Anxiety"
		local fxsize = 60
		
	}
	
	else if(strpos("`v'", "depr") > 0) {
	
		local axistog = "off"
		local title = "D. Depression"
		local fxsize = 40
	
	}
	
	else  {
	
		local axistog = ""
		local title = "A. K6"
		local fxsize = 60
	}
	
	coefplot (m_usb`v', label(Latino U.S. Born) msize(0.75)) ///
		(m_nat`v', label(Latino Naturalized) msize(0.75)) ///
		(m_nczn`v', label(Latino noncitizens) msize(0.75)) ///
		(m_bw`v', label(Non-Hispanic White & Black) msize(0.75)  mcolor(purple) ciopts(lcolor(purple)))  ///
		, /// 
		drop(_cons) keep(detain_std gtimmig_std) xline(0) name(`v'_coef, replace) sort(, descending) ///
		plotlabels("Latino U.S. Born" "Latino Naturalized" "Latino Noncitizens" "Non-Hispanic White & Black") ///
		graphregion(color(white))  ///
		title(`title', color(black) size(small)) yscale(`axistog') fxsize(`fxsize') legend(size(vsmall) rows(1) symxsize(4) colgap(0.8) keygap(0.5)) ///
		ylab(, labsize(vsmall)) xlab(, labsize(vsmall))
	graph save "$addit/coefplot_`v'.gph", replace

}


cd "$addit"

grc1leg "coefplot_k6.gph" ///
		"coefplot_k6_bin.gph" ///
		"coefplot_k6_anx.gph" ///
		"coefplot_k6_depr.gph" ///
		, r(2) name(cpcombined, replace) graphregion(color(white)) 
		
graph display cpcombined, xsize(8.6) ysize(7)
graph export "$addit/coefplot_allgroups.png", replace


* using google news searches + detainers

foreach var of varlist k6 k6_bin k6_anx k6_depr {
	
	foreach j of varlist usb nat nczn bw {
					
		reg `var' detain_std gtimmig_news_std female age employed income2_b parent marstcohab2 i.educ_4cat i.region intervlang2_b i.intervwmo i.intervwyr if `j'==1 [pweight = sampweight2], vce(cluster intervwdate)
		
		estimates store mnews_`j'`var'
	
	}

}


* FIGURE S8

local var "k6 k6_bin k6_anx k6_depr"
display "`var'"
tokenize "`var'"  

foreach v of local var {

	if(strpos("`v'", "k6_bin") > 0) {
	
		local axistog = "off"
		local title = "B. Proportion K6 >= 5"
		local fxsize = 40
	
	}
	
	else if(strpos("`v'", "anx") > 0) {
	
		local axistog = ""
		local title = "C. Anxiety"
		local fxsize = 60
		
	}
	
	else if(strpos("`v'", "depr") > 0) {
	
		local axistog = "off"
		local title = "D. Depression"
		local fxsize = 40
	
	}
	
	else  {
	
		local axistog = ""
		local title = "A. K6"
		local fxsize = 60
	}
	
	coefplot (mnews_usb`v', label(Latino U.S. Born) msize(0.75)) ///
		(mnews_nat`v', label(Latino Naturalized) msize(0.75)) ///
		(mnews_nczn`v', label(Latino noncitizens) msize(0.75)) ///
		(mnews_bw`v', label(Non-Hispanic White & Black) msize(0.75)  mcolor(purple) ciopts(lcolor(purple)))  ///
		, /// 
		drop(_cons) keep(detain_std gtimmig_news_std) xline(0) name(`v'_coef, replace) sort(, descending) ///
		plotlabels("Latino U.S. Born" "Latino Naturalized" "Latino Noncitizens" "Non-Hispanic White & Black") ///
		graphregion(color(white))  ///
		title(`title', color(black) size(vsmall)) yscale(`axistog') fxsize(`fxsize') legend(size(tiny) rows(1) symxsize(10)) ///
		ylab(, labsize(vsmall)) xlab(, labsize(vsmall))
	graph save "$addit/coefplot_news_`v'.gph", replace

}


cd "$addit"

grc1leg "coefplot_news_k6.gph" ///
		"coefplot_news_k6_bin.gph" ///
		"coefplot_news_k6_anx.gph" ///
		"coefplot_news_k6_depr.gph" ///
		, r(2) name(cpcombined, replace) graphregion(color(white)) 
		
graph display cpcombined, xsize(10) ysize(8)
graph export "$addit/coefplot_news_allgroups.png", replace



***
* DESCRIPTIVE GRAPHS
***

* FIGURE S6

use "$addit/TRAC/detainer_collapsed.dta", clear

twoway ///
	(line detain_count detain_monthyr, mcolor(navy) mfcolor(navy) msize(tiny)), ///
	xtitle("Month", size(vsmall)) ///
	ylab(0(5000)30000, labsize(*.3) nogrid) ///
	xlab(#8, format(%tm+Mon+YY) labsize(vsmall)) ///
	ytitle("Number of Detainers", size(vsmall)) ///
	graphregion(fcolor(white)) ///
	name(detainer_trend, replace)
	graph export "$addit/Results/Detainer_Trend.png", replace


* FIGURE S7
import delimited "$addit/Google Trends/immigration_only.csv", clear varnames(2)

rename immigrationunitedstates gtimmig

gen gt_monthyr = ym(real(substr(month,1,4)), real(substr(month,6,7)))
format gt_monthyr %tm
label var gt_monthyr "Month-Year for Google Trends"


twoway ///
	(line gtimmig gt_monthyr, mcolor(navy) mfcolor(navy) msize(tiny)), ///
	xtitle("Month", size(vsmall)) ///
	ylab(0(20)100, labsize(*.3) nogrid) ///
	xlab(#8, format(%tm+Mon+YY) labsize(vsmall)) ///
	ytitle("Google Trends Score", size(vsmall)) ///
	graphregion(fcolor(white)) ///
	name(detainer_trend, replace)
	graph export "$addit/Results/Google_Trend.png", replace

