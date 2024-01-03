// Replication Package for "Deportation Threat Predicts Latino U.S. Citizens and Noncitizens' Psychological Distress, 2011-2018"
// Authors: Amy L. Johnson, Christopher Levesque, Neil A. Lewis, Jr., Asad L. Asad

// Analyses of NHIS Public Data, accessed through IPUMS

clear

cd "WORKINGDIRECTORY"

use nhis_clean.dta

gen sampweight2=sampweight/8
label variable sampweight2 "Sampling Weights Adjusted by dividing by the number of years"
svyset psu [pweight = sampweight2], strata(strata)

// filtering the data
keep if year >=2011 & year <=2018
keep if filter==1
drop if citizen2 == 3 

gen latino = 1 if nczn == 1 | nat == 1 | usb == 1

// need an age standardized population --> going to use the full US Latino pop as the standard
recode age (18/44 = 1) (45/64 = 2) (65/85 = 3), gen(age_grp)
svy: tab age_grp 
gen age_adj_wt = .
replace age_adj_wt = .4682 if age_grp == 1
replace age_adj_wt = .3431 if age_grp == 2
replace age_adj_wt = .1887 if age_grp == 3

// descriptive stats for inclusion in text

// for US Latinos

svy, subpop(if latino == 1): mean k6, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if year==2011 & latino == 1): mean k6, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if year==2018 & latino == 1): mean k6, stdize(age_grp) stdweight(age_adj_wt)

svy, subpop(if latino == 1): mean k6_bin, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if latino == 1): mean k6_depr, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if latino == 1): mean k6_anx, stdize(age_grp) stdweight(age_adj_wt)

// for US Born
svy, subpop(if usb==1): mean k6, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if usb==1): mean k6_depr, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if usb==1 & year == 2011): mean k6_depr, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if usb==1 & year == 2018): mean k6_depr, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if usb==1): mean k6_anx, stdize(age_grp) stdweight(age_adj_wt)


// for Naturalized
svy, subpop(if nat==1): mean k6, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if nat==1): mean k6_depr, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if nat==1): mean k6_anx, stdize(age_grp) stdweight(age_adj_wt)

// for Non-citizens
svy, subpop(if nczn==1): mean k6, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if nczn==1): mean k6_depr, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if nczn==1): mean k6_anx, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if nczn==1 & year==2011): mean k6_anx, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if nczn==1 & year==2018): mean k6_anx, stdize(age_grp) stdweight(age_adj_wt)

// calculating age adjusted means by interview date for use in figures
gen k6_adj = .
gen k6_bin_adj = .
gen k6_anx_adj = .
gen k6_depr_adj = .

gen k6_adj_all = .
gen k6_bin_adj_all = .
gen k6_anx_adj_all = .
gen k6_depr_adj_all = .

keep if latino == 1

foreach v of varlist k6 k6_bin k6_anx k6_depr {

	forvalues d = 612/707 {

		foreach j of varlist usb nat nczn {
	
			svy, subpop(if `j'==1 & intervwdate==`d'): mean `v', stdize(age_grp) stdweight(age_adj_wt)
			matrix b = r(table)
			local adje = b[1,1]
			di `adje'
			replace `v'_adj = `adje' if `j'==1 & intervwdate==`d'
			
		}
		
		svy, subpop(if intervwdate==`d'): mean `v', stdize(age_grp) stdweight(age_adj_wt)
		matrix b = r(table)
		local adje = b[1,1]
		di `adje'
		replace `v'_adj_all = `adje' if intervwdate==`d'
	
	}
	
}


save "nhis_ageadj.dta", replace 

* for NHWB comparison group Age adjusted & Weighted

use nhis_clean.dta

gen sampweight2=sampweight/8
label variable sampweight2 "Sampling Weights Adjusted by dividing by the number of years"
svyset psu [pweight = sampweight2], strata(strata)

// filtering the data
keep if year >=2011 & year <=2018

tab race2, gen(race_)
gen nh_blackwhite = 0
replace nh_blackwhite = 1 if (race_1 == 1 | race_2 == 1) & citizen2==2

keep if filter == 1

// need an age standardized population --> using the full US pop as the standard
recode age (18/44 = 1) (45/64 = 2) (65/85 = 3), gen(age_grp)
svy: tab age_grp 
gen age_adj_wt = .
replace age_adj_wt = .4684 if age_grp == 1
replace age_adj_wt = .3431 if age_grp == 2
replace age_adj_wt = .1885 if age_grp == 3

svy, subpop(if year==2011 & nh_blackwhite == 1): mean k6, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if year==2018 & nh_blackwhite == 1): mean k6, stdize(age_grp) stdweight(age_adj_wt)

svy, subpop(if nh_blackwhite == 1): mean k6_bin, stdize(age_grp) stdweight(age_adj_wt)

svy, subpop(if nh_blackwhite == 1): mean k6_depr, stdize(age_grp) stdweight(age_adj_wt)
svy, subpop(if nh_blackwhite == 1): mean k6_anx, stdize(age_grp) stdweight(age_adj_wt)

// calculating age adjusted means by interview date for use in figures
gen k6_adj = .
gen k6_bin_adj = .
gen k6_anx_adj = .
gen k6_depr_adj = .

keep if nh_blackwhite == 1

foreach v of varlist k6 k6_bin k6_anx k6_depr {

	forvalues d = 612/707 {

		foreach j of varlist nh_blackwhite {
	
			svy, subpop(if `j'==1 & intervwdate==`d'): mean `v', stdize(age_grp) stdweight(age_adj_wt)
			matrix b = r(table)
			local adje = b[1,1]
			di `adje'
			replace `v'_adj = `adje' if `j'==1 & intervwdate==`d'
			
		}
	
	}
	
}

save "nhis_ageadj_NHWB.dta", replace 


** combining Latino & NHWB

use "nhis_ageadj.dta", clear

append using "nhis_ageadj_NHWB.dta"

rename race_1 white
rename race_2 black
gen nh_bw = nh_blackwhite

// regression models

local var "k6 k6_bin k6_anx k6_depr"
display "`var'"
tokenize "`var'"  

foreach v of local var {

	foreach j of varlist filter latino usb nat nczn nh_bw white black {
	
		if `j' == latino {
		
			reg `v'_adj_all intervwdate if `j'==1 [pweight = sampweight2]
		
		}
		
		else {
	
			reg `v'_adj intervwdate if `j'==1 [pweight = sampweight2]
			
		}
		
		estimates store m_`j'`v'
			
		outreg2 using "TimeTrend_Reg.xls", excel ///
					label append stats(coef se ci) nodepvar  ///
					title("OLS Model") ///
					ctitle ("`v' - `j'")
					
	}


}


** descriptive plots

* Figure 1 & Figure S1

collapse (mean) k6*, by(nh_blackwhite usb nat nczn intervwdate)

local var "k6 k6_bin k6_anx k6_depr"
display "`var'"
tokenize "`var'"  

foreach v of local var {
	
	if(strpos("`v'", "k6_bin") > 0) {
	
		local y_min = 0
		local y_mid = 0.1
		local y_max = 0.4
		local y_lab = "Proportion K6 >= 5"
		local title = "B. Proportion K6 >= 5"
	
	}
	
	else if(strpos("`v'", "anx") > 0) {
	
		local y_min = 0
		local y_mid = 0.5
		local y_max = 1
		local y_lab = "Anxiety"
		local title = "D. Anxiety"
		
	}
	
	else if(strpos("`v'", "depr") > 0) {
	
		local y_min = 0
		local y_mid = 0.5
		local y_max = 1
		local y_lab = "Depression"
		local title = "C. Depression"
	
	}
	
	else  {
	
		local y_min = 0
		local y_mid = 1
		local y_max = 5
		local y_lab = "K6"
		local title = "A. K6"
	
	}	
	
	* linear
	twoway ///
		(scatter `v'_adj intervwdate if usb == 1, mcolor(navy%25) mfcolor(navy%25) msize(tiny)) ///
		(scatter `v'_adj intervwdate if nat == 1, mcolor(maroon%25) mfcolor(maroon%25) msize(tiny)) ///
		(scatter `v'_adj intervwdate if nczn == 1, mcolor(forest_green%25) mfcolor(forest_green%25) msize(tiny)) ///
		(scatter `v'_adj_all intervwdate, mcolor(dkorange%25) mfcolor(dkorange%25) msize(tiny)) ///
		(scatter `v'_adj intervwdate if nh_blackwhite == 1, mcolor(purple%25) mfcolor(purple%25) msize(tiny)) ///
		(lfit `v'_adj intervwdate if usb == 1 ,  lcolor(navy) lwidth(medium)) ///
		(lfit `v'_adj intervwdate if nat == 1 ,  lcolor(maroon) lwidth(medium)) ///
		(lfit `v'_adj intervwdate if nczn == 1,  lcolor(forest_green) lwidth(medium)) ///
		(lfit `v'_adj_all intervwdate, lcolor(dkorange) lwidth(medium)) ///
		(lfit `v'_adj intervwdate if nh_blackwhite == 1, lcolor(purple) lwidth(medium) lpattern(dash)) ///
		, ///
		legend(order(6 "Latino U.S. Born" 7 "Latino Naturalized" 8 "Latino Noncitizens" 9 "All Latinos" 10 "Non-Hispanic White & Black") ///
				size(vsmall) rows(1) symxsize(4) colgap(0.8) keygap(0.5)) ///
		xtitle("Month of Interview", size(small)) ///
		ylab(`y_min'(`y_mid')`y_max', labsize(vsmall) nogrid) ///
		xlab(#8, format(%tm+Mon+YY) labsize(vsmall)) ///
		ytitle(`y_lab', size(small)) ///
		title(`title', size(small) color(black)) ///
		graphregion(fcolor(white)) ///
		name(`v'_trend, replace)
		graph save "DescTrend_`v'_month_LatinoNHWBcomb.gph", replace

	* spline
	twoway ///
		(scatter `v'_adj intervwdate if usb == 1, mcolor(navy%25) mfcolor(navy%25) msize(tiny)) ///
		(scatter `v'_adj intervwdate if nat == 1, mcolor(maroon%25) mfcolor(maroon%25) msize(tiny)) ///
		(scatter `v'_adj intervwdate if nczn == 1, mcolor(forest_green%25) mfcolor(forest_green%25) msize(tiny)) ///
		(scatter `v'_adj_all intervwdate, mcolor(dkorange%25) mfcolor(dkorange%25) msize(tiny)) ///
		(scatter `v'_adj intervwdate if nh_blackwhite == 1, mcolor(purple%25) mfcolor(purple%25) msize(tiny)) ///
		(mspline `v'_adj intervwdate if usb == 1 , n(5) lcolor(navy) lwidth(medium)) ///
		(mspline `v'_adj intervwdate if nat == 1 , n(5) lcolor(maroon) lwidth(medium)) ///
		(mspline `v'_adj intervwdate if nczn == 1, n(5) lcolor(forest_green) lwidth(medium)) ///
		(mspline `v'_adj_all intervwdate, n(5) lcolor(dkorange) lwidth(medium)) ///
		(mspline `v'_adj intervwdate if nh_blackwhite == 1, n(5) lcolor(purple) lwidth(medium) lpattern(dash)) ///
		, ///
		legend(off) ///
		xtitle("Month of Interview", size(vsmall)) ///
		ylab(`y_min'(`y_mid')`y_max', labsize(vsmall) nogrid) ///
		xlab(#8, format(%tm+Mon+YY) labsize(vsmall)) ///
		ytitle(`y_lab', size(vsmall)) ///
		title(`title', size(vsmall)) ///
		graphregion(fcolor(white)) ///
		name(`v'_strend, replace)
		graph save "DescTrend_`v'_month__LatinoNHWBcomb_spline.gph", replace
}


grc1leg "DescTrend_k6_month_LatinoNHWBcomb.gph" ///
		"DescTrend_k6_bin_month_LatinoNHWBcomb.gph" ///
		"DescTrend_k6_depr_month_LatinoNHWBcomb.gph" ///
		"DescTrend_k6_anx_month_LatinoNHWBcomb.gph" ///
		, r(2) legendfrom("DescTrend_k6_month_LatinoNHWBcomb.gph") name(combined, replace) graphregion(color(white))
		
graph display combined, xsize(8.6) ysize(7)
graph export "DescTrend_alloutcomes_LatinoNHWBcomb.png", replace

graph combine "DescTrend_k6_month__LatinoNHWBcomb_spline.gph" ///
				"DescTrend_k6_bin_month__LatinoNHWBcomb_spline.gph" ///
				"DescTrend_k6_depr_month__LatinoNHWBcomb_spline.gph" ///
				"DescTrend_k6_anx_month__LatinoNHWBcomb_spline.gph", ///
				 name(combined, replace) graphregion(color(white))
graph save "DescTrend_alloutcomes_LatinoNHWBcomb_spline.gph", replace
		
graph display combined, xsize(10) ysize(8)
graph export "DescTrend_alloutcomes_LatinoNHWBcomb_spline.png", replace
		












