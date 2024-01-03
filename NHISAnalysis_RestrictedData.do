// Replication Package for "Deportation Threat Predicts Latino U.S. Citizens and Noncitizens' Psychological Distress, 2011-2018"
// Authors: Amy L. Johnson, Christopher Levesque, Neil A. Lewis, Jr., Asad L. Asad

// Analyses of NHIS Restricted Data

clear

cd "WORKINGDIRECTORY"

***
* REGRESSION DISCONTINUITY ANALYSES
***

use nhis_clean.dta

gen sampweight2=sampweight/8
label variable sampweight2 "Sampling Weights Adjusted by dividing by the number of years"
svyset psu [pweight = sampweight2], strata(strata)

***
* Latinos
***
* TABLE S3
* TABLE S13

label var k6_anx "K6 Anxiety"
label var k6_depr "K6 Depression"

// country of origin (control)
gen h_mc = 1 if hispeth == 20 | hispeth == 23 | hispeth == 61
label var h_mc "Mexican, Central and South American"

gen h_ot = 1 if h_mc != 1 & hispeth != 10
label var h_ot "Other Hispanic"

// time in us (control)
sum yrsinus


quietly foreach v of varlist k6 k6_bin k6_anx k6_depr {

	foreach x of varlist daca_announce_d daca_rescission_d  ///
						 dapa_announce_d dapa_injunction_d dapa_rescission_d ///
						 trump_declares_d trump_elected_d {
						 
		foreach j of varlist usb nat nczn {
		
				// run regression model 

				svy, subpop(if `j'==1 & within_`x'==1):  reg `v' saend3 i.`x' 
				
				eststo basic
				
				// construct regression table
				
				outreg2 using "Table3_`x'_`j'_`v'.xls", excel ///
					label replace stats(coef se ci) addstat(Subpop Observations, `e(N_sub)') ///
					title("Linear RD Model of `v' at `x'") ///
					ctitle ("Linear") ///
					addnote("Confidence intervals included")
					
				if(strpos("`v'", "bin") > 0) {
					// run logistic regression model 
					svy, subpop(if `j'==1 & within_`x'==1):  logit `v' saend3 i.`x' 
					
					eststo basic
					
					// construct regression table
					
					outreg2 using "Table3_`x'_`j'_`v'.xls", excel ///
						label append stats(coef se ci) addstat(Subpop Observations, `e(N_sub)') ///
						ctitle("Logit")
				}
				
				// model with controls
			
				svy, subpop(if `j'==1 & within_`x'==1):  reg `v' saend3 i.`x' h_mc yrsinus
				
				eststo basic
				
				outreg2 using "Table3_`x'_`j'_`v'.xls", excel ///
					label append stats(coef se ci) addstat(Subpop Observations, `e(N_sub)') ///
					ctitle("Controls")
		}
	}
}
		

// by Hispanic subgroup

	//Mexican+MexicanAmerican+Central American v. All others

gen h_mx = 1 if hispeth == 20 | hispeth == 23
label var h_mx "Mexican and Mexican American"
gen h_ca = 1 if hispeth == 61
label var h_ca "Central and South American"

quietly foreach v of varlist k6 k6_bin k6_anx k6_depr {

	foreach j of varlist usb nat nczn {
	
		foreach x of varlist daca_announce_d daca_rescission_d ///
					dapa_announce_d dapa_injunction_d dapa_rescission_d ///
					trump_elected_d {

			
			// regressions through each subgroup
	
			foreach h of varlist h_mc h_ot {

					svy, subpop(if `j'==1 & within_`x'==1 & `h'==1):  reg `v' saend3 i.`x' 
					
					eststo basic
					
					outreg2 using "Table3XHisp_`x_short'_`j'_`v'_`h'.xls", excel ///
						label replace stats(coef se ci) addstat(Subpop Observations, `e(N_sub)') ///
						ctitle("Linear") ///
						title("Linear RD Model of `v' at `x' for `j' `h'") ///
						addnote("Confidence intervals included")
						
				if(strpos("`v'", "bin") > 0) {
					// run logistic regression model 
					svy, subpop(if `j'==1 & within_`x'==1 & `h'==1):  logit `v' saend3 i.`x' 

					eststo basic
				
					// construct regression table
				
					outreg2 using "Table3XHisp_`x_short'_`j'_`v'_`h'.xls", excel ///
						label append stats(coef se ci) addstat(Subpop Observations, `e(N_sub)') ///
						ctitle("Logit") 
				}
				
				// model with controls
			
				svy, subpop(if `j'==1 & within_`x'==1 & `h'==1):  reg `v' saend3 i.`x' yrsinus
				
				eststo basic
				
				outreg2 using "Table3XHisp_`x_short'_`j'_`v'_`h'.xls", excel ///
					label append stats(coef se ci) addstat(Subpop Observations, `e(N_sub)') ///
					ctitle("Controls") 
						
			}
		}
	}
}


		
***
* Non-Hispanic White & Black
***

tab race2, gen(race_)
gen nh_blackwhite = 0
replace nh_blackwhite = 1 if (race_1 == 1 | race_2 == 1) & citizen2==2

gen nh_white = 0
replace nh_white = 1 if race_1 == 1 & citizen2==2

gen nh_black = 0
replace nh_black = 1 if race_2 == 1 & citizen2==2

rename nh_blackwhite bw
rename nh_white w
rename nh_black b

// Linear regression approach

quietly foreach v of varlist k6 k6_bin k6_anx k6_depr {

	foreach r of varlist bw w b {

		foreach x of varlist daca_announce_d daca_rescission_d ///
					dapa_announce_d dapa_injunction_d dapa_rescission_d ///
					trump_elected_d {

				svy, subpop(if within_`x'==1&`r'==1):  reg `v' saend3 i.`x' 
				
				eststo basic
				
				outreg2 using "Table4_`x'_`v'_`r'.xls", ///
				label replace stats(coef se ci) addstat(Subpop Observations, `e(N_sub)') ///
				ctitle("LPM") ///
				title("Linear RD Model of `v' at `x' for `r'") ///
				addnote("Confidence intervals included")
				
				if(strpos("`v'", "bin") > 0) {
					* logistic regression added 11/19/22 ALJ
					svy, subpop(if within_`x'==1&`r'==1):  logit `v' saend3 i.`x' 
					
					eststo basic
					
					outreg2 using "Table4_`x'_`v'_`r'.xls", ///
					label append stats(coef se ci) addstat(Subpop Observations, `e(N_sub)') ///
					ctitle("Logit")
				}
				
		}
	}
} 


* FIGURE 2

local events `" "DACA Announcement 06/15/12" "DACA Rescission 09/05/17" "DAPA Announcement 11/20/14" "DAPA Injunction 02/16/15" "DAPA Rescission 06/15/17" "Trump Elected 11/08/16" "'

foreach x of local events {

		// setting event cutoffs in days and constants
		
		if(strpos("`x'", "DACA Announce") > 0) {
						local eventtime = 19159
						
						local usb_cons = -186.6
						local usb_datecoef = 0.00986
						local usb_cutoffcoef = -0.198
						local usb_sig = ""
						
						local nat_cons = -729.9
						local nat_datecoef = 0.0383
						local nat_cutoffcoef = -2.406
						local nat_sig = ""
						
						local nczn_cons = -856.8
						local nczn_datecoef = 0.0449
						local nczn_cutoffcoef = -1.512
						local nczn_sig = ""
						
						local nhwb_cons = -75.04
						local nhwb_datecoef = 0.00403
						local nhwb_cutoffcoef = -0.308
						local nhwb_sig = ""
						
					}
		else if(strpos("`x'", "DACA Rescis") > 0) {
						local eventtime = 21067
						
						local usb_cons = 228.1
						local usb_datecoef = -0.0107
						local usb_cutoffcoef = 0.726
						local usb_sig = ""
						
						local nat_cons = 460.2
						local nat_datecoef = -0.0218
						local nat_cutoffcoef = 0.539
						local nat_sig = ""
						
						local nczn_cons = -735.0
						local nczn_datecoef = 0.0350
						local nczn_cutoffcoef = -0.819
						local nczn_sig = ""
						
						local nhwb_cons = 85.26
						local nhwb_datecoef = -0.00393
						local nhwb_cutoffcoef = 0.394
						local nhwb_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Announce") > 0) {
						local eventtime = 20046
						
						local usb_cons = 400.2
						local usb_datecoef = -0.0198
						local usb_cutoffcoef = 0.258
						local usb_sig = ""
						
						local nat_cons = -763.8
						local nat_datecoef = 0.0383
						local nat_cutoffcoef = -1.707
						local nat_sig = ""
						
						local nczn_cons = -899.3
						local nczn_datecoef = 0.0450
						local nczn_cutoffcoef = -1.544
						local nczn_sig = ""
						
						local nhwb_cons = -246.9
						local nhwb_datecoef = 0.0125
						local nhwb_cutoffcoef = -0.442
						local nhwb_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Injunct") > 0) {
						local eventtime = 20134
						
						local usb_cons = -498.8
						local usb_datecoef = 0.0249
						local usb_cutoffcoef = -0.468
						local usb_sig = ""
						
						local nat_cons = -493.0
						local nat_datecoef = 0.0247
						local nat_cutoffcoef = -1.126
						local nat_sig = ""
						
						local nczn_cons = 997.0
						local nczn_datecoef = -0.0494
						local nczn_cutoffcoef = 1.627
						local nczn_sig = ""
						
						local nhwb_cons = 43.00
						local nhwb_datecoef = -0.00201
						local nhwb_cutoffcoef = 0.0437
						local nhwb_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Rescis") > 0) {
						local eventtime = 20985
						
						local usb_cons = -773.3
						local usb_datecoef = 0.0370
						local usb_cutoffcoef = -0.993
						local usb_sig = ""
						
						local nat_cons = 885.3
						local nat_datecoef = -0.0421
						local nat_cutoffcoef = 0.490
						local nat_sig = ""
						
						local nczn_cons = -1437
						local nczn_datecoef = 0.0686
						local nczn_cutoffcoef = -1.904
						local nczn_sig = ""
						
						local nhwb_cons = 49.21
						local nhwb_datecoef = -0.00221
						local nhwb_cutoffcoef = 0.00208
						local nhwb_sig = ""
						
					}
		else if(strpos("`x'", "Trump Elected") > 0) {
						local eventtime = 20765
						
						local usb_cons = -95.55
						local usb_datecoef = 0.00474
						local usb_cutoffcoef = -0.511
						local usb_sig = ""
						
						local nat_cons = 914.9
						local nat_datecoef = -0.0440
						local nat_cutoffcoef = 1.110
						local nat_sig = ""
						
						local nczn_cons = 96.15
						local nczn_datecoef = -0.00459
						local nczn_cutoffcoef = 2.003
						local nczn_sig = "*"
						
						local nhwb_cons = 70.75
						local nhwb_datecoef = -0.00327
						local nhwb_cutoffcoef = -0.0615
						local nhwb_sig = ""
						
					}
		
		local s = `eventtime' - 30 // start bandwidth interval
		local f = `eventtime' + 30 //finish bandwidth interval 
		
		local y_usb = `usb_cons' + `usb_datecoef'*`eventtime' + `usb_cutoffcoef'
		local y_nat = `nat_cons' + `nat_datecoef'*`eventtime' + `nat_cutoffcoef'
		local y_nczn = `nczn_cons' + `nczn_datecoef'*`eventtime' + `nczn_cutoffcoef' + 0.1
		local t = `eventtime' + 5
		
		twoway ///
			(function ypred =  `usb_cons' + `usb_datecoef'*x, range(`s' `eventtime') lcolor(navy) lpattern(solid)) ///
			(function ypred = `usb_cons' + `usb_datecoef'*x + `usb_cutoffcoef', range(`eventtime' `f') lcolor(navy) lpattern(solid)) ///
			(function ypred =  `nat_cons' + `nat_datecoef'*x, range(`s' `eventtime') lcolor(maroon) lpattern(solid)) ///
			(function ypred = `nat_cons' + `nat_datecoef'*x + `nat_cutoffcoef', range(`eventtime' `f') lcolor(maroon) lpattern(solid)) ///
			(function ypred =  `nczn_cons' + `nczn_datecoef'*x, range(`s' `eventtime') lcolor(forest_green) lpattern(solid)) ///
			(function ypred = `nczn_cons' + `nczn_datecoef'*x + `nczn_cutoffcoef', range(`eventtime' `f') lcolor(forest_green) lpattern(solid)) ///
			(function ypred =  `nhwb_cons' + `nhwb_datecoef'*x, range(`s' `eventtime') lcolor(purple) lpattern(dash)) ///
			(function ypred = `nhwb_cons' + `nhwb_datecoef'*x + `nhwb_cutoffcoef', range(`eventtime' `f') lcolor(purple) lpattern(dash)) ///
			, ///
			xline(`eventtime', lpattern(dash) lcolor(red)) ///
			text(`y_usb' `t' "`usb_sig'", size(medium) color(navy)) ///
			text(`y_nat' `t' "`nat_sig'", size(medium) color(maroon)) ///
			text(`y_nczn' `t' "`nczn_sig'", size(medium) color(forest_green)) ///
			legend(order(1 "Latino U.S. Born" ///
						3 "Latino Naturalized" ///
						5 "Latino Noncitizens" 7 "Non-Hispanic White & Black") ///
						size(vsmall) rows(1) symxsize(4) colgap(0.8) keygap(0.5)) ///
			title("`x'", size(small) color(black)) ///
			xtitle("Date", size(small)) ytitle("Predicted K6", size(small)) ///
			xlab(`s'(30)`f', format(%td+Mon+YY) labsize(vsmall)) ///
			ylab(0(1)4,labsize(vsmall) nogrid) ///
			scheme(s2mono) ///
			graphregion(fcolor(white))
			tokenize `x' 
			loc first_two_words `1' `2'
			loc name "`first_two_words'" 
			
		
			graph save "RDFig_`name'_K6.gph", replace
}


* combining K6 graphs
grc1leg "RDFig_DACA Announcement_K6.gph" ///
		"RDFig_DAPA Announcement_K6.gph" ///
		"RDFig_DAPA Injunction_K6.gph" ///
		"RDFig_Trump Elected_K6.gph" ///
		"RDFig_DAPA Rescission_K6.gph" ///
		"RDFig_DACA Rescission_K6.gph" ///
		, r(2) legendfrom("RDFig_DACA Announcement_K6.gph") name(combined, replace) graphregion(color(white))

graph display combined, xsize(8.6) ysize(5.5)
graph export "RDFig_K6.png", replace


* FIGURE 3

local events `" "DACA Announcement 06/15/12" "DACA Rescission 09/05/17" "DAPA Announcement 11/20/14" "DAPA Injunction 02/16/15" "DAPA Rescission 06/15/17" "Trump Elected 11/08/16" "'

foreach x of local events {

		// setting event cutoffs in days and constants
		
		if(strpos("`x'", "DACA Announce") > 0) {
						local eventtime = 19159
						
						local usb_cons = -39.98
						local usb_datecoef = 0.00210
						local usb_cutoffcoef = -0.0671
						local usb_sig = ""
						
						local nat_cons = -88.81
						local nat_datecoef = 0.00465
						local nat_cutoffcoef = -0.176
						local nat_sig = ""
						
						local nczn_cons = -46.87
						local nczn_datecoef = 0.00246
						local nczn_cutoffcoef = -0.0734
						local nczn_sig = ""
						
						local nhwb_cons = -12.15
						local nhwb_datecoef = 0.000643
						local nhwb_cutoffcoef = -0.0205
						local nhwb_sig = ""
					}
		else if(strpos("`x'", "DACA Rescis") > 0) {
						local eventtime = 21067
						
						local usb_cons = 49.01
						local usb_datecoef = -0.00232
						local usb_cutoffcoef = 0.150
						local usb_sig = ""
						
						local nat_cons = 17.29
						local nat_datecoef = -0.000813
						local nat_cutoffcoef = -0.0257
						local nat_sig = ""
						
						local nczn_cons = -65.38
						local nczn_datecoef = 0.00311
						local nczn_cutoffcoef = -0.0791
						local nczn_sig = ""
						
						local nhwb_cons = 13.68
						local nhwb_datecoef = -0.000640
						local nhwb_cutoffcoef = 0.0425
						local nhwb_sig = ""
					}
		else if(strpos("`x'", "DAPA Announce") > 0) {
						local eventtime = 20046
						
						local usb_cons = -3.005
						local usb_datecoef = 0.000162
						local usb_cutoffcoef = -0.00319
						local usb_sig = ""
						
						local nat_cons = -128.8
						local nat_datecoef = 0.00644
						local nat_cutoffcoef = -0.237
						local nat_sig = "+" //logit also negative and significant
						
						local nczn_cons = -52.31
						local nczn_datecoef = 0.00262
						local nczn_cutoffcoef = -0.0882
						local nczn_sig = ""
						
						local nhwb_cons = -1.059
						local nhwb_datecoef = 6.29e-05
						local nhwb_cutoffcoef = 0.00238
						local nhwb_sig = ""
					}
		else if(strpos("`x'", "DAPA Injunct") > 0) {
						local eventtime = 20134
						
						local usb_cons = -49.39
						local usb_datecoef = 0.00247
						local usb_cutoffcoef = -0.0967
						local usb_sig = ""
						
						local nat_cons = -39.00
						local nat_datecoef = 0.00195
						local nat_cutoffcoef = -0.109
						local nat_sig = ""
						
						local nczn_cons = 34.35
						local nczn_datecoef = -0.00170
						local nczn_cutoffcoef = 0.0712
						local nczn_sig = ""
						
						local nhwb_cons = 5.578
						local nhwb_datecoef = -0.000267
						local nhwb_cutoffcoef = 0.00199
						local nhwb_sig = ""
					}
		else if(strpos("`x'", "DAPA Rescis") > 0) {
						local eventtime = 20985
						
						local usb_cons = -138.5
						local usb_datecoef = 0.00662
						local usb_cutoffcoef = -0.191
						local usb_sig = ""
						
						local nat_cons = 21.09
						local nat_datecoef = -0.000995
						local nat_cutoffcoef = -0.0410
						local nat_sig = ""
						
						local nczn_cons = -2.212
						local nczn_datecoef = 0.000117
						local nczn_cutoffcoef = -0.0673
						local nczn_sig = ""
						
						local nhwb_cons = 21.53
						local nhwb_datecoef = -0.00102
						local nhwb_cutoffcoef = 0.0103
						local nhwb_sig = ""
					}
		else if(strpos("`x'", "Trump Elected") > 0) {
						local eventtime = 20765
						
						local usb_cons = -48.65
						local usb_datecoef = 0.00236
						local usb_cutoffcoef = -0.127
						local usb_sig = ""
						
						local nat_cons = 118.8
						local nat_datecoef = -0.00572
						local nat_cutoffcoef = 0.256
						local nat_sig = "+" //logit results also significant and positive
						
						local nczn_cons = -62.27
						local nczn_datecoef = 0.00300
						local nczn_cutoffcoef = 0.0816
						local nczn_sig = "" //logit also non significant
						
						local nhwb_cons = 0.572
						local nhwb_datecoef = -1.66e-05
						local nhwb_cutoffcoef = -0.0187
						local nhwb_sig = ""
					}
		
		local s = `eventtime' - 30 // start bandwidth interval
		local f = `eventtime' + 30 //finish bandwidth interval 
		
		local y_usb = `usb_cons' + `usb_datecoef'*`eventtime' + `usb_cutoffcoef'
		local y_nat = `nat_cons' + `nat_datecoef'*`eventtime' + `nat_cutoffcoef'
		local y_nczn = `nczn_cons' + `nczn_datecoef'*`eventtime' + `nczn_cutoffcoef'
		local t = `eventtime' + 5
		
		twoway ///
			(function ypred =  `usb_cons' + `usb_datecoef'*x, range(`s' `eventtime') lcolor(navy) lpattern(solid)) ///
			(function ypred = `usb_cons' + `usb_datecoef'*x + `usb_cutoffcoef', range(`eventtime' `f') lcolor(navy) lpattern(solid)) ///
			(function ypred =  `nat_cons' + `nat_datecoef'*x, range(`s' `eventtime') lcolor(maroon) lpattern(solid)) ///
			(function ypred = `nat_cons' + `nat_datecoef'*x + `nat_cutoffcoef', range(`eventtime' `f') lcolor(maroon) lpattern(solid)) ///
			(function ypred =  `nczn_cons' + `nczn_datecoef'*x, range(`s' `eventtime') lcolor(forest_green) lpattern(solid)) ///
			(function ypred = `nczn_cons' + `nczn_datecoef'*x + `nczn_cutoffcoef', range(`eventtime' `f') lcolor(forest_green) lpattern(solid)) ///
			(function ypred =  `nhwb_cons' + `nhwb_datecoef'*x, range(`s' `eventtime') lcolor(purple) lpattern(dash)) ///
			(function ypred = `nhwb_cons' + `nhwb_datecoef'*x + `nhwb_cutoffcoef', range(`eventtime' `f') lcolor(purple) lpattern(dash)) ///
			, ///
			xline(`eventtime', lpattern(dash) lcolor(red)) ///
			text(`y_usb' `t' "`usb_sig'", size(small) color(navy)) ///
			text(`y_nat' `t' "`nat_sig'", size(small) color(maroon)) ///
			text(`y_nczn' `t' "`nczn_sig'", size(small) color(forest_green)) ///
			legend(order(1 "Latino U.S. Born" ///
						3 "Latino Naturalized" ///
						5 "Latino Noncitizens" 7 "Non-Hispanic White & Black") ///
						size(vsmall) rows(1) symxsize(4) colgap(0.8) keygap(0.5)) ///
			title("`x'", size(small) color(black)) ///
			xtitle("Date", size(small)) ytitle("Proportion K6 >= 5", size(small)) ///
			xlab(`s'(30)`f', format(%td+Mon+YY) labsize(vsmall)) ///
			ylab(0(.1)0.5,labsize(vsmall) nogrid) ///
			scheme(s2mono) ///
			graphregion(fcolor(white))
			tokenize `x' 
			loc first_two_words `1' `2'
			loc name "`first_two_words'" 
			
		
			graph save "RDFig_`name'_K6_bin.gph", replace
}

* combining K6 bin graphs
grc1leg "RDFig_DACA Announcement_K6_bin.gph" ///
		"RDFig_DAPA Announcement_K6_bin.gph" ///
		"RDFig_DAPA Injunction_K6_bin.gph" ///
		"RDFig_Trump Elected_K6_bin.gph" ///
		"RDFig_DAPA Rescission_K6_bin.gph" ///
		"RDFig_DACA Rescission_K6_bin.gph" ///
		, r(2) legendfrom("RDFig_DACA Announcement_K6_bin.gph") name(combined, replace) graphregion(color(white))

graph display combined, xsize(8.6) ysize(5.5)
graph export "RDFig_K6_bin.png", replace


* FIGURE S2

local events `" "DAPA Announcement 11/20/14" "Trump Elected 11/08/16" "'

foreach x of local events {

		// setting event cutoffs in days and constants
		
		if(strpos("`x'", "DAPA Announce") > 0) {
						local eventtime = 20046
						
						local usb_cons = 62.53
						local usb_datecoef = -0.00309
						local usb_cutoffcoef = 0.0201
						local usb_sig = ""
						
						local nat_cons = -55.72
						local nat_datecoef = 0.00281
						local nat_cutoffcoef = -0.153
						local nat_sig = ""
						
						local nczn_cons = -135.3
						local nczn_datecoef = 0.00677
						local nczn_cutoffcoef = -0.247
						local nczn_sig = ""
						
						local nhwb_cons = -16.52
						local nhwb_datecoef = 0.000856
						local nhwb_cutoffcoef = -0.0524
						local nhwb_sig = ""
					}

		else if(strpos("`x'", "Trump Elected") > 0) {
						local eventtime = 20765
						
						local usb_cons = 32.55
						local usb_datecoef = -0.00154
						local usb_cutoffcoef = -0.0214
						local usb_sig = ""
						
						local nat_cons = 390.5
						local nat_datecoef = -0.0188
						local nat_cutoffcoef = 0.609
						local nat_sig = "*"
						
						local nczn_cons = -2.964
						local nczn_datecoef = 0.000150
						local nczn_cutoffcoef = 0.381
						local nczn_sig = "*"
						
						local nhwb_cons = 49.10
						local nhwb_datecoef = -0.00233
						local nhwb_cutoffcoef = 0.0542
						local nhwb_sig = ""
					}
		
		local s = `eventtime' - 30 // start bandwidth interval
		local f = `eventtime' + 30 //finish bandwidth interval 
		
		local y_usb = `usb_cons' + `usb_datecoef'*`eventtime' + `usb_cutoffcoef'
		local y_nat = `nat_cons' + `nat_datecoef'*`eventtime' + `nat_cutoffcoef' + 0.08
		local y_nczn = `nczn_cons' + `nczn_datecoef'*`eventtime' + `nczn_cutoffcoef' - 0.08
		local t = `eventtime' + 5
		
		twoway ///
			(function ypred =  `usb_cons' + `usb_datecoef'*x, range(`s' `eventtime') lcolor(navy) lpattern(solid)) ///
			(function ypred = `usb_cons' + `usb_datecoef'*x + `usb_cutoffcoef', range(`eventtime' `f') lcolor(navy) lpattern(solid)) ///
			(function ypred =  `nat_cons' + `nat_datecoef'*x, range(`s' `eventtime') lcolor(maroon) lpattern(solid)) ///
			(function ypred = `nat_cons' + `nat_datecoef'*x + `nat_cutoffcoef', range(`eventtime' `f') lcolor(maroon) lpattern(solid)) ///
			(function ypred =  `nczn_cons' + `nczn_datecoef'*x, range(`s' `eventtime') lcolor(forest_green) lpattern(solid)) ///
			(function ypred = `nczn_cons' + `nczn_datecoef'*x + `nczn_cutoffcoef', range(`eventtime' `f') lcolor(forest_green) lpattern(solid)) ///
			(function ypred =  `nhwb_cons' + `nhwb_datecoef'*x, range(`s' `eventtime') lcolor(purple) lpattern(dash)) ///
			(function ypred = `nhwb_cons' + `nhwb_datecoef'*x + `nhwb_cutoffcoef', range(`eventtime' `f') lcolor(purple) lpattern(dash)) ///
			, ///
			xline(`eventtime', lpattern(dash) lcolor(red)) ///
			text(`y_usb' `t' "`usb_sig'", size(medium) color(navy)) ///
			text(`y_nat' `t' "`nat_sig'", size(medium) color(maroon)) ///
			text(`y_nczn' `t' "`nczn_sig'", size(medium) color(forest_green)) ///
			legend(order(1 "Latino U.S. Born" ///
						3 "Latino Naturalized" ///
						5 "Latino Noncitizens" 7 "Non-Hispanic White & Black") ///
						size(tiny) rows(1)) ///
			title("`x'") ///
			xtitle("Date", size(small)) ytitle("Predicted Anxiety", size(small)) ///
			xlab(`s'(30)`f', format(%td+Mon+YY) labsize(vsmall)) ///
			ylab(0(0.5)1,labsize(vsmall) nogrid) ///
			scheme(s2mono) ///
			graphregion(fcolor(white))
			tokenize `x' 
			loc first_two_words `1' `2'
			loc name "`first_two_words'" 
			
		
			graph save "RDFig_`name'_K6anx.gph", replace
}

foreach x of local events {

		// setting event cutoffs in days and constants
		
		if(strpos("`x'", "DAPA Announce") > 0) {
						local eventtime = 20046
						
						local usb_cons = 68.77
						local usb_datecoef = -0.00341
						local usb_cutoffcoef = 0.0545
						local usb_sig = ""
						
						local nat_cons = -163.1
						local nat_datecoef = 0.00816
						local nat_cutoffcoef = -0.350
						local nat_sig = "+"
						
						local nczn_cons = -157.2
						local nczn_datecoef = 0.00787
						local nczn_cutoffcoef = -0.262
						local nczn_sig = ""
						
						local nhwb_cons = -53.07
						local nhwb_datecoef = 0.00267
						local nhwb_cutoffcoef = -0.0861
						local nhwb_sig = "+"
					}
		else if(strpos("`x'", "Trump Elected") > 0) {
						local eventtime = 20765
						
						local usb_cons = -40.16
						local usb_datecoef = 0.00195
						local usb_cutoffcoef = -0.117
						local usb_sig = ""
						
						local nat_cons = 33.48
						local nat_datecoef = -0.00160
						local nat_cutoffcoef = -0.0269
						local nat_sig = ""
						
						local nczn_cons = 25.52
						local nczn_datecoef = -0.00122
						local nczn_cutoffcoef = 0.310
						local nczn_sig = "*"
						
						local nhwb_cons = -8.200
						local nhwb_datecoef = 0.000412
						local nhwb_cutoffcoef = -0.0440
						local nhwb_sig = ""
					}
		
		local s = `eventtime' - 30 // start bandwidth interval
		local f = `eventtime' + 30 //finish bandwidth interval 
		
		local y_usb = `usb_cons' + `usb_datecoef'*`eventtime' + `usb_cutoffcoef'
		local y_nat = `nat_cons' + `nat_datecoef'*`eventtime' + `nat_cutoffcoef' - 0.01
		local y_nczn = `nczn_cons' + `nczn_datecoef'*`eventtime' + `nczn_cutoffcoef' + 0.01
		local y_nhwb = `nhwb_cons' + `nhwb_datecoef'*`eventtime' + `nhwb_cutoffcoef' - 0.03
		local t = `eventtime' + 5
		
		twoway ///
			(function ypred =  `usb_cons' + `usb_datecoef'*x, range(`s' `eventtime') lcolor(navy) lpattern(solid)) ///
			(function ypred = `usb_cons' + `usb_datecoef'*x + `usb_cutoffcoef', range(`eventtime' `f') lcolor(navy) lpattern(solid)) ///
			(function ypred =  `nat_cons' + `nat_datecoef'*x, range(`s' `eventtime') lcolor(maroon) lpattern(solid)) ///
			(function ypred = `nat_cons' + `nat_datecoef'*x + `nat_cutoffcoef', range(`eventtime' `f') lcolor(maroon) lpattern(solid)) ///
			(function ypred =  `nczn_cons' + `nczn_datecoef'*x, range(`s' `eventtime') lcolor(forest_green) lpattern(solid)) ///
			(function ypred = `nczn_cons' + `nczn_datecoef'*x + `nczn_cutoffcoef', range(`eventtime' `f') lcolor(forest_green) lpattern(solid)) ///
			(function ypred =  `nhwb_cons' + `nhwb_datecoef'*x, range(`s' `eventtime') lcolor(purple) lpattern(dash)) ///
			(function ypred = `nhwb_cons' + `nhwb_datecoef'*x + `nhwb_cutoffcoef', range(`eventtime' `f') lcolor(purple) lpattern(dash)) ///
			, ///
			xline(`eventtime', lpattern(dash) lcolor(red)) ///
			text(`y_usb' `t' "`usb_sig'", size(medium) color(navy)) ///
			text(`y_nat' `t' "`nat_sig'", size(small) color(maroon)) ///
			text(`y_nczn' `t' "`nczn_sig'", size(medium) color(forest_green)) ///
			text(`y_nhwb' `t' "`nhwb_sig'", size(small) color(purple)) ///
			legend(order(1 "Latino U.S. Born" ///
						3 "Latino Naturalized" ///
						5 "Latino Noncitizens" 7 "Non-Hispanic White & Black") ///
						size(tiny) rows(1)) ///
			title("`x'") ///
			xtitle("Date", size(small)) ytitle("Predicted Depression", size(small)) ///
			xlab(`s'(30)`f', angle(45) format(%td+Mon+YY) labsize(vsmall)) ///
			ylab(0(0.5)1,labsize(vsmall) nogrid) ///
			scheme(s2mono) ///
			graphregion(fcolor(white))
			tokenize `x' 
			loc first_two_words `1' `2'
			loc name "`first_two_words'" 
			
		
			graph save "RDFig_`name'_K6dep.gph", replace
}

* FIGURE S3

local var `" "Psychological Distress" "Severe Distress" "Depression" "Anxiety" "'

foreach v of local var {
	
	local eventtime = 20765
	
	if(strpos("`v'", "Psychological") > 0) {
	
		local mxca_cons = 214.2
		local mxca_datecoef = -0.0103
		local mxca_cutoffcoef = 1.853
		local mxca_sig = "*"
		
		local ot_cons = -992.1
		local ot_datecoef = 0.0478
		local ot_cutoffcoef = 3.348
		local ot_sig = "*"
		
		local min = 0
		local int = 1
		local max = 5
	
	}
	
	else if(strpos("`v'", "Severe") > 0) {
	
		local mxca_cons = -45.62
		local mxca_datecoef = 0.00220
		local mxca_cutoffcoef = 0.0776
		local mxca_sig = "" //logit model matches
		
		local ot_cons = -213.2
		local ot_datecoef = 0.0103
		local ot_cutoffcoef = 0.123
		local ot_sig = "" //logit didn't run (?) N of 9. 
		
		local min = 0
		local int = 0.5
		local max = 1
	
	}
	
	else if(strpos("`v'", "Depression") > 0) {
	
		local mxca_cons = 35.06
		local mxca_datecoef = -0.00168
		local mxca_cutoffcoef = 0.286
		local mxca_sig = "+"
		
		local ot_cons = -71.22
		local ot_datecoef = 0.00343
		local ot_cutoffcoef = 0.506
		local ot_sig = "*"
		
		local min = 0
		local int = 0.5
		local max = 1
	
	}
						
	else if(strpos("`v'", "Anxiety") > 0) {
	
		local mxca_cons = 37.00
		local mxca_datecoef = -0.00178
		local mxca_cutoffcoef = 0.354
		local mxca_sig = "*"
		
		local ot_cons = -353.6
		local ot_datecoef = 0.0170
		local ot_cutoffcoef = 0.661
		local ot_sig = ""
		
		local min = -1
		local int = 0.5
		local max = 1
	
	}					

	local s = `eventtime' - 30 // start bandwidth interval
	local f = `eventtime' + 30 //finish bandwidth interval 
	
	local y_mxca = `mxca_cons' + `mxca_datecoef'*`eventtime' + 0.6*`mxca_cutoffcoef'
	local y_ot = `ot_cons' + `ot_datecoef'*`eventtime' + 1.3*`ot_cutoffcoef'
	local t = `eventtime' + 5
	
	twoway ///
		(function ypred =  `mxca_cons' + `mxca_datecoef'*x, range(`s' `eventtime') lcolor(navy) lpattern(solid)) ///
		(function ypred = `mxca_cons' + `mxca_datecoef'*x + `mxca_cutoffcoef', range(`eventtime' `f') lcolor(navy) lpattern(solid)) ///
		(function ypred =  `ot_cons' + `ot_datecoef'*x, range(`s' `eventtime') lcolor(maroon) lpattern(solid)) ///
		(function ypred = `ot_cons' + `ot_datecoef'*x + `ot_cutoffcoef', range(`eventtime' `f') lcolor(maroon) lpattern(solid)) ///
		, ///
		xline(`eventtime', lpattern(dash) lcolor(red)) ///
		text(`y_mxca' `t' "`mxca_sig'", size(medium) color(navy)) ///
		text(`y_ot' `t' "`ot_sig'", size(medium) color(maroon)) ///
		legend(order(1 "Mexican/Central American" ///
					3 "Other") ///
					size(tiny) rows(1)) ///
		title("`v'") ///
		xtitle("Date", size(small)) ytitle("Predicted Outcome", size(small)) ///
		xlab(`s'(30)`f', angle(45) format(%td+Mon+YY) labsize(vsmall)) ///
		ylab(`min'(`int')`max',labsize(vsmall) nogrid) ///
		scheme(s2mono) ///
		graphregion(fcolor(white))
		tokenize `v' 
		loc first_two_words `1' `2'
		loc name "`first_two_words'" 
		
		graph save "RDFig_TrumpElected_ByEthnicity_`name'.gph", replace
	
}


grc1leg "RDFig_TrumpElected_ByEthnicity_Psychological Distress.gph" ///
		"RDFig_TrumpElected_ByEthnicity_Severe Distress.gph" ///
		"RDFig_TrumpElected_ByEthnicity_Anxiety.gph" ///
		"RDFig_TrumpElected_ByEthnicity_Depression.gph" ///
		, r(2) legendfrom("RDFig_TrumpElected_ByEthnicity_Anxiety.gph") name(combined, replace) graphregion(color(white))

graph display combined, xsize(10) ysize(8)
graph export "RDFig_TrumpXEthnicity.png", replace

* FIGURE S4
local events `" "DACA Announcement 06/15/12" "DACA Rescission 09/05/17" "DAPA Announcement 11/20/14" "DAPA Injunction 02/16/15" "DAPA Rescission 06/15/17" "Trump Elected 11/08/16" "'

foreach x of local events {

		// setting event cutoffs in days and constants
		
		if(strpos("`x'", "DACA Announce") > 0) {
						local eventtime = 19159

						local w_cons = -81.30
						local w_datecoef = 0.00436
						local w_cutoffcoef = -0.263
						local w_sig = ""
						
						local b_cons = -8.312
						local b_datecoef = 0.000554
						local b_cutoffcoef = -0.503
						local b_sig = ""
						
					}
		else if(strpos("`x'", "DACA Rescis") > 0) {
						local eventtime = 21067
						
						local w_cons = 81.79
						local w_datecoef = -0.00376
						local w_cutoffcoef = 0.240
						local w_sig = ""
						
						local b_cons = 131.1
						local b_datecoef = -0.00613
						local b_cutoffcoef = 1.350
						local b_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Announce") > 0) {
						local eventtime = 20046
						
						local w_cons = -259.5
						local w_datecoef = 0.0131
						local w_cutoffcoef = -0.496
						local w_sig = ""
						
						local b_cons = -193.5
						local b_datecoef = 0.00980
						local b_cutoffcoef = -0.0228
						local b_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Injunct") > 0) {
						local eventtime = 20134
						
						local w_cons = 27.68
						local w_datecoef = -0.00125
						local w_cutoffcoef = 0.00258
						local w_sig = ""
						
						local b_cons = 134.5
						local b_datecoef = -0.00654
						local b_cutoffcoef = 0.287
						local b_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Rescis") > 0) {
						local eventtime = 20985
						
						local w_cons = 166.2
						local w_datecoef = -0.00779
						local w_cutoffcoef = 0.116
						local w_sig = ""
						
						local b_cons = -579.0
						local b_datecoef = 0.0277
						local b_cutoffcoef = -0.652
						local b_sig = ""
						
					}
		else if(strpos("`x'", "Trump Elected") > 0) {
						local eventtime = 20765
						
						local w_cons = 201.0
						local w_datecoef = -0.00955
						local w_cutoffcoef = 0.146
						local w_sig = ""
						
						local b_cons = -602.0
						local b_datecoef = 0.0291
						local b_cutoffcoef = -1.216
						local b_sig = "+"
						
					}
		
		local s = `eventtime' - 30 // start bandwidth interval
		local f = `eventtime' + 30 //finish bandwidth interval 
		
		local y_w = `w_cons' + `w_datecoef'*`eventtime' + `w_cutoffcoef'
		local y_b = `b_cons' + `b_datecoef'*`eventtime' + `b_cutoffcoef'
		local t = `eventtime' + 5
		
		twoway ///
			(function ypred =  `w_cons' + `w_datecoef'*x, range(`s' `eventtime') lcolor(navy) lpattern(solid)) ///
			(function ypred = `w_cons' + `w_datecoef'*x + `w_cutoffcoef', range(`eventtime' `f') lcolor(navy) lpattern(solid)) ///
			(function ypred =  `b_cons' + `b_datecoef'*x, range(`s' `eventtime') lcolor(maroon) lpattern(solid)) ///
			(function ypred = `b_cons' + `b_datecoef'*x + `b_cutoffcoef', range(`eventtime' `f') lcolor(maroon) lpattern(solid)) ///
			, ///
			xline(`eventtime', lpattern(dash) lcolor(red)) ///
			text(`y_w' `t' "`w_sig'", size(medium) color(navy)) ///
			text(`y_b' `t' "`b_sig'", size(medium) color(maroon)) ///
			legend(order(1 "Non-Hispanic White" 3 "Non-Hispanic Black") ///
						size(tiny) rows(1)) ///
			title("`x'") ///
			xtitle("Date", size(small)) ytitle("Predicted K6", size(small)) ///
			xlab(`s'(30)`f', format(%td+Mon+YY) labsize(vsmall)) ///
			ylab(0(1)4,labsize(vsmall) nogrid) ///
			scheme(s2mono) ///
			graphregion(fcolor(white))
			tokenize `x' 
			loc first_two_words `1' `2'
			loc name "`first_two_words'" 
			
			graph save "RDFig_`name'_K6_BW.gph", replace
}


* combining K6 graphs
grc1leg "RDFig_DACA Announcement_K6_BW.gph" ///
		"RDFig_DAPA Announcement_K6_BW.gph" ///
		"RDFig_DAPA Injunction_K6_BW.gph" ///
		"RDFig_Trump Elected_K6_BW.gph" ///
		"RDFig_DAPA Rescission_K6_BW.gph" ///
		"RDFig_DACA Rescission_K6_BW.gph" ///
		, r(2) legendfrom("RDFig_DACA Announcement_K6_BW.gph") name(combined, replace) graphregion(color(white))

graph display combined, xsize(12) ysize(8)
graph export "RDFig_K6_BW.png", replace

* FIGURE S5

local events `" "DACA Announcement 06/15/12" "DACA Rescission 09/05/17" "DAPA Announcement 11/20/14" "DAPA Injunction 02/16/15" "DAPA Rescission 06/15/17" "Trump Elected 11/08/16" "'

foreach x of local events {

		// setting event cutoffs in days and constants
		
		if(strpos("`x'", "DACA Announce") > 0) {
						local eventtime = 19159

						local w_cons = -13.05
						local w_datecoef = 0.000689
						local w_cutoffcoef = -0.0214
						local w_sig = ""
						
						local b_cons = -7.465
						local b_datecoef = 0.000398
						local b_cutoffcoef = -0.0169
						local b_sig = ""
						
					}
		else if(strpos("`x'", "DACA Rescis") > 0) {
						local eventtime = 21067
						
						local w_cons = 11.46
						local w_datecoef = -0.000535
						local w_cutoffcoef = 0.0285
						local w_sig = ""
						
						local b_cons = 28.99
						local b_datecoef = -0.00137
						local b_cutoffcoef = 0.130
						local b_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Announce") > 0) {
						local eventtime = 20046
						
						local w_cons = 0.939
						local w_datecoef = -3.70e-05
						local w_cutoffcoef = 0.00254
						local w_sig = ""
						
						local b_cons = -14.82
						local b_datecoef = 0.000751
						local b_cutoffcoef = 0.00815
						local b_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Injunct") > 0) {
						local eventtime = 20134
						
						local w_cons = -1.584
						local w_datecoef =  8.85e-05
						local w_cutoffcoef = -0.0127
						local w_sig = ""
						
						local b_cons = 50.68
						local b_datecoef = -0.00251
						local b_cutoffcoef = 0.0941
						local b_sig = ""
						
					}
		else if(strpos("`x'", "DAPA Rescis") > 0) {
						local eventtime = 20985
						
						local w_cons = 35.90
						local w_datecoef = -0.00170
						local w_cutoffcoef = 0.0245
						local w_sig = ""
						
						local b_cons = -54.80
						local b_datecoef = 0.00262
						local b_cutoffcoef = -0.0691
						local b_sig = ""
						
					}
		else if(strpos("`x'", "Trump Elected") > 0) {
						local eventtime = 20765
						
						local w_cons = 9.796
						local w_datecoef = -0.000461
						local w_cutoffcoef = 0.00217
						local w_sig = ""
						
						local b_cons = -48.56
						local b_datecoef = 0.00235
						local b_cutoffcoef = -0.128
						local b_sig = ""
						
					}
		
		local s = `eventtime' - 30 // start bandwidth interval
		local f = `eventtime' + 30 //finish bandwidth interval 
		
		local y_w = `w_cons' + `w_datecoef'*`eventtime' + `w_cutoffcoef'
		local y_b = `b_cons' + `b_datecoef'*`eventtime' + `b_cutoffcoef'
		local t = `eventtime' + 5
		
		twoway ///
			(function ypred =  `w_cons' + `w_datecoef'*x, range(`s' `eventtime') lcolor(navy) lpattern(solid)) ///
			(function ypred = `w_cons' + `w_datecoef'*x + `w_cutoffcoef', range(`eventtime' `f') lcolor(navy) lpattern(solid)) ///
			(function ypred =  `b_cons' + `b_datecoef'*x, range(`s' `eventtime') lcolor(maroon) lpattern(solid)) ///
			(function ypred = `b_cons' + `b_datecoef'*x + `b_cutoffcoef', range(`eventtime' `f') lcolor(maroon) lpattern(solid)) ///
			, ///
			xline(`eventtime', lpattern(dash) lcolor(red)) ///
			text(`y_w' `t' "`w_sig'", size(medium) color(navy)) ///
			text(`y_b' `t' "`b_sig'", size(medium) color(maroon)) ///
			legend(order(1 "Non-Hispanic White" 3 "Non-Hispanic Black") ///
						size(tiny) rows(1)) ///
			title("`x'") ///
			xtitle("Date", size(small)) ytitle("Proportion K6 >= 5", size(small)) ///
			xlab(`s'(30)`f', format(%td+Mon+YY) labsize(vsmall)) ///
			ylab(0(.1)0.5,labsize(vsmall) nogrid) ///
			scheme(s2mono) ///
			graphregion(fcolor(white))
			tokenize `x' 
			loc first_two_words `1' `2'
			loc name "`first_two_words'" 
			
			graph save "RDFig_`name'_K6bin_BW.gph", replace
}


* combining K6 graphs
grc1leg "RDFig_DACA Announcement_K6bin_BW.gph" ///
		"RDFig_DAPA Announcement_K6bin_BW.gph" ///
		"RDFig_DAPA Injunction_K6bin_BW.gph" ///
		"RDFig_Trump Elected_K6bin_BW.gph" ///
		"RDFig_DAPA Rescission_K6bin_BW.gph" ///
		"RDFig_DACA Rescission_K6bin_BW.gph" ///
		, r(2) legendfrom("RDFig_DACA Announcement_K6bin_BW.gph") name(combined, replace) graphregion(color(white))

graph display combined, xsize(12) ysize(8)
graph export "RDFig_K6bin_BW.png", replace

***
* DESCRIPTIVE STATISTICS
***

use nhis_clean.dta, clear

gen sampweight2=sampweight/8
label variable sampweight2 "Sampling Weights Adjusted by dividing by the number of years"
svyset psu [pweight = sampweight2], strata(strata)

// variables for NH W & B CITIZENS

tab race2, gen(race_)
gen nh_blackwhite = 0
replace nh_blackwhite = 1 if (race_1 == 1 | race_2 == 1) & citizen2==2

gen nh_white = 0
replace nh_white = 1 if race_1 == 1 & citizen2==2

gen nh_black = 0
replace nh_black = 1 if race_2 == 1 & citizen2==2

// country of origin
gen h_mc = 1 if hispeth == 20 | hispeth == 23 | hispeth == 61
label var h_mc "Mexican, Central and South American"
gen h_ot = 1 if h_mc != 1 & hispeth != 10
label var h_ot "Other Hispanic"


***
* Latinos
* TABLE S2
***

* DACA

foreach x of varlist daca_announce_d daca_rescission_d {

foreach j of varlist usb nat nczn {
	
	// loop command for continuous variables
	
	foreach v of varlist k6 k6_anx k6_depr age yrsinus {
		
		tabstat k6 if `j'==1 & within_`x'==1, statistics (n) by(`x') save
		return list
		
		matrix list r(StatTotal)
		matrix list r(Stat1)
		matrix list r(Stat2)
		
		matrix n = J(1,3,.)
		
		matrix n[1,1] = r(StatTotal)
		matrix n[1,2] = r(Stat1)
		matrix n[1,3] = r(Stat2)
		
		local n_total = n[1,1]
		local n_before = n[1,2]
		local n_after = n[1,3]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v'
		estat sd
		
		matrix `v'_all = J(1,2,.)
		
		matrix `v'_all[1,1] = r(mean)
		matrix `v'_all[1,2] = r(sd)
		
		local `v'_beta = `v'_all[1,1]
		local `v'_sd = `v'_all[1,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): regress `v' i.`x'
		return list
		matrix list r(table)
		
		matrix `v'_daca = r(table)
		
		local `v'_beta0 = `v'_daca[1,3]
		local `v'_beta1 = `v'_daca[1,3] + `v'_daca[1,2]
		local `v'_p = `v'_daca[4,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v', over(`x')
		estat sd
		
		matrix `v'_sd_daca = r(sd)
		
		local `v'_sd0 = `v'_sd_daca[1,1]
		local `v'_sd1 = `v'_sd_daca[1,2]
	}
	
	// loop command for binary variables
	
	foreach v of varlist k6_bin sex hispyn marstcohab2 parent child18 child5 intervlang2 health_b h_mc h_ot {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all=e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_daca = e(Prop)
		mata: st_matrix("`v'_daca", (st_matrix("`v'_daca") :/ colsum(st_matrix("`v'_daca"))))
		
		mat li `v'_daca
		
		local `v'_dist3 = `v'_daca[1,1]*100
		local `v'_dist4 = `v'_daca[1,2]*100
		local `v'_dist5 = `v'_daca[2,1]*100
		local `v'_dist6 = `v'_daca[2,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// loop command for categorical variables
	
	foreach v of varlist race2 income2 region {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all = e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		local `v'_dist3 = `v'_all[3,1]*100
		local `v'_dist4 = `v'_all[4,1]*100
		local `v'_dist5 = `v'_all[5,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_daca = e(Prop)
		mata: st_matrix("`v'_daca", (st_matrix("`v'_daca") :/ colsum(st_matrix("`v'_daca"))))
		
		mat li `v'_daca
		
		local `v'_dist6 = `v'_daca[1,1]*100
		local `v'_dist7 = `v'_daca[1,2]*100
		local `v'_dist8 = `v'_daca[2,1]*100
		local `v'_dist9 = `v'_daca[2,2]*100
		local `v'_dist10 = `v'_daca[3,1]*100
		local `v'_dist11 = `v'_daca[3,2]*100
		local `v'_dist12 = `v'_daca[4,1]*100
		local `v'_dist13 = `v'_daca[4,2]*100
		local `v'_dist14 = `v'_daca[5,1]*100
		local `v'_dist15 = `v'_daca[5,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// build table from stored results
	
	quietly {
		
		log using "table1_Latino.csv", append text name(table)
		
		* header
		noisily di " "
		noisily di " "
		noisily di "Table for `x' `j'"
		noisily di "Variable,Level,Entire Subsample,Before `x',After `x',p-value"
		* row 0
		noisily di "n," " ," `n_total' "," `n_before' "," `n_after' ","
		* row 1
		noisily di "K6," " ," %4.3f `k6_beta' " (" %4.3f `k6_sd' ") ," %4.3f `k6_beta0' " (" %4.3f `k6_sd0' ") ," %4.3f `k6_beta1' " (" %4.3f `k6_sd1' ") ," %4.3f `k6_p'
		noisily di "K6 Anx," " ," %4.3f `k6_anx_beta' " (" %4.3f `k6_anx_sd' ") ," %4.3f `k6_anx_beta0' " (" %4.3f `k6_anx_sd0' ") ," %4.3f `k6_anx_beta1' " (" %4.3f `k6_anx_sd1' ") ," %4.3f `k6_anx_p'
		noisily di "K6 Depr," " ," %4.3f `k6_depr_beta' " (" %4.3f `k6_depr_sd' ") ," %4.3f `k6_depr_beta0' " (" %4.3f `k6_depr_sd0' ") ," %4.3f `k6_depr_beta1' " (" %4.3f `k6_depr_sd1' ") ," %4.3f `k6_depr_p'
		* row 2
		noisily di "Binary version of K6 scale," "0 = K6<5," %4.3f `k6_bin_dist1' "%," %4.3f `k6_bin_dist3' "%," %4.3f `k6_bin_dist4' "%" " ," %4.3f `k6_bin_p'
		* row 3
		noisily di " ," "1 = K6>=5," %4.3f `k6_bin_dist2' "%," %4.3f `k6_bin_dist5' "%," %4.3f `k6_bin_dist6' "%"
		* row 4
		noisily di "Age (years)," " ," %4.3f `age_beta' " (" %4.3f `age_sd' ") ," %4.3f `age_beta0' " (" %4.3f `age_sd0' ") ," %4.3f `age_beta1' " (" %4.3f `age_sd1' ") ," %4.3f `age_p'
		* row 5
		* row 6
		noisily di "Sex," "0 = female," %4.3f `sex_dist1' "%," %4.3f `sex_dist3' "%," %4.3f `sex_dist4' "%" " ," %4.3f `sex_p'
		* row 7
		noisily di " ," "1 = male," %4.3f `sex_dist2' "%," %4.3f `sex_dist5' "%," %4.3f `sex_dist6' "%"
		* row 8
		noisily di "Hispanic," "0 = no," %4.3f `hispyn_dist1' "%," %4.3f `hispyn_dist3' "%," %4.3f `hispyn_dist4' "%" " ," %4.3f `hispyn_p'
		* row 9
		noisily di " ," "1 = yes," %4.3f `hispyn_dist2' "%," %4.3f `hispyn_dist5' "%," %4.3f `hispyn_dist6' "%"
		* row 10
		noisily di "Race," "Non-Hispanic White," %4.3f `race2_dist1' "%," %4.3f `race2_dist6' "%," %4.3f `race2_dist7' "%" " ," %4.3f `race2_p'
		* row 11
		noisily di " ," "Non-Hispanic Black," %4.3f `race2_dist2' "%," %4.3f `race2_dist8' "%," %4.3f `race2_dist9' "%"
		* row 12
		noisily di " ," "Hispanic," %4.3f `race2_dist3' "%," %4.3f `race2_dist10' "%," %4.3f `race2_dist11' "%"
		* row 13
		noisily di " ," "Asian," %4.3f `race2_dist4' "%," %4.3f `race2_dist12' "%," %4.3f `race2_dist13' "%"
		* row 14
		noisily di " ," "Other," %4.3f `race2_dist5' "%," %4.3f `race2_dist14' "%," %4.3f `race2_dist15' "%"
		* row 15
		noisily di "Income," "$0-49999," %4.3f `income2_dist1' "%," %4.3f `income2_dist6' "%," %4.3f `income2_dist7' "%" " ," %4.3f `income2_p'
		* row 16
		noisily di " ," "$50000 and over," %4.3f `income2_dist2' "%," %4.3f `income2_dist8' "%," %4.3f `income2_dist9' "%"
		* row 17
		noisily di " ," "Unknown," %4.3f `income2_dist3' "%," %4.3f `income2_dist10' "%," %4.3f `income2_dist11' "%"
		* row 18
		* row 19
		noisily di "Years lived in USA," " ," %4.3f `yrsinus_beta' " (" %4.3f `yrsinus_sd' ") ," %4.3f `yrsinus_beta0' " (" %4.3f `yrsinus_sd0' ") ," %4.3f `yrsinus_beta1' " (" %4.3f `yrsinus_sd1' ") ," %4.3f `yrsinus_p'
		* row 20
		noisily di "Region of residence," "Northeast," %4.3f `region_dist1' "%," %4.3f `region_dist6' "%," %4.3f `region_dist7' "%" " ," %4.3f `region_p'
		* row 21
		noisily di " ," "North Central/Midwest," %4.3f `region_dist2' "%," %4.3f `region_dist8' "%," %4.3f `region_dist9' "%"
		* row 22
		noisily di " ," "South," %4.3f `region_dist3' "%," %4.3f `region_dist10' "%," %4.3f `region_dist11' "%"
		* row 23
		noisily di " ," "West," %4.3f `region_dist4' "%," %4.3f `region_dist12' "%," %4.3f `region_dist13' "%"
		* row 24
		noisily di "Living Status," "0 = Single," %4.3f `marstcohab2_dist1' "%," %4.3f `marstcohab2_dist3' "%," %4.3f `marstcohab2_dist4' "%" " ," %4.3f `marstcohab2_p'
		* row 25
		noisily di " ," "1 = Married or Cohabiting," %4.3f `marstcohab2_dist2' "%," %4.3f `marstcohab2_dist5' "%," %4.3f `marstcohab2_dist6' "%"
		* row 26
		noisily di "Is a Parent," "0 = no," %4.3f `parent_dist1' "%," %4.3f `parent_dist3' "%," %4.3f `parent_dist4' "%" " ," %4.3f `parent_p'
		* row 27
		noisily di " ," "1 = yes," %4.3f `parent_dist2' "%," %4.3f `parent_dist5' "%," %4.3f `parent_dist6' "%"
		* row 28
		noisily di "Has Children Under 18," "0 = no," %4.3f `child18_dist1' "%," %4.3f `child18_dist3' "%," %4.3f `child18_dist4' "%" " ," %4.3f `child18_p'
		* row 29
		noisily di " ," "1 = yes," %4.3f `child18_dist2' "%," %4.3f `child18_dist5' "%," %4.3f `child18_dist6' "%"
		* row 30
		noisily di "Has Children Under 5," "0 = no," %4.3f `child5_dist1' "%," %4.3f `child5_dist3' "%," %4.3f `child5_dist4' "%" " ," %4.3f `child5_p'
		* row 31
		noisily di " ," "1 = yes," %4.3f `child5_dist2' "%," %4.3f `child5_dist5' "%," %4.3f `child5_dist6' "%"
		* row 32
		noisily di "Interview Language," "0 = Otherwise," %4.3f `intervlang2_dist1' "%," %4.3f `intervlang2_dist3' "%," %4.3f `intervlang2_dist4' "%" " ," %4.3f `intervlang2_p'
		* row 33
		noisily di " ," "1 = English," %4.3f `intervlang2_dist2' "%," %4.3f `intervlang2_dist5' "%," %4.3f `intervlang2_dist6' "%"
		* row 34
		noisily di "Self-rated Health," "0 = Otherwise," %4.3f `health_b_dist1' "%," %4.3f `health_b_dist3' "%," %4.3f `health_b_dist4' "%" " ," %4.3f `health_b_p'
		* row 35
		noisily di " ," "1 = Excellent/Very Good," %4.3f `health_b_dist2' "%," %4.3f `health_b_dist5' "%," %4.3f `health_b_dist6' "%"
		noisily di "Mexican/Central/South American Ethnicity," "0 = No," %4.3f `h_mc_dist1' "%," %4.3f `h_mc_dist3' "%," %4.3f `h_mc_dist4' "%" " ," %4.3f `h_mc_p'
		* row 7
		noisily di " ," "1 = Yes," %4.3f `h_mc_dist2' "%," %4.3f `h_mc_dist5' "%," %4.3f `h_mc_dist6' "%"
		* row 8
		noisily di "Other Ethnicity," "0 = No," %4.3f `h_ot_dist1' "%," %4.3f `h_ot_dist3' "%," %4.3f `h_ot_dist4' "%" " ," %4.3f `h_ot_p'
		* row 7
		noisily di " ," "1 = Yes," %4.3f `h_ot_dist2' "%," %4.3f `h_ot_dist5' "%," %4.3f `h_ot_dist6' "%"
		
		log close table
	}
}

}


* DAPA

foreach x of varlist dapa_announce_d dapa_injunction_d dapa_rescission_d  {

foreach j of varlist usb nat nczn {
	
	// loop command for continuous variables
	
	foreach v of varlist k6 k6_anx k6_depr age yrsinus {
	
		tabstat k6 if `j'==1 & within_`x'==1, statistics (n) by(`x') save
		return list
		
		matrix list r(StatTotal)
		matrix list r(Stat1)
		matrix list r(Stat2)
		
		matrix n = J(1,3,.)
		
		matrix n[1,1] = r(StatTotal)
		matrix n[1,2] = r(Stat1)
		matrix n[1,3] = r(Stat2)
		
		local n_total = n[1,1]
		local n_before = n[1,2]
		local n_after = n[1,3]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v'
		estat sd
		
		matrix `v'_all = J(1,2,.)
		
		matrix `v'_all[1,1] = r(mean)
		matrix `v'_all[1,2] = r(sd)
		
		local `v'_beta = `v'_all[1,1]
		local `v'_sd = `v'_all[1,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): regress `v' i.`x'
		return list
		matrix list r(table)
		
		matrix `v'_dapa = r(table)
		
		local `v'_beta0 = `v'_dapa[1,3]
		local `v'_beta1 = `v'_dapa[1,3] + `v'_dapa[1,2]
		local `v'_p = `v'_dapa[4,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v', over(`x')
		estat sd
		
		matrix `v'_sd_dapa = r(sd)
		
		local `v'_sd0 = `v'_sd_dapa[1,1]
		local `v'_sd1 = `v'_sd_dapa[1,2]
	}
	
	// loop command for binary variables
	
	foreach v of varlist k6_bin sex hispyn marstcohab2 parent child18 child5 intervlang2 health_b h_mc h_ot {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all=e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_dapa = e(Prop)
		mata: st_matrix("`v'_dapa", (st_matrix("`v'_dapa") :/ colsum(st_matrix("`v'_dapa"))))
		
		mat li `v'_dapa
		
		local `v'_dist3 = `v'_dapa[1,1]*100
		local `v'_dist4 = `v'_dapa[1,2]*100
		local `v'_dist5 = `v'_dapa[2,1]*100
		local `v'_dist6 = `v'_dapa[2,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// loop command for categorical variables
	
	foreach v of varlist race2 income2 region {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all = e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		local `v'_dist3 = `v'_all[3,1]*100
		local `v'_dist4 = `v'_all[4,1]*100
		local `v'_dist5 = `v'_all[5,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_dapa = e(Prop)
		mata: st_matrix("`v'_dapa", (st_matrix("`v'_dapa") :/ colsum(st_matrix("`v'_dapa"))))
		
		mat li `v'_dapa
		
		local `v'_dist6 = `v'_dapa[1,1]*100
		local `v'_dist7 = `v'_dapa[1,2]*100
		local `v'_dist8 = `v'_dapa[2,1]*100
		local `v'_dist9 = `v'_dapa[2,2]*100
		local `v'_dist10 = `v'_dapa[3,1]*100
		local `v'_dist11 = `v'_dapa[3,2]*100
		local `v'_dist12 = `v'_dapa[4,1]*100
		local `v'_dist13 = `v'_dapa[4,2]*100
		local `v'_dist14 = `v'_dapa[5,1]*100
		local `v'_dist15 = `v'_dapa[5,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// build table from stored results
	
	quietly {
		
		log using "table1_Latino.csv", append text name(table)
		
		* header
		noisily di " "
		noisily di " "
		noisily di "Table for `x' `j'"
		noisily di "Variable,Level,Entire Subsample,Before `x',After `x',p-value"
		* row 0
		noisily di "n," " ," `n_total' "," `n_before' "," `n_after' ","
		* row 1
		noisily di "K6," " ," %4.3f `k6_beta' " (" %4.3f `k6_sd' ") ," %4.3f `k6_beta0' " (" %4.3f `k6_sd0' ") ," %4.3f `k6_beta1' " (" %4.3f `k6_sd1' ") ," %4.3f `k6_p'
		noisily di "K6 Anx," " ," %4.3f `k6_anx_beta' " (" %4.3f `k6_anx_sd' ") ," %4.3f `k6_anx_beta0' " (" %4.3f `k6_anx_sd0' ") ," %4.3f `k6_anx_beta1' " (" %4.3f `k6_anx_sd1' ") ," %4.3f `k6_anx_p'
		noisily di "K6 Depr," " ," %4.3f `k6_depr_beta' " (" %4.3f `k6_depr_sd' ") ," %4.3f `k6_depr_beta0' " (" %4.3f `k6_depr_sd0' ") ," %4.3f `k6_depr_beta1' " (" %4.3f `k6_depr_sd1' ") ," %4.3f `k6_depr_p'
		* row 2
		noisily di "Binary version of K6 scale," "0 = K6<5," %4.3f `k6_bin_dist1' "%," %4.3f `k6_bin_dist3' "%," %4.3f `k6_bin_dist4' "%" " ," %4.3f `k6_bin_p'
		* row 3
		noisily di " ," "1 = K6>=5," %4.3f `k6_bin_dist2' "%," %4.3f `k6_bin_dist5' "%," %4.3f `k6_bin_dist6' "%"
		* row 4
		noisily di "Age (years)," " ," %4.3f `age_beta' " (" %4.3f `age_sd' ") ," %4.3f `age_beta0' " (" %4.3f `age_sd0' ") ," %4.3f `age_beta1' " (" %4.3f `age_sd1' ") ," %4.3f `age_p'
		* row 5
		* row 6
		noisily di "Sex," "0 = female," %4.3f `sex_dist1' "%," %4.3f `sex_dist3' "%," %4.3f `sex_dist4' "%" " ," %4.3f `sex_p'
		* row 7
		noisily di " ," "1 = male," %4.3f `sex_dist2' "%," %4.3f `sex_dist5' "%," %4.3f `sex_dist6' "%"
		* row 8
		noisily di "Hispanic," "0 = no," %4.3f `hispyn_dist1' "%," %4.3f `hispyn_dist3' "%," %4.3f `hispyn_dist4' "%" " ," %4.3f `hispyn_p'
		* row 9
		noisily di " ," "1 = yes," %4.3f `hispyn_dist2' "%," %4.3f `hispyn_dist5' "%," %4.3f `hispyn_dist6' "%"
		* row 10
		noisily di "Race," "Non-Hispanic White," %4.3f `race2_dist1' "%," %4.3f `race2_dist6' "%," %4.3f `race2_dist7' "%" " ," %4.3f `race2_p'
		* row 11
		noisily di " ," "Non-Hispanic Black," %4.3f `race2_dist2' "%," %4.3f `race2_dist8' "%," %4.3f `race2_dist9' "%"
		* row 12
		noisily di " ," "Hispanic," %4.3f `race2_dist3' "%," %4.3f `race2_dist10' "%," %4.3f `race2_dist11' "%"
		* row 13
		noisily di " ," "Asian," %4.3f `race2_dist4' "%," %4.3f `race2_dist12' "%," %4.3f `race2_dist13' "%"
		* row 14
		noisily di " ," "Other," %4.3f `race2_dist5' "%," %4.3f `race2_dist14' "%," %4.3f `race2_dist15' "%"
		* row 15
		noisily di "Income," "$0-49999," %4.3f `income2_dist1' "%," %4.3f `income2_dist6' "%," %4.3f `income2_dist7' "%" " ," %4.3f `income2_p'
		* row 16
		noisily di " ," "$50000 and over," %4.3f `income2_dist2' "%," %4.3f `income2_dist8' "%," %4.3f `income2_dist9' "%"
		* row 17
		noisily di " ," "Unknown," %4.3f `income2_dist3' "%," %4.3f `income2_dist10' "%," %4.3f `income2_dist11' "%"
		* row 18
		* row 19
		noisily di "Years lived in USA," " ," %4.3f `yrsinus_beta' " (" %4.3f `yrsinus_sd' ") ," %4.3f `yrsinus_beta0' " (" %4.3f `yrsinus_sd0' ") ," %4.3f `yrsinus_beta1' " (" %4.3f `yrsinus_sd1' ") ," %4.3f `yrsinus_p'
		* row 20
		noisily di "Region of residence," "Northeast," %4.3f `region_dist1' "%," %4.3f `region_dist6' "%," %4.3f `region_dist7' "%" " ," %4.3f `region_p'
		* row 21
		noisily di " ," "North Central/Midwest," %4.3f `region_dist2' "%," %4.3f `region_dist8' "%," %4.3f `region_dist9' "%"
		* row 22
		noisily di " ," "South," %4.3f `region_dist3' "%," %4.3f `region_dist10' "%," %4.3f `region_dist11' "%"
		* row 23
		noisily di " ," "West," %4.3f `region_dist4' "%," %4.3f `region_dist12' "%," %4.3f `region_dist13' "%"
		* row 24
		noisily di "Living Status," "0 = Single," %4.3f `marstcohab2_dist1' "%," %4.3f `marstcohab2_dist3' "%," %4.3f `marstcohab2_dist4' "%" " ," %4.3f `marstcohab2_p'
		* row 25
		noisily di " ," "1 = Married or Cohabiting," %4.3f `marstcohab2_dist2' "%," %4.3f `marstcohab2_dist5' "%," %4.3f `marstcohab2_dist6' "%"
		* row 26
		noisily di "Is a Parent," "0 = no," %4.3f `parent_dist1' "%," %4.3f `parent_dist3' "%," %4.3f `parent_dist4' "%" " ," %4.3f `parent_p'
		* row 27
		noisily di " ," "1 = yes," %4.3f `parent_dist2' "%," %4.3f `parent_dist5' "%," %4.3f `parent_dist6' "%"
		* row 28
		noisily di "Has Children Under 18," "0 = no," %4.3f `child18_dist1' "%," %4.3f `child18_dist3' "%," %4.3f `child18_dist4' "%" " ," %4.3f `child18_p'
		* row 29
		noisily di " ," "1 = yes," %4.3f `child18_dist2' "%," %4.3f `child18_dist5' "%," %4.3f `child18_dist6' "%"
		* row 30
		noisily di "Has Children Under 5," "0 = no," %4.3f `child5_dist1' "%," %4.3f `child5_dist3' "%," %4.3f `child5_dist4' "%" " ," %4.3f `child5_p'
		* row 31
		noisily di " ," "1 = yes," %4.3f `child5_dist2' "%," %4.3f `child5_dist5' "%," %4.3f `child5_dist6' "%"
		* row 32
		noisily di "Interview Language," "0 = Otherwise," %4.3f `intervlang2_dist1' "%," %4.3f `intervlang2_dist3' "%," %4.3f `intervlang2_dist4' "%" " ," %4.3f `intervlang2_p'
		* row 33
		noisily di " ," "1 = English," %4.3f `intervlang2_dist2' "%," %4.3f `intervlang2_dist5' "%," %4.3f `intervlang2_dist6' "%"
		* row 34
		noisily di "Self-rated Health," "0 = Otherwise," %4.3f `health_b_dist1' "%," %4.3f `health_b_dist3' "%," %4.3f `health_b_dist4' "%" " ," %4.3f `health_b_p'
		* row 35
		noisily di " ," "1 = Excellent/Very Good," %4.3f `health_b_dist2' "%," %4.3f `health_b_dist5' "%," %4.3f `health_b_dist6' "%"
		noisily di "Mexican/Central/South American Ethnicity," "0 = No," %4.3f `h_mc_dist1' "%," %4.3f `h_mc_dist3' "%," %4.3f `h_mc_dist4' "%" " ," %4.3f `h_mc_p'
		* row 7
		noisily di " ," "1 = Yes," %4.3f `h_mc_dist2' "%," %4.3f `h_mc_dist5' "%," %4.3f `h_mc_dist6' "%"
		* row 8
		noisily di "Other Ethnicity," "0 = No," %4.3f `h_ot_dist1' "%," %4.3f `h_ot_dist3' "%," %4.3f `h_ot_dist4' "%" " ," %4.3f `h_ot_p'
		* row 7
		noisily di " ," "1 = Yes," %4.3f `h_ot_dist2' "%," %4.3f `h_ot_dist5' "%," %4.3f `h_ot_dist6' "%"
		
		log close table
	}
}

}


* TRUMP ELECTED

foreach x of varlist trump_elected_d  {

foreach j of varlist usb nat nczn {
	
	// loop command for continuous variables
	
	foreach v of varlist k6 k6_anx k6_depr age yrsinus {
		
		tabstat k6 if `j'==1 & within_`x'==1, statistics (n) by(`x') save
		return list
		
		matrix list r(StatTotal)
		matrix list r(Stat1)
		matrix list r(Stat2)
		
		matrix n = J(1,3,.)
		
		matrix n[1,1] = r(StatTotal)
		matrix n[1,2] = r(Stat1)
		matrix n[1,3] = r(Stat2)
		
		local n_total = n[1,1]
		local n_before = n[1,2]
		local n_after = n[1,3]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v'
		estat sd
		
		matrix `v'_all = J(1,2,.)
		
		matrix `v'_all[1,1] = r(mean)
		matrix `v'_all[1,2] = r(sd)
		
		local `v'_beta = `v'_all[1,1]
		local `v'_sd = `v'_all[1,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): regress `v' i.`x'
		return list
		matrix list r(table)
		
		matrix `v'_trump = r(table)
		
		local `v'_beta0 = `v'_trump[1,3]
		local `v'_beta1 = `v'_trump[1,3] + `v'_trump[1,2]
		local `v'_p = `v'_trump[4,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v', over(`x')
		estat sd
		
		matrix `v'_sd_trump = r(sd)
		
		local `v'_sd0 = `v'_sd_trump[1,1]
		local `v'_sd1 = `v'_sd_trump[1,2]
	}
	
	// loop command for binary variables
	
	foreach v of varlist k6_bin sex hispyn marstcohab2 parent child18 child5 intervlang2 health_b h_mc h_ot {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all=e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_trump = e(Prop)
		mata: st_matrix("`v'_trump", (st_matrix("`v'_trump") :/ colsum(st_matrix("`v'_trump"))))
		
		mat li `v'_trump
		
		local `v'_dist3 = `v'_trump[1,1]*100
		local `v'_dist4 = `v'_trump[1,2]*100
		local `v'_dist5 = `v'_trump[2,1]*100
		local `v'_dist6 = `v'_trump[2,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// loop command for categorical variables
	
	foreach v of varlist race2 income2 region {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all = e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		local `v'_dist3 = `v'_all[3,1]*100
		local `v'_dist4 = `v'_all[4,1]*100
		local `v'_dist5 = `v'_all[5,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_trump = e(Prop)
		mata: st_matrix("`v'_trump", (st_matrix("`v'_trump") :/ colsum(st_matrix("`v'_trump"))))
		
		mat li `v'_trump
		
		local `v'_dist6 = `v'_trump[1,1]*100
		local `v'_dist7 = `v'_trump[1,2]*100
		local `v'_dist8 = `v'_trump[2,1]*100
		local `v'_dist9 = `v'_trump[2,2]*100
		local `v'_dist10 = `v'_trump[3,1]*100
		local `v'_dist11 = `v'_trump[3,2]*100
		local `v'_dist12 = `v'_trump[4,1]*100
		local `v'_dist13 = `v'_trump[4,2]*100
		local `v'_dist14 = `v'_trump[5,1]*100
		local `v'_dist15 = `v'_trump[5,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// build table from stored results
	
	quietly {
		
		log using "table1_Latino.csv", apend text name(table)
		
		* header
		noisily di " "
		noisily di " "
		noisily di "Table for `x' `j'"
		noisily di "Variable,Level,Entire Subsample,Before `x',After `x',p-value"
		* row 0
		noisily di "n," " ," `n_total' "," `n_before' "," `n_after' ","
		* row 1
		noisily di "K6," " ," %4.3f `k6_beta' " (" %4.3f `k6_sd' ") ," %4.3f `k6_beta0' " (" %4.3f `k6_sd0' ") ," %4.3f `k6_beta1' " (" %4.3f `k6_sd1' ") ," %4.3f `k6_p'
		noisily di "K6 Anx," " ," %4.3f `k6_anx_beta' " (" %4.3f `k6_anx_sd' ") ," %4.3f `k6_anx_beta0' " (" %4.3f `k6_anx_sd0' ") ," %4.3f `k6_anx_beta1' " (" %4.3f `k6_anx_sd1' ") ," %4.3f `k6_anx_p'
		noisily di "K6 Depr," " ," %4.3f `k6_depr_beta' " (" %4.3f `k6_depr_sd' ") ," %4.3f `k6_depr_beta0' " (" %4.3f `k6_depr_sd0' ") ," %4.3f `k6_depr_beta1' " (" %4.3f `k6_depr_sd1' ") ," %4.3f `k6_depr_p'
		* row 2
		noisily di "Binary version of K6 scale," "0 = K6<5," %4.3f `k6_bin_dist1' "%," %4.3f `k6_bin_dist3' "%," %4.3f `k6_bin_dist4' "%" " ," %4.3f `k6_bin_p'
		* row 3
		noisily di " ," "1 = K6>=5," %4.3f `k6_bin_dist2' "%," %4.3f `k6_bin_dist5' "%," %4.3f `k6_bin_dist6' "%"
		* row 4
		noisily di "Age (years)," " ," %4.3f `age_beta' " (" %4.3f `age_sd' ") ," %4.3f `age_beta0' " (" %4.3f `age_sd0' ") ," %4.3f `age_beta1' " (" %4.3f `age_sd1' ") ," %4.3f `age_p'
		
		noisily di "Sex," "0 = female," %4.3f `sex_dist1' "%," %4.3f `sex_dist3' "%," %4.3f `sex_dist4' "%" " ," %4.3f `sex_p'
		* row 8
		noisily di " ," "1 = male," %4.3f `sex_dist2' "%," %4.3f `sex_dist5' "%," %4.3f `sex_dist6' "%"
		* row 9
		noisily di "Hispanic," "0 = no," %4.3f `hispyn_dist1' "%," %4.3f `hispyn_dist3' "%," %4.3f `hispyn_dist4' "%" " ," %4.3f `hispyn_p'
		* row 10
		noisily di " ," "1 = yes," %4.3f `hispyn_dist2' "%," %4.3f `hispyn_dist5' "%," %4.3f `hispyn_dist6' "%"
		* row 11
		noisily di "Race," "Non-Hispanic White," %4.3f `race2_dist1' "%," %4.3f `race2_dist6' "%," %4.3f `race2_dist7' "%" " ," %4.3f `race2_p'
		* row 12
		noisily di " ," "Non-Hispanic Black," %4.3f `race2_dist2' "%," %4.3f `race2_dist8' "%," %4.3f `race2_dist9' "%"
		* row 13
		noisily di " ," "Hispanic," %4.3f `race2_dist3' "%," %4.3f `race2_dist10' "%," %4.3f `race2_dist11' "%"
		* row 14
		noisily di " ," "Asian," %4.3f `race2_dist4' "%," %4.3f `race2_dist12' "%," %4.3f `race2_dist13' "%"
		* row 15
		noisily di " ," "Other," %4.3f `race2_dist5' "%," %4.3f `race2_dist14' "%," %4.3f `race2_dist15' "%"
		* row 16
		noisily di "Income," "$0-49999," %4.3f `income2_dist1' "%," %4.3f `income2_dist6' "%," %4.3f `income2_dist7' "%" " ," %4.3f `income2_p'
		* row 17
		noisily di " ," "$50000 and over," %4.3f `income2_dist2' "%," %4.3f `income2_dist8' "%," %4.3f `income2_dist9' "%"
		* row 18
		noisily di " ," "Unknown," %4.3f `income2_dist3' "%," %4.3f `income2_dist10' "%," %4.3f `income2_dist11' "%"
		* row 19
		noisily di "Years lived in USA," " ," %4.3f `yrsinus_beta' " (" %4.3f `yrsinus_sd' ") ," %4.3f `yrsinus_beta0' " (" %4.3f `yrsinus_sd0' ") ," %4.3f `yrsinus_beta1' " (" %4.3f `yrsinus_sd1' ") ," %4.3f `yrsinus_p'
		* row 20
		noisily di "Region of residence," "Northeast," %4.3f `region_dist1' "%," %4.3f `region_dist6' "%," %4.3f `region_dist7' "%" " ," %4.3f `region_p'
		* row 21
		noisily di " ," "North Central/Midwest," %4.3f `region_dist2' "%," %4.3f `region_dist8' "%," %4.3f `region_dist9' "%"
		* row 22
		noisily di " ," "South," %4.3f `region_dist3' "%," %4.3f `region_dist10' "%," %4.3f `region_dist11' "%"
		* row 23
		noisily di " ," "West," %4.3f `region_dist4' "%," %4.3f `region_dist12' "%," %4.3f `region_dist13' "%"
		* row 24
		noisily di "Living Status," "0 = Single," %4.3f `marstcohab2_dist1' "%," %4.3f `marstcohab2_dist3' "%," %4.3f `marstcohab2_dist4' "%" " ," %4.3f `marstcohab2_p'
		* row 25
		noisily di " ," "1 = Married or Cohabiting," %4.3f `marstcohab2_dist2' "%," %4.3f `marstcohab2_dist5' "%," %4.3f `marstcohab2_dist6' "%"
		* row 26
		noisily di "Is a Parent," "0 = no," %4.3f `parent_dist1' "%," %4.3f `parent_dist3' "%," %4.3f `parent_dist4' "%" " ," %4.3f `parent_p'
		* row 27
		noisily di " ," "1 = yes," %4.3f `parent_dist2' "%," %4.3f `parent_dist5' "%," %4.3f `parent_dist6' "%"
		* row 28
		noisily di "Has Children Under 18," "0 = no," %4.3f `child18_dist1' "%," %4.3f `child18_dist3' "%," %4.3f `child18_dist4' "%" " ," %4.3f `child18_p'
		* row 29
		noisily di " ," "1 = yes," %4.3f `child18_dist2' "%," %4.3f `child18_dist5' "%," %4.3f `child18_dist6' "%"
		* row 30
		noisily di "Has Children Under 5," "0 = no," %4.3f `child5_dist1' "%," %4.3f `child5_dist3' "%," %4.3f `child5_dist4' "%" " ," %4.3f `child5_p'
		* row 31
		noisily di " ," "1 = yes," %4.3f `child5_dist2' "%," %4.3f `child5_dist5' "%," %4.3f `child5_dist6' "%"
		* row 32
		noisily di "Interview Language," "0 = Otherwise," %4.3f `intervlang2_dist1' "%," %4.3f `intervlang2_dist3' "%," %4.3f `intervlang2_dist4' "%" " ," %4.3f `intervlang2_p'
		* row 33
		noisily di " ," "1 = English," %4.3f `intervlang2_dist2' "%," %4.3f `intervlang2_dist5' "%," %4.3f `intervlang2_dist6' "%"
		* row 34
		noisily di "Self-rated Health," "0 = Otherwise," %4.3f `health_b_dist1' "%," %4.3f `health_b_dist3' "%," %4.3f `health_b_dist4' "%" " ," %4.3f `health_b_p'
		* row 35
		noisily di " ," "1 = Excellent/Very Good," %4.3f `health_b_dist2' "%," %4.3f `health_b_dist5' "%," %4.3f `health_b_dist6' "%"
		noisily di "Mexican/Central/South American Ethnicity," "0 = No," %4.3f `h_mc_dist1' "%," %4.3f `h_mc_dist3' "%," %4.3f `h_mc_dist4' "%" " ," %4.3f `h_mc_p'
		* row 7
		noisily di " ," "1 = Yes," %4.3f `h_mc_dist2' "%," %4.3f `h_mc_dist5' "%," %4.3f `h_mc_dist6' "%"
		* row 8
		noisily di "Other Ethnicity," "0 = No," %4.3f `h_ot_dist1' "%," %4.3f `h_ot_dist3' "%," %4.3f `h_ot_dist4' "%" " ," %4.3f `h_ot_p'
		* row 7
		noisily di " ," "1 = Yes," %4.3f `h_ot_dist2' "%," %4.3f `h_ot_dist5' "%," %4.3f `h_ot_dist6' "%"
		
		log close table
	}
}

}

***
* Non-Hispanic Black and White 
* TABLE S2
* TABLE S12
***

* DACA

foreach x of varlist daca_announce_d daca_rescission_d {

foreach j of varlist nh_blackwhite nh_white nh_black {
	
	// loop command for continuous variables
	
	foreach v of varlist k6 k6_anx k6_depr age yrsinus {
		
		tabstat k6 if `j'==1 & within_`x'==1, statistics (n) by(`x') save
		return list
		
		matrix list r(StatTotal)
		matrix list r(Stat1)
		matrix list r(Stat2)
		
		matrix n = J(1,3,.)
		
		matrix n[1,1] = r(StatTotal)
		matrix n[1,2] = r(Stat1)
		matrix n[1,3] = r(Stat2)
		
		local n_total = n[1,1]
		local n_before = n[1,2]
		local n_after = n[1,3]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v'
		estat sd
		
		matrix `v'_all = J(1,2,.)
		
		matrix `v'_all[1,1] = r(mean)
		matrix `v'_all[1,2] = r(sd)
		
		local `v'_beta = `v'_all[1,1]
		local `v'_sd = `v'_all[1,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): regress `v' i.`x'
		return list
		matrix list r(table)
		
		matrix `v'_daca = r(table)
		
		local `v'_beta0 = `v'_daca[1,3]
		local `v'_beta1 = `v'_daca[1,3] + `v'_daca[1,2]
		local `v'_p = `v'_daca[4,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v', over(`x')
		estat sd
		
		matrix `v'_sd_daca = r(sd)
		
		local `v'_sd0 = `v'_sd_daca[1,1]
		local `v'_sd1 = `v'_sd_daca[1,2]
	}
	
	// loop command for binary variables
	
	foreach v of varlist k6_bin sex hispyn marstcohab2 parent child18 child5 intervlang2 health_b {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all=e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_daca = e(Prop)
		mata: st_matrix("`v'_daca", (st_matrix("`v'_daca") :/ colsum(st_matrix("`v'_daca"))))
		
		mat li `v'_daca
		
		local `v'_dist3 = `v'_daca[1,1]*100
		local `v'_dist4 = `v'_daca[1,2]*100
		local `v'_dist5 = `v'_daca[2,1]*100
		local `v'_dist6 = `v'_daca[2,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// loop command for categorical variables
	
	foreach v of varlist race2 income2 region {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all = e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		local `v'_dist3 = `v'_all[3,1]*100
		local `v'_dist4 = `v'_all[4,1]*100
		local `v'_dist5 = `v'_all[5,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_daca = e(Prop)
		mata: st_matrix("`v'_daca", (st_matrix("`v'_daca") :/ colsum(st_matrix("`v'_daca"))))
		
		mat li `v'_daca
		
		local `v'_dist6 = `v'_daca[1,1]*100
		local `v'_dist7 = `v'_daca[1,2]*100
		local `v'_dist8 = `v'_daca[2,1]*100
		local `v'_dist9 = `v'_daca[2,2]*100
		local `v'_dist10 = `v'_daca[3,1]*100
		local `v'_dist11 = `v'_daca[3,2]*100
		local `v'_dist12 = `v'_daca[4,1]*100
		local `v'_dist13 = `v'_daca[4,2]*100
		local `v'_dist14 = `v'_daca[5,1]*100
		local `v'_dist15 = `v'_daca[5,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// build table from stored results
	
	quietly {
		
		log using "table1_NHBW.csv", append text name(table)
		
		* header
		noisily di " "
		noisily di " "
		noisily di "Table for `x' `j'"
		noisily di "Variable,Level,Entire Subsample,Before `x',After `x',p-value"
		* row 0
		noisily di "n," " ," `n_total' "," `n_before' "," `n_after' ","
		* row 1
		noisily di "K6," " ," %4.3f `k6_beta' " (" %4.3f `k6_sd' ") ," %4.3f `k6_beta0' " (" %4.3f `k6_sd0' ") ," %4.3f `k6_beta1' " (" %4.3f `k6_sd1' ") ," %4.3f `k6_p'
		noisily di "K6 Anx," " ," %4.3f `k6_anx_beta' " (" %4.3f `k6_anx_sd' ") ," %4.3f `k6_anx_beta0' " (" %4.3f `k6_anx_sd0' ") ," %4.3f `k6_anx_beta1' " (" %4.3f `k6_anx_sd1' ") ," %4.3f `k6_anx_p'
		noisily di "K6 Depr," " ," %4.3f `k6_depr_beta' " (" %4.3f `k6_depr_sd' ") ," %4.3f `k6_depr_beta0' " (" %4.3f `k6_depr_sd0' ") ," %4.3f `k6_depr_beta1' " (" %4.3f `k6_depr_sd1' ") ," %4.3f `k6_depr_p'
		* row 2
		noisily di "Binary version of K6 scale," "0 = K6<5," %4.3f `k6_bin_dist1' "%," %4.3f `k6_bin_dist3' "%," %4.3f `k6_bin_dist4' "%" " ," %4.3f `k6_bin_p'
		* row 3
		noisily di " ," "1 = K6>=5," %4.3f `k6_bin_dist2' "%," %4.3f `k6_bin_dist5' "%," %4.3f `k6_bin_dist6' "%"
		* row 4
		noisily di "Age (years)," " ," %4.3f `age_beta' " (" %4.3f `age_sd' ") ," %4.3f `age_beta0' " (" %4.3f `age_sd0' ") ," %4.3f `age_beta1' " (" %4.3f `age_sd1' ") ," %4.3f `age_p'
		* row 5
		* row 6
		noisily di "Sex," "0 = female," %4.3f `sex_dist1' "%," %4.3f `sex_dist3' "%," %4.3f `sex_dist4' "%" " ," %4.3f `sex_p'
		* row 7
		noisily di " ," "1 = male," %4.3f `sex_dist2' "%," %4.3f `sex_dist5' "%," %4.3f `sex_dist6' "%"
		* row 8
		noisily di "Hispanic," "0 = no," %4.3f `hispyn_dist1' "%," %4.3f `hispyn_dist3' "%," %4.3f `hispyn_dist4' "%" " ," %4.3f `hispyn_p'
		* row 9
		noisily di " ," "1 = yes," %4.3f `hispyn_dist2' "%," %4.3f `hispyn_dist5' "%," %4.3f `hispyn_dist6' "%"
		* row 10
		noisily di "Race," "Non-Hispanic White," %4.3f `race2_dist1' "%," %4.3f `race2_dist6' "%," %4.3f `race2_dist7' "%" " ," %4.3f `race2_p'
		* row 11
		noisily di " ," "Non-Hispanic Black," %4.3f `race2_dist2' "%," %4.3f `race2_dist8' "%," %4.3f `race2_dist9' "%"
		* row 12
		noisily di " ," "Hispanic," %4.3f `race2_dist3' "%," %4.3f `race2_dist10' "%," %4.3f `race2_dist11' "%"
		* row 13
		noisily di " ," "Asian," %4.3f `race2_dist4' "%," %4.3f `race2_dist12' "%," %4.3f `race2_dist13' "%"
		* row 14
		noisily di " ," "Other," %4.3f `race2_dist5' "%," %4.3f `race2_dist14' "%," %4.3f `race2_dist15' "%"
		* row 15
		noisily di "Income," "$0-49999," %4.3f `income2_dist1' "%," %4.3f `income2_dist6' "%," %4.3f `income2_dist7' "%" " ," %4.3f `income2_p'
		* row 16
		noisily di " ," "$50000 and over," %4.3f `income2_dist2' "%," %4.3f `income2_dist8' "%," %4.3f `income2_dist9' "%"
		* row 17
		noisily di " ," "Unknown," %4.3f `income2_dist3' "%," %4.3f `income2_dist10' "%," %4.3f `income2_dist11' "%"
		* row 18
		* row 19
		noisily di "Years lived in USA," " ," %4.3f `yrsinus_beta' " (" %4.3f `yrsinus_sd' ") ," %4.3f `yrsinus_beta0' " (" %4.3f `yrsinus_sd0' ") ," %4.3f `yrsinus_beta1' " (" %4.3f `yrsinus_sd1' ") ," %4.3f `yrsinus_p'
		* row 20
		noisily di "Region of residence," "Northeast," %4.3f `region_dist1' "%," %4.3f `region_dist6' "%," %4.3f `region_dist7' "%" " ," %4.3f `region_p'
		* row 21
		noisily di " ," "North Central/Midwest," %4.3f `region_dist2' "%," %4.3f `region_dist8' "%," %4.3f `region_dist9' "%"
		* row 22
		noisily di " ," "South," %4.3f `region_dist3' "%," %4.3f `region_dist10' "%," %4.3f `region_dist11' "%"
		* row 23
		noisily di " ," "West," %4.3f `region_dist4' "%," %4.3f `region_dist12' "%," %4.3f `region_dist13' "%"
		* row 24
		noisily di "Living Status," "0 = Single," %4.3f `marstcohab2_dist1' "%," %4.3f `marstcohab2_dist3' "%," %4.3f `marstcohab2_dist4' "%" " ," %4.3f `marstcohab2_p'
		* row 25
		noisily di " ," "1 = Married or Cohabiting," %4.3f `marstcohab2_dist2' "%," %4.3f `marstcohab2_dist5' "%," %4.3f `marstcohab2_dist6' "%"
		* row 26
		noisily di "Is a Parent," "0 = no," %4.3f `parent_dist1' "%," %4.3f `parent_dist3' "%," %4.3f `parent_dist4' "%" " ," %4.3f `parent_p'
		* row 27
		noisily di " ," "1 = yes," %4.3f `parent_dist2' "%," %4.3f `parent_dist5' "%," %4.3f `parent_dist6' "%"
		* row 28
		noisily di "Has Children Under 18," "0 = no," %4.3f `child18_dist1' "%," %4.3f `child18_dist3' "%," %4.3f `child18_dist4' "%" " ," %4.3f `child18_p'
		* row 29
		noisily di " ," "1 = yes," %4.3f `child18_dist2' "%," %4.3f `child18_dist5' "%," %4.3f `child18_dist6' "%"
		* row 30
		noisily di "Has Children Under 5," "0 = no," %4.3f `child5_dist1' "%," %4.3f `child5_dist3' "%," %4.3f `child5_dist4' "%" " ," %4.3f `child5_p'
		* row 31
		noisily di " ," "1 = yes," %4.3f `child5_dist2' "%," %4.3f `child5_dist5' "%," %4.3f `child5_dist6' "%"
		* row 32
		noisily di "Interview Language," "0 = Otherwise," %4.3f `intervlang2_dist1' "%," %4.3f `intervlang2_dist3' "%," %4.3f `intervlang2_dist4' "%" " ," %4.3f `intervlang2_p'
		* row 33
		noisily di " ," "1 = English," %4.3f `intervlang2_dist2' "%," %4.3f `intervlang2_dist5' "%," %4.3f `intervlang2_dist6' "%"
		* row 34
		noisily di "Self-rated Health," "0 = Otherwise," %4.3f `health_b_dist1' "%," %4.3f `health_b_dist3' "%," %4.3f `health_b_dist4' "%" " ," %4.3f `health_b_p'
		* row 35
		noisily di " ," "1 = Excellent/Very Good," %4.3f `health_b_dist2' "%," %4.3f `health_b_dist5' "%," %4.3f `health_b_dist6' "%"
		
		log close table
	}
}

}


* DAPA

foreach x of varlist dapa_announce_d dapa_injunction_d dapa_rescission_d  {

foreach j of varlist nh_blackwhite nh_white nh_black {
	
	// loop command for continuous variables
	
	foreach v of varlist k6 k6_anx k6_depr age yrsinus {
	
		tabstat k6 if `j'==1 & within_`x'==1, statistics (n) by(`x') save
		return list
		
		matrix list r(StatTotal)
		matrix list r(Stat1)
		matrix list r(Stat2)
		
		matrix n = J(1,3,.)
		
		matrix n[1,1] = r(StatTotal)
		matrix n[1,2] = r(Stat1)
		matrix n[1,3] = r(Stat2)
		
		local n_total = n[1,1]
		local n_before = n[1,2]
		local n_after = n[1,3]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v'
		estat sd
		
		matrix `v'_all = J(1,2,.)
		
		matrix `v'_all[1,1] = r(mean)
		matrix `v'_all[1,2] = r(sd)
		
		local `v'_beta = `v'_all[1,1]
		local `v'_sd = `v'_all[1,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): regress `v' i.`x'
		return list
		matrix list r(table)
		
		matrix `v'_dapa = r(table)
		
		local `v'_beta0 = `v'_dapa[1,3]
		local `v'_beta1 = `v'_dapa[1,3] + `v'_dapa[1,2]
		local `v'_p = `v'_dapa[4,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v', over(`x')
		estat sd
		
		matrix `v'_sd_dapa = r(sd)
		
		local `v'_sd0 = `v'_sd_dapa[1,1]
		local `v'_sd1 = `v'_sd_dapa[1,2]
	}
	
	// loop command for binary variables
	
	foreach v of varlist k6_bin sex hispyn marstcohab2 parent child18 child5 intervlang2 health_b {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all=e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_dapa = e(Prop)
		mata: st_matrix("`v'_dapa", (st_matrix("`v'_dapa") :/ colsum(st_matrix("`v'_dapa"))))
		
		mat li `v'_dapa
		
		local `v'_dist3 = `v'_dapa[1,1]*100
		local `v'_dist4 = `v'_dapa[1,2]*100
		local `v'_dist5 = `v'_dapa[2,1]*100
		local `v'_dist6 = `v'_dapa[2,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// loop command for categorical variables
	
	foreach v of varlist race2 income2 region {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all = e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		local `v'_dist3 = `v'_all[3,1]*100
		local `v'_dist4 = `v'_all[4,1]*100
		local `v'_dist5 = `v'_all[5,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_dapa = e(Prop)
		mata: st_matrix("`v'_dapa", (st_matrix("`v'_dapa") :/ colsum(st_matrix("`v'_dapa"))))
		
		mat li `v'_dapa
		
		local `v'_dist6 = `v'_dapa[1,1]*100
		local `v'_dist7 = `v'_dapa[1,2]*100
		local `v'_dist8 = `v'_dapa[2,1]*100
		local `v'_dist9 = `v'_dapa[2,2]*100
		local `v'_dist10 = `v'_dapa[3,1]*100
		local `v'_dist11 = `v'_dapa[3,2]*100
		local `v'_dist12 = `v'_dapa[4,1]*100
		local `v'_dist13 = `v'_dapa[4,2]*100
		local `v'_dist14 = `v'_dapa[5,1]*100
		local `v'_dist15 = `v'_dapa[5,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// build table from stored results
	
	quietly {
		
		log using "table1_NHBW.csv", append text name(table)
		
		* header
		noisily di " "
		noisily di " "
		noisily di "Table for `x' `j'"
		noisily di "Variable,Level,Entire Subsample,Before `x',After `x',p-value"
		* row 0
		noisily di "n," " ," `n_total' "," `n_before' "," `n_after' ","
		* row 1
		noisily di "K6," " ," %4.3f `k6_beta' " (" %4.3f `k6_sd' ") ," %4.3f `k6_beta0' " (" %4.3f `k6_sd0' ") ," %4.3f `k6_beta1' " (" %4.3f `k6_sd1' ") ," %4.3f `k6_p'
		noisily di "K6 Anx," " ," %4.3f `k6_anx_beta' " (" %4.3f `k6_anx_sd' ") ," %4.3f `k6_anx_beta0' " (" %4.3f `k6_anx_sd0' ") ," %4.3f `k6_anx_beta1' " (" %4.3f `k6_anx_sd1' ") ," %4.3f `k6_anx_p'
		noisily di "K6 Depr," " ," %4.3f `k6_depr_beta' " (" %4.3f `k6_depr_sd' ") ," %4.3f `k6_depr_beta0' " (" %4.3f `k6_depr_sd0' ") ," %4.3f `k6_depr_beta1' " (" %4.3f `k6_depr_sd1' ") ," %4.3f `k6_depr_p'
		* row 2
		noisily di "Binary version of K6 scale," "0 = K6<5," %4.3f `k6_bin_dist1' "%," %4.3f `k6_bin_dist3' "%," %4.3f `k6_bin_dist4' "%" " ," %4.3f `k6_bin_p'
		* row 3
		noisily di " ," "1 = K6>=5," %4.3f `k6_bin_dist2' "%," %4.3f `k6_bin_dist5' "%," %4.3f `k6_bin_dist6' "%"
		* row 4
		noisily di "Age (years)," " ," %4.3f `age_beta' " (" %4.3f `age_sd' ") ," %4.3f `age_beta0' " (" %4.3f `age_sd0' ") ," %4.3f `age_beta1' " (" %4.3f `age_sd1' ") ," %4.3f `age_p'
		* row 5
		* row 6
		noisily di "Sex," "0 = female," %4.3f `sex_dist1' "%," %4.3f `sex_dist3' "%," %4.3f `sex_dist4' "%" " ," %4.3f `sex_p'
		* row 7
		noisily di " ," "1 = male," %4.3f `sex_dist2' "%," %4.3f `sex_dist5' "%," %4.3f `sex_dist6' "%"
		* row 8
		noisily di "Hispanic," "0 = no," %4.3f `hispyn_dist1' "%," %4.3f `hispyn_dist3' "%," %4.3f `hispyn_dist4' "%" " ," %4.3f `hispyn_p'
		* row 9
		noisily di " ," "1 = yes," %4.3f `hispyn_dist2' "%," %4.3f `hispyn_dist5' "%," %4.3f `hispyn_dist6' "%"
		* row 10
		noisily di "Race," "Non-Hispanic White," %4.3f `race2_dist1' "%," %4.3f `race2_dist6' "%," %4.3f `race2_dist7' "%" " ," %4.3f `race2_p'
		* row 11
		noisily di " ," "Non-Hispanic Black," %4.3f `race2_dist2' "%," %4.3f `race2_dist8' "%," %4.3f `race2_dist9' "%"
		* row 12
		noisily di " ," "Hispanic," %4.3f `race2_dist3' "%," %4.3f `race2_dist10' "%," %4.3f `race2_dist11' "%"
		* row 13
		noisily di " ," "Asian," %4.3f `race2_dist4' "%," %4.3f `race2_dist12' "%," %4.3f `race2_dist13' "%"
		* row 14
		noisily di " ," "Other," %4.3f `race2_dist5' "%," %4.3f `race2_dist14' "%," %4.3f `race2_dist15' "%"
		* row 15
		noisily di "Income," "$0-49999," %4.3f `income2_dist1' "%," %4.3f `income2_dist6' "%," %4.3f `income2_dist7' "%" " ," %4.3f `income2_p'
		* row 16
		noisily di " ," "$50000 and over," %4.3f `income2_dist2' "%," %4.3f `income2_dist8' "%," %4.3f `income2_dist9' "%"
		* row 17
		noisily di " ," "Unknown," %4.3f `income2_dist3' "%," %4.3f `income2_dist10' "%," %4.3f `income2_dist11' "%"
		* row 18
		* row 19
		noisily di "Years lived in USA," " ," %4.3f `yrsinus_beta' " (" %4.3f `yrsinus_sd' ") ," %4.3f `yrsinus_beta0' " (" %4.3f `yrsinus_sd0' ") ," %4.3f `yrsinus_beta1' " (" %4.3f `yrsinus_sd1' ") ," %4.3f `yrsinus_p'
		* row 20
		noisily di "Region of residence," "Northeast," %4.3f `region_dist1' "%," %4.3f `region_dist6' "%," %4.3f `region_dist7' "%" " ," %4.3f `region_p'
		* row 21
		noisily di " ," "North Central/Midwest," %4.3f `region_dist2' "%," %4.3f `region_dist8' "%," %4.3f `region_dist9' "%"
		* row 22
		noisily di " ," "South," %4.3f `region_dist3' "%," %4.3f `region_dist10' "%," %4.3f `region_dist11' "%"
		* row 23
		noisily di " ," "West," %4.3f `region_dist4' "%," %4.3f `region_dist12' "%," %4.3f `region_dist13' "%"
		* row 24
		noisily di "Living Status," "0 = Single," %4.3f `marstcohab2_dist1' "%," %4.3f `marstcohab2_dist3' "%," %4.3f `marstcohab2_dist4' "%" " ," %4.3f `marstcohab2_p'
		* row 25
		noisily di " ," "1 = Married or Cohabiting," %4.3f `marstcohab2_dist2' "%," %4.3f `marstcohab2_dist5' "%," %4.3f `marstcohab2_dist6' "%"
		* row 26
		noisily di "Is a Parent," "0 = no," %4.3f `parent_dist1' "%," %4.3f `parent_dist3' "%," %4.3f `parent_dist4' "%" " ," %4.3f `parent_p'
		* row 27
		noisily di " ," "1 = yes," %4.3f `parent_dist2' "%," %4.3f `parent_dist5' "%," %4.3f `parent_dist6' "%"
		* row 28
		noisily di "Has Children Under 18," "0 = no," %4.3f `child18_dist1' "%," %4.3f `child18_dist3' "%," %4.3f `child18_dist4' "%" " ," %4.3f `child18_p'
		* row 29
		noisily di " ," "1 = yes," %4.3f `child18_dist2' "%," %4.3f `child18_dist5' "%," %4.3f `child18_dist6' "%"
		* row 30
		noisily di "Has Children Under 5," "0 = no," %4.3f `child5_dist1' "%," %4.3f `child5_dist3' "%," %4.3f `child5_dist4' "%" " ," %4.3f `child5_p'
		* row 31
		noisily di " ," "1 = yes," %4.3f `child5_dist2' "%," %4.3f `child5_dist5' "%," %4.3f `child5_dist6' "%"
		* row 32
		noisily di "Interview Language," "0 = Otherwise," %4.3f `intervlang2_dist1' "%," %4.3f `intervlang2_dist3' "%," %4.3f `intervlang2_dist4' "%" " ," %4.3f `intervlang2_p'
		* row 33
		noisily di " ," "1 = English," %4.3f `intervlang2_dist2' "%," %4.3f `intervlang2_dist5' "%," %4.3f `intervlang2_dist6' "%"
		* row 34
		noisily di "Self-rated Health," "0 = Otherwise," %4.3f `health_b_dist1' "%," %4.3f `health_b_dist3' "%," %4.3f `health_b_dist4' "%" " ," %4.3f `health_b_p'
		* row 35
		noisily di " ," "1 = Excellent/Very Good," %4.3f `health_b_dist2' "%," %4.3f `health_b_dist5' "%," %4.3f `health_b_dist6' "%"
		
		log close table
	}
}

}


* TRUMP ELECTED


foreach x of varlist trump_elected_d  {

foreach j of varlist nh_blackwhite nh_white nh_black {
	
	// loop command for continuous variables
	
	foreach v of varlist k6 k6_anx k6_depr age yrsinus {
		
		tabstat k6 if `j'==1 & within_`x'==1, statistics (n) by(`x') save
		return list
		
		matrix list r(StatTotal)
		matrix list r(Stat1)
		matrix list r(Stat2)
		
		matrix n = J(1,3,.)
		
		matrix n[1,1] = r(StatTotal)
		matrix n[1,2] = r(Stat1)
		matrix n[1,3] = r(Stat2)
		
		local n_total = n[1,1]
		local n_before = n[1,2]
		local n_after = n[1,3]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v'
		estat sd
		
		matrix `v'_all = J(1,2,.)
		
		matrix `v'_all[1,1] = r(mean)
		matrix `v'_all[1,2] = r(sd)
		
		local `v'_beta = `v'_all[1,1]
		local `v'_sd = `v'_all[1,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): regress `v' i.`x'
		return list
		matrix list r(table)
		
		matrix `v'_trump = r(table)
		
		local `v'_beta0 = `v'_trump[1,3]
		local `v'_beta1 = `v'_trump[1,3] + `v'_trump[1,2]
		local `v'_p = `v'_trump[4,2]
		
		svy, subpop(if `j'==1 & within_`x'==1): mean `v', over(`x')
		estat sd
		
		matrix `v'_sd_trump = r(sd)
		
		local `v'_sd0 = `v'_sd_trump[1,1]
		local `v'_sd1 = `v'_sd_trump[1,2]
	}
	
	// loop command for binary variables
	
	foreach v of varlist k6_bin sex hispyn marstcohab2 parent child18 child5 intervlang2 health_b {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all=e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_trump = e(Prop)
		mata: st_matrix("`v'_trump", (st_matrix("`v'_trump") :/ colsum(st_matrix("`v'_trump"))))
		
		mat li `v'_trump
		
		local `v'_dist3 = `v'_trump[1,1]*100
		local `v'_dist4 = `v'_trump[1,2]*100
		local `v'_dist5 = `v'_trump[2,1]*100
		local `v'_dist6 = `v'_trump[2,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// loop command for categorical variables
	
	foreach v of varlist race2 income2 region {
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v', column
		matrix `v'_all = e(Prop)
		mata: st_matrix("`v'_all", (st_matrix("`v'_all") :/ colsum(st_matrix("`v'_all"))))
		
		mat li `v'_all
		
		local `v'_dist1 = `v'_all[1,1]*100
		local `v'_dist2 = `v'_all[2,1]*100
		local `v'_dist3 = `v'_all[3,1]*100
		local `v'_dist4 = `v'_all[4,1]*100
		local `v'_dist5 = `v'_all[5,1]*100
		
		svy, subpop(if `j'==1 & within_`x'==1): tab `v' `x', pearson column
		
		matrix `v'_p = e(p_Pear)
		
		matrix `v'_trump = e(Prop)
		mata: st_matrix("`v'_trump", (st_matrix("`v'_trump") :/ colsum(st_matrix("`v'_trump"))))
		
		mat li `v'_trump
		
		local `v'_dist6 = `v'_trump[1,1]*100
		local `v'_dist7 = `v'_trump[1,2]*100
		local `v'_dist8 = `v'_trump[2,1]*100
		local `v'_dist9 = `v'_trump[2,2]*100
		local `v'_dist10 = `v'_trump[3,1]*100
		local `v'_dist11 = `v'_trump[3,2]*100
		local `v'_dist12 = `v'_trump[4,1]*100
		local `v'_dist13 = `v'_trump[4,2]*100
		local `v'_dist14 = `v'_trump[5,1]*100
		local `v'_dist15 = `v'_trump[5,2]*100
		local `v'_p = `v'_p[1,1]
		
	}
	
	// build table from stored results
	
	quietly {
		
		log using "table1_NHBW.csv", append text name(table)
		
		* header
		noisily di " "
		noisily di " "
		noisily di "Table for `x' `j'"
		noisily di "Variable,Level,Entire Subsample,Before `x',After `x',p-value"
		* row 0
		noisily di "n," " ," `n_total' "," `n_before' "," `n_after' ","
		* row 1
		noisily di "K6," " ," %4.3f `k6_beta' " (" %4.3f `k6_sd' ") ," %4.3f `k6_beta0' " (" %4.3f `k6_sd0' ") ," %4.3f `k6_beta1' " (" %4.3f `k6_sd1' ") ," %4.3f `k6_p'
		noisily di "K6 Anx," " ," %4.3f `k6_anx_beta' " (" %4.3f `k6_anx_sd' ") ," %4.3f `k6_anx_beta0' " (" %4.3f `k6_anx_sd0' ") ," %4.3f `k6_anx_beta1' " (" %4.3f `k6_anx_sd1' ") ," %4.3f `k6_anx_p'
		noisily di "K6 Depr," " ," %4.3f `k6_depr_beta' " (" %4.3f `k6_depr_sd' ") ," %4.3f `k6_depr_beta0' " (" %4.3f `k6_depr_sd0' ") ," %4.3f `k6_depr_beta1' " (" %4.3f `k6_depr_sd1' ") ," %4.3f `k6_depr_p'
		* row 2
		noisily di "Binary version of K6 scale," "0 = K6<5," %4.3f `k6_bin_dist1' "%," %4.3f `k6_bin_dist3' "%," %4.3f `k6_bin_dist4' "%" " ," %4.3f `k6_bin_p'
		* row 3
		noisily di " ," "1 = K6>=5," %4.3f `k6_bin_dist2' "%," %4.3f `k6_bin_dist5' "%," %4.3f `k6_bin_dist6' "%"
		* row 4
		noisily di "Age (years)," " ," %4.3f `age_beta' " (" %4.3f `age_sd' ") ," %4.3f `age_beta0' " (" %4.3f `age_sd0' ") ," %4.3f `age_beta1' " (" %4.3f `age_sd1' ") ," %4.3f `age_p'
		*row 5
		* row 6
		* row 7
		noisily di "Sex," "0 = female," %4.3f `sex_dist1' "%," %4.3f `sex_dist3' "%," %4.3f `sex_dist4' "%" " ," %4.3f `sex_p'
		* row 8
		noisily di " ," "1 = male," %4.3f `sex_dist2' "%," %4.3f `sex_dist5' "%," %4.3f `sex_dist6' "%"
		* row 9
		noisily di "Hispanic," "0 = no," %4.3f `hispyn_dist1' "%," %4.3f `hispyn_dist3' "%," %4.3f `hispyn_dist4' "%" " ," %4.3f `hispyn_p'
		* row 10
		noisily di " ," "1 = yes," %4.3f `hispyn_dist2' "%," %4.3f `hispyn_dist5' "%," %4.3f `hispyn_dist6' "%"
		* row 11
		noisily di "Race," "Non-Hispanic White," %4.3f `race2_dist1' "%," %4.3f `race2_dist6' "%," %4.3f `race2_dist7' "%" " ," %4.3f `race2_p'
		* row 12
		noisily di " ," "Non-Hispanic Black," %4.3f `race2_dist2' "%," %4.3f `race2_dist8' "%," %4.3f `race2_dist9' "%"
		* row 13
		noisily di " ," "Hispanic," %4.3f `race2_dist3' "%," %4.3f `race2_dist10' "%," %4.3f `race2_dist11' "%"
		* row 14
		noisily di " ," "Asian," %4.3f `race2_dist4' "%," %4.3f `race2_dist12' "%," %4.3f `race2_dist13' "%"
		* row 15
		noisily di " ," "Other," %4.3f `race2_dist5' "%," %4.3f `race2_dist14' "%," %4.3f `race2_dist15' "%"
		* row 16
		noisily di "Income," "$0-49999," %4.3f `income2_dist1' "%," %4.3f `income2_dist6' "%," %4.3f `income2_dist7' "%" " ," %4.3f `income2_p'
		* row 17
		noisily di " ," "$50000 and over," %4.3f `income2_dist2' "%," %4.3f `income2_dist8' "%," %4.3f `income2_dist9' "%"
		* row 18
		noisily di " ," "Unknown," %4.3f `income2_dist3' "%," %4.3f `income2_dist10' "%," %4.3f `income2_dist11' "%"
		* row 19
		*row 20
		* row 19
		noisily di "Years lived in USA," " ," %4.3f `yrsinus_beta' " (" %4.3f `yrsinus_sd' ") ," %4.3f `yrsinus_beta0' " (" %4.3f `yrsinus_sd0' ") ," %4.3f `yrsinus_beta1' " (" %4.3f `yrsinus_sd1' ") ," %4.3f `yrsinus_p'
		* row 20
		noisily di "Region of residence," "Northeast," %4.3f `region_dist1' "%," %4.3f `region_dist6' "%," %4.3f `region_dist7' "%" " ," %4.3f `region_p'
		* row 21
		noisily di " ," "North Central/Midwest," %4.3f `region_dist2' "%," %4.3f `region_dist8' "%," %4.3f `region_dist9' "%"
		* row 22
		noisily di " ," "South," %4.3f `region_dist3' "%," %4.3f `region_dist10' "%," %4.3f `region_dist11' "%"
		* row 23
		noisily di " ," "West," %4.3f `region_dist4' "%," %4.3f `region_dist12' "%," %4.3f `region_dist13' "%"
		* row 24
		noisily di "Living Status," "0 = Single," %4.3f `marstcohab2_dist1' "%," %4.3f `marstcohab2_dist3' "%," %4.3f `marstcohab2_dist4' "%" " ," %4.3f `marstcohab2_p'
		* row 25
		noisily di " ," "1 = Married or Cohabiting," %4.3f `marstcohab2_dist2' "%," %4.3f `marstcohab2_dist5' "%," %4.3f `marstcohab2_dist6' "%"
		* row 26
		noisily di "Is a Parent," "0 = no," %4.3f `parent_dist1' "%," %4.3f `parent_dist3' "%," %4.3f `parent_dist4' "%" " ," %4.3f `parent_p'
		* row 27
		noisily di " ," "1 = yes," %4.3f `parent_dist2' "%," %4.3f `parent_dist5' "%," %4.3f `parent_dist6' "%"
		* row 28
		noisily di "Has Children Under 18," "0 = no," %4.3f `child18_dist1' "%," %4.3f `child18_dist3' "%," %4.3f `child18_dist4' "%" " ," %4.3f `child18_p'
		* row 29
		noisily di " ," "1 = yes," %4.3f `child18_dist2' "%," %4.3f `child18_dist5' "%," %4.3f `child18_dist6' "%"
		* row 30
		noisily di "Has Children Under 5," "0 = no," %4.3f `child5_dist1' "%," %4.3f `child5_dist3' "%," %4.3f `child5_dist4' "%" " ," %4.3f `child5_p'
		* row 31
		noisily di " ," "1 = yes," %4.3f `child5_dist2' "%," %4.3f `child5_dist5' "%," %4.3f `child5_dist6' "%"
		* row 32
		noisily di "Interview Language," "0 = Otherwise," %4.3f `intervlang2_dist1' "%," %4.3f `intervlang2_dist3' "%," %4.3f `intervlang2_dist4' "%" " ," %4.3f `intervlang2_p'
		* row 33
		noisily di " ," "1 = English," %4.3f `intervlang2_dist2' "%," %4.3f `intervlang2_dist5' "%," %4.3f `intervlang2_dist6' "%"
		* row 34
		noisily di "Self-rated Health," "0 = Otherwise," %4.3f `health_b_dist1' "%," %4.3f `health_b_dist3' "%," %4.3f `health_b_dist4' "%" " ," %4.3f `health_b_p'
		* row 35
		noisily di " ," "1 = Excellent/Very Good," %4.3f `health_b_dist2' "%," %4.3f `health_b_dist5' "%," %4.3f `health_b_dist6' "%"
		
		log close table
	}
}

}
