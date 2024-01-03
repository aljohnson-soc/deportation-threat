// Replication Package for "Deportation Threat Predicts Latino U.S. Citizens and Noncitizens' Psychological Distress, 2011-2018"
// Authors: Amy L. Johnson, Christopher Levesque, Neil A. Lewis, Jr., Asad L. Asad

// NHIS Cleaning Code

**
* Restricted STATA Codes
**

cd "WORKINGDIRECTORY"
clear

use  "P1976_2010_18PubRes_Updated.dta", clear
tab1 sadate
tab1 saend year sadate
tab1 year

destring saend, replace
gen str8 saend2 = string(saend, "%08.0f") //this is later dropped
sort saend2
gen saend3 = date(saend2,"MDY") // date of survey completion --> this is used in downstream do files
label var saend3 "Date of survey completion"

gen resadate = date(sadate,"MDY") // date of survey start --> this is used below and then dropped

// date codes for key events 
di date("6/15/2012", "MDY", 2020) 
di date("11/20/2014", "MDY", 2020)
di date("2/16/2015", "MDY", 2020)
di date("6/15/2015", "MDY", 2020)
di date("11/8/2016", "MDY", 2020) 
di date("6/15/2017", "MDY", 2020)
di date("9/5/2017", "MDY", 2020)

// SAEND -> _d
// Sample Adult section completed before, or on/after
*d. Code those who completed the survey on or after the day of the event as “1” and those who completed the survey before the day of the event as “0” (SAEND).

*Daca announcement: JUNE 15, 2012*
gen daca_announce_d=0
replace daca_announce_d=1 if saend3>=19159
label variable daca_announce_d "Daca Announcement Cutoff (0=Survey completed before, 1= Survey completed on/After)"

*Announcement of DAPA November 20, 2014
gen dapa_announce_d=0
replace dapa_announce_d=1 if saend3>=20047
label variable dapa_announce_d "November DAPA Announcement Cutoff (0=Survey completed before, 1=Survey completed on/After)"

*DAPA Injunction February 16, 2015
gen dapa_injunction_d=0
replace dapa_injunction_d=1 if saend3>=20135
label variable dapa_injunction_d "Dapa Injunction Cutoff (0=Survey completed Before, 1=Survey completed on/After)"

*TRUMP ELECTED: November 8, 2016*
gen trump_elected_d=0
replace trump_elected_d=1 if saend3>=20766
*replace trump_elected_d=1 if saend> November 7, 2016
label variable trump_elected_d "Trump Elected Cutoff (0=Survey completed Before, 1=Survey completed on/After)"

*DAPA RESCISSION: June 15, 2017*
gen dapa_rescission_d=0
replace dapa_rescission_d=1 if saend3>=20985
*replace dapa_rescission_d=1 if saend> June 14, 2017
label variable dapa_rescission_d "DAPA Rescission Cutoff (0=Survey completed before, 1=Survey completed on/After)"

*DACA RESCISSION: September 5, 2017*
gen daca_rescission_d=0
replace daca_rescission_d=1 if saend3>=21067
*replace daca_rescission_d=1 if saend> September 4, 2017
label variable daca_rescission_d "Daca Rescission Cutoff (0=Survey completed Before, 1=Survey completed on/After)"
tab1 daca_rescission_d
*e. Code those who started the Adult Core section before the event but completed it after the event as “1”; all others are coded as “0" (SADATE, SAEND).
*Daca announcement: JUNE 15, 2012*

drop    sadate resadate saend saend2 
save "p1764_2010_2018NHIS_pubresdropfin.dta",  replace 
des
tab1 year sex
tab1 region sex

**
* Non-restricted codes
**

use p1764_2010_2018NHIS_pubresdropfin.dta 

merge 1:1 hhx fmx px year using "/projects/dstafftransfer/transfer.20201021/p1764age.dta" 
drop _merge
order year hhx fmx px age

*CREATING THE K6 SCALE

tab aeffort
tab aeffort, nolabel
tab ahopeless
tab anervous
tab arestless
tab asad
tab aworthless

*From code book:  To produce valid results, users must exclude not in universe cases 
*(persons other than sample adults, code 6 in IPUMS NHIS) and unknown cases (codes 7, 8, and 9 
*in IPUMS NHIS) before summing the responses.

//accounting for missingness before summing K6
replace asad = . if asad > 4
replace anervous = . if anervous > 4
replace arestless = . if arestless > 4
replace aeffort = . if aeffort >4
replace aworthless = . if aworthless > 4
replace ahopeless = . if ahopeless > 4

*This is the Kessler 6 scale
gen k6= aeffort+ahopeless+anervous+arestless+asad+aworthless
tab k6
label variable k6 "Kessler 6 scale"

*A binary version of the k6 scale, 1=moderate or worse psychological distress

gen k6_bin=0
replace k6_bin=1 if k6>=5 
replace k6_bin = . if k6 == . 
label variable k6_bin "Binary version of Kessler 6 scale"

* splitting k6 into anxiety and depression separately
egen k6_anx = rowmean(anervous arestless)

egen k6_depr = rowmean(asad aeffort aworthless ahopeless)

*Citizenship
tab citizen
tab citizen, nolabel 

*Creating a variable that lumps the unknown categories together
gen citizen2=1
replace citizen2=2 if citizen==2
replace citizen2=3 if citizen==7|citizen==9
label define citizens 1 "No, not U.S. Citizen"  2 "Yes, U.S. citizen" 3 "Unknown--refused and don't know" 4 "Asian"
label values citizen2 citizens
label variable citizen2  "Citizenship (3 level)"
tab citizen2 

**Region where born
tab regionbr
tab regionbr, nolabel

**Collapsing this down into three levels U.S.\ Mexico & Central American \ Other
gen regionbr2=1
replace regionbr2=2 if regionbr==2
replace regionbr2=3 if regionbr>2

label define births 1 "United States"  2 "Mexico, Central American, Caribbean Islands" 3 "Other"
label values regionbr2 births
label variable regionbr2  "Region of birth (3 level)"
tab regionbr2 

**Creating a composite variable of citizenship and region of birth
tab citizen2 regionbr2

egen citizen_region = group(citizen2 regionbr2), label
tab citizen_region

*creating race variable
*Want a race variable categorized as: non-hisp white, non-hisp black, hisp (of all races), asian, other*

tab racenew
tab racenew, nolabel
tab hispeth
tab hispyn
tab hispyn, nolabel


//note: in public NHIS, values are 100, 200, 400 (ALJ 10/20/21)
gen race2=5
replace race2=1 if hispyn==1 & racenew==10
replace race2=2 if hispyn==1 & racenew==20
replace race2=3 if hispyn==2  
replace race2=4 if racenew==40  


label define races 1 "Non-hispanic White"  2 "Non-hispanic Black" 3 "Hispanic"   4 "Asian" 5 "Other"
label values race2 races
label variable race2 "5 Level Race Variable"
tab race2

*creating hispanic categories by citizenship
gen latino_nzn = 1 if citizen2==1 & hispyn==2
label variable latino_nzn "Latino noncitizens"

gen latino_nat = 1 if citizen2==2 & hispyn==2 & regionbr2 !=1
label variable latino_nat "Latino naturalized citizens"

gen latino_us = 1 if citizen2==2 & hispyn==2 & citizen_region==3
label variable latino_us "Latino US born"

tab latino_nzn 
tab latino_nat
tab latino_us

*Interview Language (Create a binary variable)
tab intervlang
tab intervlang, nolabel

gen intervlang2=2
replace intervlang2=1 if intervlang==1
label define languages 1 "English"  2 "Other"
label values intervlang2 languages
label variable intervlang2 "Binary Interview Lanaguage 1=English"
tab intervlang2
tab intervlang intervlang2


*Creating a new married\cohabitating variable
*cohab or married=1, otherwise=0
*cohab or married=1, otherwise=0
tab marstcohab
tab marstcohab, nolabel

gen marstcohab2=0
replace marstcohab2=1 if marstcohab==1 | marstcohab==2 | marstcohab==7
label define living 0 "Single"  1 "Married or Cohabitating" 
label values marstcohab2 living
label variable marstcohab2 "Living Status"
tab marstcohab2

*Creating Parental Status variable
*Parent=1 means they are a parent, otherwise 0=not a parent
tab nchild
tab nchild, nolabel

gen parent=1
replace parent=0 if nchild==0
label define parents 0 "Not Parents"  1 "Parents" 
label values parent parents
label variable parent "Is a Parent"
tab parent
tab nchild parent

*creating a binary variable if they have children under 18 (1=yes)
tab eldch
tab eldch, nolabel
tab yngch
tab yngch, nolabel

gen child18=0
replace child18=1 if eldch<18
replace child18=1 if yngch>=0 & yngch<18 

label define child18s 0 "No Children under 18"  1 "Has Children under 18" 
label values child18 child18s
label variable child18 " Children Under 18 "
tab child18

*creating a binary variable if they have childeren under 5 or not (1=yes)

gen child5=0
replace child5=1 if eldch<5 // CML added 10/05/2020
replace child5=1 if yngch>=0 & yngch<5 // CML added 10/05/2020

label define child5s 0 "No Children under 5"  1 "Has Children under 5" 
label values child5 child5s
label variable child5 " Children Under 5 "
tab child5

*income
tab incfam07on
tab incfam07on, nolabel

gen income2=3 
replace income2=1 if incfam07on==10| incfam07on==11 | incfam07on==12
replace income2=2 if incfam07on==20| incfam07on==21 | incfam07on==22 | incfam07on==23 | incfam07on==24
label define incomes 1 "$0-$49,999"  2 "$50,000 and over"  3"unknown"
label values income2 incomes
label variable income2 "Income (Collapsed)"
tab income2

*time in months 
tab intervwyr intervwmo

gen intervwdate = ym(intervwyr, intervwmo) 

*Time since DACA annoucement (months) 
gen daca_months = intervwdate - ym(2012, 6)
label variable daca_months "Time since DACA Announcement (months)"

*Age at DACA announcment (years) 
gen DOB_Y2=DOB_Y
destring DOB_Y2, replace
replace DOB_Y2=. if DOB_Y2 >=9997 // note: 75k observations have DOB_M or DOB_Y as missing.

gen daca_age=2012-DOB_Y2 if DOB_M<="06"
replace daca_age=2011-DOB_Y2 if DOB_M>"06" & DOB_M<="12"
label variable daca_age "Age at DACA Announcement (years)"

*Time since DAPA annoucement (months) 
gen dapa_months = intervwdate - ym(2014, 11) 
label variable dapa_months "Time since DAPA Announcement (months)"

*Age at DAPA announcement (years)

gen dapa_age=2014-DOB_Y2 if DOB_M<="11"
replace dapa_age=2013-DOB_Y2 if DOB_M>"11" & DOB_M<="12"
label variable dapa_age "Age at DAPA Announcement (years)"

*Time since Trump election (months) 
gen trump_months = intervwdate - ym(2016, 11)
label variable trump_months "Time since Trump election (months)"

*Age at Trump election (years) 

gen trump_age=2016-DOB_Y2 if DOB_M<="11"
replace trump_age=2015-DOB_Y2 if DOB_M>"11" & DOB_M<="12"
label variable trump_age "Age at Trump election (years)"

* create a binary health variable (1=Excellent/Very Good, 0=Below threshold) 
gen health_b=0
replace health_b=1 if health==1 | health==2
label define health_b 0 "Below threshold"  1 "Excellent/Very Good"
label variable health_b "Self-rated Health"
tab health_b

* exact years in us variable

replace usyr = . if usyr == 9999 | usyr == 9997 
gen yrsinus_exact = year - usyr
drop yrsinus 
rename yrsinus_exact yrsinus

*recoding variables for logistic regressions
recode sex (1=0 "Male") (2=1 "Female"), gen(female)
recode hispyn (1=0 "No, not of Hispanic ethnicity") (2=1 " Yes, of Hispanic ethnicity"), gen(hispyn_b)
recode income2 (1=0 " $0-$49,999") (2=1 "$50,000 and over") (3=2 "unknown"), gen(income2_b)
recode intervlang2 (1=0 "English") (2=1 "Other"), gen(intervlang2_b)

*missingness based on K6 measures
capture drop nomiss
mark nomiss
markout nomiss k6 k6_bin k6_anx k6_depr
tab nomiss

*create a variable where 1 means the sample has no mising, and 0 means the sample does have missing
gen filter=0
replace filter=1 if nomiss==1
tab filter

gen all=0
replace all=1 if filter==1 // overall

*create new categories of Latinos 

gen nczn=0
replace nczn=1 if filter==1&latino_nzn==1 // Latino non-citizens

gen nczn_mxca=0
replace nczn_mxca=1 if filter==1&citizen_region==1 // non-citizens from MX or central america

gen nat=0
replace nat=1 if filter==1&latino_nat==1 // Latino naturalized

gen usb=0
replace usb=1 if filter==1&latino_us==1 //Latino US born

replace yrsinus = age if usb==1

gen czn=0
replace czn=1 if filter==1&latino_us==1|latino_nat==1 // all Latino citizens

gen czn_mxca=0
replace czn_mxca=1 if filter==1&citizen_region==4 // citizens from MX/CA

gen czn_us=0
replace czn_us=1 if filter==1&citizen_region==3 // citizens born in U.S.


* bandwidths and distances at day level, using restricted data
* 30 days on either side
	
capture drop rvd_* bwd_* within_*

foreach x of varlist daca_announce_d daca_rescission_d ///
					dapa_announce_d dapa_rescission_d dapa_injunction_d /// 
					 trump_elected_d  {
					
	if(strpos("`x'", "daca_announce") > 0) {
		// 90 day bandwidth
		gen rvd_`x' = saend3 - 19159
		gen within_`x' = 0
		replace within_`x' = 1 if (rvd_`x' >= -30) & (rvd_`x' <= 30) 
		
	}
	
	else if(strpos("`x'", "dapa_announce") > 0) {
		// DAPA Announcement: August 22, 2014 - February 15, 2015
		gen rvd_`x' = saend3 - 20047
		gen within_`x' = 0
		replace within_`x' = 1 if (rvd_`x' >= -30) & (rvd_`x' <= 30) 
		
	}
	
	else if(strpos("`x'", "dapa_injunction") > 0) {
		// DAPA Injunction: November 21, 2014 - May 17, 2015
		gen rvd_`x' = saend3 - 20135
		gen within_`x' = 0
		replace within_`x' = 1 if (rvd_`x' >= -30) & (rvd_`x' <= 30) 
		
	}
	
	else if(strpos("`x'", "trump_elected") > 0) {
		// 90 day bandwidth
		gen rvd_`x' = saend3 - 20766
		gen within_`x' = 0
		replace within_`x' = 1 if (rvd_`x' >= -30) & (rvd_`x' <= 30) 
	
	}
	
	else if(strpos("`x'", "dapa_rescission") > 0) {
		// DAPA Rescission: March 17, 2017 - September 4, 2017
		gen rvd_`x' = saend3 - 20985
		gen within_`x' = 0
		replace within_`x' = 1 if (rvd_`x' >= -30) & (rvd_`x' <= 30) 
		
	}
	
	else if(strpos("`x'", "daca_rescission") > 0) {
		//DACA Rescission: June 16, 2017 - December 4, 2017
		gen rvd_`x' = saend3 - 21067
		gen within_`x' = 0
		replace within_`x' = 1 if (rvd_`x' >= -30) & (rvd_`x' <= 30)
		
	}

}


save "/projects/dstafftransfer/transfer.20220420/Results/nhis_clean.dta", replace

