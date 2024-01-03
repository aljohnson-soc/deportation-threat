Replication Package for "Deportation Threat Predicts Latino U.S. Citizens and Noncitizens' Psychological Distress, 2011-2018"

Authors: Amy L. Johnson, Christopher Levesque, Neil A. Lewis, Jr., Asad L. Asad

# Data

## NHIS
Publicly available National Health Interview Survey (NHIS) data were downloaded from the [Integrated Public Use Microdata Series (IPUMS)](https://nhis.ipums.org/nhis/). Our data extract included 2011-2018 data for the following variables: 

- YEAR 	Survey year
- SERIAL 	Sequential Serial Number, Household Record
- NUMPREC 	Number of person records in household
- QUARTER 	Sample quarter, household record
- STRATA 	Stratum for variance estimation
- PSU 	Primary sampling unit (PSU) for variance estimation
- NHISHID 	NHIS Unique identifier, household
- HHWEIGHT 	Household weight, final annual
- PROCYEAR 	Processing year
- NONIVIEW 	Noninterview reason, Type A
- FAMNUMTOT 	Number of families in hh (from reformatting)
- FAMACPTNO 	Number of families in household responding
- REGION 	Region of residence
- LIVINGQTR 	Type of living quarters
- PERNUM 	Person number within family/household (from reformatting)
- NHISPID 	NHIS Unique Identifier, person
- HHX 	Household number (from NHIS)
- FMX 	Family number (from NHIS)
- PX 	Person number of respondent (from NHIS).
- PERWEIGHT 	Final basic annual weight
- SAMPWEIGHT 	Sample Person Weight
- FWEIGHT 	Final annual family weight
- SUPP1WT 	Supplemental Person Weight 1
- INTERVWMO 	Month of NHIS interview
- INTERVWYR 	Year of NHIS interview
- ASTATFLG 	Sample adult flag
- CSTATFLG 	Sample child flag
- AGE 	Age
- SEX 	Sex
- MARSTAT 	Legal marital status
- MARST 	Current marital status
- MARSTCOHAB 	Marital status, including living with partner
- COHABMARST 	Legal marital status of cohabiting person
- COHABEVMAR 	Cohabiting person ever married
- FAMSIZE 	Number of persons in family
- NCHILD 	Number of own children (from programming)
- NCHLT5 	Number of own children less than 5 years old (from programming)
- ELDCH 	Age of eldest own child (from programming)
- YNGCH 	Age of youngest own child (from programming)
- FAMKIDNO 	Number of family members under 18 (fam record)
- RACENEW 	Self-reported Race (Post-1997 OMB standards)
- RACEA 	Main Racial Background (Pre-1997 Revised OMB Standards), self-reported or interviewer - reported
- HISPETH 	Hispanic ethnicity
- RACESR 	Self-Reported Main Racial Background (Pre-1997 Revised OMB Standards)
- HISPYN 	Hispanic ethnicity, dichotomous
- USBORN 	Born in the United States
- CITIZEN 	U.S. citizenship
- REGIONBR 	Global region of birth
- INTERVLANG 	Language of interview
- EDUCREC1 	Educational attainment recode, nonintervalled
- EDUC 	Educational attainment
- EMPSTAT 	Employment status in past 1 to 2 weeks
- POORYN 	Above or below poverty threshold
- INCFAM07ON 	Total combined family income (2007+)
- CPI2009 	CPI conversion factor
- EARNINGS 	Person's total earnings, previous calendar year
- POVERTY 	Ratio of family income to poverty threshold
- POVIMP2 	Ratio of imputed family income to poverty threshold
- HEALTH 	Health status
- AEFFORT 	Felt everything an effort, past 30 days (adults)
- AFEELINT1MO 	Feelings interfered w. life, past 30 days (adults)
- AHOPELESS 	How often felt hopeless, past 30 days (adults)
- ANERVOUS 	How often felt nervous, past 30 days (adults)
- ARESTLESS 	How often felt restless, past 30 days (adults)
- ASAD 	How often felt sad, past 30 days (adults)
- AWORTHLESS 	How often felt worthless, past 30 days (adults)

Figures 2 and 3, Figures S2-S5, and Tables S2, S3, S12, and S13 rely on restricted-access NHIS data. We obtained access to these data following approval from the National Center for Health Statistics (NCHS), which assessed the proposed analyses for possible disclosure risk. Analyses were conducted in the Federal Statistical Research Data Center (RDC) at Stanford University. Restricted-access NHIS data included the same variables as the public data extract, as well as the following additional variables, from 2011 to 2018:

- SADATE	Date Sample Adult Core Section was started
- SAEND	Date Sample Adult section was completed
- USYR	Year respondent came to the U.S.

## ICE Detainers
These restricted-access data on detainers issued by Immigration and Customs Enforcement between 2011 and 2018 are provided by Syracuse University's Transactional Access Records Clearinghouse (TRAC). To obtain the data, which were provided to us free of charge as university researchers affiliated with institutions who subscribe to TRAC, may apply for the data at the following website: https://trac.syr.edu/fellows/. The application process, as described on that website, is the following: 

To facilitate data access for scholarly research, TRAC has set up the TRAC Fellows program. Fellows can be appointed as Fellows In-Residence or as Outside Fellows. How to apply to become a TRAC Fellow and the terms of appointments are briefly described below:

1. Appointments as TRAC Fellows are limited to researchers of subscribing organizations who are bound by the License Agreement governing data use. If collaborating researchers are from different organizations, each organization must be a subscriber. If during the term of appointment the organization does not renew their annual subscription, the appointment automatically terminates.

2. To apply for data access for scholarly research, submit a brief research plan including the proposed analysis, a description of the data needed, the project proposed beginning and end dates, and a list of collaborators and co-authors with their affiliations. If some period in-residence is desired, specify the proposed period and any additional research support needed.

3. TRAC reviews proposals on an on-going basis, and will notify the submitter whether their proposal is accepted, and what if any additional costs would be involved if specialized data sets or other support services are requested.

4. Special data access will be provided with the following understanding:
a. Use will be limited to only that described in the research plan;
b. Data access will not be shared with anyone not listed as a collaborator or co-author in the application;
c. Use of data will terminate automatically if the organization(s) cease to be TRAC subscriber(s).

5. TRAC Fellows receiving special data access agree to provide (i) a 1-2 page synopsis of their research project, and (ii) upon completion, a synopsis including findings, both of which can be published on TRAC's web site. At their discretion, TRAC Fellows may also provide a link to a working paper on another web site, and citation to a publication.

## Google Trends Scores
Publicly available Google Trends data were downloaded from Google Trends (google.com/trends). We searched for the "Immigration" topic, exclusive to the United States, between January 1, 2011, through December 31, 2018. We then downloaded these data in .csv format (immigration_only.csv). In our Supplementary Analyses, we use a similar measure in which we adjust the default Trends option from "Web Search" to "News Search," capturing the "Immigration" topic within news articles. We downloaded these data in .csv format as well (immigration_newssearch.csv).

# Code Files
NHISCleaning.do

NHISAnalysis_PublicData.do
- Figure 1
- Figure S1

NHISAnalysis_RestrictedData.do
- Figure 2
- Figure 3
- Figure S2
- Figure S3
- Figure S4
- Figure S5
- Table S2
- Table S3
- Table S12
- Table S13

DetainersandTrendsAnalysis.do
- Figure 4
- Figure S6
- Figure S7
- Figure S8
- Table S4
- Table S5
- Table S6

FactorAnalysis.do
- Table S8

MeasurementInvarianceTesting.R
- Table S9
- Table S10
- Table S11
