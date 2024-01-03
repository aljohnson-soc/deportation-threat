// Replication Package for "Deportation Threat Predicts Latino U.S. Citizens and Noncitizens' Psychological Distress, 2011-2018"
// Authors: Amy L. Johnson, Christopher Levesque, Neil A. Lewis, Jr., Asad L. Asad

// Factor Analysis of Anxiety and Depression Subscales

use nhis_clean.dta, clear

keep if filter==1

// Cronbach's alpha
alpha aeffort ahopeless anervous arestless asad aworthless, item 
alpha anervous arestless, item 
alpha asad aeffort aworthless ahopeless, item 

// Factor analysis

* TABLE S8
factor anervous arestless
factor asad aeffort aworthless ahopeless
