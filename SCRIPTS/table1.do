import excel "~\git\ProstateCancerFattyacid\DATA\original_data.xlsx", sheet("var") firstrow
findit zscore 
*install the zscore package from stata*

foreach v of var fa_* {
zscore `v'
}	
gen race_num=.
replace race_num=1 if race=="European American"
replace race_num=2 if race=="African American"
replace race_num=3 if race=="African"

foreach v of varlist z_fa_Myristic-z_fa_Docosahexaenoic {                
 logistic case `v' age race_num
 }
**By Race group (adjusted for age)
 
 foreach v of varlist z_fa_Myristic-z_fa_Docosahexaenoic  {                
 logistic case `v' age if race=="African"
 }  
 foreach v of varlist z_fa_Myristic-z_fa_Docosahexaenoic {                
 logistic case `v' age if race=="African American"
 }
 foreach v of varlist z_fa_Myristic-z_fa_Docosahexaenoic {                
 logistic case `v' age if race=="European American"
 }
