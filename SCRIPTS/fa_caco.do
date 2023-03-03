

import excel "~\git\ProstateCancerFattyacid\DATA\original_data.xlsx", sheet("var") firstrow
sort case 
export excel case fa_* using "~\git\ProstateCancerFattyacid\RESULTS\Af_caco_fa.xlsx" if race=="African", firstrow(variables) replace
export excel case fa_* using "~\git\ProstateCancerFattyacid\RESULTS\AA_caco_fa.xlsx" if race=="African American", firstrow(variables) replace 
export excel case fa_* using "~\git\ProstateCancerFattyacid\RESULTS\EA_caco_fa.xlsx" if race=="European American", firstrow(variables) replace 

/*replace the file paths with your local path*/
