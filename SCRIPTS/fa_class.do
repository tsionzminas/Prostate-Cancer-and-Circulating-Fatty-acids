import excel "~\git\ProstateCancerFattyacid\DATA\original_data.xlsx", sheet("var") firstrow /*replace this line with your local path*/
sort race case
export excel id case race saturated trans cis_mono omega6 omega3 omega6_3 using "~\git\ProstateCancerFattyacid\RESULTS\fa_class.xlsx", firstrow(variables)/*replace this line with your local path*/
