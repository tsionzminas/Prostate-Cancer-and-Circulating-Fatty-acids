

import excel "~\git\ProstateCancerFattyacid\DATA\original_data.xlsx", sheet("var") firstrow
sort case race
export excel id case race saturated cis_mono omega3 omega6 omega6_3 trans apoptosis autophagy chemotaxis inflammation promoteTI suppressTI vasculature using "~\git\ProstateCancerFattyacid\RESULTS\correlation_heatmap_data.xlsx",firstrow(variables) replace

/*replace the file paths with your local path*/
