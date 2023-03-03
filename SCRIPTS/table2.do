
import excel "~\git\ProstateCancerFattyacid\DATA\original_data.xlsx", sheet("var") firstrow /*replace the file paths with your local path*//

*Elaidic acid NCI-MD study 
pctile fa_Elaidic_pct= fa_Elaidic if case==0 & study=="NCI-MD", nq(3)
xtile q3_fa_Elaidic = fa_Elaidic if study=="NCI-MD" , cutpoints(fa_Elaidic_pct)
bys q3_fa_Elaidic: tabstat fa_Elaidic, stats(n min max)
drop fa_Elaidic_pct

tab q3_fa_Elaidic case if race=="African American", column
tab q3_fa_Elaidic case if race=="European American", column

logistic case i.q3_fa_Elaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="African American"
logistic case q3_fa_Elaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="African American"

logistic case i.q3_fa_Elaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="European American"
logistic case q3_fa_Elaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="European American"


*Elaidic acid Ghana study 

pctile fa_Elaidic_pct= fa_Elaidic if case==0 & study=="Ghana", nq(3)
xtile q3_af_fa_Elaidic = fa_Elaidic if study=="Ghana" , cutpoints(fa_Elaidic_pct)
bys q3_af_fa_Elaidic: tabstat fa_Elaidic, stats(n min max)
drop fa_Elaidic_pct


logistic case i.q3_af_fa_Elaidic i.age_quart i.bmi_tert i.education_Ghana i.aspirin i.diabetes i.smoke if race=="African"

*Determining the median cocentrations for each cutoff and for each population group
sum fa_Elaidic if q3_af_fa_Elaidic==1, detail 
sum fa_Elaidic if q3_af_fa_Elaidic==2, detail
sum fa_Elaidic if q3_af_fa_Elaidic==3, detail
sum fa_Elaidic if q3_fa_Elaidic==1 & race=="African American", detail
sum fa_Elaidic if q3_fa_Elaidic==2 & race=="African American", detail
sum fa_Elaidic if q3_fa_Elaidic==3 & race=="African American", detail
sum fa_Elaidic if q3_fa_Elaidic==1 & race=="European American", detail
sum fa_Elaidic if q3_fa_Elaidic==2 & race=="European American", detail
sum fa_Elaidic if q3_fa_Elaidic==3 & race=="European American", detail

*Palmitelaidic acid for NCI-MD study
pctile fa_Palmitelaidic_pct= fa_Palmitelaidic if case==0 & study=="NCI-MD", nq(3)
xtile q3_fa_Palmitelaidic = fa_Palmitelaidic if study=="NCI-MD" , cutpoints(fa_Palmitelaidic_pct)
bys q3_fa_Palmitelaidic: tabstat fa_Palmitelaidic, stats(n min max)
drop fa_Palmitelaidic_pct

tab q3_fa_Palmitelaidic case if race=="African American", column
tab q3_fa_Palmitelaidic case if race=="European American", column

logistic case i.q3_fa_Palmitelaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="African American"
logistic case q3_fa_Palmitelaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="African American"

logistic case i.q3_fa_Palmitelaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="European American"
logistic case q3_fa_Palmitelaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="European American"

*Palmitelaidic acid for Ghana study
pctile fa_Palmitelaidic_pct= fa_Palmitelaidic if case==0 & study=="Ghana", nq(3)
xtile q3_af_fa_Palmitelaidic = fa_Palmitelaidic if study=="Ghana" , cutpoints(fa_Palmitelaidic_pct)
bys q3_af_fa_Palmitelaidic: tabstat fa_Palmitelaidic if study=="Ghana", stats(n min max)
drop fa_Palmitelaidic_pct

tab q3_af_fa_Palmitelaidic case if race=="African" & case!=2, column
logistic case i.q3_af_fa_Palmitelaidic i.age_quart i.bmi_tert i.education_Ghana i.aspirin i.diabetes i.smoke if race=="African"
logistic case q3_af_fa_Palmitelaidic i.age_quart i.bmi_tert i.education_Ghana i.aspirin i.diabetes i.smoke if race=="African"

*Determining the median cocentrations for each cutoff and for each population group
sum fa_Palmitelaidic if q3_af_fa_Palmitelaidic==1, detail
sum fa_Palmitelaidic if q3_af_fa_Palmitelaidic==2, detail
sum fa_Palmitelaidic if q3_af_fa_Palmitelaidic==3, detail
sum fa_Palmitelaidic if q3_fa_Palmitelaidic==1 & race=="African American", detail
sum fa_Palmitelaidic if q3_fa_Palmitelaidic==2 & race=="African American", detail
sum fa_Palmitelaidic if q3_fa_Palmitelaidic==3 & race=="African American", detail
sum fa_Palmitelaidic if q3_fa_Palmitelaidic==1 & race=="European American", detail
sum fa_Palmitelaidic if q3_fa_Palmitelaidic==2 & race=="European American", detail
sum fa_Palmitelaidic if q3_fa_Palmitelaidic==3 & race=="European American", detail

*Linoelaidic acid for NCI-MD study
pctile fa_Linoelaidic_pct= fa_Linoelaidic if case==0 & study=="NCI-MD", nq(3)
xtile q3_fa_Linoelaidic = fa_Linoelaidic if study=="NCI-MD" , cutpoints(fa_Linoelaidic_pct)
bys q3_fa_Linoelaidic: tabstat fa_Linoelaidic, stats(n min max)
drop fa_Linoelaidic_pct

tab q3_fa_Linoelaidic case if race=="European American", column
logistic case i.q3_fa_Linoelaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="European American"
logistic case q3_fa_Linoelaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="European American"

tab q3_fa_Linoelaidic case if race=="African American", column
logistic case i.q3_fa_Linoelaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="African American"
logistic case q3_fa_Linoelaidic i.age_quart i.bmi_tert i.education2_NCIMD i.aspirin i.diabetes i.smoke if race=="African American"

*Linoelaidic acid for Ghana study
pctile fa_Linoelaidic_pct= fa_Linoelaidic if case==0 & study=="Ghana", nq(3)
xtile q3_af_fa_Linoelaidic = fa_Linoelaidic if study=="Ghana" , cutpoints(fa_Linoelaidic_pct)
bys q3_af_fa_Linoelaidic: tabstat fa_Linoelaidic if study=="Ghana", stats(n min max)
drop fa_Linoelaidic_pct

tab q3_af_fa_Linoelaidic case if race=="African" & case!=2, column
logistic case i.q3_af_fa_Linoelaidic i.age_quart i.bmi_tert i.education_Ghana i.aspirin i.diabetes i.smoke if race=="African"
logistic case q3_af_fa_Linoelaidic i.age_quart i.bmi_tert i.education_Ghana i.aspirin i.diabetes i.smoke if race=="African"

*Determining the median cocentrations for each cutoff and for each population group
sum fa_Linoelaidic if q3_af_fa_Linoelaidic==1, detail
sum fa_Linoelaidic if q3_af_fa_Linoelaidic==2, detail
sum fa_Linoelaidic if q3_af_fa_Linoelaidic==3, detail
sum fa_Linoelaidic if q3_fa_Linoelaidic==1 & race=="African American", detail
sum fa_Linoelaidic if q3_fa_Linoelaidic==2 & race=="African American", detail
sum fa_Linoelaidic if q3_fa_Linoelaidic==3 & race=="African American", detail
sum fa_Linoelaidic if q3_fa_Linoelaidic==1 & race=="European American", detail
sum fa_Linoelaidic if q3_fa_Linoelaidic==2 & race=="European American", detail
sum fa_Linoelaidic if q3_fa_Linoelaidic==3 & race=="European American", detail
