*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
		**  PROGRAM FOR:    TRUE PANEL ANALYSIS OF CONSOLIDATED DATA FOR RESILIENCE STUDY	**  
		**  CREATED BY:     BELIYOU HAILE (W/ INPUT FROM SARA S.)				** 
		**	CREATED ON:     JULY 2016 											** 
		**	UPDATED ON:     BEING UPDATED NON-STOP!!!							** 
**!!!!!! RE-RUN "1 Analysis-Additional-Shock-Vars.do" FIRST IF THERE ARE UPDATES TO:
				**1. WEATHER DATA
				**2. HOUSEHOLD DATA 
				**3. DEFINITION OF SHOCK VARIABLES 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
/*
Note from Mel:
I only took a quick stab at it so far (no time), but what happens is that a simple OLS does not yield any significant result, 
whereas accounting for spatial auto-correlation yields an equivalent R-square (Rho) of 0.71. So maybe you're struggling because 
your model specification is not adequate, and in fact you might need to use an equivalent Spatial AutoCorrelation Panel (or cross-section) Regression (SAC)
or Spatial Lag Panel Regression (SAR) command in STATA (they are `spregsacxt` and `spregsarxt` or `spregsac` and `spregsar`). I'm not familiar with the STATA commands,
but should be relatively easy to implement once you have the correct spatial weight matrix (see commands `spweight` or `spweightxt`).
*/
version 14
set more off
clear all
set maxvar 30000
set matsize 11000
cap log close
estimates clear 
macro drop _all

gl drive "C:\Users\BHAILE\Dropbox (IFPRI)\ifpri_fao (1)\Analysis"
gl DATA  "C:\Users\BHAILE\Dropbox (IFPRI)\ifpri_fao (1)\Preliminary statistics"
gl TEMP	${drive}\temp
gl OUTg	${drive}\out\graphs
gl OUTt	${drive}\out\tables 
gl AEC	"C:\Conferences\African Economic Conference"
gl PAPER "C:\Research\SIF Resilience\Paper"

gl today = c(current_date)
cd "${drive}"

gl wt 		    "[aweight=weight]"
gl filter       "rural~=0 & agri_hh~=0" 

	*****************************************************************************************
		**			PART I. REAL PANEL ANALYSIS OF TZA/UGA NPS PANEL DATA	     **
	*****************************************************************************************
u "$DATA\Combined_fulldataset_chirps_spei_pdsi_udel_dist_hh_shock_plant_grow_analysis.dta", clear

gl headcont0 agehead 
gl hhcont0   hhsize hhsizesq educave
gl wealth0   landown tlu_total agwealth_paran nonagwealth_paran //electricity   electricity is already included in the non-agr wealth index...index may need to be re-constructed 
gl bio0      TT20k_hours elevation lgp 
gl health0   malariain

*Rebecca to check missing data //short-term solution 
replace femhead=1 if femhead==.
foreach var in $headcont0 $hhcount0 $wealth0 $bio0 $health0 {
count if `var'==.
bysort ISO3 survey region district: egen av_`var' = median(`var')
replace `var'=av_`var' if `var'==.
}

*There is a mismatch with the coding of the round variable in TZA NPS
replace round = 1 if ISO3=="TZA" &(round==2008|round==2009)
replace round = 2 if ISO3=="TZA" &(round==2010|round==2011)
replace round = 3 if ISO3=="TZA" &(round==2012|round==2013)

/*
????????????Check if Mel needs this for the spatial analysis
g dist_new = 100*region + district
replace district = dist_new if ISO3=="TZA"
*/

destring hhid_merge, replace 

foreach var of varlist pcexp_pppimp pcfoodexp_pppimp{
gen ln`var' = ln(`var')
}
la var lnpcexp_pppimp     "Ln per capita monthly expenditure (imputed) (2011 PPP$)"
la var lnpcfoodexp_pppimp "Ln per capita monthly food expenditure (imputed) (2011 PPP$)"

sa "$TEMP\Combined_4_Mel.dta", replace 





gl TZA_label "Tanzania (NPS)"
gl UGA_label "Uganda (NPS)" 

gl outcome1 lnpcexp_pppimp // lnpcexp_pppimp //lnpcexp_pppimp  //pcfoodexp pcfoodexp_ppp sh_foodexp_own===UGA NPS HAS SERIOUS ISSUE WITH FOOD EXPENDITURE VARAIBLES....REBECCA TO CHECK...
gl outcome2 lnpcfoodexp_pppimp
gl outcome3 hhdds

gl outcome1l Total  
gl outcome2l Food


gl region    i.region 
gl year     i.round
gl admin1   i.admin1

gl headcont femhead agehead 
gl hhcont   hhsize hhsizesq educave
gl wealth   landown tlu_total agwealth_paran nag_w  //electricity   electricity is already included in the non-agr wealth index...index may need to be re-constructed 
gl bio      far elevation lgp //TT20k_hours mean_popden2000 
gl health   malariain

gl cont0  $headcont $hhcont 

gl lt2 1
gl lt3 2
gl lt4 3
gl lt5 4

gl lt1_l "T-1"
gl lt2_l "T-2"
gl lt3_l "T-3"
gl lt4_l "T-4"
gl lt5_l "Cumulative"

gl title1 "t-1"
gl title2 "t-2"
gl title3 "t-3"
gl title4 "t-4"
gl title5 "cumulative (from t-4 to t-1)"

*++++++++++++++++*Globals for the regressions to examine long-term effects of shocks 
foreach c in TZA UGA{  
foreach v in h_pre {
forval t=1/1 {
gl weat`t'_t`t'`c'`v'1  "L`t'_`v'loshock1 L`t'_`v'hishock1 `v'_lt L`t'_h_temploshock1 L`t'_h_temphishock1 h_temp_lt" 
gl weat`t'_t`t'`c'`v'2  "L`t'_`v'_lshock1 L`t'_`v'_hshock1 `v'_lt L`t'_h_temp_lshock1 L`t'_h_temp_hshock1 h_temp_lt" 
gl weat`t'_t`t'`c'`v'3  "L`t'_h_pdsi_lshock1 L`t'_h_pdsi_hshock1 h_pdsi_lt" 

}
forval t=2/4 {
gl weat`t'_t`t'`c'`v'1 "L`t'_`v'loshock1  L`t'_`v'hishock1 `v'_lt L`t'_h_temploshock1 L`t'_h_temphishock1 h_temp_lt " // L1${lt`t'}_`v'_hshock1 //control for only negative rainfall shock 
gl weat`t'_t`t'`c'`v'2 "L`t'_`v'_lshock1  L`t'_`v'_hshock1 `v'_lt L`t'_h_temp_lshock1 L`t'_h_temp_hshock1 h_temp_lt " // L1${lt`t'}_`v'_hshock1 //control for only negative rainfall shock 
gl weat`t'_t`t'`c'`v'3 "L`t'_h_pdsi_lshock1 L`t'_h_pdsi_hshock1 h_pdsi_lt " 
}
forval t=5/5 {
gl weat`t'_t`t'`c'`v'1  "L1${lt`t'}_`v'loshock1  L1${lt`t'}_`v'hishock1 `v'_lt L1${lt`t'}_h_temploshock1 L1${lt`t'}_h_temphishock1 h_temp_lt "  
gl weat`t'_t`t'`c'`v'2  "L1${lt`t'}_`v'_lshock1  L1${lt`t'}_`v'_hshock1 `v'_lt L1${lt`t'}_h_temp_lshock1 L1${lt`t'}_h_temp_hshock1 h_temp_lt "  
gl weat`t'_t`t'`c'`v'3  "L1${lt`t'}_h_pdsi_lshock1 L1${lt`t'}_h_pdsi_lshock1 h_pdsi_lt"  
}
}
}
foreach c in TZA UGA {
foreach v in h_pre  {
forval t=1/5 {
gl cont`t'1`c'`v't`t'  ${weat`t'_t`t'`c'`v'1} 
gl cont`t'2`c'`v't`t'  ${weat`t'_t`t'`c'`v'2}   
gl cont`t'3`c'`v't`t'  ${weat`t'_t`t'`c'`v'3}   
}
}
}

**********************************************************************************
				*NPS PANEL ANALYSIS  (t-1, t-2, t-3, t-4, and cummulative)
				*vce(cluster district) results in "panels are not nested within clusters" Check!
**********************************************************************************
gl UGAhhid hhid
gl TZAhhid hhid_merge  

foreach v in h_pre  {

foreach c in TZA UGA {
u "$TEMP\Combined_4_Mel.dta", clear
keep if balanced==1 // ????????????????????????????????????????????????????????????/for panel analysis based on NPS TZA/UGA
keep if `c'==1 
xtset ${`c'hhid} round
forval j=1/2{ //outcomes

foreach p in fe /* re*/ { 

forval t = 1/5 {
forval i=2/2{ //controls 
/*
xtreg ${outcome`j'} ${cont`t'`i'`c'`v't`t'} if $filter $round /*[pweight=weight_]*/, `p' vce(r)  
est store c`t'1_`j'_w`i'`c'`v't`t'`p' , t("${outcome`j'}")
xi:xtreg ${outcome`j'} ${cont`t'`i'`c'`v't`t'} $cont0 $wealth $bio $round if $filter  /*[pweight=weight_]*/, `p' vce(r)  
est store c`t'2_`j'_w`i'`c'`v't`t'`p' , t("${outcome`j'}")
*/
xi:xtreg ${outcome`j'} ${cont`t'`i'`c'`v't`t'} $cont0 $wealth $bio $round $region if $filter  /*[pweight=weight_]*/, `p' vce(r)  
est store c`t'3_`j'_w`i'`c'`v't`t'`p', t("`c':`p': ${outcome`j'l}")
}
}
}

*Pooled OLS
forval t = 1/5 {
forval i=2/2{ //controls 
foreach p in reg{
xi:`p' ${outcome`j'} ${cont`t'`i'`c'`v't`t'} $cont0 $wealth $bio $round $region if $filter [pweight=weight],  vce(cluster ${`c'hhid})  
est store c`t'3_`j'_w`i'`c'`v't`t'o, t("`c':ols: ${outcome`j'l}")
}
}
}

}
}

forval t = 1/1 {
forval i=2/2{ //controls 
noi xml_tab c`t'3_1_w`i'TZA`v't`t'o c`t'3_1_w`i'TZA`v't`t'fe  c`t'3_2_w`i'TZA`v't`t'o c`t'3_2_w`i'TZA`v't`t'fe   ///
c`t'3_1_w`i'UGA`v't`t'o c`t'3_1_w`i'UGA`v't`t'fe  c`t'3_2_w`i'UGA`v't`t'o c`t'3_2_w`i'UGA`v't`t'fe  ,  ///
sd stars(* 0.1 ** 0.05 *** 0.01) below  c(Constant) stats (N N_g r2 r2_w r2_b r2_o rho sigma sigma_e sigma_u chi2 p)  cwidth(0 220)  ///
cnames(1 1 2 2 3 3 4 4) ceq ("") /* showeq ceq(${outcome`j'} ${outcome`j'} ${outcome`j'} ${outcome`j'})*/ ///
t("OLS and fixed effects (fe) estimates of the effects of ${title`t'} weather shocks on `: variable label ${outcome1}' and `: variable label ${outcome2}' (${`c'_label})") ///
drop(_Iregion* _Irural_* _Iround* _Iyear* /*o._I**/ _Iadmin1*) notes ("Robust standard errors in parenthesis. OLS standard errors clustered at household level.", ///
"All column 3 controls for region fixed effects. Sample includes only rural agricultural households.", ///
"LTA= Long-term average") ///
lines(SCOL_NAMES 13 COL_NAMES 2 _cons 2 LAST_ROW 13) font("Times New Roman" 10) ///
save("$OUTt\True-Panel-Analysis_shocks_Expenditure(total and food)_$today.xls") sh(T-1) replace
}
}

forval t = 2/5 {
forval i=2/2{ //controls 
noi xml_tab c`t'3_1_w`i'TZA`v't`t'o c`t'3_1_w`i'TZA`v't`t'fe  c`t'3_2_w`i'TZA`v't`t'o c`t'3_2_w`i'TZA`v't`t'fe   ///
c`t'3_1_w`i'UGA`v't`t'o c`t'3_1_w`i'UGA`v't`t'fe  c`t'3_2_w`i'UGA`v't`t'o c`t'3_2_w`i'UGA`v't`t'fe  ,  ///
sd stars(* 0.1 ** 0.05 *** 0.01) below  c(Constant) stats (N N_g r2 r2_w r2_b r2_o rho sigma sigma_e sigma_u chi2 p)  cwidth(0 220)  ///
cnames(1 1 2 2 3 3 4 4) ceq ("") /* showeq ceq(${outcome`j'} ${outcome`j'} ${outcome`j'} ${outcome`j'})*/ ///
t("OLS and fixed effects (fe) estimates of the effects of ${title`t'} weather shocks on `: variable label ${outcome1}' and `: variable label ${outcome2}' (TZA and UGA NPS waves)") ///
drop(_Iregion* _Irural_* _Iround* _Iyear* /*o._I**/ _Iadmin1*) notes ("Robust standard errors in parenthesis. OLS standard errors clustered at household level. ", ///
"All columns controls for region fixed effects. Sample includes only rural agricultural households.", ///
"LTA= Long-term average") ///
lines(SCOL_NAMES 13 COL_NAMES 2 _cons 2 LAST_ROW 13) font("Times New Roman" 10) ///
save("$OUTt\True-Panel-Analysis_shocks_Expenditure(total and food)_$today.xls") sh(T-`t') append
}
}

forval t= 1(4)5 {
noi xml_tab m`t'c_1_wTZA`v'* m`t'c_2_wTZA`v'* f`t'c_1_wTZA`v'* f`t'c_2_wTZA`v'* w`t'c_1_wTZA`v'* w`t'c_2_wTZA`v'* t`t'c_1_wTZA`v'* t`t'c_2_wTZA`v'* ///
m`t'c_1_wUGA`v'* m`t'c_2_wUGA`v'* f`t'c_1_wUGA`v'* f`t'c_2_wUGA`v'* w`t'c_1_wUGA`v'* w`t'c_2_wUGA`v'* t`t'c_1_wUGA`v'* t`t'c_2_wUGA`v'*,  ///
sd stars(* 0.1 ** 0.05 *** 0.01) below  c(Constant) stats (N N_g r2 r2_w r2_b r2_o rho sigma sigma_e sigma_u chi2 p)  cwidth(0 220)  ///
cnames(1 1 2 2 3 3 4 4 5 5 6 6 7 7 8  8 ) ceq ("") /* showeq ceq(${outcome`j'} ${outcome`j'} ${outcome`j'} ${outcome`j'})*/ ///
t("Fixed effects (fe) estimates of heterogeneous effects of ${title`t'} weather shocks on `: variable label ${outcome1}' and `: variable label ${outcome2}' (TZA and UGA NPS waves)") ///
drop(_Iregion* _Irural_* _Iround* _Iyear* /*o._I**/ _Iadmin1*) notes ("Robust standard errors in parenthesis. OLS standard errors clustered at household level. ", ///
"All columns controls for region fixed effects. Sample includes only rural agricultural households.", ///
"LTA= Long-term average") ///lines(SCOL_NAMES 13 COL_NAMES 2 _cons 2 LAST_ROW 13) font("Times New Roman" 10) ///
save("$OUTt\True-Panel-Analysis_shocks_Expenditure(total and food)_$today.xls")  sh(T-`t'_het) append
}
}

**********************************************************************************
				*POOLED OLS (t-1, t-2, t-3, t-4, and cummulative)
**********************************************************************************
foreach v in rain {
foreach c in TZA UGA GHA {
u "$TEMP\Combined_4_Mel.dta", clear
keep if ISO3=="`c'"
forval j=1/2{ //outcomes
forval i=2/2{ //controls 

foreach p in reg { 

forval t = 1/5 {
`p' ${outcome`j'} ${cont`t'`i'`c'`v't`t'} $round [pweight=weight] , vce(cluster district)  
est store c`t'1_`j'_w`i'`c'`v't`t'`p' , t("`c': ${outcome`j'l}")
xi: `p'  ${outcome`j'} ${cont`t'`i'`c'`v't`t'} $cont0 $wealth $bio $round [pweight=weight] , vce(cluster district) 
est store c`t'2_`j'_w`i'`c'`v't`t'`p' , t("`c': ${outcome`j'l}")
xi: `p'  ${outcome`j'} ${cont`t'`i'`c'`v't`t'} $cont0 $wealth $bio $round $region [pweight=weight] , vce(cluster district)   
est store c`t'3_`j'_w`i'`c'`v't`t'`p', t("`c': ${outcome`j'l}")
}
}

}
}
}

forval t = 1/1 {
forval i=2/2{ //controls 
noi xml_tab c`t'3_1_w`i'TZA`v't`t'* c`t'3_2_w`i'TZA`v't`t'*  c`t'3_1_w`i'UGA`v't`t'*  c`t'3_2_w`i'UGA`v't`t'*  c`t'3_1_w`i'GHA`v't`t'*  c`t'3_2_w`i'GHA`v't`t'* ,  ///
sd stars(* 0.1 ** 0.05 *** 0.01) below  c(Constant) stats (N N_g r2)  cwidth(0 220)  ///
cnames(1 1 2 2 3 3 ) ceq ("") /* showeq ceq(${outcome`j'} ${outcome`j'} ${outcome`j'} ${outcome`j'})*/ ///
t("Pooled ordinary least squares estimates of the effects of ${title`t'} weather shocks on `: variable label ${outcome1}' and `: variable label ${outcome2}' (TZA, UGA, and GHA (all waves))") ///
drop(_Iregion* _Irural_* _Iround* _Iyear* /*o._I**/ _Iadmin1*) notes ("All column 3 controls for region fixed effects. Sample includes only rural agricultural households.", ///
"LTA= Long-term average") ///
lines(SCOL_NAMES 13 COL_NAMES 2 _cons 2 LAST_ROW 13) font("Times New Roman" 10) ///
save("$OUTt\\Pooled-OLS-Analysis_shocks_TZA_UGA_GHA_allwaves_Expenditure(total and food)_$today.xls") sh(T-`t') replace
}
}

forval t = 2/5 {
forval i=2/2{ //controls 
noi xml_tab c`t'3_1_w`i'TZA`v't`t'* c`t'3_2_w`i'TZA`v't`t'*  c`t'3_1_w`i'UGA`v't`t'*  c`t'3_2_w`i'UGA`v't`t'*  c`t'3_1_w`i'GHA`v't`t'*  c`t'3_2_w`i'GHA`v't`t'* ,  ///
sd stars(* 0.1 ** 0.05 *** 0.01) below  c(Constant) stats (N N_g r2)  cwidth(0 220)  ///
cnames(1 1 2 2 3 3 )  ceq ("") /* showeq ceq(${outcome`j'} ${outcome`j'} ${outcome`j'} ${outcome`j'})*/ ///
t("Pooled ordinary least squares estimates of the effects of ${title`t'} weather shocks on `: variable label ${outcome1}' and `: variable label ${outcome2}' (TZA, UGA, and GHA (all waves))") ///
drop(_Iregion* _Irural_* _Iround* _Iyear* /*o._I**/ _Iadmin1*) notes ("All column 3 controls for region fixed effects. Sample includes only rural agricultural households.", ///
"LTA= Long-term average") ///
lines(SCOL_NAMES 13 COL_NAMES 2 _cons 2 LAST_ROW 13) font("Times New Roman" 10) ///
save("$OUTt\\Pooled-OLS-Analysis_shocks_TZA_UGA_GHA_allwaves_Expenditure(total and food)_$today.xls") sh(T-`t') append
}
}

forval t= 1(4)5 {
noi xml_tab m`t'c_1_wTZA`v'* m`t'c_2_wTZA`v'* f`t'c_1_wTZA`v'* f`t'c_2_wTZA`v'* w`t'c_1_wTZA`v'* w`t'c_2_wTZA`v'* t`t'c_1_wTZA`v'* t`t'c_2_wTZA`v'* ///
m`t'c_1_wUGA`v'* m`t'c_2_wUGA`v'* f`t'c_1_wUGA`v'* f`t'c_2_wUGA`v'* w`t'c_1_wUGA`v'* w`t'c_2_wUGA`v'* t`t'c_1_wUGA`v'* t`t'c_2_wUGA`v'*,  ///
sd stars(* 0.1 ** 0.05 *** 0.01) below  c(Constant) stats (N r2 r2_a F)  cwidth(0 220)  ///
cnames(1 1 2 2 3 3 4 4 5 5 6 6 7 7 8  8 ) ceq ("") /* showeq ceq(${outcome`j'} ${outcome`j'} ${outcome`j'} ${outcome`j'})*/ ///
t("Pooled ordinary least squares estimates of heterogeneous effects of ${title`t'} weather shocks on `: variable label ${outcome1}' and `: variable label ${outcome2}' (TZA, UGA, and GHA (all waves))") ///
drop(_Iregion* _Irural_* _Iround* _Isurvey* _Iyear* /*o._I**/ _Iadmin1*) notes ("Cluster-robust standard errors in parenthesis.", ///
"All columns controls for region fixed effects. Sample includes only rural agricultural households.", ///
"LTA= Long-term average") ///lines(SCOL_NAMES 13 COL_NAMES 2 _cons 2 LAST_ROW 13) font("Times New Roman" 10) ///
save("$OUTt\\Pooled-OLS-Analysis_shocks_TZA_UGA_GHA_allwaves_Expenditure(total and food)_$today.xls")  sh(T-`t'_het) append
}
}

estimates clear 
cap log close 
