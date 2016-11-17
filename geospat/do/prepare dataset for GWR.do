
**************************************************************************************
*gl drive		"D:\Users\SSIGNORELLI\Dropbox (IFPRI)\SDA\Data\analysis\_global_codes"
gl drive        "C:\Users\SARA\Dropbox\_global_codes"
gl input        "$drive\results\SSA Dist Pov&CC"
gl table        "$input\tables"
gl graph        "$input\graphs"
**************************************************************************************

***-------------------------------**
************************************
******* REGRESSION ANALYSIS ********
************************************
***-------------------------------**

* ONLY RURAL HOUSEHOLDS
use "$input\datasets\_Stata13\Final_SSA_DistRur_POV_shock_29 Jul 2016.dta", clear


************************************************
******* 0. ADDITIONAL VAR CONSTRUCTION ********
***********************************************
set more off

** Construction of year-specific biophisycal variables
**********************************************************

** nightlight and malaria **
foreach var in F PfPR {
egen `var'_ave=rowmean(`var'2000-`var'2013)
}

foreach var in F PfPR {
gen `var'yearly=.
forval i=2000/2013 {
replace `var'yearly=`var'`i' if interview_year==`i'
}
}

replace Fyearly=F2013 if interview_year==2014 | interview_year==2015
replace PfPRyearly=PfPR2014 if interview_year==2014 | interview_year==2015

rename Fyearly nighlight_yearly
rename F_ave nighlight_ave
rename PfPRyearly Malaria_yearly
rename PfPR_ave Malaria_ave

** NDVI **
** replace the missing districts by the country mean
foreach var of varlist ndvi_1982-ndvi_2013 {
bysort ISO3: egen mean_`var'=mean(`var')
replace `var'=mean_`var' if `var'==.
drop mean_`var'
}

egen ndvi_lt=rowmean(ndvi_1982-ndvi_2013)
egen ndvi_sd=rowsd(ndvi_1982-ndvi_2013)

gen L1_ndvi=.
gen Lcum_ndvi=.
forval i=1997/2013 {
replace L1_ndvi=ndvi_`i' if interview_year==`i'+1
replace Lcum_ndvi=(ndvi_`i'+ndvi_`i'-1+ndvi_`i'-2)/3 if interview_year==`i'+1
}

replace L1_ndvi=ndvi_2013 if interview_year==2015
replace Lcum_ndvi=(ndvi_2013+ndvi_2012+ndvi_2011)/3 if interview_year==2015


drop PfPR2000-F2013 SPEI_YEARLY-spei2014 WEATHER_YEARLY-temp_max_2014
gen YEARLY_VAR=.
order YEARLY_VAR, before(nighlight_ave)

** cumulative rainfall, temperature and spei

** replace the missing districts by the country mean
foreach var in pre_total temp_mean spei {
forval i=1/3 {
bysort ISO3: egen mean_L`i'_`var'=mean(L`i'_`var')
replace L`i'_`var'=mean_L`i'_`var' if L`i'_`var'==.
drop mean_L`i'_`var'
}
foreach stat in lt sd {
bysort ISO3: egen mean_`var'_`stat'=mean(`var'_`stat')
replace `var'_`stat'=mean_`var'_`stat' if `var'_`stat'==.
drop mean_`var'_`stat'
}
}

foreach var in pre_total temp_mean spei {
gen Lcum_`var'=(L1_`var'+L2_`var'+L3_`var')/3 // average over last 3 years
}


** Construction of deviation from the mean - weather variables **
******************************************************************
drop *shock* // construct new shock variables
rename *pre_total* *pre*  
rename *temp_mean* *temp*
foreach var in spei pre ndvi  {
foreach i in 1 cum {
gen L`i'_`var'dif=(L`i'_`var'-`var'_lt)
gen L`i'_`var'hishock=(L`i'_`var'>`var'_lt+`var'_sd)
gen L`i'_`var'loshock=(L`i'_`var'<`var'_lt-`var'_sd)
}
}

foreach var in temp {
foreach i in 1 cum {
gen L`i'_`var'dif=(L`i'_`var'-`var'_lt)
gen L`i'_`var'hishock=(L`i'_`var'>`var'_lt+2*`var'_sd)
}
}


** Construction of square terms for non-linearities **
******************************************************

foreach var in spei pre temp ndvi {
gen `var'_lt2=`var'_lt^2
foreach i in 1 cum {
gen L`i'_`var'dif2=L`i'_`var'dif^2
}
}


** Construction of other variables
*************************************

gen cropland_pcap=cpland_mean_ha/pop
gen irrland_pcap=GMIA_V5/pop 
gen TLU_pcap=AN05_TLU/pop
tab interview_year, gen(yr)
tab ISO3, gen(ctry)
encode ISO3, gen(countrycode)

label var depratio2 "Share of working age in the household"
label var ELEVATION "elevation (meter, mean)"
label var cropland_pcap "Per capita crop land area in the district (Ha)"
label var TLU_pcap "Per capita TLU in the district (Ha)"
label var nighlight_yearly "Nighlight"
label var Malaria_yearly "Malaria incidence"
label var spei_lt  "SPEI, long term average"
label var L1_speihishock  "SPEI flood shock dummy (1 sd)"
label var L1_speiloshock  "SPEI drought shock dummy (1 sd)"
label var L1_speidif      "SPEI difference (lag - long term)"
label var L1_speidif2     "SPEI difference squared (lag - long term)"
label var pre_lt          "Rainfall long term average (mm)"
label var temp_lt         "Temperature long term average (C)"
label var L1_prehishock   "Rainfall flood shock dummy (1 sd)" 
label var L1_preloshock   "Rainfall drought shock dummy (1 sd)" 
label var L1_temphishock  "Temperature heat shock dummy(2 sd)"
label var L1_predif      "Rainfall difference (lag - long term)"
label var L1_predif2     "Rainfall difference squared (lag - long term)"
label var L1_tempdif     "Temperature difference (lag - long term)" 
label var L1_tempdif2     "Temperature difference squared (lag - long term)"
label var ndvi_lt          "NDVI long term average"
label var L1_ndvihishock   "NDVI flood shock dummy (1 sd)"
label var L1_ndviloshock   "NDVI drought shock dummy (1 sd)"

gl outcome1 pcexp_ppp_m 
gl outcome2 foodexp_ppp_m
gl outcome3 foodshare
gl outcome4 wealth_index_all

gl socio     hh_female agehead marriedhead meaneduc depratio2 children 
gl agri      LGP_AVG ELEVATION cropland_pcap TLU_pcap 
gl access    nighlight_yearly // tt10_20k PD12_TOT

gl weather1   spei_lt L1_speidif L1_speidif2 
gl weather2   spei_lt Lcum_speidif Lcum_speidif2 
gl weather3   spei_lt L1_speihishock L1_speiloshock
gl weather4  pre_lt L1_predif L1_predif2  ///
             temp_lt L1_tempdif L1_tempdif2
gl weather5  pre_lt Lcum_predif Lcum_predif2  ///
             temp_lt Lcum_tempdif Lcum_tempdif2
gl weather6  pre_lt temp_lt L1_prehishock L1_preloshock L1_temphishock 
gl weather7  ndvi_lt L1_ndvidif L1_ndvidif2 
gl weather8  ndvi_lt Lcum_ndvidif Lcum_ndvidif2 
gl weather9  ndvi_lt L1_ndvihishock L1_ndviloshock

gl other     Malaria_yearly 
gl AEZ       arid_warm humid_cool humid_warm
gl country   ctry2-ctry24
gl yr        yr2-yr13
