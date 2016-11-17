********************************************************
******** 4. GEOGRAPHICALLY WEIGHTED REGRESSIONS ********
********************************************************

use "D:\Sara\current\SSApoverty_Dist_forGWR.dta", clear

** Define global with variables to use in the regression
gl outcome1 pcexp_ppp_m 
gl outcome2 foodexp_ppp_m
gl outcome3 foodshare
gl outcome4 wealth_index_all

gl socio     rural hh_female agehead marriedhead meaneduc depratio2 children 
gl agri      LGP_AVG ELEVATION cropland_pcap TLU_pcap 
gl access    nighlight_yearly // tt10_20k PD12_TOT
gl weather0   
gl weather1   spei_lt L1_speihishock L1_speiloshock
gl weather2   spei_lt L1_speidif L1_speidif2 
gl weather3  pre_lt temp_lt L1_prehishock L1_preloshock ///
             L1_temphishock 
gl weather4  pre_lt L1_predif L1_predif2  ///
             temp_lt L1_tempdif L1_tempdif2 
gl weather5  ndvi_ave L1_ndvihishock L1_ndviloshock
gl weather6  ndvi_ave L1_ndvidif L1_ndvidif2 
gl other     Malaria_yearly 
gl AEZ       arid_warm humid_cool humid_warm
gl country   ctry2-ctry24
gl yr        yr2-yr13


** Generate a variable recentering the coordinates to make them all positive (required by the gwr command)
foreach var in X Y {
sum `var', det
gen `var'min=r(min)
gen `var'linear=`var'-`var'min
replace `var'linear=abs(`var'linear)
drop `var'min
}

rename Xlinear east
rename Ylinear north

** multiply all the variable by the survey weights since cannot use survey weigths in gwr command
foreach var in $outcome1 $weather1 $weather2 $weather3 $weather4 $weather5 $weather6 ///
               $socio $access $agri $other {  
 replace `var'=`var'*pop 
 }

 ** Run geographically weighted regressions using both outcomes of interest and all the 6 weather specifications
 **!! Save dta files with the district specific coefficient for each variable
 **!! Command tests the additional fit of the gwr model with respect to general spatial model
 forval i=1/2 {
gwr ${outcome`i'} $weather1 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_spei1.dta")
gwr ${outcome`i'} $weather2 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_spei2.dta")
gwr ${outcome`i'} $weather3 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_weat1.dta")
gwr ${outcome`i'} $weather4 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_weat2.dta")
gwr ${outcome`i'} $weather5 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_ndvi1.dta")
gwr ${outcome`i'} $weather6 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_ndvi2.dta")

}
