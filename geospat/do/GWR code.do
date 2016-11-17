********************************************************
******** 4. GEOGRAPHICALLY WEIGHTED REGRESSIONS ********
********************************************************

drop if ISO3=="ZAF"


** shift the coordinates to have them all positive
foreach var in X Y {
sum `var', det
gen `var'min=r(min)
gen `var'linear=`var'-`var'min
replace `var'linear=abs(`var'linear)
drop `var'min
}

rename Xlinear east
rename Ylinear north

** multiply variables by weights
foreach var in $outcome1 $weather1 $weather2 $weather3 $weather4 $weather5 $weather6 ///
               $socio $access $agri $other {  
 replace `var'=`var'*pop // multiply by weights since cannot use weights with spmlreg
 }
 

 ** run gwr and save dta with coefficiens
gwr $outcome1 $weather3 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_spei.dta")
gwr $outcome1 $weather6 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_weat.dta")
gwr $outcome1 $weather9 $socio $socio2 $access $agri $other  $country $yr, east(east) north(north) test saving("$table\gwr_ndvi.dta")
