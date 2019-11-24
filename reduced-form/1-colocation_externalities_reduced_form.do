********************************************************************************
********************************************************************************
*
* FILE: colocation_externalities_reduced_form.do
*
* BY: Dmitry Sedov 
*
* CREATED: Wed Nov 20 2019 
*
* DESC: Analysis for the presentation.
*
********************************************************************************
********************************************************************************

clear

* Keep the log file
log using "/home/quser/project_dir/urban/docs/initial_reduced_form", text replace

****************************** NAICS - visits **********************************
* Import the data
import delimited /home/quser/downloads/cbgs_naics.csv

* Relabel variables
label variable cbg "CBG"
label variable tract "Census Tract"
label variable raw_visit_count "Visits"
label variable devices "Devices"
label variable area_m2 "Area (sq. m.)"
label variable naics11 "Agriculture"
label variable naics21 "Mining"
label variable naics22 "Utilities"
label variable naics23 "Construction"
label variable naics31 "Manufacturing (1)"
label variable naics32 "Manufacturing (2)"
label variable naics33 "Manufacturing (3)"
label variable naics42 "Wholesale"
label variable naics44 "Retail (1)"
label variable naics45 "Retail (2)"
label variable naics48 "Transportation"
label variable naics49 "Warehousing"
label variable naics51 "Information"
label variable naics52 "Finance"
label variable naics53 "R. Estate"
label variable naics54 "Professional"
label variable naics55 "Management"
label variable naics56 "Administrative"
label variable naics61 "Education"
label variable naics62 "Health"
label variable naics71 "Entertainment"
label variable naics72 "Food + Accom."
label variable naics81 "Services"
label variable naics92 "Pub. Adm."
label variable naicsna "NA" 

* Run the regression without Census Tract fixed effects
reg raw_visit_count devices area_m2 naics*, cluster(tract)
estimates store wo_fe

* Run the regression with Census Tract fixed effects
reghdfe raw_visit_count devices area_m2 naics*, absorb(tract) vce(cluster tract)
estimates store w_fe

* Plot the coefficients
coefplot (wo_fe, label(Without CT FE)) (w_fe, label(With CT FE)), drop(_cons naics21 naics22) xline(0, lcolor(gray) lpattern(dash)) title("Regression coefficients") xtitle("Delta (Visits)") 
graph export "/home/quser/project_dir/urban/output/reg_visits_naics.pdf", replace

********************************************************************************

clear

**************************** Visits - co-visits ********************************

* Import data
import delimited /home/quser/downloads/restaurants_daily.csv
* Generate restaurant id
egen id = group(sname_place_id)
* Generate CT indicator
gen double tract = floor(cbg / 1000)

* Run the regression:
quietly reghdfe own_visits co_visits_all, absorb(id tract#day) vce(cluster tract) poolsize(1)
estadd beta
estimates store model1
quietly reghdfe own_visits comp_visits_all, absorb(id tract#day) vce(cluster tract) poolsize(1)
estadd beta
estimates store model2
quietly reghdfe own_visits co_visits_all comp_visits_all, absorb(id tract#day) vce(cluster tract) poolsize(1)
estadd sd
estadd ysumm
estadd beta
estimates store model3
estout model1 model2 model3, cells((b(star label(Coef.) fmt(3)) sd) se(par) beta) stats(N r2 vce ymean ysd, fmt(0 2)) legend

********************************************************************************

clear

**************************** Visits - distance ********************************

* Import data
import delimited /home/quser/project_dir/data/restaurants_daily_multiple_distances.csv

* Generate restaurant id
egen id = group(sname_place_id)
* Generate CT indicator
gen double tract = floor(cbg / 1000)

* Missing values to 0s
replace visits_within_100 = 0 if missing(visits_within_100)
replace visits_within_200 = 0 if missing(visits_within_200)
replace visits_within_300 = 0 if missing(visits_within_300)
replace visits_within_400 = 0 if missing(visits_within_400)
replace visits_within_500 = 0 if missing(visits_within_500)

* Visits in disks
gen visits_within_100_200 = visits_within_200 - visits_within_100
gen visits_within_200_300 = visits_within_300 - visits_within_200
gen visits_within_300_400 = visits_within_400 - visits_within_300
gen visits_within_400_500 = visits_within_500 - visits_within_400

* Run the regression
quietly reghdfe own_visits visits_within_100 visits_within_100_200 visits_within_200_300 visits_within_300_400 visits_within_400_500, absorb(id tract#day) vce(cluster tract) poolsize(1) old
estadd sd
estadd ysumm
estadd beta
estimates store model_mult

* Rename variables
label variable visits_within_100 "(0, 100)"
label variable visits_within_100_200 "(100, 200)"
label variable visits_within_200_300 "(200, 300)"
label variable visits_within_300_400 "(300, 400)"
label variable visits_within_400_500 "(400, 500)"

* Coefplot
coefplot (model_mult, recast(connected) ciopts(recast(rcap))), vertical xtitle("Distance range") title("Regression coefficients")
graph export "/home/quser/project_dir/urban/output/reg_visits_multiple.pdf", replace

********************************************************************************



************************** Prices and colocation *******************************

* Import the data
import delimited /home/quser/downloads/yelp_restaurants_colocation.csv

* Turn price into a binary variable
gen price_cat = 0 if price == 1
replace price_cat = 1 if price > 1

* Turn categories into a group variable
egen type = group(category1)

* Generate the CT variable
gen double tract = floor(cbg / 1000)

* Missing visits - equal to 0
replace visits_within_100 = 0 if missing(visits_within_100)
replace visits_within_500 = 0 if missing(visits_within_500)
replace visits_within_900 = 0 if missing(visits_within_900)

* Generate visits in disks
gen visits_within_100_500 = visits_within_500 - visits_within_100
gen visits_within_500_900 = visits_within_900 - visits_within_500
gen pois_within_100_500 = pois_within_500 - pois_within_100
gen pois_within_500_900 = pois_within_900 - pois_within_500

* Run the regressions on visits to nearby places
quietly reghdfe price_cat visits_within_100 visits_within_100_500 visits_within_500_900, absorb(type tract) vce(cluster tract)
estadd sd
estadd ysumm
estadd beta
estimates store model_price_visits_tract
quietly reghdfe price_cat visits_within_100 visits_within_100_500 visits_within_500_900, absorb(type cbg) vce(cluster cbg)
estadd beta
estimates store model_price_visits_cbg
estout model_price_visits_tract model_price_visits_cbg, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

* Run the regression on nearby POIs
quietly reghdfe price_cat pois_within_100 pois_within_100_500 pois_within_500_900, absorb(type tract) vce(cluster tract)
estadd sd
estadd ysumm
estadd beta
estimates store model_price_pois_cbg
estout model_price_pois_cbg, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend
********************************************************************************

clear

****************************** IV approach *************************************
* Import the monthly panel
import delimited /home/quser/project_dir/data/monthly_events_panel_200.csv

replace co_visits_all = 0 if missing(co_visits_all)
egen id = group(sname_place_id)
gen double tract = floor(cbg / 1000)

* Simple fixed effects regression
reghdfe own_visits co_visits_all, absorb(id tract#year#month) vce(cluster tract)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_monthly

* Instrumenting with music events
ivreghdfe own_visits (co_visits_all=co_events), absorb(id tract#year#month) cluster(tract) first
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
quietly estimates store model_monthly_iv
estout model_monthly model_monthly_iv, cells((b(star label(Coef.) fmt(3)) sd) se(par) beta)  drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend
********************************************************************************

* Close the log
log close
