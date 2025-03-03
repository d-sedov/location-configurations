********************************************************************************
********************************************************************************
*
* FILE: 6-panel-regressions-5-19.do
*
* BY: Dmitry Sedov 
*
* CREATED: Mon Feb 24 2020
*
* DESC: Panel data analysis: how do restaurant visits react to entry / exit of
* new establishments nearby? how about the local device count? Only 5-19
* entrants / exitors considered.
*
********************************************************************************
********************************************************************************

*************************** Settings, preimport data  **************************

log using "/home/quser/project_dir/urban/docs/panel-regressions-5-19", text replace

* Import the conservatively labeled changes in the cbg establishment scene
import delimited /home/quser/project_dir/data/cbg_establishments_over_time_conservative_5_19.csv
* import delimited /home/quser/project_dir/urban/data/output/reduced-form/cbg_establishments_over_time_conservative_5_19.csv
tempfile cbg_est_cons_5_19
save `cbg_est_cons_5_19'
clear

* Import the dirtyly labeled changed in the cbg establishment scene
import delimited /home/quser/project_dir/data/cbg_categs.csv
* import delimited /home/quser/project_dir/urban/data/output/reduced-form/cbg_categs.csv
tempfile cbg_est_simple 
save `cbg_est_simple'
clear

********************************************************************************


******************************** Dirty analysis ********************************

* Import main data with restaurant visits
import delimited /home/quser/project_dir/data/full_monthly_panel.csv
* import delimited /home/quser/project_dir/urban/data/output/reduced-form/full_monthly_panel.csv

* Merge with dirty changes
merge m:1 cbg year month using `cbg_est_simple'
drop if _merge == 2

* Generate restaurant id
egen id = group(rest_id)

* Regressions of visits on establishments in cbg and devices in cbg with and
* without ct fixed effects, with and without countrol for restaurants
reghdfe raw_visit_counts est_in_cbg devices_in_cbg, absorb(id year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_simple_no_ct

reghdfe raw_visit_counts est_in_cbg devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_simple_ct

reghdfe raw_visit_counts est_in_cbg devices_in_cbg rest_open, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_simple_rest

estout model_simple_no_ct model_simple_ct model_simple_rest, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

* Break down by naics categories, with and without restaurants
reghdfe raw_visit_counts devices_in_cbg naics*, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store naics_simple

reghdfe raw_visit_counts devices_in_cbg naics* rest_open, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store naics_simple_rest

estout naics_simple naics_simple_rest, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

* Repeat analysis for urban locations only
drop if missing(cbsa)

reghdfe raw_visit_counts est_in_cbg devices_in_cbg, absorb(id year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_simple_no_ct

reghdfe raw_visit_counts est_in_cbg devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_simple_ct

reghdfe raw_visit_counts est_in_cbg devices_in_cbg rest_open, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_simple_rest

estout urban_model_simple_no_ct urban_model_simple_ct urban_model_simple_rest, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

reghdfe raw_visit_counts devices_in_cbg naics*, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_naics_simple

reghdfe raw_visit_counts devices_in_cbg naics* rest_open, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_naics_simple_rest

estout urban_naics_simple urban_naics_simple_rest, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

* Regression with establishment in cbg and devices in cbg as the independent
* variable
* gen devices_in_200_400_m = devices_in_400m - devices_in_200m
* gen devices_in_400_600_m = devices_in_600m - devices_in_400m
* Regression with distance bands as independent variable
* reghdfe raw_visit_counts est_in_200m est_in_400m est_in_600m devices_in_200m devices_in_200_400_m devices_in_400_600_m, absorb(id ct#year#month) vce(cluster ct) 
* reghdfe raw_visit_counts est_in_200m est_in_400m est_in_600m devices_in_200m devices_in_200_400_m devices_in_400_600_m, absorb(id cbg#year#month) vce(cluster cbg) 

clear

********************************************************************************


**************************** Conservative analysis *****************************

* Import main data with restaurant visits
import delimited /home/quser/project_dir/data/full_monthly_panel.csv

* Merge with dirty changes
merge m:1 cbg year month using `cbg_est_cons_5_19'
drop if _merge == 2

* Generate restaurant id
egen id = group(rest_id)

* Regressions of visits on establishments in cbg and devices in cbg with and
* without ct fixed effects, with and without countrol for restaurants
reghdfe raw_visit_counts total_est devices_in_cbg, absorb(id year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_cons_no_ct

reghdfe raw_visit_counts total_est devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_cons_ct

reghdfe raw_visit_counts total_est devices_in_cbg rest, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_cons_rest

estout model_cons_no_ct model_cons_ct model_cons_rest, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

* Break down by naics categories, with and without restaurants
reghdfe raw_visit_counts devices_in_cbg naics*, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store naics_cons

reghdfe raw_visit_counts devices_in_cbg naics* rest, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store naics_cons_rest

estout naics_cons naics_cons_rest, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

* Repeat analysis for urban locations only
drop if missing(cbsa)

reghdfe raw_visit_counts total_est devices_in_cbg, absorb(id year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_cons_no_ct

reghdfe raw_visit_counts total_est devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_cons_ct

reghdfe raw_visit_counts total_est devices_in_cbg rest, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_cons_rest

estout urban_model_cons_no_ct urban_model_cons_ct urban_model_cons_rest, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

reghdfe raw_visit_counts devices_in_cbg naics*, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_naics_cons

reghdfe raw_visit_counts devices_in_cbg naics* rest, absorb(id ct#year#month) vce(cluster ct) 
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_naics_cons_rest

estout urban_naics_cons urban_naics_cons_rest, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

clear

log close

********************************************************************************
