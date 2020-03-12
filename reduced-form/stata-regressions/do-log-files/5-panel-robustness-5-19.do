********************************************************************************
********************************************************************************
*
* FILE: 5-panel-robustness-5-19.do
*
* BY: Dmitry Sedov 
*
* CREATED: Tue Feb 25 2020
*
* DESC: Panel data robustness check analysis: entry / exit between kind = 5 and
*       kind = 19 considered only.
*
********************************************************************************
********************************************************************************

*************************** Settings, preimport data  **************************

log using "/home/quser/project_dir/urban/docs/panel-regressions-robustness-5-19", text replace

* Import the conservatively labeled changes in the cbg establishment scene
import delimited /home/quser/project_dir/data/cbg_establishments_over_time_conservative_5_19.csv
* import delimited /home/quser/project_dir/urban/data/output/reduced-form/cbg_establishments_over_time_conservative_5_19.csv
tempfile cbg_est_cons_5_19
save `cbg_est_cons_5_19'
clear

********************************************************************************


**************************** Conservative analysis *****************************

* Import main data with restaurant visits
import delimited /home/quser/project_dir/data/full_monthly_panel.csv
* import delimited /home/quser/project_dir/urban/data/output/reduced-form/full_monthly_panel.csv

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

clear

log close

********************************************************************************
