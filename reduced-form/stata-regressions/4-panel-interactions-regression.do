********************************************************************************
********************************************************************************
*
* FILE: 4-panel-interactions-regression.do
*
* BY: Dmitry Sedov 
*
* CREATED: Mon Feb 24 2020
*
* DESC: Panel data analysis: how do restaurant visits react to entry / exit of
* new establishments nearby? how about the local device count? Which restaurants
* react more (heterogeneity of the effect with respect to price / rating). 
*
********************************************************************************
********************************************************************************

*************************** Settings, preimport data  **************************

log using "/home/quser/project_dir/urban/docs/panel-interaction-regressions-3-20", text replace

* Import the conservatively labeled changes in the cbg establishment scene
import delimited /home/quser/project_dir/data/cbg_establishments_over_time_conservative_3_20.csv
tempfile cbg_est_cons_3_20
save `cbg_est_cons_3_20'
clear

* Import the dirtyly labeled changed in the cbg establishment scene
import delimited /home/quser/project_dir/data/cbg_categs.csv
tempfile cbg_est_simple 
save `cbg_est_simple'
clear

********************************************************************************


******************************** Dirty analysis ********************************

* Import main data with restaurant visits
import delimited /home/quser/project_dir/data/full_monthly_panel.csv

* Numeric to factor variables
tostring price, generate(price_string)
encode price_string, generate(price_ind)
tostring rating, generate(rating_string)
encode rating_string, generate(rating_ind)

* Merge with dirty changes
merge m:1 cbg year month using `cbg_est_simple'
drop if _merge == 2

* Generate restaurant id
egen id = group(rest_id)

* Regressions of visits on establishments in cbg and devices in cbg with ct fx
* and interaction with price and rating variables
reghdfe raw_visit_counts est_in_cbg i.price_ind#c.est_in_cbg devices_in_cbg i.price_ind#c.devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_simple_price
estout model_simple_price, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

reghdfe raw_visit_counts est_in_cbg i.rating_ind#c.est_in_cbg devices_in_cbg i.rating_ind#c.devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_simple_rating
estout model_simple_rating, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

* Repeat analysis for urban locations only
drop if missing(cbsa)

reghdfe raw_visit_counts est_in_cbg i.price_ind#c.est_in_cbg devices_in_cbg i.price_ind#c.devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_simple_price
estout urban_model_simple_price, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

reghdfe raw_visit_counts est_in_cbg i.rating_ind#c.est_in_cbg devices_in_cbg i.rating_ind#c.devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_simple_rating
estout urban_model_simple_rating, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

clear

********************************************************************************


**************************** Conservative analysis *****************************

* Import main data with restaurant visits
import delimited /home/quser/project_dir/data/full_monthly_panel.csv

* Numeric to factor variables
tostring price, generate(price_string)
encode price_string, generate(price_ind)
tostring rating, generate(rating_string)
encode rating_string, generate(rating_ind)

* Merge with dirty changes
merge m:1 cbg year month using `cbg_est_cons_3_20'
drop if _merge == 2

* Generate restaurant id
egen id = group(rest_id)

* Regressions of visits on establishments in cbg and devices in cbg with ct fx
* and interaction with price and rating variables
reghdfe raw_visit_counts total_est i.price_ind#c.total_est devices_in_cbg i.price_ind#c.devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_cons_price
estout model_cons_price, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

reghdfe raw_visit_counts total_est i.rating_ind#c.total_est devices_in_cbg i.rating_ind#c.devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_cons_rating
estout model_cons_rating, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

reghdfe raw_visit_counts total_est c.rest#c.total_est devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store model_cons_rest_comp
estout model_cons_rest_comp, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

* Repeat analysis for urban locations only
drop if missing(cbsa)

reghdfe raw_visit_counts total_est i.price_ind#c.total_est devices_in_cbg i.price_ind#c.devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_cons_price
estout urban_model_cons_price, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

reghdfe raw_visit_counts total_est i.rating_ind#c.total_est devices_in_cbg i.rating_ind#c.devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_cons_rating
estout urban_model_cons_rating, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

reghdfe raw_visit_counts total_est c.rest#c.total_est devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)
quietly estadd sd
quietly estadd ysumm
quietly estadd beta
estimates store urban_model_cons_rest_comp
estout urban_model_cons_rest_comp, cells((beta(star label(Beta) fmt(3)) sd) se(par)) drop(_cons) stats(N r2 vce ymean ysd, fmt(0 2)) legend

clear

log close

********************************************************************************
