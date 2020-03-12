********************************************************************************
********************************************************************************
*
* FILE: 2-cross-section-regressions.do
*
* BY: Dmitry Sedov 
*
* CREATED: Wed Feb 26 2020
*
* DESC: Cross-section analysis
*
********************************************************************************
********************************************************************************

clear

* Keep the log file
log using "/home/quser/project_dir/urban/docs/cross-section-regressions", text replace

* Import data
use /home/quser/project_dir/data/restaurants_cross_section

* Numeric to factor variables
tostring price, generate(price_string)
encode price_string, generate(price_nominal)
tostring rating, generate(rating_string)
encode rating_string, generate(rating_nominal)

* All restaurants
* Price and ratings indicators
reghdfe raw_visit_counts est_number number_devices_residing i.price_bin##i.rating_bin i.naics_code area_m2 i.phone median_hh_income, absorb(ct brands category1) vce(cluster ct) 
margins, dydx(price_bin rating_bin) post noestimcheck
reghdfe raw_visit_counts est_number number_devices_residing rest_number i.price_bin##i.rating_bin i.naics_code area_m2 i.phone median_hh_income, absorb(ct brands category1) vce(cluster ct) 
margins, dydx(price_bin rating_bin) post noestimcheck

* Price and ratings nominal
reghdfe raw_visit_counts est_number number_devices_residing i.price_nominal##i.rating_nominal i.naics_code area_m2 i.phone median_hh_income, absorb(ct brands category1) vce(cluster ct) 
margins, dydx(price_nominal rating_nominal) post noestimcheck
reghdfe raw_visit_counts est_number number_devices_residing rest_number i.price_nominal##i.rating_nominal i.naics_code area_m2 i.phone median_hh_income, absorb(ct brands category1) vce(cluster ct) 
margins, dydx(price_nominal rating_nominal) post noestimcheck

* Urban restauarants only
drop if missing(cbsa)
* Price and ratings indicators
reghdfe raw_visit_counts est_number number_devices_residing i.price_bin##i.rating_bin i.naics_code area_m2 i.phone median_hh_income, absorb(ct brands category1) vce(cluster ct) 
margins, dydx(price_bin rating_bin) post noestimcheck
reghdfe raw_visit_counts est_number number_devices_residing rest_number i.price_bin##i.rating_bin i.naics_code area_m2 i.phone median_hh_income, absorb(ct brands category1) vce(cluster ct) 
margins, dydx(price_bin rating_bin) post noestimcheck

* Price and ratings nominal
reghdfe raw_visit_counts est_number number_devices_residing i.price_nominal##i.rating_nominal i.naics_code area_m2 i.phone median_hh_income, absorb(ct brands category1) vce(cluster ct) 
margins, dydx(price_nominal rating_nominal) post noestimcheck
reghdfe raw_visit_counts est_number number_devices_residing rest_number i.price_nominal##i.rating_nominal i.naics_code area_m2 i.phone median_hh_income, absorb(ct brands category1) vce(cluster ct) 
margins, dydx(price_nominal rating_nominal) post noestimcheck

log close
