********************************************************************************
********************************************************************************
*
* FILE: 0-logit-cross-section-first-price.do
*
* BY: Dmitry Sedov 
*
* CREATED: Fri Mar 20 2020
*
* DESC: Logit + nested logit demand estimation instrumenting the price and the
*       within-category share.
*
********************************************************************************
********************************************************************************

*************************** Settings, preimport data  **************************

set linesize 200
log using "/home/quser/project_dir/urban/docs/logit-demand-cross-section-first-price", text replace
* Import data
import delimited /home/quser/project_dir/urban/data/output/spatial-demand/restaurants-direct/data_restaurants_oct18.csv

* Encode missing price as 0
replace price = 0 if price == -1
replace price = 0 if missing(price)
* Keep only if price is 1 or 2
drop if price == 0
drop if price == 3
drop if price == 4
* Create high price indicator
replace price = price - 1

* A common 'brand' for all non-branded restaurants
replace brands = "none" if missing(brands)
* Log market share of the product
gen lsj = ln(visits_from_home_cbg)
gen lsj_ls0 = ln(visits_from_home_cbg / outside_good_count)

* Distance in kilometers
gen distance_centroid_point_km = distance_centroid_point / 1000
gen distance_centroid_point_km_2 = distance_centroid_point_km^2

* Drop too-far-away-observations
summarize distance_centroid_point_km, detail
drop if distance_centroid_point_km > r(p99)

* Merge with the instruments
merge m:1 sname_place_id using /home/quser/project_dir/urban/data/output/spatial-demand/restaurants-direct/restaurants_neighbors.dta
drop if _merge == 2

* Restaurant fixed effect
egen rest_id = group(sname_place_id)

* Categorical rating 
tostring rating, generate(rating_string)
encode rating_string, generate(rating_id)
* Rating^2
gen rating_2 = rating^2

* Generate total visits to category of each restaurant
egen group_visits = total(visits_from_home_cbg), by(home_cbg category1)
gen within_group = ln(visits_from_home_cbg / group_visits)

* Generate average distance to other restaurants in the same category
egen group_distance = total(distance_centroid_point_km), by(home_cbg category1)
replace group_distance = group_distance - distance_centroid_point_km
egen n_home_category = count(rest_id), by(home_cbg category1)
replace group_distance = group_distance / (n_home_category - 1)

********************************************************************************


**************************** Run the estimation ********************************

* Simple logit specification
*eststo: reghdfe lsj_ls0 distance_centroid_point_km i.price rating rating_2 area_m2 i.naics_code cbg_est_number, absorb(home_cbg brands category1) vce(robust) noconstant
*vce(cluster home_cbg rest_id) noconstant
* eststo: reghdfe lsj_ls0 distance_centroid_point_km, absorb(home_cbg rest_id) vce(robust) noconstant
*vce(cluster home_cbg rest_id) noconstant

* IV logit specification
eststo: ivreghdfe lsj_ls0 distance_centroid_point_km (i.price = i.category1_equal) rating rating_2 area_m2 i.naics_code cbg_est_number, absorb(home_cbg brands category1) robust noconstant
* cluster(home_cbg rest_id) noconstant

* Nested logit specifications
*eststo: reghdfe lsj distance_centroid_point_km distance_centroid_point_km_2 i.price rating rating_2 within_group area_m2 i.naics_code cbg_est_number, absorb(home_cbg brands) vce(cluster home_cbg rest_id)
*eststo: reghdfe lsj_ls0 distance_centroid_point_km distance_centroid_point_km_2 i.price rating rating_2 within_group area_m2 i.naics_code cbg_est_number, absorb(brands) vce(cluster home_cbg rest_id)
*eststo: ivreghdfe lsj distance_centroid_point_km distance_centroid_point_km_2 rating rating_2 (i.price within_group = i.category1_equal neighbor_price group_distance) area_m2 i.naics_code cbg_est_number, absorb(home_cbg brands) cluster(home_cbg rest_id) first noconstant

esttab, se

********************************************************************************
