********************************************************************************
********************************************************************************
*
* FILE: 0-logit-cross-section.do
*
* BY: Dmitry Sedov 
*
* CREATED: Fri Mar 20 2020
*
* DESC: Logit demand estimation using cross-section data.
*
********************************************************************************
********************************************************************************

*************************** Settings, preimport data  **************************

set linesize 200
log using "/home/quser/project_dir/urban/docs/logit-demand-cross-section-first", text replace
* Import data
import delimited /home/quser/project_dir/urban/data/output/spatial-demand/restaurants-direct/data_restaurants_oct18.csv

* Encode missing price as 0
replace price = 0 if price == -1
replace price = 0 if missing(price)

* A common 'brand' for all non-branded restaurants
replace brands = "none" if missing(brands)
* Log market share difference between product and outside good
gen lsj_ls0 = ln(visits_from_home_cbg / outside_good_count)

* Distance in kilometers
gen distance_closest_point_km = distance_closest_point / 1000
gen distance_closest_point_km_2 = distance_closest_point_km^2

* Drop too-far-away-observations
summarize distance_closest_point_km, detail
drop if distance_closest_point_km > r(p99)

* Restaurant fixed effect
egen rest_id = group(sname_place_id)

* Generate total visits to category of each restaurant
egen group_visits = total(visits_from_home_cbg), by(home_cbg category1)
gen within_group = ln(visits_from_home_cbg / group_visits)

* Generate average distance to other restaurants in the same category
egen group_distance = total(distance_closest_point_km), by(home_cbg category1)
replace group_distance = group_distance - distance_closest_point_km
egen n_home_category = count(rest_id), by(home_cbg category1)
replace group_distance = group_distance / (n_home_category - 1)

********************************************************************************


**************************** Run the estimation ********************************

* reghdfe lsj_ls0 distance_closest_point_km i.price rating area_m2 i.naics_code, absorb(home_cbg brands category1) vce(cluster home_cbg rest_id)  
* reghdfe lsj_ls0 distance_closest_point_km i.price rating area_m2 i.naics_code cbg_est_number, absorb(home_cbg brands category1) vce(cluster home_cbg rest_id)  
* reghdfe lsj_ls0 distance_closest_point_km i.price rating area_m2 i.naics_code cbg_naics*, absorb(home_cbg brands category1) vce(cluster home_cbg rest_id)
* reghdfe lsj_ls0 distance_closest_point_km, absorb(home_cbg rest_id) vce(cluster home_cbg rest_id)

* Simple logit specifications
*reghdfe lsj_ls0 distance_closest_point_km distance_closest_point_km_2 i.price rating area_m2 i.naics_code, absorb(home_cbg) vce(cluster home_cbg rest_id)
reghdfe lsj_ls0 distance_closest_point_km distance_closest_point_km_2 i.price rating area_m2 i.naics_code, absorb(home_cbg brands category1) vce(cluster home_cbg rest_id)
*reghdfe lsj_ls0 distance_closest_point_km distance_closest_point_km_2 i.price##c.rating area_m2 i.naics_code, absorb(home_cbg brands category1) vce(cluster home_cbg rest_id)

* Nested logit specifications
ivreghdfe lsj_ls0 distance_closest_point_km distance_closest_point_km_2 i.price rating (within_group = group_distance) area_m2 i.naics_code, absorb(home_cbg brands) cluster(home_cbg rest_id) first
reghdfe lsj_ls0 distance_closest_point_km distance_closest_point_km_2 i.price rating within_group area_m2 i.naics_code, absorb(home_cbg brands) vce(cluster home_cbg rest_id)

tostring rating, generate(rating_string)
encode rating_string, generate(rating_id)
reghdfe lsj_ls0 distance_closest_point_km distance_closest_point_km_2 i.price i.rating_id area_m2 i.naics_code, absorb(home_cbg brands category1) vce(cluster home_cbg rest_id)
reghdfe lsj_ls0 distance_closest_point_km distance_closest_point_km_2 i.price##i.rating_id area_m2 i.naics_code, absorb(home_cbg brands category1) vce(cluster home_cbg rest_id)
*reghdfe lsj_ls0 distance_closest_point_km distance_closest_point_km_2 i.price rating area_m2 i.naics_code cbg_naics* ct_naics*, absorb(home_cbg brands category1) vce(cluster home_cbg rest_id)

********************************************************************************
