
*************************** Settings, preimport data  **************************

set linesize 200
log using "/home/quser/project_dir/urban/docs/temp-delete", text replace
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
gen distance_closest_point_km = distance_closest_point / 1000
gen distance_closest_point_km_2 = distance_closest_point_km^2

* Drop too-far-away-observations
summarize distance_closest_point_km, detail
drop if distance_closest_point_km > r(p99)

* Restaurant fixed effect
egen rest_id = group(sname_place_id)

egen brand_id = group(brands)

egen categ_id = group(category1)

* Rating^2
gen rating_2 = rating^2


********************************************************************************


**************************** Run the estimation ********************************

* Simple logit specification
reghdfe lsj distance_closest_point_km distance_closest_point_km_2 i.price rating rating_2 area_m2 i.naics_code cbg_est_number i.brand_id i.categ_id, absorb(home_cbg) vce(robust) noconstant
