********************************************************************************
********************************************************************************
*
* FILE: 1-logit-demand-alt.do
*
* BY: Dmitry Sedov 
*
* CREATED: Thu Mar 19 2020
*
* DESC: Logit demand estimation using cross-section and panel data.
*
********************************************************************************
********************************************************************************

*************************** Settings, preimport data  **************************

log using "/home/quser/project_dir/urban/docs/logit-demand-alt", text replace
* Import the conservatively labeled changes in the cbg establishment scene
import delimited /home/quser/project_dir/urban/data/output/reduced-form/cbg_establishments_over_time_conservative_3_20.csv
* Change cbg column type and rename
gen str12 r_cbg = string(cbg, "%012.0f")
drop cbg
* Save
tempfile cbg_est_cons_3_20
save `cbg_est_cons_3_20'
clear

* Import data on within-CBG choices
use /home/quser/project_dir/urban/data/output/spatial-demand/restaurants-direct/months/restaurants_shares_0.dta
forvalues i = 1(1)25 {
    disp `i'
    append using "/home/quser/project_dir/urban/data/output/spatial-demand/restaurants-direct/months/restaurants_shares_`i'.dta"
}

* Recast data types
recast int year
recast byte month

* Merge datasets
merge m:1 r_cbg year month using `cbg_est_cons_3_20'
* Drop unmerged rows
drop if _merge==1
drop if _merge==2

* Generate needed variables
gen lsj_ls0 = ln(visits_from_home_cbg / outside_good_count)
egen rest_id = group(sname_place_id)
egen home_cbg_id = group(home_cbg) 
egen r_ct_id = group(r_ct)
* Drop unneeded variables
drop r_cbg home_cbg r_ct home_ct sname_place_id home_cbsa visits_from_home_cbg outside_good_count

********************************************************************************

**************************** Run the estimation ********************************

reghdfe lsj_ls0 total_est, absorb(home_cbg_id#year#month home_cbg_id#rest_id) vce(cluster home_cbg_id rest_id)

reghdfe lsj_ls0 naics*, absorb(home_cbg_id#year#month home_cbg_id#rest_id) vce(cluster home_cbg_id rest_id)

********************************************************************************
