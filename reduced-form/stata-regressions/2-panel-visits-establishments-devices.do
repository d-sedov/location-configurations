********************************************************************************
********************************************************************************
*
* FILE: 2-panel-visits-establishments-devices.do
*
* BY: Dmitry Sedov 
*
* CREATED: Tue Feb 18 2020
*
* DESC: Panel data analysis
*
********************************************************************************
********************************************************************************

clear

* Keep the log file
log using "/home/quser/project_dir/urban/docs/panel_visits_establishments_devices", text replace

import delimited /home/quser/project_dir/data/cbg_categs.csv
tempfile cbg_categs
save `cbg_categs'
clear

* Import data
import delimited /home/quser/project_dir/data/full_monthly_panel.csv

merge m:1 cbg year month using `cbg_categs'
drop if _merge == 2

* Generate restaurant id
egen id = group(rest_id)

reghdfe raw_visit_counts rest_open devices_in_cbg naics*, absorb(id ct#year#month) vce(cluster ct) 

reghdfe raw_visit_counts est_in_cbg devices_in_cbg, absorb(id ct#year#month) vce(cluster ct) 

* Regression with establishment in cbg and devices in cbg as the independent
* variable
reghdfe raw_visit_counts est_in_cbg devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)

gen devices_in_200_400_m = devices_in_400m - devices_in_200m
gen devices_in_400_600_m = devices_in_600m - devices_in_400m

* Regression with distance bands as independent variable
reghdfe raw_visit_counts est_in_200m est_in_400m est_in_600m devices_in_200m devices_in_200_400_m devices_in_400_600_m, absorb(id ct#year#month) vce(cluster ct) 
reghdfe raw_visit_counts est_in_200m est_in_400m est_in_600m devices_in_200m devices_in_200_400_m devices_in_400_600_m, absorb(id cbg#year#month) vce(cluster cbg) 

log close
