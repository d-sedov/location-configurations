********************************************************************************
********************************************************************************
*
* FILE: 4-panel-visits-establishments-devices-open-close.do
*
* BY: Dmitry Sedov 
*
* CREATED: Sun Feb 23 2020
*
* DESC: Panel data analysis with open-close establishemnts and restaurants
* only.
*
********************************************************************************
********************************************************************************

clear

* Keep the log file
log using "/home/quser/project_dir/urban/docs/panel_visits_establishments_devices_open_close_cons", text replace

import delimited /home/quser/project_dir/data/cbg_open_close_categs_cons.csv
tempfile cbg_categs_open_close_cons
save `cbg_categs_open_close_cons'
clear

* Import data
import delimited /home/quser/project_dir/data/full_monthly_panel.csv

merge m:1 cbg year month using `cbg_categs_open_close_cons'
drop if _merge == 2

* Generate restaurant id
egen id = group(rest_id)

reghdfe raw_visit_counts est_open rest_open devices_in_cbg, absorb(id ct#year#month) vce(cluster ct)

log close
