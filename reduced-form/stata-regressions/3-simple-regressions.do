********************************************************************************
********************************************************************************
*
* FILE: 3-simple-regressions.do
*
* BY: Dmitry Sedov 
*
* CREATED: Tue Feb 18 2020
*
* DESC: Cross-section analysis
*
********************************************************************************
********************************************************************************

clear

* Keep the log file
log using "/home/quser/project_dir/urban/docs/simple_regressions", text replace

* Import data
import delimited /home/quser/project_dir/data/full_monthly_panel.csv

* Get a month of cross-section data
keep if year == 2018 & month == 10

egen irating=group(rating), label

reghdfe raw_visit_counts i.price i.irating est_in_cbg devices_in_cbg, absorb(ct) vce(cluster ct) poolsize(1) old
reghdfe raw_visit_counts price rating est_in_cbg devices_in_cbg, absorb(ct) vce(cluster ct) poolsize(1) old

log close
