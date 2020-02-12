#!/bin/bash
################################################################################
################################################################################
#
# FILE: address_request_maker.sh
#
# BY: Dmitry Sedov 
#
# CREATED: Tue Jan 21 2020
#
# DESC: 
#
# EXEC: CHANGE PARAMETERS api_key, n_records, output_folder_path,
#       log_file_path, it your preferred ones.
#       Run with ./phone_request_maker.sh
#
# NOTE: FILES HAVE BEEN MOVED, PARAMETERS NEED TO BE CHANGED.
#      
################################################################################
################################################################################


############################# PARAMETERS #######################################
today=$(date '+%Y%m%d-%H%M')
api_key=***REMOVED***
n_records=4800
output_folder_path=/home/user/projects/urban/data/input/Yelp/phone_address/address/$today/
log_file_path=/home/user/projects/urban/code/yelp-more-download/logs/yelp_address_requests.log
postfix=user.$today
################################################################################

# Run the python script
mkdir $output_folder_path
cd /home/user/projects/urban/code/yelp-more-download/
echo "Making address requests."
python3 yelp_address_requests.py $api_key $n_records $output_folder_path $log_file_path $postfix

################################################################################
