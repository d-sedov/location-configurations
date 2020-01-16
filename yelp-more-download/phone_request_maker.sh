#!/bin/bash
################################################################################
################################################################################
#
# FILE: phone_request_maker.sh
#
# BY: Dmitry Sedov 
#
# CREATED: Fri Dec 13 2019
#
# DESC: 
#
# EXEC: CHANGE PARAMETERS api_key, n_records, output_folder_path,
#       log_file_path, it your preferred ones.
#       Run with ./phone_request_maker.sh
#      
################################################################################
################################################################################


############################# PARAMETERS #######################################
today=$(date '+%Y%m%d-%H%M')
api_key=***REMOVED***
n_records=4800
output_folder_path=/home/user/projects/urban/data/input/Yelp/phone_address/phone/$today/
log_file_path=/home/user/projects/urban/code/yelp-more-download/logs/yelp_phone_requests.log
postfix=user.$today
################################################################################

# Run the python script
mkdir $output_folder_path
cd /home/user/projects/urban/code/yelp-more-download/
echo "Making phone requests."
python3 yelp_requests.py $api_key $n_records $output_folder_path $log_file_path $postfix

################################################################################
