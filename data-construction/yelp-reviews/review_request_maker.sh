#!/bin/bash
################################################################################
################################################################################
#
# FILE: review_request_maker.sh
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Apr 9 2020
#
# DESC: 
#
# EXEC: CHANGE PARAMETERS api_key, n_records,
#       log_file_path, it your preferred ones.
#       Run with ./review_request_maker.sh
#
# NOTE: FILES HAVE BEEN MOVED, PARAMETERS NEED TO BE CHANGED.
#      
################################################################################
################################################################################


############################# PARAMETERS #######################################
today=$(date '+%Y%m%d-%H%M')
api_key=***REMOVED***
n_records=4925
log_file_path=/home/user/projects/urban/code/data-construction/yelp-reviews/review_download.log
postfix=user.$today
################################################################################

# Run the python script
cd /home/user/projects/urban/code/data-construction/yelp-reviews/
echo "Making review requests."
python3 review_request.py $api_key $n_records $log_file_path $postfix

################################################################################
