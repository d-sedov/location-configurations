#!/bin/bash
################################################################################
################################################################################
#
# FILE: request_maker.sh
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Oct 14 2019
#
# DESC: This file contains the code that takes a single file from input_folder_path
#       folder, makes Yelp API Search requests for the locations in that file
#       using the python script make_requests_part.py and moves the input file
#       to the 'done' location.
#
# EXEC: CHANGE PARAMETERS api_key, input_folder_path, output_folder_path,
#       log_file_path, done_folder_path to your preferred ones.
#       Run with ./request_maker.sh
#
# NOTE: FILE HAS BEEN MOVED, PARAMETERS NEED TO BE CHANGED.
#      
################################################################################
################################################################################


############################# PARAMETERS #######################################
api_key=***REMOVED***
input_folder_path=/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/add_part/to_do/
output_folder_path=/home/user/projects/urban/data/input/Yelp/from_api/add_part/
log_file_path=/home/user/projects/urban/code/yelp-api-download/logs/yelp_api_add.log
done_folder_path=/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/add_part/done/
################################################################################


############################# Full file paths ##################################
# Take a file from the input folder
input_file_name=$(ls -v $input_folder_path | head -1)
input_file_path=$input_folder_path$input_file_name
done_file_path=$done_folder_path$input_file_name
################################################################################

############################# If input file found ##############################
if [ -z "$input_file_name" ]
then
    echo "No input files found."
else
    echo "File $input_file_name chosen. Starting to make API requests."
    # Run python script and move the input file to 'done' folder
    python3 make_requests_part.py $api_key $input_file_path $output_folder_path $log_file_path &&
        mv $input_file_path $done_file_path
fi
################################################################################
