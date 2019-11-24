/*******************************************************************************
*******************************************************************************/
--
-- FILE: 5-yelp-description.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Wed Nov 20 2019
--
-- DESC: This file produces the Yelp dataset extract for descriptive summary
-- statistics purposes.
-- 
-- REF:
--
-- COMMENT: 
/*******************************************************************************
*******************************************************************************/

CREATE TEMPORARY TABLE temp_yelp_table AS
    SELECT 
        phone IS NOT NULL AS phone_available,
        price,
        review_count,
        rating,
        categories,
        zip_code,
        pois_within_500,
        category1
    FROM 
        yelp_restaurants;

\copy (SELECT * FROM temp_yelp_table) to '/home/user/projects/urban/data/processed/descriptive/yelp_description.csv' with csv header
