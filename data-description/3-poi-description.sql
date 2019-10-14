/*******************************************************************************
*******************************************************************************/
--
-- FILE: 3-poi-description.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Thu Sep 19 2019
--
-- DESC: This file produces the POIs table needed for summary statistics.
-- 
-- REF:
--
-- COMMENT: 
/*******************************************************************************
*******************************************************************************/

-- Create a table with the POI information needed for the summary statistics
CREATE TEMPORARY TABLE temp_poi_table AS
    SELECT 
        p.naics_first2,
        p.phone_number IS NOT NULL AS phone_available,
        p.total_minutes_open,
        p.time_ok,
        gp.msa_bool,
        gp.cbsa,
        gp.cbg,
        gp.area_m2,
        pa.raw_visit_counts,
        pa.raw_visitor_counts,
        pa.distance_from_home,
        pa.median_dwell, 
        (SELECT COUNT(*) FROM jsonb_object_keys(visitor_home_cbgs::jsonb)) from_cbgs,
        (pa.device_type->>'ios')::integer AS ios,
        (pa.device_type->>'android')::integer AS android
    FROM 
        pois AS p
    LEFT JOIN 
        geometry_pois AS gp 
    ON 
        p.sname_place_id = gp.sname_place_id
    LEFT JOIN 
        patterns AS pa 
    ON 
        p.sname_place_id = pa.sname_place_id;

UPDATE 
    temp_poi_table
SET 
    from_cbgs = NULL
WHERE 
    from_cbgs = 0;

/***************** draw random sample to explore the data *********************/

-- RANDOM sample
-- \copy (select * FROM temp_poi_table tablesample bernoulli(5)) to '/home/user/projects/urban/data/processed/descriptive/pois_sample_5pct.csv' with csv header

-- FULL table:
\copy (SELECT * FROM temp_poi_table) to '/home/user/projects/urban/data/processed/descriptive/pois_full.csv' with csv header
/******************************************************************************/
