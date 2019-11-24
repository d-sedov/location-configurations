/*******************************************************************************
*******************************************************************************/
--
-- FILE: 4-poi-visits-geo-description.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Wed Nov 20 2019
--
-- DESC: This file produces the POIs and visits breakdown by state and zip_code.
-- 
-- REF:
--
-- COMMENT: 
/*******************************************************************************
*******************************************************************************/

CREATE TEMPORARY TABLE temp_poi_geo_table AS 
    SELECT 
        state,
        COUNT(*) AS pois
    FROM 
        pois
    GROUP BY 
        state;

\copy (SELECT * FROM temp_poi_geo_table) to '/home/user/projects/urban/data/processed/descriptive/pois_by_state.csv' with csv header

DROP TABLE temp_poi_geo_table;

CREATE TEMPORARY TABLE temp_poi_geo_table AS 
    SELECT 
        zip_code,
        COUNT(*) AS pois
    FROM 
        pois
    GROUP BY 
        zip_code;

\copy (SELECT * FROM temp_poi_geo_table) to '/home/user/projects/urban/data/processed/descriptive/pois_by_zip.csv' with csv header

DROP TABLE temp_poi_geo_table;

CREATE TEMPORARY TABLE temp_visits_geo_table AS 
    SELECT 
        state,
        SUM(raw_visit_counts) AS visits
    FROM 
        patterns
    GROUP BY 
        state;

\copy (SELECT * FROM temp_visits_geo_table) to '/home/user/projects/urban/data/processed/descriptive/visits_by_state.csv' with csv header

DROP TABLE temp_visits_geo_table;

CREATE TEMPORARY TABLE temp_visits_geo_table AS 
    SELECT 
        zip_code,
        SUM(raw_visit_counts) AS visits
    FROM 
        patterns
    GROUP BY 
        zip_code;

\copy (SELECT * FROM temp_visits_geo_table) to '/home/user/projects/urban/data/processed/descriptive/visits_by_zip.csv' with csv header

DROP TABLE temp_visits_geo_table;
