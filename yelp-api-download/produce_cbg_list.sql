/*******************************************************************************
*******************************************************************************/
--
-- FILE: produce_cbg_list.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Mon Oct 14 2019
--
-- DESC: This files produces a CSV file with centroids of CBGs which have 
--       restaurants in them.
-- 
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/

-- Create a view that lists all CBGs with restaurants as well as restaurant
-- count.
CREATE TEMPORARY VIEW cbgs_rest AS (
    SELECT 
        g.cbg AS cbg,
        COUNT(DISTINCT p.sname_place_id) AS quantity
    FROM
        geometry_pois AS g
    LEFT JOIN 
        pois AS p
    ON 
        g.sname_place_id = p.sname_place_id
    WHERE
        p.naics_code = 722511 OR p.naics_code = 722513
    GROUP BY
        g.cbg
);

-- Create a table with centroid coordinates of CBGs with restaurants.
CREATE TEMPORARY TABLE cbgs_rest_coord AS (
    SELECT
        c.cbg,
        ST_Y(ST_Centroid(g.wkb_geometry)) AS lat,
        ST_X(ST_Centroid(g.wkb_geometry)) AS lon,
        c.quantity
    FROM 
        cbgs_rest AS c
    LEFT JOIN
        geometry_cbg AS g
    ON
        c.cbg = g.censusblockgroup
);
        
-- Export the resulting table
\copy (SELECT * FROM cbgs_rest_coord) to '/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/all/cbgs_rest_coord.csv' with csv header
