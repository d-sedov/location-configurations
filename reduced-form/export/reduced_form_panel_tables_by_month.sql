/*******************************************************************************
*******************************************************************************/
--
-- FILE: reduced_form_panel_tables_by_month.sql
--
-- BY: Timur Abbiasov <tabbyasov@gmail.com> and 
--     Dmitry Sedov 
--
-- CREATED: Sun Feb 16 2020
--
-- DESC: This file constructs monthly csv tables with establishments / devices 
--       in proximity to each restaurant (distance-disks, cbgs).
-- 
-- EXEC: psql -d dataname2 < reduced_form_panel_tables_by_month.sql
--
/*******************************************************************************
*******************************************************************************/


-- Create establishments table with location turned into geography type
CREATE TEMPORARY TABLE t_establishments_with_geo AS
    SELECT
        sname_place_id AS est_id,
        ST_SetSRID(ST_Point(longitude, latitude),4326)::geography AS location,
        cbg
    FROM
        establishments
;
-- Create indices 
CREATE UNIQUE INDEX establishments_pg_idx
ON t_establishments_with_geo (est_id);
CREATE  INDEX establishments_cbg_idx
ON t_establishments_with_geo (cbg);
CREATE INDEX establishments_geo_idx
ON t_establishments_with_geo
USING GIST (location);

-- Create restaurants table with location turned into geography type
CREATE TEMPORARY TABLE t_restaurants_with_geo AS
    SELECT
        sname_place_id AS rest_id,
        ST_SetSRID(ST_Point(longitude, latitude),4326)::geography AS location,
        cbg
    FROM
        restaurants
;
-- Create indices 
CREATE UNIQUE INDEX restaurants_pg_idx
ON t_restaurants_with_geo (rest_id);
CREATE INDEX restaurants_cbg_idx
ON t_restaurants_with_geo (cbg);
CREATE INDEX restaurants_geo_idx
ON t_restaurants_with_geo
USING GIST (location);
-- Precompute distance-buffers around restaurants
ALTER TABLE t_restaurants_with_geo
    ADD COLUMN buffer200 geography,
    ADD COLUMN buffer400 geography,
    ADD COLUMN buffer600 geography
;
UPDATE t_restaurants_with_geo
    SET buffer200 = ST_Buffer(location, 200),
    SET buffer400 = ST_Buffer(location, 400),
    SET buffer600 = ST_Buffer(location, 600)
;

-- Create cbgs table with shape as geography type
CREATE TEMPORARY TABLE t_cbgs_with_geo AS
    SELECT
        censusblockgroup AS cbg,
        number_devices_residing,
        wkb_geometry::geography(MultiPolygon,4326) AS geog
    FROM
        cbgs
;
-- Create indices
CREATE INDEX cbgs_cbg_idx
ON t_cbgs_with_geo (cbg);
CREATE INDEX cbgs_geo_idx
ON t_cbgs_with_geo
USING GIST (geog);
-- Precompute the area column on the temporary cbgs table
ALTER TABLE t_cbgs_with_geo
ADD COLUMN area NUMERIC;
UPDATE t_cbgs_with_geo
SET area = ST_Area(geog);

-- Raise notice about temp tables being ready
DO language plpgsql $$
BEGIN
    RAISE NOTICE '- geo tables ready';
END;
$$;

-- Join restaurants and nearby establishments by distance / same cbg indicator,
-- then assign the distance disk indicator to each restaurant-establishment pair
CREATE TABLE rest_join_est AS
    SELECT
        r.rest_id,
        e.est_id,
        ST_DWithin(r.location, e.location, 200)::int AS in_200m,
        ST_DWithin(r.location, e.location, 400)::int - ST_DWithin(r.location, e.location, 200)::int AS in_400m,
        ST_DWithin(r.location, e.location, 600)::int - ST_DWithin(r.location, e.location, 400)::int AS in_600m,
        (e.cbg = r.cbg)::int AS in_cbg
    FROM
        t_restaurants_with_geo AS r
    LEFT JOIN 
        t_establishments_with_geo AS e
    ON 
        ST_DWithin(r.location, e.location, 600) OR e.cbg = r.cbg
;

-- Raise notice about restaurants-establishment join being ready
DO language plpgsql $$
BEGIN
    RAISE NOTICE '- establishments join ready';
END;
$$;

-- NOTE: Split in two steps due to earlier RAM issues (Out-of-memory-errors)
-- Join restaurants and nearby CBGs by distance / same cbg indicator,
CREATE TABLE rest_join_cbg AS
    SELECT
        r.rest_id,
        c.cbg,
        c.number_devices_residing,
        (c.cbg = r.cbg)::int AS in_cbg
    FROM
        t_restaurants_with_geo AS r
    LEFT JOIN 
        t_cbgs_with_geo AS c
    ON 
        ST_DWithin(r.location, c.geog, 600) OR c.cbg = r.cbg;
-- Create indices on the joined table
CREATE INDEX rest_join_cbg_rest_idx
ON rest_join_cbg (rest_id);
CREATE INDEX rest_join_cbg_cbg_idx
ON rest_join_cbg (cbg);
-- Add the percentage of intersection area columns
ALTER TABLE rest_join_cbg
ADD COLUMN weight200 NUMERIC,
ADD COLUMN weight400 NUMERIC,
ADD COLUMN weight600 NUMERIC;
-- Update the percentage-intersection area columns
UPDATE rest_join_cbg AS t
    SET
        weight200 = ST_Area(ST_Intersection(r.buffer200, c.geog))/c.area,
        weight400 = ST_Area(ST_Intersection(r.buffer400, c.geog))/c.area,
        weight600 = ST_Area(ST_Intersection(r.buffer600, c.geog))/c.area
    FROM
        t_restaurants_with_geo AS r,
        t_cbgs_with_geo AS c
    WHERE
        t.rest_id = r.rest_id AND
        t.cbg = c.cbg;
-- Raise notice that the cbg join is ready
DO language plpgsql $$
BEGIN
  RAISE NOTICE '- cbgs join ready';
END;
$$;

CREATE EXTENSION IF NOT EXISTS dblink;
-- Function that creates file path for export
CREATE OR REPLACE FUNCTION file_path(m integer, y integer, post varchar(255))
RETURNS text AS $$
BEGIN
    RETURN CONCAT('/home/user/projects/urban/data/output/reduced-form/sql_monthly_panel_', LPAD(m::varchar(256),2,'0'), '_', y::varchar(255), '_', post, '.csv');
END;
$$ LANGUAGE plpgsql;

SET search_path to public, debug;

-- Function to export the panel month-by-month
CREATE OR REPLACE FUNCTION export_by_month(m integer, y integer, e_done boolean, c_done boolean) RETURNS integer AS
$$
BEGIN
    -- DROP temporary tables if they exist
    DROP TABLE IF EXISTS
        visits_select,
        home_select,
        rest_nearby_devices,
        rest_nearby_establishments
    ;
    RAISE NOTICE 'Processing % %', LPAD(m::varchar(255),2,'0'), y::varchar(255);
    -- Exports restaurants-establishments table 
    IF NOT e_done THEN
        RAISE NOTICE 'processing establishments...';
        -- Table with the visits for month that is being processed
        CREATE TEMPORARY TABLE visits_select AS
            SELECT
                sname_place_id,
                raw_visit_counts,
                1 AS open
            FROM
                visits
            WHERE
                year = y
            AND
                month = m
        ;
        -- Create index on the month-visits table
        CREATE INDEX visits_select_pg_idx
        ON visits_select(sname_place_id);
        RAISE NOTICE '- visits_select ready';
        -- Table with nearby monthly establishments
        CREATE TEMPORARY TABLE rest_nearby_establishments AS
            WITH no_own_rest_visits AS (
                WITH rest_visits_month AS (
                    SELECT
                        rje.rest_id,
                        v.open,
                        rje.in_200m,
                        rje.in_400m,
                        rje.in_600m,
                        rje.in_cbg
                    FROM
                    rest_join_est AS rje
                    LEFT JOIN
                    visits_select AS v
                    ON
                    rje.est_id = v.sname_place_id
                )
                SELECT
                    rest_id,
                    m AS month,
                    y AS year,
                    SUM(open * in_200m) AS est_in_200m,
                    SUM(open * in_400m) AS est_in_400m,
                    SUM(open * in_600m) AS est_in_600m,
                    SUM(open * in_cbg) AS est_in_cbg
                FROM
                    rest_visits_month
                GROUP BY
                    rest_id
            )
            SELECT
                rest_id,
                n.month,
                n.year,
                est_in_200m,
                est_in_400m,
                est_in_600m,
                est_in_cbg,
                v.raw_visit_counts
            FROM no_own_rest_visits AS n
            LEFT JOIN visits_select AS v
            ON n.rest_id = v.sname_place_id
        ;
        -- Raise notice and export
        RAISE NOTICE '- part 1 (establishments) ready';
        RAISE NOTICE 'exporting...';
        EXECUTE CONCAT('COPY rest_nearby_establishments TO ''',file_path(m,y,'e'),''' CSV HEADER;');
        -- Update the tracking table
        PERFORM dblink_connect('dblink_trans','dbname=dataname2 user=postgres');
        PERFORM dblink('dblink_trans', 'UPDATE debug.export SET est_status = TRUE WHERE year = ' || y || ' AND month = ' || m || ';');
        PERFORM dblink('dblink_trans', 'COMMIT;');
        PERFORM dblink_disconnect('dblink_trans');
    ELSE
        RAISE NOTICE '- part 1 (establishments) is already done, skipping...';
    END IF;

    IF NOT c_done THEN
        RAISE NOTICE 'processing cbgs...';
        -- Create the month-device counts table
        CREATE TEMPORARY TABLE home_select AS
            SELECT
                census_block_group AS cbg,
                number_devices_residing
            FROM
                home
            WHERE
                year = y
            AND
                month = m
        ;
        -- Create index on the temp table
        CREATE INDEX home_select_pg_idx
        ON home_select(cbg);
        RAISE NOTICE '- home_select ready';
        -- Create the month-visits table
        CREATE TEMPORARY TABLE IF NOT EXISTS visits_select AS
            SELECT
                sname_place_id,
                raw_visit_counts,
                1 AS open
            FROM
                visits
            WHERE
                year = y
            AND
                month = m
        ;
        -- Create index on the visits table
        CREATE INDEX IF NOT EXISTS visits_select_pg_idx
        ON visits_select(sname_place_id);
        RAISE NOTICE '- visits_select ready';
        -- Join restaurants to this-month nearby devices 
        CREATE TEMPORARY TABLE rest_nearby_devices AS
        WITH no_own_rest_visits AS (
            WITH rest_home_month AS (
                SELECT
                    rjc.rest_id,
                    h.number_devices_residing,
                    rjc.weight200,
                    rjc.weight400,
                    rjc.weight600,
                    rjc.in_cbg
                FROM
                    rest_join_cbg AS rjc
                LEFT JOIN
                    home_select AS h
                ON
                    rjc.cbg = h.cbg
            )
            SELECT
                rest_id,
                m AS month,
                y AS year,
                SUM(number_devices_residing * weight200) AS devices_in_200m,
                SUM(number_devices_residing * weight400) AS devices_in_400m,
                SUM(number_devices_residing * weight600) AS devices_in_600m,
                SUM(number_devices_residing * in_cbg) AS devices_in_cbg
            FROM
                rest_home_month
            GROUP BY
                rest_id
        )
        SELECT
            rest_id,
            n.month,
            n.year,
            devices_in_200m,
            devices_in_400m,
            devices_in_600m,
            devices_in_cbg,
            v.raw_visit_counts
        FROM 
            no_own_rest_visits AS n
        LEFT JOIN 
            visits_select AS v
        ON 
            n.rest_id = v.sname_place_id
        ;
        -- Export the resulting table to CSV
        RAISE NOTICE '- part 2 (devices) ready';
        RAISE NOTICE 'exporting...';
        EXECUTE CONCAT('COPY rest_nearby_devices TO ''',file_path(m,y,'d'),''' CSV HEADER;');
        -- Update the traking table
        PERFORM dblink_connect('dblink_trans','dbname=dataname2 user=postgres');
        PERFORM dblink('dblink_trans', 'UPDATE debug.export SET cbg_status = TRUE WHERE year = ' || y || ' AND month = ' || m || ';');
        PERFORM dblink('dblink_trans', 'COMMIT;');
        PERFORM dblink_disconnect('dblink_trans');
    ELSE
        RAISE NOTICE '- part 2 (devices) is already done, skipping...';
    END IF;

    -- Notify of month completion
    RAISE NOTICE 'done with % %',LPAD(m::varchar(255),2,'0'), y::varchar(255);
    RETURN 1;
END;
$$ LANGUAGE plpgsql VOLATILE COST 100;

-- Do the export for all months
SELECT 
    export_by_month(month,year,est_status,cbg_status) 
    FROM (
        SELECT 
            * 
        FROM 
            debug.export 
        ORDER BY 
            year, month ASC
        ) e;
