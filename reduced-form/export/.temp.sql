CREATE EXTENSION IF NOT EXISTS dblink;

CREATE SCHEMA IF NOT EXISTS debug;
CREATE TABLE IF NOT EXISTS debug.export AS
    SELECT
        extract(year from to_timestamp(month_ts)::date)::int AS year,
        extract(month from to_timestamp(month_ts)::date)::int AS month,
        FALSE AS est_status
    FROM
        unnest(ARRAY[1496275200, 1498867200, 1501545600, 1504224000, 1506816000, 1509494400, 1512086400, 1514764800, 1517443200, 1519862400, 1522540800, 1525132800, 1527811200, 1530403200, 1533081600, 1535760000, 1538352000, 1541030400, 1543622400, 1546300800, 1548979200, 1551398400, 1554076800, 1556668800, 1559347200, 1561939200]) AS months(month_ts)
;

SET search_path to public, debug;

-- Add the first 2 naics digits as a column in the restaurants - joined with
-- establishments
ALTER TABLE rest_join_est
ADD COLUMN naics2 integer;
UPDATE rest_join_est r
    SET 
        naics2 = e.naics_first2 
    FROM
        establishments AS e
    WHERE 
        r.est_id = e.sname_place_id;


-- Function that creates file path for export
CREATE OR REPLACE FUNCTION file_path(m integer, y integer, post varchar(255))
RETURNS text AS $$
BEGIN
    RETURN CONCAT('/home/user/projects/urban/data/output/reduced-form/sql_monthly_panel_', LPAD(m::varchar(256),2,'0'), '_', y::varchar(255), '_', post, '.csv');
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION export_est_categs_by_month(m integer, y integer, e_done boolean) RETURNS integer AS
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
        RAISE NOTICE 'processing establishment categories...';
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
        -- Table with nearby monthly establishments
        CREATE TEMPORARY TABLE rest_nearby_est_categs AS
            WITH rest_visits_month AS (
                SELECT
                    rje.rest_id,
                    v.open,
                    rje.in_200m,
                    rje.in_cbg,
                    rje.naics2
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
                SUM(open * in_cbg) AS est_in_cbg
            FROM
                rest_visits_month
            GROUP BY
                rest_id, naics2
        ;
        -- Raise notice and export
        RAISE NOTICE '- part 1 (establishment category) ready';
        RAISE NOTICE 'exporting...';
        EXECUTE CONCAT('COPY rest_nearby_est_categs TO ''',file_path(m,y,'c'),''' CSV HEADER;');
        -- Update the tracking table
        PERFORM dblink_connect('dblink_trans','dbname=dataname2 user={user} password={user_pass}');
        PERFORM dblink('dblink_trans', 'UPDATE debug.export SET est_status = TRUE WHERE year = ' || y || ' AND month = ' || m || ';');
        PERFORM dblink('dblink_trans', 'COMMIT;');
        PERFORM dblink_disconnect('dblink_trans');
    ELSE
        RAISE NOTICE '- part 1 (establishment categories) are already done, skipping...';
    END IF;

    
    -- Notify of month completion
    RAISE NOTICE 'done with % %',LPAD(m::varchar(255),2,'0'), y::varchar(255);
    RETURN 1;
END;
$$ LANGUAGE plpgsql VOLATILE COST 100;

-- Do the export for all months
SELECT 
    export_est_categs_by_month(month,year,est_status) 
    FROM (
        SELECT 
            * 
        FROM 
            debug.export 
        ORDER BY 
            year, month ASC
        ) e;
