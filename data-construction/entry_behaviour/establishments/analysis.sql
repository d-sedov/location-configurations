-- CREATE INDEX IF NOT EXISTS visits_place_idx ON visits(sname_place_id);
-- DROP INDEX IF EXISTS visits_place_idx;

CREATE OR REPLACE FUNCTION to_month_indicator(anyarray)
RETURNS anyarray AS $$
  SELECT array_remove(
    ARRAY(
      SELECT month = ANY($1)::int from unnest(
          ARRAY[1496275200, 1498867200, 1501545600, 1504224000, 1506816000, 1509494400, 1512086400, 1514764800, 1517443200, 1519862400, 1522540800, 1525132800, 1527811200, 1530403200, 1533081600, 1535760000, 1538352000, 1541030400, 1543622400, 1546300800, 1548979200, 1551398400, 1554076800, 1556668800, 1559347200, 1561939200]
        ) as months(month)
      ), NULL
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION to_array_by_month(var_array numeric[], month_idx_arr int[])
RETURNS numeric[] AS $$
  DECLARE
    r numeric[] := var_array[27:27];
    v numeric[] := var_array;
    x int;
  BEGIN
  FOREACH x IN ARRAY month_idx_arr
  LOOP
    IF x = 1 THEN
      r := array_append(r, v[1]);
      v := v[2:26];
    ELSE
      r := array_append(r, 0::numeric);
    END IF;
  END LOOP;
  RETURN r;
  END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION ym_to_unix(year integer, month integer)
RETURNS integer AS $$
BEGIN
   RETURN EXTRACT(EPOCH FROM to_date(CONCAT(year::varchar(255),LPAD(month::varchar(255),2,'0')),'YYYYMM'))::int;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION point_buffer_cbg_share(location geography(Point,4326), cbg_geog geography(MultiPolygon,4326), distance integer)
RETURNS numeric AS $$
BEGIN
  RETURN ST_Area( ST_Intersection(ST_Buffer(location,distance), cbg_geog) ) / ST_Area( cbg_geog );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION vec_add(arr1 numeric[], arr2 numeric[])
RETURNS numeric[] AS
$$
SELECT array_agg(result)
FROM (SELECT tuple.val1 + tuple.val2 AS result
      FROM (SELECT UNNEST($1) AS val1
                   ,UNNEST($2) AS val2
                   ,generate_subscripts($1, 1) AS ix) tuple
      ORDER BY ix) inn;
$$ LANGUAGE SQL IMMUTABLE STRICT;

DROP AGGREGATE vec_sum (numeric[]);
CREATE AGGREGATE vec_sum (numeric[])
(
  sfunc = vec_add,
  stype = numeric[]
);

-- DROP AGGREGATE vec_sum (int[]);
-- CREATE AGGREGATE vec_sum (int[])
-- (
--   sfunc = vec_add,
--   stype = int[]
-- );

-- CREATE TABLE IF NOT EXISTS t_home_by_month AS
--     WITH home_by_month AS (
--       SELECT
--         census_block_group as cbg,
--         to_month_indicator(
--           array_agg(
--             ym_to_unix(year, month) ORDER BY ym_to_unix(year, month) ASC
--           )
--         ) as months_idx,
--         to_array_by_month(
--           array_agg(
--             number_devices_residing ORDER BY ym_to_unix(year, month) ASC
--           ),
--           to_month_indicator(
--             array_agg(
--               ym_to_unix(year, month) ORDER BY ym_to_unix(year, month) ASC
--             )
--           )
--         ) as num_devices_by_month
--       FROM
--           home
--       GROUP BY cbg
--     )
--     SELECT
--         cbg,
--         c.wkb_geometry as geom,
--         num_devices_by_month,
--         months_idx
--     FROM home_by_month h
--     LEFT JOIN cbgs c
--     ON c.censusblockgroup = h.cbg
--     WHERE cardinality(months_idx)>0
-- ;
--
-- CREATE TABLE IF NOT EXISTS t_visits_by_month AS
--   SELECT sname_place_id, months_idx from (SELECT
--     sname_place_id,
--     to_month_indicator(
--       array_agg(  date_range_start ORDER BY date_range_start ASC  )
--     ) as months_idx
--   FROM
--       visits
--   GROUP BY sname_place_id
--   ) s
--   WHERE cardinality(months_idx)>0
-- ;
--
-- CREATE INDEX IF NOT EXISTS home_by_month_idx
-- ON t_home_by_month
-- USING GIST (geom);


CREATE TABLE IF NOT EXISTS t_visits_by_month AS
  SELECT sname_place_id, months_idx from (SELECT
    sname_place_id,
    to_month_indicator(
      array_agg(  date_range_start ORDER BY date_range_start ASC  )
    ) as months_idx
  FROM
      visits
  GROUP BY sname_place_id
  ) s
  WHERE cardinality(months_idx)>0
;

CREATE INDEX IF NOT EXISTS visits_by_month_pg_idx
ON t_visits_by_month(sname_place_id)
;

-- DROP FUNCTION IF EXISTS export_by_group(sg varchar(255));
-- CREATE FUNCTION export_by_group(sg varchar(255)) RETURNS integer AS
-- $$
-- DECLARE
--   items RECORD;
--   file_path text := CONCAT('/home/tuser/project/code/data_export/csv/sql_panel_', sg, '_', (cast(extract(epoch from current_timestamp) as integer))::varchar(255),'.csv');
-- BEGIN
--
--
--   CREATE TEMPORARY TABLE t_rest_with_devices AS
--       SELECT
--         rest_id,
--         r.location,
--         r.cbg,
--         vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * point_buffer_cbg_share(r.location, h.geog, 200))) as devices_in_200m,
--         vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * point_buffer_cbg_share(r.location, h.geog, 400))) as devices_in_400m,
--         vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * point_buffer_cbg_share(r.location, h.geog, 600))) as devices_in_600m,
--         vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * (h.cbg = r.cbg)::int)) devices_in_cbg
--       FROM t_rest_visits_with_geo as r
--       LEFT JOIN t_home_by_month_select as h
--       ON ST_DWithin(r.location, h.geog, 600) or h.cbg = r.cbg
--       GROUP BY rest_id, r.location, r.cbg
--   ;
--
--   CREATE INDEX rest_with_devices_geo_idx
--   ON t_rest_with_devices
--   USING GIST (location);
--   RAISE NOTICE '- part 1 (devices) ready';
--
--   FOR items IN SELECT COUNT(*) as count FROM t_rest_with_devices
--   LOOP
--     RAISE NOTICE 't_rest_with_devices: %', items.count::varchar(255);
--   END LOOP;
--
--   CREATE TEMPORARY TABLE t_panel_to_export AS
--     SELECT
--       rest_id,
--       devices_in_200m, devices_in_400m, devices_in_600m, devices_in_cbg,
--       vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 200)::int))) est_in_200m,
--       vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 400)::int))) est_in_400m,
--       vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 600)::int))) est_in_600m,
--       vec_sum(ARRAY(SELECT unnest(e.months_idx) * (e.cbg = r.cbg)::int)) est_in_cbg
--     FROM t_rest_with_devices as r
--     LEFT JOIN t_est_visits_with_geo as e
--     ON ST_DWithin(r.location, e.location, 600) or e.cbg = r.cbg
--     GROUP BY rest_id, devices_in_200m, devices_in_400m, devices_in_600m, devices_in_cbg
--   ;
--   RAISE NOTICE '- part 2 (establishments) ready';
--   RAISE NOTICE 'exporting...';
--   -- UPDATE debug.export SET done = 1 WHERE select_group = sg;
--   EXECUTE CONCAT('COPY t_panel_to_export TO ''',file_path,''' CSV HEADER;');
--   RAISE NOTICE 'Done with #%', sg;
--   RETURN 1;
-- END;
-- $$ LANGUAGE plpgsql;

-- SELECT export_by_group('34');

-- SELECT export_by_state(state) from (
--   SELECT DISTINCT(substring(cbg from 1 for 2)) as state
--   FROM restaurants
--   WHERE cbg IS NOT NULL
-- ) s;

-- SELECT
--     rest_id,
--     vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * point_buffer_cbg_share(r.location, h.geom, 200))) as devices_in_200m,
--     vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * point_buffer_cbg_share(r.location, h.geom, 400))) as devices_in_400m,
--     vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * point_buffer_cbg_share(r.location, h.geom, 600))) as devices_in_600m,
--     vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * point_buffer_cbg_share(r.location, h.geom, 800))) as devices_in_800m,
--     vec_sum(ARRAY(SELECT unnest(num_devices_by_month) * point_buffer_cbg_share(r.location, h.geom, 1000))) as devices_in_1000m,
--     vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 200)::int))) est_in_200m,
--     vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 400)::int))) est_in_400m,
--     vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 600)::int))) est_in_600m,
--     vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 800)::int))) est_in_800m,
--     vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 1000)::int))) est_in_1000m,
--     vec_sum(ARRAY(SELECT unnest(e.months_idx) * (CASE WHEN e.cbg = r.cbg THEN 1 ELSE 0 END))) est_in_cbg
-- FROM (SELECT * FROM t_rest_visits_with_geo WHERE cardinality(months_idx)>0) as r
-- LEFT JOIN (SELECT cbg, num_devices_by_month, geom FROM t_home_by_month WHERE cardinality(months_idx)>0) as h
-- ON ST_DWithin(r.location, h.geom, 1000)
-- LEFT JOIN (SELECT * FROM t_est_visits_with_geo WHERE cardinality(months_idx)>0) as e
-- ON ST_DWithin(r.location, e.location, 1000) or e.cbg = r.cbg
-- GROUP BY rest_id, r.location
-- ;

-- SELECT
--   rest_id,
--   vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 200)::int))) est_in_200m,
--   vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 400)::int))) est_in_400m,
--   vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 600)::int))) est_in_600m,
--   vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 800)::int))) est_in_800m,
--   vec_sum(ARRAY(SELECT unnest(e.months_idx) * (ST_DWithin(r.location, e.location, 1000)::int))) est_in_1000m,
--   vec_sum(ARRAY(SELECT unnest(e.months_idx) * (CASE WHEN e.cbg = r.cbg THEN 1 ELSE 0 END))) est_in_cbg
-- FROM (SELECT * FROM t_rest_visits_with_geo WHERE cardinality(months_idx)>0) as r
-- LEFT JOIN (SELECT * FROM t_est_visits_with_geo WHERE cardinality(months_idx)>0) as e
-- ON ST_DWithin(r.location, e.location, 1000) or e.cbg = r.cbg
-- GROUP BY rest_id
