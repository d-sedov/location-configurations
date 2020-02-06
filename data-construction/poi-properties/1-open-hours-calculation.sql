/*******************************************************************************
*******************************************************************************/
--
-- FILE: 1-open-hours-calculation.py
--
-- BY: Dmitry Sedov 
--
-- CREATED: Mon Sep 16 2019 16:41:47 GMT+0200
--
-- DESC: This file contains the code (OPTION 2) that can be relatively safely
--       used to compute total open minutes in Oct 2018 for each POI in the
--       dataset. Safely here means that some known problems are taken into
--       account (see the testing section). However, a careful read of the code
--       and IMPORTANTLY change of table names and other inputs ARE NEEDED.
-- 
-- REF:
--  https://www.periscopedata.com/blog/the-lazy-analysts-guide-to-postgres-json
--  https://www.sqlstyle.guide/ 
--
-- COMMENT: For sure this is not the best way to do the task. In the end there
--          is a join even though the job can be done row-by-row. Suggest
--          improvements? 
--          For 'security' purposes TABLE pois is substituted with
--          TABLE test_open_hours.
--          EXECUTED LINES 194-200 to update TABLE pois.
/*******************************************************************************
*******************************************************************************/

/**************************** OPTION 2: USE THIS ******************************/
/******************************************************************************/

-- This function checks if a text array can be converted to time array.
CREATE OR REPLACE FUNCTION is_time(s text[]) 
    RETURNS boolean AS 
    $$
    BEGIN
        PERFORM s::time[];
        RETURN TRUE;
    EXCEPTION WHEN OTHERS THEN
        RETURN FALSE;
    END;
    $$
LANGUAGE plpgsql;

-- This function counts the number of days of week in the input time range.
CREATE OR REPLACE FUNCTION dows(date, date) 
    RETURNS TABLE(dow text, num integer) AS
    $$
    BEGIN
        RETURN QUERY
            WITH month AS (
                SELECT generate_series($1, $2, '1 day') AS days),
            month_with_dows AS (
                SELECT days, to_char(days, 'Dy') dosi FROM month)
            SELECT dosi, COUNT(days)::integer FROM month_with_dows GROUP BY dosi ORDER BY dosi;
    END
    $$
LANGUAGE 'plpgsql';

-- Create temportary table to check the time ranges validity and compute 
-- opening times.
-- jsonb_each.* creates the key (day of week), value columns (time ranges)
-- jsonb_array_elements creates a separate row for each time range within a day
-- jsonb_array_elements_text transforms json into array-ready form
-- LEFT JOIN keeps the empty arrays for days like Sun:[] (closed?).
-- test_open_hours used as example for some security.
CREATE TEMPORARY TABLE open_times_expanded AS
    WITH times AS (
        SELECT sname_place_id, (jsonb_each(open_hours)).* 
        -- NEXT LINE IS IMPORTANT (input table)
        FROM test_open_hours
    ), times_expanded AS (
        SELECT sname_place_id, 
               key, 
               jsonb_array_elements(value) prelim_range
        FROM times)
    SELECT times.sname_place_id,
           times.key dow,
           ARRAY(SELECT jsonb_array_elements_text(times_expanded.prelim_range))
                prelim_range,
           NULL::time[] as time_range,
           NULL::integer as minutes,
           NULL::boolean as proper_time 
    FROM times
    LEFT JOIN times_expanded
    ON 
        times.sname_place_id=times_expanded.sname_place_id AND
        times.key=times_expanded.key;

-- Convert ARRAY to time range where possible
UPDATE open_times_expanded 
SET time_range = prelim_range::time[] 
WHERE is_time(prelim_range)=TRUE;

-- Compute the minutes open on each day of the week.
UPDATE open_times_expanded 
SET minutes = extract(hours FROM time_range[2] - 
                time_range[1])::integer * 60 + 
              extract(minutes FROM time_range[2] - 
                time_range[1])::integer;

-- 0 open time for NULL-time-range rows.
UPDATE open_times_expanded 
SET minutes = 0 
WHERE array_length(time_range, 1) IS NULL;

-- Check whether open times are valid (valid times, non-negative difference).
UPDATE open_times_expanded 
SET proper_time = is_time(prelim_range) AND (minutes >= 0);

-- Compute total open time in October 2018.
CREATE TEMPORARY TABLE open_times AS
    WITH tab1 AS (
        SELECT open_times_expanded.*, tab2.num 
        FROM open_times_expanded 
        LEFT JOIN (
            SELECT * 
            FROM dows('2018-10-01', '2018-10-31')
        ) tab2 
        ON open_times_expanded.dow=tab2.dow
    )
    SELECT sname_place_id,
           SUM(minutes * num) total_minutes_open, 
           bool_and(proper_time) time_ok
    FROM tab1 
    GROUP BY sname_place_id;

/*****************************TEST BEHAVIOR***********************************/

-- Test cases table

CREATE TEMPORARY TABLE test_open_hours (
    sname_place_id text PRIMARY KEY, 
    open_hours jsonb
);

INSERT INTO test_open_hours (sname_place_id, open_hours) VALUES
    -- ok
    ('id1', '{"Fri": [["9:00", "19:30"]], "Mon": [["9:00", "20:00"]]}'),
    -- ok
    ('id2', '{"Sat": [], "Sun": []}'),
    -- ok, multiple times per day
    ('id3', '{"Mon": [["0:00", "3:00"], ["7:00", "24:00"]], "Tue": []}'),
    -- ok, not present in the resulting open_hours
    ('id4', NULL),
    -- bad time range (reverse)
    ('id5', '{"Fri": [["9:00", "8:00"]]}'),
    -- bad time range (reverse), multiple entries
    ('id6', '{"Fri": [["9:00", "8:00"]], "Tue": [["09:00", "10:00"]]}'),
    -- bad time range (out of bounds)
    ('id7', '{"Fri": [["1:00", "38:00"]], "Sun": [["02:00", "21:00"]]}');

-- Desired output after previous section executed.

SELECT * FROM open_hours;
/*
 sname_place_id | total_minutes_open | time_ok
--------------------+--------------------+---------
 id1                |               5820 | t
 id7                |               4560 | f
 id5                |               -240 | f
 id3                |               6000 | t
 id6                |                 60 | f
 id2                |                  0 | t
(6 rows)
*/

-- DESIRED output on join with test_open_hours

SELECT test_open_hours.*,
       open_times.total_minutes_open,
       open_times.time_ok 
FROM test_open_hours 
LEFT JOIN open_times 
ON test_open_hours.sname_place_id=open_times.sname_place_id;

/*
 sname_place_id |                        open_hours                         | total_minutes_open | time_ok
--------------------+-----------------------------------------------------------+--------------------+---------
 id1                | {"Fri": [["9:00", "19:30"]], "Mon": [["9:00", "20:00"]]}  |               5820 | t
 id7                | {"Fri": [["1:00", "38:00"]], "Sun": [["02:00", "21:00"]]} |               4560 | f
 id5                | {"Fri": [["9:00", "8:00"]]}                               |               -240 | f
 id3                | {"Mon": [["0:00", "3:00"], ["7:00", "24:00"]], "Tue": []} |               6000 | t
 id6                | {"Fri": [["9:00", "8:00"]], "Tue": [["09:00", "10:00"]]}  |                 60 | f
 id2                | {"Sat": [], "Sun": []}                                    |                  0 | t
 id4                |                                                           |                    |
(7 rows)
*/

-- Desired output when updating the initial table
ALTER TABLE test_open_hours
ADD COLUMN total_minutes_open INTEGER,
ADD COLUMN time_ok BOOLEAN;

UPDATE test_open_hours AS toh
SET 
    total_minutes_open = ot.total_minutes_open,
    time_ok = ot.time_ok
FROM open_times AS ot
WHERE toh.sname_place_id=ot.sname_place_id;

SELECT * FROM test_open_hours;
/*
sname_place_id |                        open_hours                         | total_minutes_open | time_ok 
--------------------+-----------------------------------------------------------+--------------------+---------
 id4                |                                                           |                    | 
 id1                | {"Fri": [["9:00", "19:30"]], "Mon": [["9:00", "20:00"]]}  |               5820 | t
 id7                | {"Fri": [["1:00", "38:00"]], "Sun": [["02:00", "21:00"]]} |               4560 | f
 id5                | {"Fri": [["9:00", "8:00"]]}                               |               -240 | f
 id3                | {"Mon": [["0:00", "3:00"], ["7:00", "24:00"]], "Tue": []} |               6000 | t
 id6                | {"Fri": [["9:00", "8:00"]], "Tue": [["09:00", "10:00"]]}  |                 60 | f
 id2                | {"Sat": [], "Sun": []}                                    |                  0 | t
/*

/******************************************************************************/
/******************************************************************************/


/**************************OPTION 1: DID NOT WORK******************************/
/******************************************************************************/

-- This function computes the number of minutes POI is open on the input DOW.
-- LOGIC:
--  1) Parse open_hours JSON for the input day turning JSON into array.
--  2) For each openinig interval compute the difference, sum over all
--  intervals. 
-- NOT INTENDED TO BE SECURE OR ANYTHING.
CREATE OR REPLACE FUNCTION for_day(day text)
    RETURNS TABLE(sname_id text, minutes integer) AS 
    $$
    BEGIN
        RETURN QUERY 
            WITH time_arrays AS (
                SELECT tab1.sname_place_id,
                       ARRAY(SELECT jsonb_array_elements_text(tab1.hours))::TIME[] times 
                FROM (
                    SELECT sname_place_id, 
                           jsonb_array_elements(open_hours->day) hours 
                    FROM test_open_hours) tab1
            )
            SELECT sname_place_id, SUM(ms)::integer
                FROM (
                    SELECT sname_place_id, 
                           extract(hours FROM quant)::integer * 60 + extract(minutes FROM quant)::integer ms 
                    FROM (
                        SELECT sname_place_id,
                               times[2] - times[1] quant
                        FROM time_arrays
                    ) tab2
                ) tab3 
                GROUP BY tab3.sname_place_id;
    END;
    $$ 
    LANGUAGE plpgsql;

-- This function adds the dow-opening-minutes column to the pois table.
-- Logic:
--  Use the function above, insert zero for the POIs with the empty JSON array
--  for the input DOW.
CREATE OR REPLACE FUNCTION add_day_minutes(day text)
    RETURNS void AS 
    $$
    DECLARE 
        lower_day text; 
    BEGIN 
        lower_day := LOWER(day);
        EXECUTE 'ALTER TABLE test_open_hours ADD COLUMN ' || quote_ident(lower_day) || ' integer';
        EXECUTE 'UPDATE test_open_hours SET ' || quote_ident(lower_day) || ' = r.minutes FROM for_day('||quote_literal(day)||') AS r WHERE test_open_hours.sname_place_id=r.sname_id';
        EXECUTE 'UPDATE test_open_hours SET ' || quote_ident(lower_day) || ' = coalesce('||quote_ident(lower_day)||', 0) + (open_hours ? '||quote_literal(lower_day)||')::int * 0';
    END;
    $$
    LANGUAGE plpgsql;

-- Add new columns and multiply by the number of days by hand...
SELECT add_day_minutes('Mon')
UPDATE test_open_hours SET mon = mon * 5;
/******************************************************************************/
/******************************************************************************/
