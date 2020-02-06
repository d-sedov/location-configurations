/*******************************************************************************
*******************************************************************************/
--
-- FILE: 11-events-parse-date.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Sun Nov 17 2019
--
-- DESC: This file parses the events start_time to extract month and year.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/

-- Convert start_time to timestamp (new column created)
ALTER TABLE 
    eventful_by_point
ADD column start_time_ts TIMESTAMP;

UPDATE 
    eventful_by_point 
SET 
    start_time_ts = to_date(start_time, 'YYYY-MM-DD HH24:MI:SS');
-- Alternative:
-- ALTER TABLE <tablename> ALTER COLUMN <columnname> TYPE DATE
-- using to_date(<columnname>, 'YYYY-MM-DD');


ALTER TABLE 
    eventful_by_point
ADD COLUMN year INTEGER,
ADD COLUMN month INTEGER;

UPDATE 
    eventful_by_point
SET
    year = date_part('year', start_time_ts),
    month = date_part('month', start_time_ts);
