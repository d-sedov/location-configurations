/*******************************************************************************
*******************************************************************************/
--
-- FILE: 
--
-- BY: Dmitry Sedov 
--
-- CREATED: Tue Feb 25 2020
--
-- DESC: This file contains simple commands to export tables from PostgreSQL
-- database.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/

-- Export selected demographics
\COPY select_demog_blck_grp TO '/home/user/projects/urban/data/output/reduced-form/census_cbg_extract.csv' CSV HEADER

-- Export home panel
\COPY home TO '/home/user/projects/urban/data/output/reduced-form/home.csv' CSV HEADER

-- Export entry info
CREATE TEMPORARY TABLE entry_info_establishments AS (
SELECT * FROM entry_info AS e WHERE EXISTS(SELECT 1 FROM establishments WHERE sname_place_id = e.sname_place_id) );

\COPY entry_info_establishments TO '/home/user/projects/urban/data/output/reduced-form/entry_info_establishments.csv' CSV HEADER

