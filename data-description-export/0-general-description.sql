/*******************************************************************************
*******************************************************************************/
--
-- FILE: 0-general description
--
-- BY: Dmitry Sedov 
--
-- CREATED: Fri Oct 4 2019
--
-- DESC: This file contains the script that produces the very basic summary
--       statistics about the dataset.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/

-- Count the number of POIs
SELECT 
    COUNT(*) 
FROM 
    pois;

-- Count the unique NAICS codes in the data
SELECT 
    COUNT (DISTINCT naics_code) 
FROM 
    pois;

