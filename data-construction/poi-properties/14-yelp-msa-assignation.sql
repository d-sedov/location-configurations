/*******************************************************************************
*******************************************************************************/
--
-- FILE: 14-yelp-msa-assignation.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Sun Nov 17 2019 
--
-- DESC: This file contains the code that adds cbsa column to the
-- yelp_restauratns dataset.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/


/****************************** MSA ASSIGNATION *******************************/ 

-- Add the new columns to geometry_pois
ALTER TABLE yelp_restaurants 
ADD COLUMN cbsa VARCHAR(5);

UPDATE yelp_restaurants AS yr
SET cbsa = gc.cbsa
FROM geometry_cbg AS gc
WHERE
yr.cbg = gc.censusblockgroup;

/******************************************************************************/

