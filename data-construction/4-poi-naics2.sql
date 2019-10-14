/*******************************************************************************
*******************************************************************************/
--
-- FILE: 4-poi-naics2.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Tue Sep 26 2019 
--
-- DESC: This file contains the code that adds the first two digits of the
--       NAICSs code as a separate column to the pois table.
-- 
-- REF:
--
-- COMMENT: 
/*******************************************************************************
*******************************************************************************/

-- ADD the two first digits of NAICS code as a column to the pois table
ALTER TABLE
    pois
ADD COLUMN
    naics_first2 INTEGER;

UPDATE
    pois
SET
    naics_first2 = (LEFT(naics_code::text, 2))::INTEGER;
