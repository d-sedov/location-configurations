/*******************************************************************************
*******************************************************************************/
--
-- FILE: 3-poi-msa-assignation.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Tue Sep 17 2019 
--
-- DESC: This file contains the code that adds the 'msa_bool' and 'cbsa' columns
--       to the geometry_pois table by a simple join with geometry_cbg table.
-- 
-- REF:
--
-- COMMENT: 
--          For 'security' purposes TABLE geometry_pois is substituted with
--          TABLE test_msa_assignation.
/*******************************************************************************
*******************************************************************************/


/****************************** MSA ASSIGNATION *******************************/ 

-- Add the new columns to geometry_pois
ALTER TABLE test_msa_assignation 
ADD COLUMN msa_bool BOOLEAN,
ADD COLUMN cbsa VARCHAR(5);

UPDATE test_msa_assignation AS tma
SET msa_bool = gc.msa_bool,
    cbsa = gc.cbsa
FROM geometry_cbg AS gc
WHERE
tma.cbg = gc.censusblockgroup;

/******************************************************************************/



/************************** TEST MSA ASSIGNATION ******************************/

-- Create test table
CREATE TEMPORARY TABLE test_msa_assignation (
    sname_place_id text PRIMARY KEY, 
    cbg text
);

INSERT INTO test_msa_assignation (sname_place_id, cbg) 
    VALUES
    ('id1', '060375334021'),
    ('id2', '040190001001'),
    ('id3', '111111111111');


-- Desired output after MSA assignation
SELECT * FROM test_msa_assignation;

/*
 sname_place_id |     cbg      | msa_bool | cbsa
--------------------+--------------+----------+-------
 id3                | 111111111111 |          |
 id1                | 060375334021 | t        | 31080
 id2                | 040190001001 | t        | 46060
*/
/******************************************************************************/

