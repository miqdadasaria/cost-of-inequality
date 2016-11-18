CREATE TABLE nhs_reference_costs_2011_12(
hrg4 varchar(10) primary key,
hrg4_description varchar(1000),
unit_cost_total numeric,
unit_cost_ei numeric,
unit_cost_ei_xs numeric,
unit_cost_nei_l numeric,
unit_cost_nei_l_xs numeric,
unit_cost_nei_s numeric,
unit_cost_dc numeric
);

UPDATE nhs_reference_costs_2011_12 SET unit_cost_ei_xs = 0 WHERE unit_cost_ei_xs IS NULL;
UPDATE nhs_reference_costs_2011_12 SET unit_cost_nei_l_xs = 0 WHERE unit_cost_nei_l_xs IS NULL;

DELETE FROM nhs_reference_costs_2011_12 WHERE hrg4 IN ('LA08E','UZ01Z','WD22Z','WD11Z','WF01A','WF02A');

INSERT INTO nhs_reference_costs_2011_12 
(hrg4, hrg4_description, unit_cost_total, unit_cost_ei, unit_cost_ei_xs, unit_cost_nei_l, unit_cost_nei_l_xs, unit_cost_nei_s, unit_cost_dc) 
VALUES
('LA08E','Imputed 2009-10', 0, 531.08, 0, 510.39, 0, 510.39, 510.39);

INSERT INTO nhs_reference_costs_2011_12 
(hrg4, hrg4_description, unit_cost_total, unit_cost_ei, unit_cost_ei_xs, unit_cost_nei_l, unit_cost_nei_l_xs, unit_cost_nei_s, unit_cost_dc) 
VALUES
('UZ01Z','Imputed 2009-10', 1329.59, 1161.08, 0, 1329.59, 0, 1329.59, 1161.08);

INSERT INTO nhs_reference_costs_2011_12 
(hrg4, hrg4_description, unit_cost_total, unit_cost_ei, unit_cost_ei_xs, unit_cost_nei_l, unit_cost_nei_l_xs, unit_cost_nei_s, unit_cost_dc) 
VALUES
('WD22Z','Imputed 2009-10', 0, 1161.08, 0, 1329.59, 0, 1329.59, 1161.08);

INSERT INTO nhs_reference_costs_2011_12 
(hrg4, hrg4_description, unit_cost_total, unit_cost_ei, unit_cost_ei_xs, unit_cost_nei_l, unit_cost_nei_l_xs, unit_cost_nei_s, unit_cost_dc) 
VALUES
('WD11Z','Imputed 2009-10', 0, 1161.08, 0, 1329.59, 0, 1329.59, 1161.08);

INSERT INTO nhs_reference_costs_2011_12 
(hrg4, hrg4_description, unit_cost_total, unit_cost_ei, unit_cost_ei_xs, unit_cost_nei_l, unit_cost_nei_l_xs, unit_cost_nei_s, unit_cost_dc) 
VALUES
('WF01A','Imputed 2009-10', 0, 1161.08, 0, 1329.59, 0, 1329.59, 1161.08);

INSERT INTO nhs_reference_costs_2011_12 
(hrg4, hrg4_description, unit_cost_total, unit_cost_ei, unit_cost_ei_xs, unit_cost_nei_l, unit_cost_nei_l_xs, unit_cost_nei_s, unit_cost_dc) 
VALUES
('WF02A','Imputed', 0, 1161.08, 0, 1329.59, 0, 1329.59, 1161.08);


CREATE TABLE hes_episode_cost_2011 AS
SELECT e.extract_hesid, e.episode_number, ROUND((admidate-dob)/365) AGE,
QUINTILE AS IMD_QUINTILE,
CASE SEX WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END AS SEX,
CASE WHEN s.admimeth > 20 AND s.admimeth < 30 THEN 1 ELSE 0 END AS EMERGENCY,
SUBSTR(d.DIAG_CODE,1,1) AS PRIMARY_ICD,
CASE
WHEN s.classpat=2 AND c.unit_cost_dc IS NOT NULL THEN c.unit_cost_dc
WHEN s.classpat=2 AND c.unit_cost_dc IS NULL THEN c.unit_cost_total
WHEN (s.admimeth < 20 OR s.admimeth > 30) AND c.unit_cost_ei IS NOT NULL THEN c.unit_cost_ei + g.fceexcessbeddays*c.unit_cost_ei_xs
WHEN (s.admimeth < 20 OR s.admimeth > 30) AND c.unit_cost_ei IS NULL THEN c.unit_cost_total
WHEN s.admimeth > 20 AND s.admimeth < 30 AND e.epidur < 2 AND c.unit_cost_nei_s IS NOT NULL THEN c.unit_cost_nei_s
WHEN s.admimeth > 20 AND s.admimeth < 30 AND e.epidur < 2 AND c.unit_cost_nei_s IS NULL THEN c.unit_cost_total
WHEN s.admimeth > 20 AND s.admimeth < 30 AND c.unit_cost_nei_l IS NOT NULL THEN c.unit_cost_nei_l + g.fceexcessbeddays*c.unit_cost_nei_l_xs
WHEN s.admimeth > 20 AND s.admimeth < 30 AND c.unit_cost_nei_l IS NULL THEN c.unit_cost_total
ELSE null
END fce_cost,
(u.U1+u.U2+u.U3+u.U4+u.U5+u.U6+u.U7+u.U8) unbundled_cost 
FROM 
HES_INPATIENT_EPISODE e
INNER JOIN
(SELECT EPISODE_NUMBER, DIAG_CODE FROM HES_INPATIENT_EPISODE_DIAG WHERE IS_PRIMARY = 1) d
ON e.episode_number = d.episode_number
INNER JOIN
HES_INPATIENT_SPELL s 
ON e.SPELL_NUMBER = s.SPELL_NUMBER
INNER JOIN HES_PATIENT p 
ON s.EXTRACT_HESID = p.EXTRACT_HESID
INNER JOIN
IMD_2010 imd
ON imd.LSOA01CD = s.SOAL
INNER JOIN
HES_INPATIENT_GROUPER g
ON g.EPIKEY = e.EPIKEY AND g.YEAR=e.HES_YEAR
INNER JOIN nhs_reference_costs_2011_12 c ON g.fce_hrg = c.hrg4
INNER JOIN (
SELECT g.epikey, g.year,
CASE WHEN U1.unit_cost_total IS NULL THEN 0 ELSE U1.unit_cost_total END U1, 
CASE WHEN U2.unit_cost_total IS NULL THEN 0 ELSE U2.unit_cost_total END U2, 
CASE WHEN U3.unit_cost_total IS NULL THEN 0 ELSE U3.unit_cost_total END U3, 
CASE WHEN U4.unit_cost_total IS NULL THEN 0 ELSE U4.unit_cost_total END U4, 
CASE WHEN U5.unit_cost_total IS NULL THEN 0 ELSE U5.unit_cost_total END U5, 
CASE WHEN U6.unit_cost_total IS NULL THEN 0 ELSE U6.unit_cost_total END U6, 
CASE WHEN U7.unit_cost_total IS NULL THEN 0 ELSE U7.unit_cost_total END U7, 
CASE WHEN U8.unit_cost_total IS NULL THEN 0 ELSE U8.unit_cost_total END U8
FROM HES_INPATIENT_GROUPER g 
LEFT OUTER JOIN nhs_reference_costs_2011_12 U1 ON U1.hrg4 = g.unbundledhrgs
LEFT OUTER JOIN nhs_reference_costs_2011_12 U2 ON U2.hrg4 = g.unbundledhrgs_2
LEFT OUTER JOIN nhs_reference_costs_2011_12 U3 ON U3.hrg4 = g.unbundledhrgs_3
LEFT OUTER JOIN nhs_reference_costs_2011_12 U4 ON U4.hrg4 = g.unbundledhrgs_4
LEFT OUTER JOIN nhs_reference_costs_2011_12 U5 ON U5.hrg4 = g.unbundledhrgs_5
LEFT OUTER JOIN nhs_reference_costs_2011_12 U6 ON U6.hrg4 = g.unbundledhrgs_6
LEFT OUTER JOIN nhs_reference_costs_2011_12 U7 ON U7.hrg4 = g.unbundledhrgs_7
LEFT OUTER JOIN nhs_reference_costs_2011_12 U8 ON U8.hrg4 = g.unbundledhrgs_8
) u ON u.epikey = g.epikey AND g.year=u.year
WHERE e.HES_YEAR=2011;


CREATE TABLE hes_appointments_2011 AS
SELECT e.extract_hesid, e.appointment_number, ROUND((apptdate-dob)/365) AGE,
QUINTILE AS IMD_QUINTILE,
CASE SEX WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END AS SEX,
MAINSPEF
FROM 
HES_OUTPATIENT_APPOINTMENT e
INNER JOIN HES_PATIENT p 
ON e.EXTRACT_HESID = p.EXTRACT_HESID
INNER JOIN
IMD_2010 imd
ON imd.LSOA01CD = e.SOAL
WHERE e.HES_YEAR=2011;

CREATE TABLE hes_patient_costs_2011 AS
SELECT extract_hesid, 
min(AGE) AS AGE, 
min(SEX) AS SEX, 
min(IMD_QUINTILE) AS IMD_QUINTILE,
SUM(NVL(FCE_COST,0)) + SUM(NVL(UNBUNDLED_COST,0)) AS TOTAL_COST
FROM HES_EPISODE_COST_2011
WHERE 
AGE IS NOT NULL AND
SEX IS NOT NULL AND
IMD_QUINTILE IS NOT NULL
GROUP BY EXTRACT_HESID;