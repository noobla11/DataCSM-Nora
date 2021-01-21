--create a table with only the measurements from this term 
DROP VIEW IF EXISTS data_2021 CASCADE;
CREATE VIEW data_2021 AS
(SELECT 
	meta_id,
	tstamp,
	value,
	device_id
FROM data AS d
JOIN metadata AS m ON m.id=d.meta_id WHERE term_id='11')

--calculate the mean (t_avg)
DROP VIEW IF EXISTS mean_all CASCADE;
CREATE VIEW mean_all AS
(SELECT 
	avg(value) AS t_avg,
	device_id,
	meta_id
FROM data_2021
GROUP BY meta_id, device_id
ORDER BY device_id ASC)

-- create a table with only the day measurements 
DROP VIEW IF EXISTS day_2021 CASCADE;
CREATE VIEW day_2021 AS
	(SELECT * FROM data_2021 WHERE date_part('hour', tstamp)>=6 AND date_part('hour', tstamp)<=17)
--calculate mean temperature day (t_d)
DROP VIEW IF EXISTS mean_day CASCADE;
CREATE VIEW mean_day AS
(SELECT 
	avg(value) AS t_d,
	device_id,
	meta_id
FROM day_2021
GROUP BY meta_id, device_id
ORDER BY device_id ASC)

-- create a table with only the night measurements
DROP VIEW IF EXISTS night_2021 CASCADE;
CREATE VIEW night_2021 AS
	(SELECT * FROM data_2021 WHERE date_part('hour', tstamp) <=5 OR date_part('hour', tstamp) >=18)
--calculate mean temperature night (t_n)
DROP VIEW IF EXISTS mean_night CASCADE;
CREATE VIEW mean_night AS
(SELECT 
	avg(value) AS t_n,
	device_id,
	meta_id
FROM night_2021
GROUP BY meta_id, device_id
ORDER BY device_id ASC)

--create view with the closest stations of 2020 and 2019 for each HOBO21
DROP VIEW IF EXISTS meta21 CASCADE;
CREATE VIEW meta21 AS
(SELECT *, 
	(SELECT id FROM metadata ly WHERE term_id=9 ORDER BY st_distance(m.location, ly.location) ASC LIMIT 1) as close_meta20_id,
	(SELECT id FROM metadata ly WHERE term_id=7 ORDER BY st_distance(m.location, ly.location) ASC LIMIT 1) as close_meta19_id
	FROM metadata m
	WHERE term_id = 11 AND sensor_id = 1)
	
--create view with continuous id and comparable value
DROP VIEW IF EXISTS data_norm CASCADE;
CREATE VIEW data_norm AS
SELECT 
	row_number() OVER (PARTITION BY meta_id ORDER BY tstamp ASC) as measurement_index,
	*,
	value - avg(value) OVER (PARTITION BY meta_id) AS norm
FROM data;

--correlate the values from this term to the 19 and 20 term join them 
--using the index and id to ensure the correct assignment of the values
DROP VIEW IF EXISTS indices_corr CASCADE;
CREATE VIEW indices_corr AS 
	SELECT 
		meta21.id AS meta_id, 											
		corr(d.norm, d20.norm) AS "t_corr1y",	
		corr(d.norm, d19.norm) AS "t_corr2y"-- Mirkos solution of the correlation
	FROM data_norm d													
	JOIN meta21 on meta21.id = d.meta_id		
	JOIN metadata m20 on meta21.close_meta20_id=m20.id
	JOIN data_norm d20 on m20.id=d20.meta_id AND d.measurement_index=d20.measurement_index
	JOIN metadata m19 on meta21.close_meta19_id=m19.id
	JOIN data_norm d19 on m19.id=d19.meta_id AND d.measurement_index=d19.measurement_index
	GROUP BY meta21.id

--Join all views to one
DROP VIEW IF EXISTS indices_t CASCADE;
CREATE VIEW indices_t AS
SELECT 
	d.meta_id,
	d.device_id,
	t_avg,
    t_d,
	t_n,
	t_d - t_n AS t_dn,
	indices_corr.t_corr1y,
	indices_corr.t_corr2y
FROM mean_day AS d
JOIN mean_night AS n ON d.device_id = n.device_id 
JOIN mean_all AS a ON d.device_id = a.device_id
FULL JOIN indices_corr ON d.meta_id = indices_corr.meta_id

SELECT * FROM indices_t ORDER BY meta_id ASC
