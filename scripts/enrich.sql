create table "2019_enriched"
as
select
	raw.date,
	raw.hour,
	raw.rain,
	pmax / 10 as pmax,
	pmin / 10 as pmin,
	tmax,
	tmin,
	dpmax,
	dpmin,
	hmax,
	hmin,
	(pmax / 10) - (pmin / 10) as pdiff,
	tmax - tmin as tdiff,
	dpmax - dpmin as dpdiff,
	hmax - hmin as hdiff,
	AVG(pmax / 10) as avg_pmax,
	AVG(pmin / 10) as avg_pmin,
	AVG(tmax) as avg_tmax,
	AVG(tmin) as avg_tmin,
	AVG(dpmax) as avg_dpmax,
	AVG(dpmin) as avg_dpmin,
	AVG(hmax) as avg_hmax,
	AVG(hmin) as avg_hmin
from public."2019" as raw
group by raw.date, raw.hour, raw.rain, raw.pmax, raw.pmin, raw.tmax, raw.tmin, raw.dpmax, raw.dpmin, raw.hmax, raw.hmin
order by raw.date asc;
