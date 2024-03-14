SELECT 
  akfin_year,
  FMP_AREA,
  adfg_i_harvest_code,
  fmp_gear,
  SUM (cfec_whole_pounds / 2204.622) AS CATCH_MT
FROM council.comprehensive_Ft
WHERE akfin_year BETWEEN 1997 AND 2002
AND adfg_i_species_code = 110
AND adfg_i_harvest_code = 80
AND fmp_area = 'GOA'
GROUP BY 
  adfg_i_harvest_code,
  FMP_AREA,
  fmp_gear,
  akfin_year
