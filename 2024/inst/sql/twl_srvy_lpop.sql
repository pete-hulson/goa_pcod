SELECT
    year,
    survey_definition_id as survey,
    area_id as strata,
    species_code,
    length_mm / 10 as length,
    sex,
    population_count as num
FROM gap_products.akfin_sizecomp
WHERE
survey_definition_id
-- insert survey
AND species_code
-- insert species
AND area_id in ('99903')
