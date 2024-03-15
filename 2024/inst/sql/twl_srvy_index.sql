SELECT
    year,
    survey_definition_id as survey,
    area_id as strata,
    species_code,
    biomass_mt as biom,
    biomass_var as biom_var,
    population_count as num,
    population_var as num_var
FROM gap_products.akfin_biomass
WHERE
survey_definition_id
-- insert survey
AND species_code
-- insert species
AND area_id in ('803','804','805', '99903')
