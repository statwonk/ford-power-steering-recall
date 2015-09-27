library(dplyr)
library(RSQLite)

production_data <- function() {

  vehicle_production_database <- src_sqlite("unpacked_databases/61128/61128.db")

  # Stack all models, these will give us the date each vehicle was
  # produced so we can measure rates of failure.
  tbl_df(
    rbind(collect(tbl(vehicle_production_database, "Fusion")),
          collect(tbl(vehicle_production_database, "Milan")),
          collect(tbl(vehicle_production_database, "MKZ")))) %>%
    rename(vin = VIN) # lower case for join to failure data
}

failure_data <- function() {

  failure_database <- src_sqlite("unpacked_databases/61129/61129.db")

  # Ford provides three sources that collect and report vehicle
  # failures:
  # 1. AWS
  # 2. CQIS
  # 3. FM360

  aws_source <- tbl(failure_database, "t_A_Import") %>%
    # It seems the model type is not provided in this
    # data source. However this can be uniquely
    # identified from the production database.
    select(vin = VIN_CD,
           model_year = MDL_YR,
           production_date = PRODN_DT,
           report_date = RPR_DT,
           mileage = MILGE,
           duplicate_report = Dup,
           data_source = Source,
           category_of_incident = Cat)

  # AWS doesn't include a model field, but since models
  # are unique within the included censored blocks of VIN
  # identification numbers, we can join this from the
  # production database.
  production_data() %>%
    group_by(vin, model) %>%
    summarise(count = n())
  # Source: local data frame [1 x 2]
  #
  # non_unique_model_per_vin count
  # (lgl) (int)
  # 1                    FALSE   424

  aws_source <- left_join(
    collect(aws_source),
    production_data() %>%
      filter(!duplicated(vin)) %>%
      select(vin, model))

  # The CQIS doesn't uniquely identify models.
  # For example, Fusions Hybrid or not are identified in the same way
  # collect(tbl(failure_database, "t_C_Header") %>% group_by(VEHICLE_DESC) %>% summarise(count = n()))
  # Let's use the more specific production data to more uniquely identify models for reliability
  # analysis.


  cqis_source <- tbl(failure_database, "t_C_Header") %>%
    select(vin = VIN_NUMBER,
           model_year = MODEL_YEAR,
           production_date = PROD_DATE,
           report_date = REPORT_DATE,
           mileage = ODO_MILES,
           duplicate_report = Dup,
           data_source = Source,
           category_of_incident = Cat)

  cqis_source <- left_join(
    collect(cqis_source),
    production_data() %>%
      filter(!duplicated(vin)) %>%
      select(vin, model))

  fmc360_source <- tbl(failure_database, "t_M3_Header") %>%
    # Production date is not included in this database,
    # but it is included in the other two databases.
    # It's likely to be valuable in terms of joining
    # on production data.
    mutate(production_date = NA) %>%
    select(vin = VIN,
           model = MODELCODE,
           model_year = MODELYEAR,
           production_date, # see comment above
           report_date = CASEOPENDATE,
           mileage = MILEAGE,
           duplicate_report = Dup,
           data_source = Source,
           category_of_incident = Cat)

  # Stack all failure reports, these will give us the date
  # of reported failure from each source. The next step will
  # be to de-duplicate reports.

  tbl_df(
    rbind(aws_source, # already collected when joining models from production database.
          cqis_source, # already collected when joining models from production database.
          collect(fmc360_source)))
}


