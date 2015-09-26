library(dplyr)
library(RSQLite)

production_data <- function() {

  vehicle_production_database <- src_sqlite("unpacked_databases/61128/61128.db")

  # Stack all models, these will give us the date each vehicle was
  # produced so we can measure rates of failure.
  tbl_df(
    rbind(collect(tbl(vehicle_production_database, "Fusion")),
          collect(tbl(vehicle_production_database, "Milan")),
          collect(tbl(vehicle_production_database, "MKZ"))))
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
    # data source.
    mutate(model = NA) %>%
    select(vin = VIN_CD,
           model, # see comment above.
           model_year = MDL_YR,
           production_date = PRODN_DT,
           report_date = RPR_DT,
           mileage = MILGE,
           duplicate_report = Dup,
           data_source = Source,
           category_of_incident = Cat)

  cqis_source <- tbl(failure_database, "t_C_Header") %>%
    select(vin = VIN_NUMBER,
           model = VEHICLE_DESC,
           model_year = MODEL_YEAR,
           production_date = PROD_DATE,
           report_date = REPORT_DATE,
           mileage = ODO_MILES,
           duplicate_report = Dup,
           data_source = Source,
           category_of_incident = Cat)

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
    rbind(collect(aws_source),
          collect(cqis_source),
          collect(fmc360_source)))
}


