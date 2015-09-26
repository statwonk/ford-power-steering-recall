library(dplyr)
library(RSQLite)

production_data <- src_sqlite("unpacked_databases/61128/61128.db")

# Production data
tbl(production_data, "Fusion")
tbl(production_data, "Milan")
tbl(production_data, "MKZ")


# Failure data
failure_data <- src_sqlite("unpacked_databases/61129/61129.db")

tbl(failure_data, "t_A_Import") %>%
  select(model_year = MDL_YR,
         production_date = PRODN_DT,
         report_date = RPR_DT,
         vin = VIN_CD,
         mileage = MILGE,
         duplicate_report = Dup,
         category_of_incident = Cat)

tbl(failure_data, "t_C_Header") %>%
  select(model_year = MODEL_YEAR,
         production_date = PROD_DATE,
         report_date = REPORT_DATE,
         mileage = ODO_MILES,
         model = VEHICLE_DESC,
         vin = VIN_NUMBER,
         duplicate_report = Dup,
         category_of_incident = Cat)

tbl(failure_data, "t_M3_Header") %>%
  select(vin = VIN,
         model_year = MODELYEAR,
         model = MODELCODE,
         report_date = CASEOPENDATE,
         mileage = MILEAGE,
         duplicate_report = Dup,
         category_of_incident = Cat)







