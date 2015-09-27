source("organize_data.R")

non_duplicated_failure_data <- function() {

  failure_data() %>%
    # "10-year failure rate" implies reliability modeling.
    # this requires "birth" and "death" dates. Unfortunately,
    # Ford did not include "birth dates" for the FMC360 facility
    # so we won't be able to join these do the production data
    # on the VIN numbers alone. VIN numbers don't uniquely
    # identify a production date.
    filter(!is.na(production_date),
           category_of_incident == "A") %>% # Category A failures are failures that Ford identified as,
    # Loss of electric power assisted steering while driving. Let's focus on these as they're
    # what I experienced and it's dangerous.
    arrange(vin, production_date, report_date) %>%
    filter(is.na(duplicate_report)) # remove duplicates
}
