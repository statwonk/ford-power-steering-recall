library(openNHTSA)
library(dplyr)
library(scales)

collate_failures <- function(year) {
  head(
  tbl_df(facility("complaints") %>%
           model_year(year) %>%
           vehicle_make("ford") %>%
           vehicle_model("fusion") %>%
           nhtsa_fetch()) %>%
    mutate(total = n()) %>%
    group_by(Component, total) %>%
    summarise(count = n()) %>% ungroup %>%
    mutate(share = count / total) %>%
    arrange(desc(share)) %>%
    mutate(share = percent(share)), 5) # show only the top 5
}

collate_failures(2009) # power steering not in the top 10
collate_failures(2010) # steering failures @ 10.5% of all complaints
collate_failures(2011) # steering failures @ 34.5% of all complaints
collate_failures(2012) # steering failures @ 52.6% of all complaints
collate_failures(2013) # steering failures @ 10.7% of all complaints
collate_failures(2014) # steering failures @ 13.5% of all complaints

