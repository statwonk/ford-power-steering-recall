source("organize_data.R")
source("de_duplicated_data.R")

library(survival)
library(ggthemes)
library(scales)

# Ford identifies 824,378 vehicles as equiped with the
# EPAS system (source: INRL-PE14030-61119P.pdf)
production_data()

# However, Ford's recall only covers 393,623 vehicles
# (source http://www-odi.nhtsa.dot.gov/acms/cs/jaxrs/download/doc/UCM486038/INCLA-PE14030-7526.PDF),
# I'd like to analyze if the failure rates for vehicles not covered
# match the failure rates of vehicles recalled.

# According to the NHTSA, vehicles covered by the recall have a failure
# rate at 10-years of 8-14 percent while those not covered ~ 1 percent.

# NHTSA quote citing the reason the remaining 430,755 vehicles were not recalled:

# > In addition, statistical modeling of the failure data
# > by NHTSA's National Center for Statistics and Analysis projected 10-year failure rates of approximately 8-14 percent
# > for the recalled vehicles and approximately 1 percent for the non-recalled subject vehicles. This investigation is
# > closed. All ODI complaints associated with this closing resume are listed in the Attachment.
# (source: http://www-odi.nhtsa.dot.gov/acms/cs/jaxrs/download/doc/UCM486038/INCLA-PE14030-7526.PDF)

d <- left_join(
  production_data() %>%
    rename(production_date = DOB) %>%
    group_by(vin, production_date) %>%
    mutate(records = 1:n()),
  non_duplicated_failure_data() %>%
    group_by(vin, production_date) %>%
    mutate(records = 1:n()) %>%
    select(-model), # duplicated in both, not necessary
  by = c("vin", "production_date", "records")) %>%
  ungroup %>%
  mutate(production_date = as.Date(production_date),
         report_date = as.Date(report_date)) %>%
  mutate(time_to_failure = ifelse(
    is.na(report_date),
    as.numeric(difftime(as.Date("2014-11-20"), # last report date
                        production_date, units = "weeks")) / 52,
    as.numeric(difftime(report_date, production_date, units = "weeks")) / 52),
    censor = ifelse(!is.na(report_date), 1, 0),
    model_by_year = paste(model, MY, sep = "_"))

model_year_km <- survfit(Surv(time_to_failure, censor) ~ factor(MY), data = d)
model_year_km <- createSurvivalFrame(model_year_km)
model_year_km$strata <- gsub("factor\\(MY\\)\\=", "", model_year_km$strata)

source("plotting.R")
ggplot(data = model_year_km,
       aes(colour = strata,
           fill = strata,
           x = time)) +
  geom_ribbon(aes(ymin = 1 - upper,
                  ymax = 1 - lower),
              size = 0.5,
              alpha = 0.8) +
  geom_line(aes(y = 1 - surv),
            size = 1) +
  scale_colour_discrete(name = "", guide = "none") +
  scale_fill_discrete(name = "Model Year") +
  scale_y_continuous(labels = percent,
                     breaks = seq(0, 0.1, 0.005)) +
  scale_x_continuous(labels = comma,
                     breaks = seq(0, 30, 1)) +
  theme_bw(base_size = 25) +
  xlab("Years from production") +
  ylab("Cumulative Percentage Failing") +
  ggtitle("Ford Power Steering Failure Rates")

model_km <- survfit(Surv(time_to_failure, censor) ~ factor(model), data = d)
model_km <- createSurvivalFrame(model_km)
model_km$strata <- gsub("factor\\(model\\)\\=", "", model_km$strata)
ggplot(data = model_km,
       aes(colour = strata,
           fill = strata,
           x = time)) +
  geom_ribbon(aes(ymin = 1 - upper,
                  ymax = 1 - lower),
              size = 0.5,
              alpha = 0.5) +
  geom_line(aes(y = 1 - surv),
            size = 1) +
  scale_colour_discrete(name = "", guide = "none") +
  scale_fill_discrete(name = "Model") +
  scale_y_continuous(labels = percent,
                     breaks = seq(0, 0.1, 0.005)) +
  scale_x_continuous(labels = comma,
                     breaks = seq(0, 30, 1)) +
  theme_bw(base_size = 25) +
  xlab("Years from production") +
  ylab("Cumulative Percentage Failing") +
  ggtitle("Ford Power Steering Failure Rates") +
  custom_theme()


model_my_km <- survfit(Surv(time_to_failure, censor) ~ factor(model) + factor(MY), data = d)
model_my_km <- createSurvivalFrame(model_my_km)
model_my_km$strata <- gsub("factor\\(model\\)\\=", "", model_my_km$strata)
model_my_km$strata <- gsub("factor\\(MY\\)\\=", "", model_my_km$strata)

model_my_km$year <- as.vector(sapply(model_my_km$strata, FUN = function(x) { strsplit(x, ", ")[[1]][2] }))
model_my_km$model <- as.vector(sapply(model_my_km$strata, FUN = function(x) { strsplit(x, ", ")[[1]][1] }))

ggplot(data = model_my_km,
       aes(colour = model,
           fill = model,
           x = time)) +
  geom_ribbon(aes(ymin = 1 - upper,
                  ymax = 1 - lower),
              size = 0,
              alpha = 0.1) +
  geom_line(aes(y = 1 - surv),
            size = 1) +
  scale_colour_discrete(name = "", guide = "none") +
  scale_fill_discrete(name = "Model") +
  scale_y_continuous(labels = percent,
                     breaks = seq(0, 0.1, 0.005)) +
  scale_x_continuous(labels = comma,
                     breaks = seq(0, 30, 1)) +
  theme_bw(base_size = 25) +
  xlab("Years from production") +
  ylab("Cumulative Percentage Failing") +
  ggtitle("Ford Power Steering Failure Rates") +
  custom_theme() +
  facet_wrap(~ year) +
  theme(strip.text = element_text(size = 40, face = "bold"))



