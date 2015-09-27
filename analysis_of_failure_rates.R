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

failure_rates <- survfit(Surv(time_to_failure, censor) ~ factor(MY), data = d)

source("plotting.R")

fit <- createSurvivalFrame(failure_rates)
# fit$strata <- gsub("factor\\(model_by_year\\)\\=", "", fit$strata)
fit$strata <- gsub("factor\\(MY\\)\\=", "", fit$strata)

ggplot(data = fit,
       aes(colour = strata,
           fill = strata,
           x = time)) +
  geom_line(aes(y = 1 - surv),
            size = 1) +
  geom_ribbon(aes(ymin = 1 - upper,
                  ymax = 1 - lower),
              size = 0.5,
              alpha = 0.8) +
  scale_colour_discrete(name = "", guide = "none") +
  scale_fill_discrete(name = "Model Year") +
  scale_y_continuous(labels = percent,
                     breaks = seq(0, 0.1, 0.005)) +
  scale_x_continuous(labels = comma,
                     breaks = seq(0, 30, 1)) +
  theme_bw(base_size = 25) +
  xlab("Years to power steering failure") +
  ylab("Cumulative Percentage Failing") +
  ggtitle("Ford Power Steering Failure Rates") +
  theme(axis.text = element_text(colour = "black", face = "bold"),
        axis.text.y = element_text(size = "18"),
        panel.grid.major.y = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 22))

