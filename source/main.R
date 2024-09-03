rm(list = ls())

# List of required packages
packages <- c(
  "sf", "tidyverse",
  "janitor", "stars", "readxl", "fixest", "kableExtra"
)

pacman::p_load(char = packages)

# Get functions

source("source/functions.R")

# Read data --------------------------------------------------------------

df <- read_csv("data/clean_data_upload.csv")

# Subset sample based on inclusion criteria ----------------------------

# 1. Pass attention check
# 2. Finished the survey
# 3. Ideology smaller than 4

df <- df %>%
  filter(attention_check == 8 & finished == 1 &
    ideology_left < 4)

# Scale indices
df$post_democ_index <- scale(df$post_democ_index)
df$pre_democ_index <- scale(df$pre_democ_index)

# Variable    Measurement
# Age         Measured in years
# Gender      Male/Female/Other
# ZIP         U.S. postal code
# Education level
#             Did not graduate high school / High school graduate / Some college /
#             College graduate / Postgraduate degree
#
# Income (pre-tax)
#             Individual income in the last 12 months, before taxes
#
# Marital status
#             Married / single / widowed / divorced
#
# Employment  Unemployed / in education or training / not on the labor market /
#             full-time employed / part-time employed / retired
#
# Race        White / Black or African American / Asian, American Indian and
#             Alaska Native / Native Hawaiian and Other Pacific Islander / some
#             other race (based on US census)
#
# Ideology    7-point left-right scale (based on ANES)

# Scale remaining outcomes

df <- df %>%
  mutate_at(vars(
    democ1_squabble_pre, democ2_econ_bad_pre, democ3_no_order_pre,
    democ4_military_pre, democ5_better_pre, dem_estimate_pre, dem_estimate_post,
    democ1_squabble_post, democ2_econ_bad_post, democ3_no_order_post,
    democ4_military_post, democ5_better_post
  ), scale) %>%
  mutate(across(ends_with("_pre"), as.numeric),
    across(ends_with("_post"), as.numeric),
    post_democ_index = as.numeric(post_democ_index)
  )

# Models ---------------------------------------------------------------

# Vector of outcomes

olist <- c(
  "dem_estimate_post", "post_democ_index", "democ1_squabble_post", "democ2_econ_bad_post",
  "democ3_no_order_post", "democ4_military_post", "democ5_better_post", "behavioral"
)

# Base spec (no controls)

m <- fixest::feols(.[olist] ~ treat_share_high + treat_turnout_high,
  data = df, se = "standard"
)

# Spec w/ controls

m_ctrl <- feols(
  .[olist] ~ treat_share_high + treat_turnout_high + age + income +
    dem_estimate_pre + pre_democ_index + educ + ideology_left |
    male + zip_first_digit + +part_time_employ + full_time_employ,
  data = df,
  se = "standard"
)

# Outcome dictionary

dict_o <- c(
  "dem_estimate_post" = "Belief updating",
  "post_democ_index" = "Pro-democracy index",
  "behavioral" = "Donated to\ndemocracy NGO",
  "democ1_squabble_post" = "Democracies squabble too much\n(reversed)",
  "democ2_econ_bad_post" = "Democracies run economy badly\n(reversed)",
  "democ3_no_order_post" = "Democracies have no order\n(reversed)",
  "democ4_military_post" = "Military should govern country\n(reversed)",
  "democ5_better_post" = "Democracies better"
) %>%
  data.frame(dv = names(.), outcome = .) %>%
  mutate(order = 1:n())

# Clean up coefs
# Define function to clean up coefs
# Get coefs

coef_base <- m %>%
  map_dfr(tidy_feols) %>%
  filter(term == "treat_share_high") %>%
  mutate(what = "Base model\n(no controls)")

coef_ctrl <- m_ctrl %>%
  map_dfr(tidy_feols) %>%
  filter(term == "treat_share_high") %>%
  mutate(what = "Controls included")

## Combine, merge dictionaries

coef_df <- bind_rows(coef_base, coef_ctrl) %>%
  left_join(dict_o, by = "dv")

# set offset of lines in different models
pd <- position_dodge(0.4)

# Main results: belief updating, index, and behavioral

p1_main <- coef_df %>%
  filter(dv %in% c("dem_estimate_post", "post_democ_index", "behavioral")) %>%
  mutate(outcome = fct_reorder(outcome, rev(order))) %>%
  ggplot() +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) +
  geom_errorbar(
    aes(
      x = outcome, ymin = conf.low,
      ymax = conf.high,
      color = what
    ),
    width = 0, position = pd
  ) +
  geom_point(
    aes(
      x = outcome, y = estimate,
      color = what,
      fill = what
    ),
    position = pd, size = 3, shape = 21
  ) +
  theme_bw() +
  ylab("Standardized treatment effect") +
  xlab("") +
  theme(text = element_text(size = 24, family = "Times")) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(type = "qual", name = "", palette = "Set2") +
  scale_color_brewer(type = "qual", name = "", palette = "Set2") +
  coord_flip()
p1_main

# Main results: table ----------------------------------------------------

kable_main <- coef_df %>%
  filter(str_detect(what, "2")) %>%
  filter(dv %in% c("dem_estimate_post", "post_democ_index", "behavioral")) %>%
  mutate(
    outcome = fct_reorder(outcome, rev(order)),
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = round(p.value, 3) # Add p-value and round it
  ) %>%
  select(outcome, estimate, conf.low, conf.high, p.value) %>% # Include p.value in selection
  kable(
    col.names = c("Outcome", "Estimate", "Lower CI", "Upper CI", "P-value"), # Add P-value column name
    caption = "Standardized Treatment Effects",
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r", "r", "r") # Add alignment for p-value column
  ) %>%
  kable_styling(latex_options = c("hold_position"))

# Print the table
print(kable_main)

# Create a formatted string with estimates and CIs - for paper -------------------------

ci_text <- coef_df %>%
  filter(str_detect(what, "2")) %>%
  filter(dv %in% c("dem_estimate_post", "post_democ_index", "behavioral")) %>%
  mutate(
    outcome = fct_reorder(outcome, rev(order)),
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3)
  ) %>%
  mutate(ci_string = sprintf("%s: %.3f [%.3f, %.3f]", outcome, estimate, conf.low, conf.high)) %>%
  pull(ci_string) %>%
  paste(collapse = "\n")

# Print the formatted string using cat()
cat("Estimates with 95% Confidence Intervals:\n\n", ci_text, "\n", sep = "")

# Coefficients for the democracy index subcomponents --------------------------------

p1_components <- coef_df %>%
  filter(str_detect(what, "2")) %>%
  filter(dv %in% c("democ1_squabble_post", "democ2_econ_bad_post", "democ3_no_order_post", "democ4_military_post", "democ5_better_post")) %>%
  mutate(outcome = fct_reorder(outcome, rev(order))) %>%
  ggplot() +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) +
  geom_errorbar(
    aes(
      x = outcome, ymin = conf.low,
      ymax = conf.high
    ),
    width = 0, position = pd
  ) +
  geom_point(
    aes(
      x = outcome, y = estimate
    ),
    position = pd, size = 3
  ) +
  theme_bw() +
  scale_shape_manual(values = c(21, 22, 23), name = "") +
  ylab("Standardized treatment effect") +
  xlab("") +
  theme(text = element_text(size = 18, family = "Times")) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(type = "qual", name = "") +
  scale_color_brewer(type = "qual", name = "") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-0.5, 0.3, 0.1))
p1_components

# Interactions ---------------------------------------------------------

source("Code/analysis/fixest_dict.R")

olist_short <- c(
  "dem_estimate_post",
  "post_democ_index",
  "behavioral"
)

m_ctrl_interact <- feols(
  .[olist_short] ~ treat_share_high * treat_turnout_high + age +
    income + dem_estimate_pre + pre_democ_index + educ + ideology_left |
    male + zip_first_digit + +part_time_employ + full_time_employ,
  data = df, se = "standard"
)

# Table of coefficients

sdf <- style.tex(
  depvar.title = "",
  fixef.title = "",
  fixef.suffix = " FE",
  var.title = "",
  stats.title = "",
  model.title = "",
  yesNo = c("Yes", "No"),
  signif.code = c(
    "***" = 0.001,
    "**" = 0.01, "*" = 0.05
  )
)

## Table

etable(m_ctrl_interact,
  keep = "Treated|treat",
  tex = T,
  style.tex = sdf,
  digits = 3,
  title = "Interaction specifications",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  drop.section = "fixef",
  label = "tab:interact",
  placement = "!h",
  fontsize = "scriptsize"
)

# Specification that has the effect of the turnout treatment alone ------------------------------

# Specification for turnout treatment effect alone
m_turnout_only <- feols(
  .[olist_short] ~ treat_turnout_high + age + income + dem_estimate_pre +
    pre_democ_index + educ + ideology_left |
    male + zip_first_digit + part_time_employ + full_time_employ,
  data = df, se = "standard"
)

# Table of coefficients for turnout treatment effect
etable(m_turnout_only,
  keep = "turnout",
  tex = TRUE,
  style.tex = sdf,
  digits = 3,
  title = "Turnout Treatment Effect",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  drop.section = "fixef",
  label = "tab:turnout_only",
  placement = "!h",
  fontsize = "scriptsize"
)
