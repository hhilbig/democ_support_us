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
glimpse(df)

# Prep data ---------------------------------------------------------------

# 1. Pass attention check
# 2. Finished the survey
# 3. Not too left-leaning

## Select main sample

df <- df %>% filter(attention_check == 8)
df <- df %>% filter(finished == 1)
df <- df %>% filter(ideology_left < 4)

### scale all outcomes
df <- df %>%
    mutate(across(all_of(c("post_democ_index", "pre_democ_index", "dem_estimate_post")), scale)) %>%
    mutate(across(ends_with("_pre"), as.numeric),
        across(ends_with("_post"), as.numeric),
        post_democ_index = as.numeric(post_democ_index)
    )

# Def categories for HTE

quantile(df$income)

df <- df %>%
    mutate(
        educ_college = educ > 3,
        age_categ = case_when(
            age < 30 ~ "1. <30",
            age >= 30 & age < 40 ~ "2. 30-39",
            age >= 40 & age < 50 ~ "3. 40-49",
            age >= 50 & age < 60 ~ "4. 50-59",
            age >= 60 ~ "5. 60+"
        ),
        non_white_categeorical = case_when(
            non_white_share <= 0.25 ~ "1. <25%",
            non_white_share > 0.25 & non_white_share <= 0.5 ~ "2. 25-50%",
            non_white_share > 0.5 & non_white_share <= 0.75 ~ "3. 50-75%",
            non_white_share > 0.75 ~ "4. >75%"
        )
    ) %>%
    mutate(income_categ = case_when(
        income < 25000 ~ "<25k",
        income >= 25000 & income < 50000 ~ "25-50k",
        income >= 50000 & income < 75000 ~ "50-75k",
        income >= 75000 ~ ">75k"
    ))

# Models ---------------------------------------------------------------

# Vector of outcomes

olist <- c(
    "dem_estimate_post", "post_democ_index"
)

df$treat_turnout_high

m_ideology <- feols(
    .[olist] ~ treat_share_high + treat_turnout_high + age + income + dem_estimate_pre + pre_democ_index + educ |
        male + zip_first_digit + +part_time_employ + full_time_employ,
    data = df,
    split = ~ideology_left
) %>%
    map_dfr(tidy_feols) %>%
    filter(term == "treat_share_high") %>%
    mutate(ideology_left = rep(c("Extremely\nconservative", "Conservative", "Slightly\nconservative"), each = length(olist))) %>%
    mutate(ideology_left = factor(ideology_left, levels = c("Extremely\nconservative", "Conservative", "Slightly\nconservative")))

m_age <- feols(
    .[olist] ~ treat_share_high + treat_turnout_high + age + income + dem_estimate_pre + pre_democ_index + educ + ideology_left |
        male + zip_first_digit + +part_time_employ + full_time_employ,
    data = df,
    split = ~age_categ
) %>%
    map_dfr(tidy_feols) %>%
    filter(term == "treat_share_high") %>%
    mutate(age_categ = rep(c("<30", "30-39", "40-49", "50-59", "60+"), each = length(olist))) %>%
    mutate(age_categ = factor(age_categ, levels = c("<30", "30-39", "40-49", "50-59", "60+")))

m_educ <- feols(
    .[olist] ~ treat_share_high + treat_turnout_high + age + income + dem_estimate_pre + pre_democ_index + ideology_left |
        male + zip_first_digit + +part_time_employ + full_time_employ,
    data = df,
    split = ~educ_college
) %>%
    map_dfr(tidy_feols) %>%
    filter(term == "treat_share_high") %>%
    mutate(educ_college = rep(c("No college", "College"), each = length(olist)))

m_gender <- feols(
    .[olist] ~ treat_share_high + age + income + dem_estimate_pre + pre_democ_index + ideology_left + educ |
        zip_first_digit + +part_time_employ + full_time_employ,
    data = df,
    split = ~male
) %>%
    map_dfr(tidy_feols) %>%
    filter(term == "treat_share_high") %>%
    mutate(male = rep(c("Female", "Male"), each = length(olist)))

m_income <- feols(
    .[olist] ~ treat_share_high + treat_turnout_high + age + income + dem_estimate_pre + pre_democ_index + ideology_left + educ |
        zip_first_digit + +part_time_employ + full_time_employ,
    data = df,
    split = ~income_categ
) %>%
    map_dfr(tidy_feols) %>%
    filter(term == "treat_share_high") %>%
    mutate(income_categ = rep(c("<25k", "25-50k", "50-75k", ">75k"), each = length(olist))) %>%
    mutate(income_categ = factor(income_categ, levels = c("<25k", "25-50k", "50-75k", ">75k")))

m_non_white <- feols(
    .[olist] ~ treat_share_high + treat_turnout_high + age + income + dem_estimate_pre + pre_democ_index + ideology_left + educ |
        zip_first_digit + +part_time_employ + full_time_employ,
    data = df,
    split = ~non_white_categeorical
) %>%
    map_dfr(tidy_feols) %>%
    filter(term == "treat_share_high") %>%
    mutate(non_white_categeorical = rep(c(
        "<25%", "25-50%",
        "50-75%", " >75%"
    ), each = length(olist))) %>%
    mutate(non_white_categeorical = factor(non_white_categeorical, levels = c(
        "<25%", "25-50%",
        "50-75%", " >75%"
    )))

# Estimates w/ SEs ---------------------------------------------------------------

# Function that returns the estimate for each subgroup and outcome with a 95% CI and N as a text string
format_estimate_ci_n <- function(estimate, conf.low, conf.high, n) {
    formatted_estimate <- sprintf("%.2f", estimate)
    formatted_ci <- sprintf("CI: [%.2f, %.2f]", conf.low, conf.high)
    formatted_n <- sprintf("N=%d", n)
    paste(formatted_estimate, paste0("(", formatted_ci, ", ", formatted_n, ")"))
}

hte_vars <- c("ideology_left", "age_categ", "educ_college", "male", "income_categ", "non_white_categeorical")

# Function to get subgroup estimates for each model
get_subgroup_estimates <- function(model, hte_var) {
    model %>%
        mutate(
            estimate_ci_n = format_estimate_ci_n(
                estimate, conf.low, conf.high, n
            ),
            .groups = "drop"
        ) %>%
        select(dv, {{ hte_var }}, everything()) %>%
        rename(subgroup = {{ hte_var }}) %>%
        select(dv, subgroup, estimate_ci_n) %>%
        arrange(dv, subgroup)
}


# Here, the models are tidy models (ie data frames)

# Apply the function to each model
educ_estimates <- get_subgroup_estimates(m_educ, "educ_college")
gender_estimates <- get_subgroup_estimates(m_gender, "male")
income_estimates <- get_subgroup_estimates(m_income, "income_categ")
non_white_estimates <- get_subgroup_estimates(m_non_white, "non_white_categeorical")

# Print all estimates
# 1. Education
print(educ_estimates)
# 2. Gender
print(gender_estimates)
# 3. Income
print(income_estimates)
# 4. Non-white share (zip-code level)
print(non_white_estimates)


# Plot ---------------------------------------------------------------

do_hte_plot <- function(hte_var, plot_label, data) {
    data[, "hte_var"] <- data[[hte_var]]

    pd <- position_dodge(width = 0.5)

    ggplot(data %>%
        mutate(dv = case_when(
            dv == "dem_estimate_post" ~ "Belief updating",
            dv == "post_democ_index" ~ "Pro-democracy index"
        )), aes(hte_var, estimate,
        ymin = conf.low, ymax = conf.high, color = dv, fill = dv
    )) +
        geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) +
        geom_errorbar(position = pd, width = 0.0) +
        geom_point(position = pd, shape = 21, size = 2) +
        labs(
            x = plot_label,
            y = "Standardized treatment effect",
            fill = "",
            shape = "",
            color = ""
        ) +
        theme_bw() +
        coord_flip() +
        scale_color_brewer(palette = "Set1", type = "qual") +
        scale_fill_brewer(palette = "Set1", type = "qual") +
        theme(legend.position = "bottom")
}

# Do across all variables

p1 <- do_hte_plot(
    "ideology_left",
    NULL, m_ideology
) +
    ggtitle("Ideology") +
    theme(legend.position = "none")
p1

p2 <- do_hte_plot(
    "age_categ",
    NULL, m_age
) +
    ggtitle("Age") +
    theme(legend.position = "none")
p2

p3 <- do_hte_plot(
    "educ_college",
    NULL, m_educ
) +
    ggtitle("Education") +
    theme(legend.position = "none")
p3

p4 <- do_hte_plot("male", NULL, m_gender) +
    ggtitle("Gender") +
    theme(legend.position = "none")
p4

p5 <- do_hte_plot(
    "income_categ",
    NULL, m_income
) +
    ggtitle("Income")
p5


p6 <- do_hte_plot(
    "non_white_categeorical",
    NULL, m_non_white
) +
    ggtitle("Non-white share (zip-code)")
p6

# Combine using cowplot ---------------------------------------------------------------

library(cowplot)

plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, align = "v", rel_widths = c(1, 1))
