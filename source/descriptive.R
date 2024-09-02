rm(list = ls())

# List of required packages
packages <- c(
    "sf", "tidyverse",
    "janitor", "stars", "readxl", "fixest", "kableExtra", "ggh4x"
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

n_fail_check <- sum(is.na(df$attention_check) | df$attention_check != 8, na.rm = TRUE)
n_finished <- sum(df$finished == 1, na.rm = TRUE)
n_left <- sum(df$ideology_left < 4, na.rm = TRUE)

df <- df %>% filter(attention_check == 8)
df <- df %>% filter(finished == 1)
df <- df %>% filter(ideology_left < 4)

# Print quantities

n_fail_check %>% cat("Respondents who failed the attention check: ", ., "\n")
n_finished %>% cat("Respondents who finished the survey: ", ., "\n")
n_left %>% cat("Respondents who are not too left-leaning: ", ., "\n")

# Variable dictionary

dict_o <- c(
    "dem_estimate_post" = "Belief updating",
    "post_democ_index" = "Pro-democracy index",
    "democ1_squabble_post" = "Democracies squabble too much\n(reversed)",
    "democ2_econ_bad_post" = "Democracies run economy badly\n(reversed)",
    "democ3_no_order_post" = "Democracies have no order\n(reversed)",
    "democ4_military_post" = "Military should govern country\n(reversed)",
    "democ5_better_post" = "Democracies better",
    "behavioral" = "Donated to democracy NGO"
) %>%
    data.frame(dv = names(.), outcome = .) %>%
    mutate(order = 1:n())

# List of outcomes

olist <- c(
    "dem_estimate_post", "post_democ_index", "democ1_squabble_post", "democ2_econ_bad_post",
    "democ3_no_order_post", "democ4_military_post", "democ5_better_post", "behavioral"
)

# Group means by treatment status ---------------------------------------------------

# Get group means

df_summary <- df %>%
    filter(!is.na(treat_share_high)) %>%
    group_by(treat_share_high) %>%
    summarise(across(
        all_of(olist),
        list(
            mean = ~ mean(.x, na.rm = TRUE),
            se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
        ),
        .names = "{col}_{fn}"
    )) %>%
    pivot_longer(
        cols = -treat_share_high,
        names_to = "variable",
        values_to = "value"
    ) %>%
    mutate(what = ifelse(str_detect(variable, "_mean"), "mean", "se")) %>%
    mutate(variable = str_remove(variable, "_mean|_se")) %>%
    pivot_wider(
        names_from = what,
        values_from = value
    ) %>%
    left_join(dict_o, by = c("variable" = "dv")) %>%
    filter(!str_detect(variable, "_pre")) %>%
    mutate(group = case_when(
        str_detect(variable, "index") ~ "Index\n(Scale: 3-15)",
        str_detect(variable, "estimate") ~ "Belief updating\n(Scale: 0-100)",
        str_detect(variable, "behavioral") ~ "Behavioral\n(Scale: 0-1)",
        TRUE ~ "Index components\n(Scale: 1-5)"
    )) %>%
    mutate(conf.low = mean - 1.96 * se, conf.high = mean + 1.96 * se) %>%
    mutate(treated = ifelse(treat_share_high == 1, "Treated", "Control"))

# Plot

df_summary %>%
    filter(str_detect(group, "components")) %>%
    pull(mean) %>%
    range()

p1 <- df_summary %>%
    ggplot(aes(outcome, mean,
        fill = treated, color = treated,
        shape = treated, ymin = conf.low, ymax = conf.high
    )) +
    geom_errorbar(width = 0, position = position_dodge(0.5)) +
    geom_point(position = position_dodge(0.5)) +
    ggh4x::facet_grid2(group ~ 1,
        scales = "free", drop = TRUE, space = "free_y", independent = "x"
    ) +
    coord_flip() +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    scale_shape_manual(values = c(21, 22)) +
    labs(
        x = NULL,
        y = "Mean",
        fill = "",
        color = "",
        shape = ""
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        legend.key.width = unit(3, "lines"), # Increase the size of the legend key symbols
        legend.key.size = unit(1.5, "lines"), # Increase the size of the legend key symbols
        strip.text.x = element_blank(), # Remove the top strip text
        strip.background.x = element_blank(), # Remove the top strip background
        strip.placement = "outside"
    ) +
    force_panelsizes(rows = c(0.6, 0.6, 0.6, 1.2))

# Remove top strip

p1 <- p1 +
    theme(strip.placement = "outside")
p1

# Missingness ---------------------------------------------------

df %>%
    summarise(across(
        all_of(olist),
        list(
            miss = ~ sum(is.na(.x)) / n()
        ),
        .names = "{col}"
    )) %>%
    pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "value"
    ) %>%
    left_join(dict_o, by = c("variable" = "dv")) %>%
    mutate(outcome = factor(outcome)) %>%
    mutate(outcome = fct_reorder(outcome, value)) %>%
    ggplot(aes(outcome, value)) +
    geom_point(shape = 21, fill = "white", size = 3) +
    labs(
        x = NULL,
        y = "Share missing observations",
        fill = "",
        shape = ""
    ) +
    theme_bw() +
    coord_flip()

# Balance ---------------------------------------------------

cov_list <- c(
    "age", "income", "dem_estimate_pre", "pre_democ_index",
    "educ", "ideology_left", "part_time_employ",
    "full_time_employ", "male"
)


# Dictionary

dict_cov <- c(
    "age" = "Age",
    "income" = "Income",
    "educ" = "Education",
    "part_time_employ" = "Part-time employment",
    "full_time_employ" = "Full-time employment",
    "ideology_left" = "Ideology",
    "dem_estimate_pre" = "Group size beliefs\n(pre-treatment)",
    "pre_democ_index" = "Pro-democracy index\n(pre-treatment)"
) %>%
    data.frame(dv = names(.), label = .) %>%
    mutate(order = rev(1:n()))

# Balance ---------------------------------------------------

b1 <- get_bal("treat_share_high", dict_cov$dv, df) %>%
    mutate(treat = "Group size treatment")

b2 <- get_bal("treat_turnout_high", dict_cov$dv, df) %>%
    mutate(treat = "Turnout treatment")

# Combine

bind_rows(b1, b2) %>%
    left_join(dict_cov, by = c("cov" = "dv")) %>%
    mutate(label = fct_reorder(label, order)) %>%
    ggplot(aes(label, coef, lower = lower, upper = upper)) +
    geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
    geom_point(shape = 21, fill = "white", size = 2) +
    labs(
        x = NULL,
        y = "Standardized difference",
        fill = "",
        shape = ""
    ) +
    theme_bw() +
    coord_flip() +
    facet_wrap(~treat)

# Summary statistics ---------------------------------------------------

vlist <- c(
    dict_cov$dv,
    dict_o$dv,
    "treat_share_high",
    "treat_turnout_high"
)

vlist_names <- c(
    dict_cov$label,
    dict_o$outcome,
    "Treated (group size)",
    "Treated (turnout)"
) %>%
    str_replace("\n", " ")

# Summary statistics

sdf <- df[, vlist] %>%
    map_dfr(haschaR::summarize_vec) %>%
    mutate(variable = vlist_names) %>%
    dplyr::select(
        variable, mean_v,
        median_v, sd_v, min_v, max_v
    )

tab_sum <- kbl(sdf,
    digits = 2,
    linesep = "",
    booktabs = T,
    format = "latex",
    col.names = c("", "Mean", "Median", "SD", "Min", "Max"),
    caption = "Summary statistics",
    label = "sum",
    position = "h"
) %>%
    kable_paper(full_width = FALSE) %>%
    footnote(
        general = "The table shows summary statistics for aggregate-level data in 2013.",
        general_title = "Notes: ",
        alphabet_title = "Type II: ",
        footnote_as_chunk = T, title_format = c("italic")
    ) %>%
    kable_styling(font_size = 12) %>%
    pack_rows("Controls", 1, 7) %>%
    pack_rows("Outcomes", 8, 16) %>%
    pack_rows("Treatment", 17, 18)

tab_sum

# Change between pre- and post-treatment group size beliefs by treatment group (distributions) ------------------
# Calculate diff_belief outside of ggplot to ensure it exists
df <- df %>%
    mutate(diff_belief = dem_estimate_post - dem_estimate_pre) %>%
    mutate(treat_share_high_label = ifelse(treat_share_high == 1,
        "Treated: high estimate of the\nnon-White population in 2040",
        "Control: low estimate of the\nnon-White population in 2040"
    ))

# Box plot to show the change in beliefs by treatment group

df %>%
    ggplot(aes(x = treat_share_high_label, y = diff_belief, fill = treat_share_high_label)) +
    stat_summary(
        fun.data = mean_se,
        geom = "errorbar",
        width = 0.0
    ) +
    stat_summary(
        fun = mean,
        geom = "point",
        position = "dodge"
    ) +
    labs(
        x = NULL,
        y = "Change in beliefs about the size\nof the non-White population in 2040\n(percentage points)"
    ) +
    haschaR::theme_hanno() +
    theme(legend.position = "bottom") +
    theme(legend.position = "none") +
    coord_flip()
