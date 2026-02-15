# Load libraries
library(haven)
library(tidyverse)
library(survey)
library(broom)
library(patchwork)

#-------------------------------------------------------
# NHIS cleaned individual-level data
#-------------------------------------------------------

nhis_clean_indv <- read_dta("outputs/nhis_clean_indv.dta")

# Drop years after 2020
nhis_clean_indv <- nhis_clean_indv %>% filter(srvy_yr <= 2020)

#-------------------------------------------------------
# Cohort-sex dummies
#-------------------------------------------------------

# Create cohort-sex dummies
nhis_clean_indv <- nhis_clean_indv %>%
  mutate(
    c5to7in19m    = as.integer(cohort_sex == 1),     # males aged 5-7 in 2019
    c5to7in19f    = as.integer(cohort_sex == 2),     # females aged 5-7 in 2019
    c8to10in19m   = as.integer(cohort_sex == 3),     # males aged 8-10 in 2019
    c8to10in19f   = as.integer(cohort_sex == 4),     # females aged 8-10 in 2019
    c11to13in19m  = as.integer(cohort_sex == 5),     # males aged 11-13 in 2019
    c11to13in19f  = as.integer(cohort_sex == 6)      # females aged 11-13 in 2019
  )

# Store variable names
cohorts <- c(
  "c11to13in19f", "c11to13in19m", "c8to10in19f", 
  "c8to10in19m", "c5to7in19f", "c5to7in19m"
)

#-------------------------------------------------------
# Event-time dummies
#-------------------------------------------------------

# Time and event (using quarters)
nhis_clean_indv <- nhis_clean_indv %>%
  mutate(
    time = srvy_yr * 4 + intv_qrt,       # simple quarter index
    event = 2020 * 4 + 1,                # 2020Q1
    timeToTreat = time - event
  )

# Create event-time dummies
event_breaks <- -4:3   # lead4..lead1, lag0..lag3
nhis_clean_indv <- nhis_clean_indv %>%
  mutate(
    lead4 = as.integer(timeToTreat == -4),
    lead3 = as.integer(timeToTreat == -3),
    lead2 = as.integer(timeToTreat == -2),
    lead1 = as.integer(timeToTreat == -1),
    lag0  = as.integer(timeToTreat == 0),
    lag1  = as.integer(timeToTreat == 1),
    lag2  = as.integer(timeToTreat == 2),
    lag3  = as.integer(timeToTreat == 3)
  )

# Store variable names
lagvars <- c("lag0","lag1","lag2","lag3")
leadvars <- c("lead4","lead3","lead2")

#-------------------------------------------------------
# Cohort x event time interactions
#-------------------------------------------------------

# Loop over cohorts and event-time dummies to create interaction terms
for(c in cohorts){
  for(ev in c(lagvars, leadvars)){
    nhis_clean_indv[[paste0(c, "_", ev)]] <- nhis_clean_indv[[c]] * nhis_clean_indv[[ev]]
  }
}

# Store variable names
cohort_lags <- unlist(lapply(cohorts, function(c) paste0(c, "_", lagvars)))
cohort_leads <- unlist(lapply(cohorts, function(c) paste0(c, "_", leadvars)))

#-------------------------------------------------------
# Control variables
#-------------------------------------------------------

# Transform variables to factors
nhis_clean_indv <- nhis_clean_indv %>% 
  mutate(
    race = as_factor(race),
    lvledu = as_factor(lvledu),
    cntparnts = as_factor(cntparnts),
    income = as_factor(income)
  )

# Store variable names
controls <- c("race", "cntparnts", "lvledu", "income")

#-------------------------------------------------------
# Survey design
#-------------------------------------------------------

nhis_clean_indv <- nhis_clean_indv %>% 
  mutate(wtfa_c5 = wtfa_c / 5)

nhis_design <- svydesign(
  id = ~ppsu,
  strata = ~pstrat,
  weights = ~wtfa_c5,
  data = nhis_clean_indv,
  nest = TRUE
)

#-------------------------------------------------------
# Fully pooled model (common event-time effects)
#-------------------------------------------------------
# This model is not included in the paper.

# Depression
formula_fully_pooled_dep <- paste(
  "dep ~ -1 +", 
  paste(c(cohorts, c(leadvars, lagvars), controls), collapse=" + ")
)

model_fully_pooled_dep <- svyglm(as.formula(formula_fully_pooled_dep), design = nhis_design)
summary(model_fully_pooled_dep)

# Anxiety
formula_fully_pooled_anx <- paste(
  "anx ~ -1 +", 
  paste(c(cohorts, c(leadvars, lagvars), controls), collapse=" + ")
)

model_fully_pooled_anx <- svyglm(as.formula(formula_fully_pooled_anx), design = nhis_design)
summary(model_fully_pooled_anx)

#-------------------------------------------------------
# Model 1: Partially pooled model (shared leads & cohort × lag interactions)
#-------------------------------------------------------
# This model is summarized in Figure 2 and Figure 3.

# Depression
formula_partially_pooled_dep <- paste(
  "dep ~ -1 +",
  paste(c(cohorts, leadvars, cohort_lags, controls), collapse = " + ")
)

model_partially_pooled_dep <- svyglm(as.formula(formula_partially_pooled_dep), design = nhis_design)
summary(model_partially_pooled_dep)

# Anxiety
formula_partially_pooled_anx <- paste(
  "anx ~ -1 +",
  paste(c(cohorts, leadvars, cohort_lags, controls), collapse = " + ")
)

model_partially_pooled_anx <- svyglm(as.formula(formula_partially_pooled_anx), design = nhis_design)
summary(model_partially_pooled_anx)

#-------------------------------------------------------
# Model 2: Fully interacted model (cohort × lead and lag interactions)
#-------------------------------------------------------
# This model is summarized in eFigure 1 and eFigure 2 in the Appendix.

# Depression
formula_fully_interacted_dep <- paste(
  "dep ~ -1 +", 
  paste(c(cohorts, cohort_leads, cohort_lags, controls), collapse=" + ")
)

model_fully_interacted_dep <- svyglm(as.formula(formula_fully_interacted_dep), design = nhis_design)
summary(model_fully_interacted_dep)

# Anxiety
formula_fully_interacted_anx <- paste(
  "anx ~ -1 +", 
  paste(c(cohorts, cohort_leads, cohort_lags, controls), collapse=" + ")
)

model_fully_interacted_anx <- svyglm(as.formula(formula_fully_interacted_anx), design = nhis_design)
summary(model_fully_interacted_anx)

#-------------------------------------------------------
# Plot function
#-------------------------------------------------------

plot_event_study <- function(data) {
  
  ggplot(
    data,
    aes(
      x = event_time,
      y = estimate
    )
  ) +
    
    geom_point(color="red") +
    
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.2,
      color = "blue"
    ) +
    
    geom_point(aes(x = -1, y = 0), color="red") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -1, linetype = "dashed") +
    
    scale_x_continuous(
      breaks = -4:3,
      labels = c(
        "2019 Q1", "Q2", "Q3", "Q4",
        "2020 Q1", "Q2", "Q3", "Q4"
      )
    ) +
    
    scale_y_continuous(breaks = seq(-100, 100, by = 10)) +
    
    facet_wrap(~ sex, ncol = 2, scales = "fixed") +
    
    labs(x = "", y = "") +
    
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95")
    )
}

three_panel_plot <- function(data, ylab){
  
  # Necessary for males to be on the left side of the plot
  data <- data %>% mutate(
    sex = factor(sex, levels = c("Males", "Females"))
  )
  
  plot1 <- data %>% 
    filter(cohort == "c11to13in19") %>% 
    plot_event_study() +
    labs(title = "A. 11-13 years in 2019")
  
  plot2 <- data %>% 
    filter(cohort == "c8to10in19") %>% 
    plot_event_study() + 
    labs(
      title = "B. 8-10 years in 2019",
      y = ylab
    )
  
  plot3 <- data %>% 
    filter(cohort == "c5to7in19") %>% 
    plot_event_study() +
    labs(
      title = "C. 5-7 years in 2019"
    )
  
  combined_plot <- plot1 / plot2 / plot3
  return(combined_plot)
}

#-------------------------------------------------------
# Figures 2 & 3: partially pooled model
#-------------------------------------------------------

# Depression --------------------
results_partially_pooled_dep <- 
  tidy(model_partially_pooled_dep, conf.int = TRUE) %>%
  mutate(
    estimate = estimate * 100,
    conf.low = conf.low * 100,
    conf.high = conf.high * 100
  )

# Shared leads (lead4, lead3, lead2) – same for all cohorts
shared_leads_dep <- results_partially_pooled_dep %>%
  filter(term %in% leadvars) %>%
  mutate(
    time_type = "lead",
    time_num = case_when(
      term == "lead4" ~ 4,
      term == "lead3" ~ 3,
      term == "lead2" ~ 2
    ),
    event_time = -time_num # negative for leads
  )

# Cohort–specific lags
results_partially_pooled_dep <- results_partially_pooled_dep %>%
  filter(term %in% cohort_lags) %>%
  mutate(
    cohort = str_extract(term, "c\\d+to\\d+in19"),
    sex = str_extract(term, "(?<=in19)[fm]") %>%
      recode(m = "Males", f = "Females"),
    time_type = "lag",
    time_num = as.integer(str_extract(term, "\\d+$")),
    event_time = time_num # positive for lags
  ); results_partially_pooled_dep

# Expand shared leads so each cohort gets a copy
shared_leads_dep_expand <- shared_leads_dep %>%
  crossing(
    cohort = c("c11to13in19", "c8to10in19", "c5to7in19"), 
    sex = c("Males", "Females")
  ) # creates all combinations

# Final event-study dataset
results_partially_pooled_dep <- bind_rows(
  results_partially_pooled_dep,
  shared_leads_dep_expand
)

# Plot event study
fig_partially_pooled_dep <- 
  three_panel_plot(results_partially_pooled_dep, "Depression, %"); fig_partially_pooled_dep
ggsave("plots/fig2_partially_pooled_dep.png", fig_partially_pooled_dep, width = 8, height = 8)

# Anxiety --------------------
results_partially_pooled_anx <- 
  tidy(model_partially_pooled_anx, conf.int = TRUE) %>%
  mutate(
    estimate = estimate * 100,
    conf.low = conf.low * 100,
    conf.high = conf.high * 100
  )

# Shared leads (lead4, lead3, lead2) – same for all cohorts
shared_leads_anx <- results_partially_pooled_anx %>%
  filter(term %in% leadvars) %>%
  mutate(
    time_type = "lead",
    time_num = case_when(
      term == "lead4" ~ 4,
      term == "lead3" ~ 3,
      term == "lead2" ~ 2
    ),
    event_time = -time_num # negative for leads
  )

# Cohort–specific lags
results_partially_pooled_anx <- results_partially_pooled_anx %>%
  filter(term %in% cohort_lags) %>%
  mutate(
    cohort = str_extract(term, "c\\d+to\\d+in19"),
    sex = str_extract(term, "(?<=in19)[fm]") %>%
      recode(m = "Males", f = "Females"),
    time_type = "lag",
    time_num = as.integer(str_extract(term, "\\d+$")),
    event_time = time_num # positive for lags
  ); results_partially_pooled_anx

# Expand shared leads so each cohort gets a copy
shared_leads_anx_expand <- shared_leads_anx %>%
  crossing(
    cohort = c("c11to13in19", "c8to10in19", "c5to7in19"), 
    sex = c("Males", "Females")
  ) # creates all combinations

# Final event-study dataset
results_partially_pooled_anx <- bind_rows(
  results_partially_pooled_anx,
  shared_leads_anx_expand
)

# Plot event study
fig_partially_pooled_anx <- 
  three_panel_plot(results_partially_pooled_anx, "Anxiety, %"); fig_partially_pooled_anx
ggsave("plots/fig3_partially_pooled_anx.png", fig_partially_pooled_anx, width = 8, height = 8)

#-------------------------------------------------------
# eFigures 1 & 2: fully interacted model
#-------------------------------------------------------

# Depression --------------------
results_fully_interacted_dep <- 
  tidy(model_fully_interacted_dep, conf.int = TRUE) %>%
  mutate(
    estimate = estimate * 100,
    conf.low = conf.low * 100,
    conf.high = conf.high * 100
  )

# Filter lead and lag coefficients
results_fully_interacted_dep <- results_fully_interacted_dep %>%
  filter(str_detect(term, "_lead") | str_detect(term, "_lag")) %>%
  mutate(
    cohort = str_extract(term, "c\\d+to\\d+in19"),
    sex = str_extract(term, "(?<=in19)[fm]") %>%
      recode(m = "Males", f = "Females"),
    time_type = if_else(str_detect(term, "lead"), "lead", "lag"),
    time_num = as.integer(str_extract(term, "\\d+$")),
    event_time = if_else(time_type == "lead", -time_num, time_num)
  ); results_fully_interacted_dep

# Plot event study
fig_fully_interacted_dep <- 
  three_panel_plot(results_fully_interacted_dep, "Depression, %"); fig_fully_interacted_dep
ggsave("plots/efig1_fully_interacted_dep.png", fig_fully_interacted_dep, width = 8, height = 8)

# Anxiety --------------------
results_fully_interacted_anx <- 
  tidy(model_fully_interacted_anx, conf.int = TRUE) %>%
  mutate(
    estimate = estimate * 100,
    conf.low = conf.low * 100,
    conf.high = conf.high * 100
  )

# Filter lead and lag coefficients
results_fully_interacted_anx <- results_fully_interacted_anx %>%
  filter(str_detect(term, "_lead") | str_detect(term, "_lag")) %>%
  mutate(
    cohort = str_extract(term, "c\\d+to\\d+in19"),
    sex = str_extract(term, "(?<=in19)[fm]") %>%
      recode(m = "Males", f = "Females"),
    time_type = if_else(str_detect(term, "lead"), "lead", "lag"),
    time_num = as.integer(str_extract(term, "\\d+$")),
    event_time = if_else(time_type == "lead", -time_num, time_num)
  ); results_fully_interacted_anx

# Plot event study
fig_fully_interacted_anx <- 
  three_panel_plot(results_fully_interacted_anx, "Anxiety, %"); fig_fully_interacted_anx
ggsave("plots/efig_fully_interacted_anx.png", fig_fully_interacted_anx, width = 8, height = 8)
