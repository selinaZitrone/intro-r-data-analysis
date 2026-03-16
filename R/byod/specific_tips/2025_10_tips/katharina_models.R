# Workflow steps
# 1. Fit the model in NONMEM and get parameter estimates
# 2. Code the same model in mrgsolve/RxODE2 with those estimates
# 3. Simulate virtual patients with the same dosing/sampling as the study
# 4. Make plots for Visual predictive checks
# 5. Explore scenarios (dose optimization, special populations)

# The R packages:
# Two options:
# https://mrgsolve.org/
# https://nlmixr2.github.io/rxode2/

# Install all packages if not already installed
# install.packages("mrgsolve")
# install.packages("vpc")

library(tidyverse)
library(mrgsolve)
library(vpc)

# set a seed for reproducibility
set.seed(42)

# ADJUST: folder where to save the data and then read it
data_folder <- "R/byod/specific_tips/2025_10_tips/data"

# ----------------------------------------------------------------------------#
# Step 0: Create some dummy data ----------------------------------------------
# ----------------------------------------------------------------------------#

# In reality, this data comes from your patient data and NONMEM model.
# Here I create some dummy data and save it to the data folder. This data has to
# be replaced with your own data.
# To follow this script, you also need to run step 0 to create and save the dummy data.
# The actual workflow then starts at step 1.
# What you need for the workflow are the following things:
# - NONMEM estimates:
#   - Fixed effects (THETA)
#   - Between-subject variability OMEGA
#   - Residual error SIGMA
# - Patient data: Same data you used for the NONMEM model

params_theta <- data.frame(
  name = c("CL", "V", "KA", "PROP_ERR", "ADD_ERR"),
  value = c(12, 100, 1.2, 0.2, 0.5)
)

# Diagonal between-subject variances for CL, V, KA (20% CV ~ 0.04 on log-scale)
params_omega <- data.frame(
  row = c("CL", "V", "KA"),
  CL = c(0.04, 0.00, 0.00),
  V = c(0.00, 0.04, 0.00),
  KA = c(0.00, 0.00, 0.04)
)

params_sigma <- data.frame(
  row = "EPS1",
  EPS1 = 1
)

# for reproducibility
set.seed(123)
n_sub <- 60
ids <- 1:n_sub
WT <- round(rnorm(n_sub, mean = 75, sd = 12), 1)
CRCL <- round(rnorm(n_sub, mean = 85, sd = 20), 0)
DOSE <- sample(c(100, 200, 400), n_sub, replace = TRUE)

dose_recs <- data.frame(
  ID = ids,
  TIME = 0,
  AMT = DOSE,
  DV = NA,
  EVID = 1,
  MDV = 1,
  CMT = 1,
  WT = WT,
  CRCL = CRCL,
  DOSE = DOSE
)

obs_times <- c(0.5, 1, 2, 4, 8, 12, 24)
obs_recs <- do.call(
  rbind,
  lapply(ids, function(i) {
    data.frame(
      ID = i,
      TIME = obs_times,
      AMT = 0,
      DV = NA,
      EVID = 0,
      MDV = 1,
      CMT = 2,
      WT = WT[i],
      CRCL = CRCL[i],
      DOSE = DOSE[i]
    )
  })
)

study_data <- rbind(dose_recs, obs_recs)
study_data <- study_data[
  order(study_data$ID, study_data$TIME, -study_data$EVID),
]

# Save all files to the data folder
write_csv(
  params_theta,
  paste0(data_folder, "/dummy_theta.csv")
)
write_csv(
  params_omega,
  paste0(data_folder, "/dummy_omega.csv")
)
write_csv(
  params_sigma,
  paste0(data_folder, "/dummy_sigma.csv")
)
write_csv(
  study_data,
  paste0(data_folder, "/dummy_study_data.csv")
)

# ----------------------------------------------------------------------------#
# Step 1: Import NONMEM estimates and data ------------------------------------
# ----------------------------------------------------------------------------#

# What you need for the workflow are the following things:
# - NONMEM estimates:
#   - Fixed effects (THETA)
#   - Between-subject variability OMEGA
#   - Residual error SIGMA
# - Patient data: Same data you used for the NONMEM model

th <- read_csv(paste0(data_folder, "/dummy_theta.csv"))
omega_df <- read_csv(paste0(data_folder, "/dummy_omega.csv"))
sigma_df <- read_csv(paste0(data_folder, "/dummy_sigma.csv"))
study_df <- read_csv(paste0(data_folder, "/dummy_study_data.csv"))

# Look at the data to see how the structure should be
th
omega_df
sigma_df
study_df

# ---------------------------------------------------------------------------#
# Step 2: Code the same model in mrgsolve ------------------------------------
# ----------------------------------------------------------------------------#

# Check out mread() to read a model from a file
# But you can also define the model code as a character string

# First define the model code as a character string
code <- '
$PARAM CL = 12, V = 100, KA = 1.2, PROP_ERR = 0.2, ADD_ERR = 0.5
$CMT GUT CENT
$OMEGA
0.04
0.04
0.04
$SIGMA
1
$MAIN
  double CLi = CL * exp(ETA(1));
  double Vi  = V  * exp(ETA(2));
  double KAi = KA * exp(ETA(3));
$ODE
  dxdt_GUT  = -KAi * GUT;
  dxdt_CENT =  KAi * GUT - (CLi/Vi) * CENT;
$TABLE
  double CP = CENT / Vi;
  double DV = CP * (1 + PROP_ERR * EPS(1)) + ADD_ERR * EPS(1);
$CAPTURE CP DV
'

# Write, compile and load model code
mod <- mcode(
  model = "nm_to_mrg_pk", # model name
  code = code, # model code
  project = data_folder # where should the model be saved
)

# Check out the model
mod

# Replace defaults with NONMEM estimates from CSV

# Theta: Extract values from data frame and set the parameters
theta_vals <- setNames(th$value, th$name)
mod <- param(
  mod,
  CL = theta_vals[["CL"]],
  V = theta_vals[["V"]],
  KA = theta_vals[["KA"]],
  PROP_ERR = theta_vals[["PROP_ERR"]],
  ADD_ERR = theta_vals[["ADD_ERR"]]
)

# Omega:
# Convert omega tibble to matrix with row and column names
omega_mat <- as.matrix(omega_df[, -1])
rownames(omega_mat) <- omega_df$row
colnames(omega_mat) <- colnames(omega_df)[-1]

# Set omega in the model
mod <- omat(mod, omega_mat)

# In this case sigma is set to 1 in model code, so it is controlled via theta
# parameters prop_err and add_err.
# In case you have custom variances or multiple EPS, you need a sigma matrix and cou can
# set it with smat()

# ---------------------------------------------------------------------------#
# Step 3: Simulate predictions from our model --------------------------------
# ----------------------------------------------------------------------------#

# Same design as the NONMEM dataset

sim_out <- mod |>
  # use the dummy study data as template (columns need to match the NONMEM dataset)
  data_set(study_df) |>
  # Tell mrgsolve which columns to carry to the simulated output
  # by default, mrgsolve only carries what is in $CAPTURE in the model code
  carry_out(ID, TIME, AMT, EVID, CMT, WT, CRCL, DOSE) |>
  # run the simulation, check ?mrgsim for options
  mrgsim(recsort = 3, obsonly = TRUE)

# Ceck out the simulation output
sim_out

# Plot the results
plot(sim_out)

# Convert to tibble for easier handling
sim_out_tbl <- sim_out |>
  as_tibble()

# Do a ggplot of the simulated data (same as plot above but directly with ggplot)
# First, data is pivoted to long format for easier plotting with ggplot
sim_out_tbl |>
  dplyr::select(ID, TIME, CP, DV, GUT, CENT) |>
  pivot_longer(
    cols = c("CP", "DV", "GUT", "CENT"),
    names_to = "variable",
    values_to = "value"
  ) |>
  ggplot(aes(x = TIME, y = value, color = factor(ID), group = factor(ID))) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  theme(legend.position = "none")

# ---------------------------------------------------------------------------#
# Step 4: Make plots for Visual predictive checks ----------------------------
# ----------------------------------------------------------------------------#

# Simulate 500 results per subject to get prediction intervals

set.seed(1001)
n_reps <- 200

# Build a function to simulate a single replicate
sim_one <- function(rep_id) {
  mod |>
    data_set(study_df) |>
    mrgsim(recsort = 3, obsonly = TRUE) |>
    as_tibble()
}

# Predict n_reps times
sim_list <- lapply(seq_len(n_reps), sim_one)
# Bind all replicates to one big table
sim_data <- bind_rows(sim_list, .id = "rep")

# Create a pseudo observed dataset by taking the first replicate’s DV
# In reality this is your observed data e.g. study_data |> filter(EVID == 0)
# But in this dummy data scenario, DV is NA, so we take it from the first replicate
# observed data is the first replicate
obs_data <- sim_data |>
  filter(rep == 1L)
# simulated data is all data except the first replicate
sim_data <- sim_data |>
  filter(rep != 1L)

# Compute VPC using the vpc package
# Look at all plot options in ?vpc
vpc_plot <- vpc(
  obs = obs_data,
  sim = sim_data,
  show = list(
    obs_dv = TRUE # show observed data points
  ),
  xlab = "Time",
  ylab = "Concentration (DV)"
)
vpc_plot

# it's a ggplot so you can modify it with ggplot functions
# e.g.
vpc_plot +
  ggtitle("Visual Predictive Check") +
  theme_bw(paper = "cornsilk", ink = "navy")

# ---------------------------------------------------------------------------#
# Step 5: Explore scenarios --------------------------------------------------
# ----------------------------------------------------------------------------#

# Run the model with different dosing or patient characteristics

# Examples:
# - Double the dose (Scenario A)
# - Reduce creatinine clearance (CRCL) to mimic renal impairment (Scenario B)
# - Change body weight (WT)

# Scenario A: Double the dose for everyone
# Create a new dataset where the dose is doubled
study_double_dose <- study_df |>
  mutate(AMT = ifelse(EVID == 1, AMT * 2, AMT))

# Simulate data using the double dose
sim_double <- mod |>
  data_set(study_double_dose) |>
  # mrgsim_df is like mrgsim but returns a data frame directly
  mrgsim_df(recsort = 3, obsonly = TRUE)

# Scenario B: Moderate renal impairment (reduce CRCL by 50%)
# Create a new dataset with reduced CRCL
study_renal <- study_df |>
  mutate(CRCL = 0.5 * CRCL)

# Simulate data using the renal impairment dataset
sim_renal <- mod |>
  data_set(study_renal) |>
  mrgsim_df(recsort = 3, obsonly = TRUE)

# Scenario C: Low body weight (reduce WT by 25%)
# Create a new dataset with reduced WT
study_low_wt <- study_df |>
  mutate(WT = 0.75 * WT)

sim_lowwt <- mod |>
  data_set(study_low_wt) |>
  mrgsim_df(recsort = 3, obsonly = TRUE)

# Combine all scenarios for plotting
scen_all <- bind_rows(
  baseline = sim_out_tbl, # from step 3
  double_dose = sim_double,
  renal_impairment = sim_renal,
  low_weight = sim_lowwt,
  .id = "scenario" # add a scenario column
)

# Plot median and 90% PI across scenarios
scen_summ <- scen_all |>
  summarize(
    med = median(DV, na.rm = TRUE),
    p05 = quantile(DV, 0.05, na.rm = TRUE),
    p95 = quantile(DV, 0.95, na.rm = TRUE),
    # calculate separately by scenario and time
    .by = c(scenario, TIME)
  )

p_scen <- ggplot(scen_summ, aes(x = TIME, y = med, color = scenario)) +
  geom_line(linewidth = 1) +
  # add a ribbon for the 90% prediction interval
  geom_ribbon(
    aes(ymin = p05, ymax = p95, fill = scenario, color = NULL),
    alpha = 0.15
  ) +
  labs(
    title = "Scenario exploration: median and 90% PI",
    x = "Time",
    y = "Concentration (DV)"
  ) +
  theme_bw()
p_scen
