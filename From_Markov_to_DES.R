## ----load_packages, message = FALSE, warning = FALSE--------------------------

library(tidyverse)
library(readxl)
# library(future)
# library(future.apply)
library(scales)
library(truncnorm)
library(MASS)

library(bookdown)
library(knitr)
library(kableExtra)

# library(profvis)


## ----format_for_tables--------------------------------------------------------
format_tab_0 <- function(x) sprintf("%.0f", x)
format_tab_1 <- function(x) sprintf("%.1f", x)
format_tab_2 <- function(x) sprintf("%.2f", x)
format_tab_3 <- function(x) sprintf("%.3f", x)
format_tab_4 <- function(x) sprintf("%.4f", x)
format_tab_5 <- function(x) sprintf("%.5f", x)

format_tab_0GBP <- label_currency(accuracy = 1,
                                  prefix = "\u00a3") # UTF-8 for UK pound sign

format_tab_2GBP <- label_currency(accuracy = 0.01,
                                  prefix = "\u00a3") # UTF-8 for UK pound sign

format_tab_4sign <- function(x) {
  sign_mark <- case_when(x<0 ~ "-",
                         .default = "+")
  paste0(sign_mark, sprintf("%.4f", abs(x)))
}

## ----user_input---------------------------------------------------------------
# Age in year (continuous)
# Normal distribution
list_age <- list(mean = 60, sd = 0, dist = "normal")

# Scottish Index of Multiple Deprivation (continuous, range: 0 to 100)
# Beta distribution
list_SIMD <- list(mean = 60.8, sd = 15, min = 0, max = 100, dist = "beta")

# % of diabetes (proportion, range: from 0 to 1)
# Bernoulli distribution for each patient
list_Diabetes <- list(prop = 0, label = c("yes", "no"))

# % of family history (proportion, range: from 0 to 1)
# Bernoulli distribution for each patient
list_FH <- list(prop = 0, label = c("yes", "no"))

# No. of cigarette per day (continuous)
# Gamma distribution
list_CPD <- list(mean = 20, sd = 0, dist = "gamma")

# Systolic blood pressure (continuous)
# Normal distribution
list_SBP <- list(mean = 160, sd = 0, dist = "normal")

# Total cholesterol (continuous)
# Normal distribution
list_TC <- list(mean = 7, sd = 0, dist = "normal")

# High density cholesterol (continuous)
# Normal distribution
list_HDL <- list(mean = 1, sd = 0, dist = "normal")

# % of male (proportion, range: from 0 to 1)
# Bernoulli distribution for each patient
list_sex <- list(prop = 0.5, label = c("male", "female"))

tm <- 100 # maximum = 100
# time spline variables for cost models were reported up to t = 100 
# (Lawson 2016. Appendix. Table A7 A8 A9 A10 A11 A12)

disc <- 0.035 # disc <-0 for undiscounted

# treatment effects on ldl and hdl
list_tx <- list(ldleffect = 0.26, 
                hdleffect = 1.04, 
                cost = 13, 
                disu = 0.001)

## -----------------------------------------------------------------------------
sheet_names <- excel_sheets("Data/CVDparameters.xlsx")

list_coef <- lapply(seq_along(sheet_names), 
                    function(x) {
  dt <- read_excel("Data/CVDparameters.xlsx", sheet = x, col_names = TRUE)
})

names(list_coef) <- sheet_names

## -----------------------------------------------------------------------------
list_u_norms <- list(u_norms_m = tibble(age = c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", ">74"), 
                                        Index = c(0, seq(from = 25, to = 75, by = 10)), 
                                        utility = c(0.831, 0.823, 0.820, 0.806, 0.801, 0.788, 0.774)), 
                     u_norms_f = tibble(age = c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", ">74"), 
                                        Index = c(0, seq(from = 25, to = 75, by = 10)), 
                                        utility = c(0.809, 0.811, 0.802, 0.785, 0.787, 0.777, 0.721)))

list_coef <- append(list_coef, list_u_norms)

rm(list_u_norms)

## -----------------------------------------------------------------------------
calib <- list(f1 = 0.96,
              multi_m = 0.99,
              multi_f = 1.05)

# Constant adjustment
i <- which(list_coef$first_event_coef_m$covariate == "constant")
list_coef$first_event_coef_m[i, -1] <- list_coef$first_event_coef_m[i, -1] * 
  calib$multi_m
list_coef$first_event_coef_f[i, -1] <- list_coef$first_event_coef_f[i, -1] * 
  calib$multi_f

# coefficient adjustment for TC and HDL for nonCVDdeath first event for male
i <- which(list_coef$first_event_coef_m$covariate %in% c("TC", "HDL"))
# print(list_coef$first_event_coef_m)
list_coef$first_event_coef_m[i, "first_event_nonCVD_death"] <- 0
# print(list_coef$first_event_coef_m)

# coefficient adjustment for TC CBVD and nonCVDdeath first events for female
i <- which(list_coef$first_event_coef_m$covariate %in% "TC")
# print(list_coef$first_event_coef_f)
list_coef$first_event_coef_f[i, c("first_event_nonfatal_CBVD", "first_event_nonCVD_death")] <- 0
# print(list_coef$first_event_coef_f)



## -----------------------------------------------------------------------------
gen_continuous <- function(n_simind = 100000, list_cont) {
  dist <- list_cont$dist
  mean <- list_cont$mean
  sd <- list_cont$sd
  min <- list_cont$min
  max <- list_cont$max
  if (sd == 0) {
    return(rep(mean, n_simind))
  } else {
      if (dist == "normal") { # normal distribution
        if (is.null(list_cont$min) | is.null(list_cont$max)) {
          # not truncated
          return(rnorm(n_simind, mean, sd))
        } else {
          # truncated between min and max
          return(rtruncnorm(n_simind, a = min, b = max,
                            mean = mean, sd = sd))
        }
      
    } else if (dist == "gamma") { # gamma distribution
      alpha <- (mean ^ 2) / (sd ^ 2)
      beta <- (sd ^ 2) / mean
      return(rgamma(n_simind, shape = alpha, scale = beta))
    } else if (dist == "beta") { # beta distribution
      range_len <- max - min
      mu_unit <- (mean - min) / range_len
      sd_unit <- sd / range_len
      max_var <- mu_unit * (1 - mu_unit)
      if (sd_unit^2 >= max_var) {
        stop("Specified sd too large for provided mean and range.")
      }
      k <- mu_unit * (1 - mu_unit) / (sd_unit^2) - 1
      alpha <- mu_unit * k
      beta <- (1 - mu_unit) * k
      return(
        rbeta(n_simind, shape1 = alpha, shape2 = beta) * range_len + min
      )
    } 
  }
}

gen_proportion <- function(n_simind = 100000, list_prop) {
  prop <- list_prop$prop
  label_1 <- list_prop$label[1]
  label_0 <- list_prop$label[2]
  return(as.factor(ifelse(runif(n_simind) < prop, label_1, label_0)))
}

choose_coef_by_sex <- function(sex, coef, list = list_coef) {
  if (sex == "male") {
    as.matrix(list[[paste0(coef,"_m")]] %>% keep(is.numeric))
    } else if (sex == "female"){
    as.matrix(list[[paste0(coef,"_f")]] %>% keep(is.numeric))
    } else {
      stop("The sex argument should be either 'male' or 'female'.")
    }
}


## ----IDR----------------------------------------------------------------------
IDR <- log(1 + disc)

## ----disc_LY_pre--------------------------------------------------------------
disc_ly1_fn <- function(t, r = IDR) {
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    t
  }else{
    (1 - exp(-r * t)) / r
  }
}

## ----disc_LY_post-------------------------------------------------------------
disc_ly2_fn <- function(t1, t2, r = IDR) {
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    t2
  }else{
    ((exp(r * t2) - 1) * exp(- r * t1 - r * t2)) / r
  }
}


## -----------------------------------------------------------------------------
find_largest_of_continuous_sets <- function(vec) {
  result <- c()
  prev <- vec[1]

  for (i in 2:length(vec)) {
    if (vec[i] != prev + 1) {result <- c(result, prev)}
    prev <- vec[i]
  }
  result <- c(result, prev)
  return(result)
}

## ----reconstruct_non_linear, warning=FALSE------------------------------------
# Three knots divide the transformed time into four pieces, which should typically be two cubic curves surrounded by two straight lines. 

recons_nl_RCS <- function(sex = "male", item, tol.small = 0.001) {
  transt_pre <- choose_coef_by_sex(sex, "coef_c2") %>% 
    as_tibble() %>%
    dplyr::select(C_ti1, all_of(item)) %>%
    rename(ti2 = all_of(item)) %>%
    mutate(C_ti1 = as.integer(C_ti1),
           d_1 = lead(ti2) - ti2,
           d_2 = lead(d_1) - d_1,
           d_3 = lead(d_2) - d_2,
           d_4 = lead(d_3) - d_3) 
  
  search_turning <- transt_pre %>%
    filter(abs(d_4) > tol.small & C_ti1 > 1)
  
  # position of knots
  knots_t <- rep(NA, 3)
  
  knots_t[2:3] <- find_largest_of_continuous_sets(search_turning$C_ti1) + 1
  
  knots_t[1] <- max(filter(transt_pre, ti2 == 0)$C_ti1)

  # 1st piece: 1st straight line
  line_1 <- transt_pre %>%
    slice_head(n = knots_t[1])

  # 2nd piece: 1st cubic curve
  cubic_1 <- transt_pre %>%
    slice(knots_t[1]:knots_t[2]) %>%
    mutate(x2 = C_ti1 ^ 2,
           x3 = C_ti1 ^ 3)

  cubic_model_1 <- lm(ti2 ~ C_ti1 + x2 + x3, data = cubic_1)

  cubic_parms_1 <- cubic_model_1$coefficients

  cubic_1_r2 <- summary(cubic_model_1)$r.squared
  
  # 3rd piece: 2nd cubic curve
  cubic_2 <- transt_pre %>%
  slice(knots_t[2]:knots_t[3]) %>%
  mutate(x2 = C_ti1 ^ 2,
         x3 = C_ti1 ^ 3)

  cubic_model_2 <- lm(ti2 ~ C_ti1 + x2 + x3, data = cubic_2)

  cubic_parms_2 <- cubic_model_2$coefficients

  cubic_2_r2 <- summary(cubic_model_2)$r.squared

  # 4th piece: 2nd straight line
  line_2 <- transt_pre %>%
    slice(knots_t[3]:(knots_t[3]+10))

  line_model_2 <- lm(ti2 ~ C_ti1, data = line_2)

  line_parms_2 <- line_model_2$coefficients

  line_2_r2 <- summary(line_model_2)$r.squared
  
  output <- list(knots = knots_t,
                 coef_a_1j = cubic_parms_1,
                 coef_a_2j = cubic_parms_2,
                 coef_a_3j = line_parms_2,
                 r2_check = c(cubic_1_r2, cubic_2_r2, line_2_r2))
  
  return(output)
  
}

nl_RCS_pre <- lapply(c("male", "female"),
                     recons_nl_RCS,
                     item = "C_death_ti2")

names(nl_RCS_pre) <- c("male", "female")

print(paste0("Check the R-squared for ", 
             c("the 1st cubic model", "the 2nd cubic model", "the 2nd line model"), 
             " is 1: male, ", 
             nl_RCS_pre$male$r2_check, 
             "; female, ", 
             nl_RCS_pre$female$r2_check))

nl_RCS_postCHD <- lapply(c("male", "female"),
                         recons_nl_RCS,
                         item = "C_postCHD_ti2")

names(nl_RCS_postCHD) <- c("male", "female")

print(paste0("Check the R-squared for ", 
             c("the 1st cubic model", "the 2nd cubic model", "the 2nd line model"), 
             " is 1: male, ", 
             nl_RCS_postCHD$male$r2_check, 
             "; female, ", 
             nl_RCS_postCHD$female$r2_check))

nl_RCS_postCBVD <- lapply(c("male", "female"),
                          recons_nl_RCS,
                          item = "C_postCBVD_ti2")

names(nl_RCS_postCBVD) <- c("male", "female")

print(paste0("Check the R-squared for ", 
             c("the 1st cubic model", "the 2nd cubic model", "the 2nd line model"), 
             " is 1: male, ", 
             nl_RCS_postCBVD$male$r2_check, 
             "; female, ", 
             nl_RCS_postCBVD$female$r2_check))


## ----disc_cost_pre_l----------------------------------------------------------
disc_cost1_fn <- function(C_0, b_1, t, r = IDR) {
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    t * (t * b_1 + 2 * C_0) / 2
  }else{
    (b_1 + C_0 * r) / r^2 - (exp(-r * t) * ((r * t + 1) * b_1 + C_0 * r)) / r^2
  }
}

## -----------------------------------------------------------------------------
Int1_FN <- function(K, T_1, A_3, A_2, A_1, A_0, b_2, r = IDR) {
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    ((3 * A_3 * T_1^4 + 4 * A_2 * T_1^3 + 6 * A_1 * T_1^2 + 12 * A_0 * T_1) / 12 - (3 * A_3 * K^4 + 4 * A_2 * K^3 + 6 * A_1 * K^2 + 12 * A_0 * K) / 12) * b_2
  }else{
    ((((A_3 * K^3 + A_2 * K^2 + A_1 * K + A_0) * r^3 + (3 * A_3 * K^2 + 2 * A_2 * K + A_1) * r^2 + (6 * A_3 * K + 2 * A_2) * r + 6 * A_3) * exp(-K * r) - ((A_3 * T_1^3 + A_2 * T_1^2 + A_1 * T_1 + A_0) * r^3 + (3 * A_3 * T_1^2 + 2 * A_2 * T_1 + A_1) * r^2 + (6 * A_3 * T_1 + 2 * A_2) * r + 6 * A_3) * exp(-T_1 * r)) * b_2) / r^4
  }
}


## ----disc_cost_pre_nl---------------------------------------------------------
disc_cost1_nl_fn <- function(v_knots, 
                             v_a_1j, v_a_2j, v_a_3j,
                             b_2, t, r = IDR) {
  
  k_1 <- v_knots[1]
  k_2 <- v_knots[2]
  k_3 <- v_knots[3]
  
  a_13 <- v_a_1j[4]
  a_12 <- v_a_1j[3]
  a_11 <- v_a_1j[2]
  a_10 <- v_a_1j[1]
  
  a_23 <- v_a_2j[4]
  a_22 <- v_a_2j[3]
  a_21 <- v_a_2j[2]
  a_20 <- v_a_2j[1]
  
  a_31 <- v_a_3j[2]
  a_30 <- v_a_3j[1]

  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    output <- 0 + # t < k_1
      Int1_FN(k_1, t, a_13, a_12, a_11, a_10, b_2, r) * (t >= k_1) * (t < k_2) + # k_1 <= t < k_2
      Int1_FN(k_1, k_2, a_13, a_12, a_11, a_10, b_2, r) * (t >= k_2) + # t >= k_2
      Int1_FN(k_2, t, a_23, a_22, a_21, a_20, b_2, r) * (t >= k_2)  * (t < k_3) + # k_2 <= t < k_3
      Int1_FN(k_2, k_3, a_23, a_22, a_21, a_20, b_2, r) * (t >= k_3) + # k_2 <= t < k_3
      (((t - k_3) * ((t + k_3) * a_31 + 2 * a_30) * b_2) / 2) * (t >= k_3) # t >= k_3
  }else{
    output <- 0 + # t < k_1
      Int1_FN(k_1, t, a_13, a_12, a_11, a_10, b_2, r) * (t >= k_1) * (t < k_2) + # k_1 <= t < k_2
      Int1_FN(k_1, k_2, a_13, a_12, a_11, a_10, b_2, r) * (t >= k_2) + # t >= k_2
      Int1_FN(k_2, t, a_23, a_22, a_21, a_20, b_2, r) * (t >= k_2)  * (t < k_3) + # k_2 <= t < k_3
      Int1_FN(k_2, k_3, a_23, a_22, a_21, a_20, b_2, r) * (t >= k_3) + # k_2 <= t < k_3
      (((((a_31 * k_3 + a_30) * r + a_31) * exp(-k_3 * r) - (a_31 * r * t + a_30 * r + a_31) * exp(-r * t)) * b_2) / r^2) * (t >= k_3) # t >= k_3
  }
  
  return(output)
}

## ----disc_fn------------------------------------------------------------------


disc_cost2_fn <- function(C_0, b_3, t1, t2, r = IDR) {
  # t1 < t2
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    (t2 * (t2 * b_3 + 2 * C_0)) / 2
  }else{
    (exp(-r * t1) * (b_3 + C_0 * r)) / r^2 - (exp(-r * t2 - r * t1) * ((r * t2 + 1) * b_3 + C_0 * r)) / r^2
  }
}



## -----------------------------------------------------------------------------
Int2_FN <- function(T_1, T_2, A_3, A_2, A_1, A_0, K, b_4, r = IDR) {
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    ((3 * A_3 * T_2^4 + 4 * A_2 * T_2^3 + 6 * A_1 * T_2^2 + 12 * A_0 * T_2) / 12 - (3 * A_3 * K^4 + 4 * A_2 * K^3 + 6 * A_1 * K^2 + 12 * A_0 * K) / 12) * b_4
  }else{
    (((1 + r)^(-T_1-K) * ((A_3 * K^3 + A_2 * K^2 + A_1 * K + A_0) * log(1 + r)^3 + (3 * A_3 * K^2 + 2 * A_2 * K + A_1) * log(1 + r)^2 + (6 * A_3 * K + 2 * A_2) * log(1 + r) + 6 * A_3) - (1 + r)^(-T_1-T_2) * ((A_3 * T_2^3 + A_2 * T_2^2 + A_1 * T_2 + A_0) * log(1 + r)^3 + (3 * A_3 * T_2^2 + 2 * A_2 * T_2 + A_1) * log(1 + r)^2 + (6 * A_3 * T_2 + 2 * A_2) * log(1 + r) + 6 * A_3)) * b_4) / log(1 + r)^4
  }
}


## ----disc_cost_post_nl--------------------------------------------------------

disc_cost2_nl_fn <- function(v_knots, 
                             v_a_1j, v_a_2j, v_a_3j,
                             b_4, t_1, t_2, r = IDR) {
  
  k_1 <- v_knots[1]
  k_2 <- v_knots[2]
  k_3 <- v_knots[3]
  
  a_13 <- v_a_1j[4]
  a_12 <- v_a_1j[3]
  a_11 <- v_a_1j[2]
  a_10 <- v_a_1j[1]
  
  a_23 <- v_a_2j[4]
  a_22 <- v_a_2j[3]
  a_21 <- v_a_2j[2]
  a_20 <- v_a_2j[1]
  
  a_31 <- v_a_3j[2]
  a_30 <- v_a_3j[1]

  # t_2 >= k_3
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    output <- 0 + # t_2 < k_1
      Int2_FN(t_1, t_2, a_13, a_12, a_11, a_10, k_1, b_4, r) * (t_2 >= k_1) * (t_2 < k_2) + # k_1 <= t_2 < k_2
      Int2_FN(t_1, k_2, a_13, a_12, a_11, a_10, k_1, b_4, r) * (t_2 >= k_2) + # t_2 >= k_2
      Int2_FN(t_1, t_2, a_23, a_22, a_21, a_20, k_2, b_4, r) * (t_2 >= k_2)  * (t_2 < k_3) + # k_2 <= t_2 < k_3
      Int2_FN(t_1, k_3, a_23, a_22, a_21, a_20, k_2, b_4, r) * (t_2 >= k_3) + # k_2 <= t_2 < k_3
      (((t_2 - k_3) * ((t_2 + k_3) * a_31 + 2 * a_30) * b_4) / 2) * (t_2 >= k_3) # t_2 >= k_3
  }else{
    output <- 0 + # t_2 < k_1
      Int2_FN(t_1, t_2, a_13, a_12, a_11, a_10, k_1, b_4, r) * (t_2 >= k_1) * (t_2 < k_2) + # k_1 <= t_2 < k_2
      Int2_FN(t_1, k_2, a_13, a_12, a_11, a_10, k_1, b_4, r) * (t_2 >= k_2) + # t_2 >= k_2
      Int2_FN(t_1, t_2, a_23, a_22, a_21, a_20, k_2, b_4, r) * (t_2 >= k_2)  * (t_2 < k_3) + # k_2 <= t_2 < k_3
      Int2_FN(t_1, k_3, a_23, a_22, a_21, a_20, k_2, b_4, r) * (t_2 >= k_3) + # k_2 <= t_2 < k_3
      (b_4 * (((a_31 * k_3 + a_30) * r + a_31) * exp(-r * t_1 - k_3 * r) - (a_31 * r * t_2 + a_30 * r + a_31) * exp(-r * t_2 - r * t_1))) / r^2 * (t_2 >= k_3) # t_2 >= k_3
  }
  
  return(output)
}



## -----------------------------------------------------------------------------
Int3_FN <- function(K, T_1, U, r = IDR) {
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    U * (T_1 - K)
  }else{
    U / r * (exp(-K * r) - exp(-T_1 * r))
  }
}

## ----disc_qaly_pre------------------------------------------------------------
disc_qaly1_fn <- function(age, u_norms, t, r = IDR) {
  
  u_1 <- u_norms[1]
  u_2 <- u_norms[2]
  u_3 <- u_norms[3]
  u_4 <- u_norms[4]
  u_5 <- u_norms[5]
  u_6 <- u_norms[6]
  u_7 <- u_norms[7]

  output <- (age < 25) * (
      Int3_FN(0, t, u_1, r) * (t + age < 25) +
      Int3_FN(0, 25 - age, u_1, r) * (t + age >= 25) + 
      Int3_FN(25 - age, t, u_2, r) * (t + age >= 25) * (t + age < 35) + 
      Int3_FN(25 - age, 35 - age, u_2, r) * (t + age >= 35) + 
      Int3_FN(35 - age, t, u_3, r) * (t + age >= 35) * (t + age < 45) + 
      Int3_FN(35 - age, 45 - age, u_3, r) * (t + age >= 45) + 
      Int3_FN(45 - age, t, u_4, r) * (t + age >= 45) * (t + age < 55) + 
      Int3_FN(45 - age, 55 - age, u_4, r) * (t + age >= 55) + 
      Int3_FN(55 - age, t, u_5, r) * (t + age >= 55) * (t + age < 65) + 
      Int3_FN(55 - age, 65 - age, u_5, r) * (t + age >= 65) + 
      Int3_FN(65 - age, t, u_6, r) * (t + age >= 65) * (t + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t + age >= 75) + 
      Int3_FN(75 - age, t, u_7, r) * (t + age >= 75)
    ) + (age >= 25) * (age < 35) * (
      Int3_FN(0, t, u_2, r) * (t + age < 35) +
      Int3_FN(0, 35 - age, u_3, r) * (t + age >= 35) +
      Int3_FN(35 - age, t, u_3, r) * (t + age >= 35) * (t + age < 45) + 
      Int3_FN(35 - age, 45 - age, u_3, r) * (t + age >= 45) + 
      Int3_FN(45 - age, t, u_4, r) * (t + age >= 45) * (t + age < 55) + 
      Int3_FN(45 - age, 55 - age, u_4, r) * (t + age >= 55) + 
      Int3_FN(55 - age, t, u_5, r) * (t + age >= 55) * (t + age < 65) + 
      Int3_FN(55 - age, 65 - age, u_5, r) * (t + age >= 65) + 
      Int3_FN(65 - age, t, u_6, r) * (t + age >= 65) * (t + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t + age >= 75) + 
      Int3_FN(75 - age, t, u_7, r) * (t + age >= 75)
    ) + (age >= 35) * (age < 45) * (
      Int3_FN(0, t, u_3, r) * (t + age < 45) +
      Int3_FN(0, 45 - age, u_4, r) * (t + age >= 45) +
      Int3_FN(45 - age, t, u_4, r) * (t + age >= 45) * (t + age < 55) + 
      Int3_FN(45 - age, 55 - age, u_4, r) * (t + age >= 55) + 
      Int3_FN(55 - age, t, u_5, r) * (t + age >= 55) * (t + age < 65) + 
      Int3_FN(55 - age, 65 - age, u_5, r) * (t + age >= 65) + 
      Int3_FN(65 - age, t, u_6, r) * (t + age >= 65) * (t + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t + age >= 75) + 
      Int3_FN(75 - age, t, u_7, r) * (t + age >= 75)
    ) + (age >= 45) * (age < 55) * (
      Int3_FN(0, t, u_4, r) * (t + age < 55) +
      Int3_FN(0, 55 - age, u_5, r) * (t + age >= 55) +
      Int3_FN(55 - age, t, u_5, r) * (t + age >= 55) * (t + age < 65) + 
      Int3_FN(55 - age, 65 - age, u_5, r) * (t + age >= 65) + 
      Int3_FN(65 - age, t, u_6, r) * (t + age >= 65) * (t + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t + age >= 75) + 
      Int3_FN(75 - age, t, u_7, r) * (t + age >= 75)
    ) + (age >= 55) * (age < 65) * (
      Int3_FN(0, t, u_5, r) * (t + age < 65) +
      Int3_FN(0, 65 - age, u_6, r) * (t + age >= 65) +
      Int3_FN(65 - age, t, u_6, r) * (t + age >= 65) * (t + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t + age >= 75) + 
      Int3_FN(75 - age, t, u_7, r) * (t + age >= 75)
    ) + (age >= 65) * (age < 75) * (
      Int3_FN(0, t, u_6, r) * (t + age < 75) +
      Int3_FN(0, 75 - age, u_7, r) * (t + age >= 75) +
      Int3_FN(75 - age, t, u_7, r) * (t + age >= 75)
    ) + (age >= 75) * (
      Int3_FN(0, t, u_7, r)
    )
  
  return(output)
}

## -----------------------------------------------------------------------------
Int4_FN <- function(T_1, T_2, DU, r = IDR) {
  if (r < 0.000001) { # to avoid the possible numerical instability in case r is too small
    DU * T_2
  }else{
    (DU * (exp(r * T_2) - 1) * exp(-r * (T_2 + T_1))) / r
  }
}


disc_qaly21_fn <- function(age, u_norms, t_1, t_2, du_1e, r = IDR) {
  
  u_1 <- u_norms[1]
  u_2 <- u_norms[2]
  u_3 <- u_norms[3]
  u_4 <- u_norms[4]
  u_5 <- u_norms[5]
  u_6 <- u_norms[6]
  u_7 <- u_norms[7]

  output <- (age < 25) * (
      Int3_FN(0, t_1 + t_2, u_1, r) * (t_1 + t_2 + age < 25) +
      Int3_FN(0, 25 - age, u_1, r) * (t_1 + t_2 + age >= 25) + 
      Int3_FN(25 - age, t_1 + t_2, u_2, r) * (t_1 + t_2 + age >= 25) * (t_1 + t_2 + age < 35) + 
      Int3_FN(25 - age, 35 - age, u_2, r) * (t_1 + t_2 + age >= 35) + 
      Int3_FN(35 - age, t_1 + t_2, u_3, r) * (t_1 + t_2 + age >= 35) * (t_1 + t_2 + age < 45) + 
      Int3_FN(35 - age, 45 - age, u_3, r) * (t_1 + t_2 + age >= 45) + 
      Int3_FN(45 - age, t_1 + t_2, u_4, r) * (t_1 + t_2 + age >= 45) * (t_1 + t_2 + age < 55) + 
      Int3_FN(45 - age, 55 - age, u_4, r) * (t_1 + t_2 + age >= 55) + 
      Int3_FN(55 - age, t_1 + t_2, u_5, r) * (t_1 + t_2 + age >= 55) * (t_1 + t_2 + age < 65) + 
      Int3_FN(55 - age, 65 - age, u_5, r) * (t_1 + t_2 + age >= 65) + 
      Int3_FN(65 - age, t_1 + t_2, u_6, r) * (t_1 + t_2 + age >= 65) * (t_1 + t_2 + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t_1 + t_2 + age >= 75) + 
      Int3_FN(75 - age, t_1 + t_2, u_7, r) * (t_1 + t_2 + age >= 75)
    ) + (age >= 25) * (age < 35) * (
      Int3_FN(0, t_1 + t_2, u_2, r) * (t_1 + t_2 + age < 35) +
      Int3_FN(0, 35 - age, u_3, r) * (t_1 + t_2 + age >= 35) +
      Int3_FN(35 - age, t_1 + t_2, u_3, r) * (t_1 + t_2 + age >= 35) * (t_1 + t_2 + age < 45) + 
      Int3_FN(35 - age, 45 - age, u_3, r) * (t_1 + t_2 + age >= 45) + 
      Int3_FN(45 - age, t_1 + t_2, u_4, r) * (t_1 + t_2 + age >= 45) * (t_1 + t_2 + age < 55) + 
      Int3_FN(45 - age, 55 - age, u_4, r) * (t_1 + t_2 + age >= 55) + 
      Int3_FN(55 - age, t_1 + t_2, u_5, r) * (t_1 + t_2 + age >= 55) * (t_1 + t_2 + age < 65) + 
      Int3_FN(55 - age, 65 - age, u_5, r) * (t_1 + t_2 + age >= 65) + 
      Int3_FN(65 - age, t_1 + t_2, u_6, r) * (t_1 + t_2 + age >= 65) * (t_1 + t_2 + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t_1 + t_2 + age >= 75) + 
      Int3_FN(75 - age, t_1 + t_2, u_7, r) * (t_1 + t_2 + age >= 75)
    ) + (age >= 35) * (age < 45) * (
      Int3_FN(0, t_1 + t_2, u_3, r) * (t_1 + t_2 + age < 45) +
      Int3_FN(0, 45 - age, u_4, r) * (t_1 + t_2 + age >= 45) +
      Int3_FN(45 - age, t_1 + t_2, u_4, r) * (t_1 + t_2 + age >= 45) * (t_1 + t_2 + age < 55) + 
      Int3_FN(45 - age, 55 - age, u_4, r) * (t_1 + t_2 + age >= 55) + 
      Int3_FN(55 - age, t_1 + t_2, u_5, r) * (t_1 + t_2 + age >= 55) * (t_1 + t_2 + age < 65) + 
      Int3_FN(55 - age, 65 - age, u_5, r) * (t_1 + t_2 + age >= 65) + 
      Int3_FN(65 - age, t_1 + t_2, u_6, r) * (t_1 + t_2 + age >= 65) * (t_1 + t_2 + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t_1 + t_2 + age >= 75) + 
      Int3_FN(75 - age, t_1 + t_2, u_7, r) * (t_1 + t_2 + age >= 75)
    ) + (age >= 45) * (age < 55) * (
      Int3_FN(0, t_1 + t_2, u_4, r) * (t_1 + t_2 + age < 55) +
      Int3_FN(0, 55 - age, u_5, r) * (t_1 + t_2 + age >= 55) +
      Int3_FN(55 - age, t_1 + t_2, u_5, r) * (t_1 + t_2 + age >= 55) * (t_1 + t_2 + age < 65) + 
      Int3_FN(55 - age, 65 - age, u_5, r) * (t_1 + t_2 + age >= 65) + 
      Int3_FN(65 - age, t_1 + t_2, u_6, r) * (t_1 + t_2 + age >= 65) * (t_1 + t_2 + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t_1 + t_2 + age >= 75) + 
      Int3_FN(75 - age, t_1 + t_2, u_7, r) * (t_1 + t_2 + age >= 75)
    ) + (age >= 55) * (age < 65) * (
      Int3_FN(0, t_1 + t_2, u_5, r) * (t_1 + t_2 + age < 65) +
      Int3_FN(0, 65 - age, u_6, r) * (t_1 + t_2 + age >= 65) +
      Int3_FN(65 - age, t_1 + t_2, u_6, r) * (t_1 + t_2 + age >= 65) * (t_1 + t_2 + age < 75) + 
      Int3_FN(65 - age, 75 - age, u_6, r) * (t_1 + t_2 + age >= 75) + 
      Int3_FN(75 - age, t_1 + t_2, u_7, r) * (t_1 + t_2 + age >= 75)
    ) + (age >= 65) * (age < 75) * (
      Int3_FN(0, t_1 + t_2, u_6, r) * (t_1 + t_2 + age < 75) +
      Int3_FN(0, 75 - age, u_7, r) * (t_1 + t_2 + age >= 75) +
      Int3_FN(75 - age, t_1 + t_2, u_7, r) * (t_1 + t_2 + age >= 75)
    ) + (age >= 75) * (
      Int3_FN(0, t_1 + t_2, u_7, r)
    ) + Int4_FN(t_1, t_2, du_1e, r)

  
  return(output)
}

## -----------------------------------------------------------------------------
disc_disu2_fun <- function(t_2, t_1, DU_all, 
                           DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                           b_5_1, b_5_2, b_5_3, b_5_4, b_5_5, 
                           b_6_1, b_6_2, b_6_3, b_6_4, b_6_5, 
                           A_3, A_2, A_1, A_0, r = IDR) {
  (DU_all[1] * pnorm(DU_0_1 + b_5_1 * t_2 + b_6_1 * (A_3 * t_2^3 + A_2 * t_2^2 + A_1 * t_2 + A_0)) +
     DU_all[2] * pnorm(DU_0_2 + b_5_2 * t_2 + b_6_2 * (A_3 * t_2^3 + A_2 * t_2^2 + A_1 * t_2 + A_0)) +
     DU_all[3] * pnorm(DU_0_3 + b_5_3 * t_2 + b_6_3 * (A_3 * t_2^3 + A_2 * t_2^2 + A_1 * t_2 + A_0)) +
     DU_all[4] * pnorm(DU_0_4 + b_5_4 * t_2 + b_6_4 * (A_3 * t_2^3 + A_2 * t_2^2 + A_1 * t_2 + A_0)) +
     DU_all[5] * pnorm(DU_0_5 + b_5_5 * t_2 + b_6_5 * (A_3 * t_2^3 + A_2 * t_2^2 + A_1 * t_2 + A_0))) * 
    exp(- r * (t_1 + t_2))
}

## -----------------------------------------------------------------------------
disc_disQ2_1p_fn <- function(t_1, t_2, DU_all, 
                             DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                             b_5_1, b_5_2, b_5_3, b_5_4, b_5_5, 
                             b_6_1, b_6_2, b_6_3, b_6_4, b_6_5,
                             v_knots, v_a_1j, v_a_2j, v_a_3j, r = IDR) {
  # t_2 < k_1
  
  v_inputs <- list(t_1 = t_1, t_2 = t_2, 
                   DU_0_1 = DU_0_1, 
                   DU_0_2 = DU_0_2, 
                   DU_0_3 = DU_0_3, 
                   DU_0_4 = DU_0_4, 
                   DU_0_5 = DU_0_5, 
                   b_5_1 = b_5_1,
                   b_5_2 = b_5_2,
                   b_5_3 = b_5_3,
                   b_5_4 = b_5_4,
                   b_5_5 = b_5_5,
                   b_6_1 = b_6_1,
                   b_6_2 = b_6_2,
                   b_6_3 = b_6_3,
                   b_6_4 = b_6_4,
                   b_6_5 = b_6_5)
  
  pmap_dbl(v_inputs, function(t_1, t_2, 
                              DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                              b_5_1, b_5_2, b_5_3, b_5_4, b_5_5, 
                              b_6_1, b_6_2, b_6_3, b_6_4, b_6_5) {
    output <- 
      stats::integrate(disc_disu2_fun, 
                       lower = 0, upper = t_2, 
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,                     
                       DU_0_2 = DU_0_2,                     
                       DU_0_3 = DU_0_3,                     
                       DU_0_4 = DU_0_4,                     
                       DU_0_5 = DU_0_5, 
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = 0, A_2 = 0, A_1 = 0, A_0 = 0, 
                       r = r)$value # from 0 to t_2
  
    return(output)
  })
}

disc_disQ2_2p_fn <- function(t_1, t_2, DU_all, 
                             DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                             b_5_1, b_5_2, b_5_3, b_5_4, b_5_5,
                             b_6_1, b_6_2, b_6_3, b_6_4, b_6_5,
                             v_knots, v_a_1j, v_a_2j, v_a_3j, r = IDR) {
  # t_2 < k_2
  
  k_1 <- v_knots[1]
  
  a_13 <- v_a_1j[4]
  a_12 <- v_a_1j[3]
  a_11 <- v_a_1j[2]
  a_10 <- v_a_1j[1]
  
  v_inputs <- list(t_1 = t_1, t_2 = t_2, 
                   DU_0_1 = DU_0_1, 
                   DU_0_2 = DU_0_2, 
                   DU_0_3 = DU_0_3, 
                   DU_0_4 = DU_0_4, 
                   DU_0_5 = DU_0_5, 
                   b_5_1 = b_5_1,
                   b_5_2 = b_5_2,
                   b_5_3 = b_5_3,
                   b_5_4 = b_5_4,
                   b_5_5 = b_5_5,
                   b_6_1 = b_6_1,
                   b_6_2 = b_6_2,
                   b_6_3 = b_6_3,
                   b_6_4 = b_6_4,
                   b_6_5 = b_6_5)
  
  pmap_dbl(v_inputs, function(t_1, t_2, 
                              DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                              b_5_1, b_5_2, b_5_3, b_5_4, b_5_5, 
                              b_6_1, b_6_2, b_6_3, b_6_4, b_6_5) {
    output <- 
      stats::integrate(disc_disu2_fun, 
                       lower = 0, upper = k_1, 
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,                   
                       DU_0_2 = DU_0_2,      
                       DU_0_3 = DU_0_3,          
                       DU_0_4 = DU_0_4,                 
                       DU_0_5 = DU_0_5, 
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = 0, A_2 = 0, A_1 = 0, A_0 = 0, 
                       r = r)$value + # from 0 to k_1
      stats::integrate(disc_disu2_fun, 
                       lower = k_1, upper = t_2, 
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,          
                       DU_0_2 = DU_0_2,            
                       DU_0_3 = DU_0_3,              
                       DU_0_4 = DU_0_4,
                       DU_0_5 = DU_0_5, 
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = a_13, A_2 = a_12, A_1 = a_11, A_0 = a_10, 
                       r = r)$value # from k_1 to t_2
  
    return(output)
  })
}

disc_disQ2_3p_fn <- function(t_1, t_2, DU_all, 
                             DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                             b_5_1, b_5_2, b_5_3, b_5_4, b_5_5,
                             b_6_1, b_6_2, b_6_3, b_6_4, b_6_5,
                             v_knots, v_a_1j, v_a_2j, v_a_3j, r = IDR) {
  # t_2 < k_3
  
  k_1 <- v_knots[1]
  k_2 <- v_knots[2]
  
  a_13 <- v_a_1j[4]
  a_12 <- v_a_1j[3]
  a_11 <- v_a_1j[2]
  a_10 <- v_a_1j[1]
  
  a_23 <- v_a_2j[4]
  a_22 <- v_a_2j[3]
  a_21 <- v_a_2j[2]
  a_20 <- v_a_2j[1]
  
  v_inputs <- list(t_1 = t_1, t_2 = t_2, 
                   DU_0_1 = DU_0_1, 
                   DU_0_2 = DU_0_2, 
                   DU_0_3 = DU_0_3, 
                   DU_0_4 = DU_0_4, 
                   DU_0_5 = DU_0_5, 
                   b_5_1 = b_5_1,
                   b_5_2 = b_5_2,
                   b_5_3 = b_5_3, 
                   b_5_4 = b_5_4,
                   b_5_5 = b_5_5,
                   b_6_1 = b_6_1,
                   b_6_2 = b_6_2,
                   b_6_3 = b_6_3,
                   b_6_4 = b_6_4,
                   b_6_5 = b_6_5)
  
  pmap_dbl(v_inputs, function(t_1, t_2, 
                              DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                              b_5_1, b_5_2, b_5_3, b_5_4, b_5_5,
                              b_6_1, b_6_2, b_6_3, b_6_4, b_6_5) {
    output <- 
      stats::integrate(disc_disu2_fun, 
                       lower = 0, upper = k_1, 
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,                     
                       DU_0_2 = DU_0_2,                     
                       DU_0_3 = DU_0_3,                     
                       DU_0_4 = DU_0_4,                     
                       DU_0_5 = DU_0_5, 
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = 0, A_2 = 0, A_1 = 0, A_0 = 0, 
                       r = r)$value + # from 0 to k_1
      stats::integrate(disc_disu2_fun, 
                       lower = k_1, upper = k_2, 
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,
                       DU_0_2 = DU_0_2,
                       DU_0_3 = DU_0_3,
                       DU_0_4 = DU_0_4,
                       DU_0_5 = DU_0_5,
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = a_13, A_2 = a_12, A_1 = a_11, A_0 = a_10,
                       r = r)$value + # from k_1 to k_2
      stats::integrate(disc_disu2_fun, 
                       lower = k_2, upper = t_2,  
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,
                       DU_0_2 = DU_0_2,
                       DU_0_3 = DU_0_3,
                       DU_0_4 = DU_0_4,
                       DU_0_5 = DU_0_5,
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5, 
                       b_6_1 = b_6_1, 
                       b_6_2 = b_6_2, 
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = a_23, A_2 = a_22, A_1 = a_21, A_0 = a_20,
                       r = r)$value # from k_2 to t_2

    return(output)
  })
}

disc_disQ2_4p_fn <- function(t_1, t_2, DU_all,
                             DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5,
                             b_5_1, b_5_2, b_5_3, b_5_4, b_5_5,
                             b_6_1, b_6_2, b_6_3, b_6_4, b_6_5,
                             v_knots, v_a_1j, v_a_2j, v_a_3j, r = IDR) {
  # t_2 >= k_3
  
  k_1 <- v_knots[1]
  k_2 <- v_knots[2]
  k_3 <- v_knots[3]
  
  a_13 <- v_a_1j[4]
  a_12 <- v_a_1j[3]
  a_11 <- v_a_1j[2]
  a_10 <- v_a_1j[1]
  
  a_23 <- v_a_2j[4]
  a_22 <- v_a_2j[3]
  a_21 <- v_a_2j[2]
  a_20 <- v_a_2j[1]
  
  a_31 <- v_a_3j[2]
  a_30 <- v_a_3j[1]
  
  v_inputs <- list(t_1 = t_1, t_2 = t_2, 
                   DU_0_1 = DU_0_1, 
                   DU_0_2 = DU_0_2, 
                   DU_0_3 = DU_0_3, 
                   DU_0_4 = DU_0_4, 
                   DU_0_5 = DU_0_5, 
                   b_5_1 = b_5_1,
                   b_5_2 = b_5_2,
                   b_5_3 = b_5_3,
                   b_5_4 = b_5_4,
                   b_5_5 = b_5_5,
                   b_6_1 = b_6_1,
                   b_6_2 = b_6_2,
                   b_6_3 = b_6_3,
                   b_6_4 = b_6_4,
                   b_6_5 = b_6_5)
  
  pmap_dbl(v_inputs, function(t_1, t_2,
                              DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5,
                              b_5_1, b_5_2, b_5_3, b_5_4, b_5_5,
                              b_6_1, b_6_2, b_6_3, b_6_4, b_6_5) {
    output <- 
      stats::integrate(disc_disu2_fun, 
                       lower = 0, upper = k_1, 
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,
                       DU_0_2 = DU_0_2,
                       DU_0_3 = DU_0_3,
                       DU_0_4 = DU_0_4,
                       DU_0_5 = DU_0_5,
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = 0, A_2 = 0, A_1 = 0, A_0 = 0, 
                       r = r)$value + # from 0 to k_1
      stats::integrate(disc_disu2_fun, 
                       lower = k_1, upper = k_2, 
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,
                       DU_0_2 = DU_0_2,
                       DU_0_3 = DU_0_3,
                       DU_0_4 = DU_0_4,
                       DU_0_5 = DU_0_5,
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = a_13, A_2 = a_12, A_1 = a_11, A_0 = a_10,
                       r = r)$value + # from k_1 to k_2
      stats::integrate(disc_disu2_fun, 
                       lower = k_2, upper = t_2,  
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,
                       DU_0_2 = DU_0_2,
                       DU_0_3 = DU_0_3,
                       DU_0_4 = DU_0_4,
                       DU_0_5 = DU_0_5,
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = a_23, A_2 = a_22, A_1 = a_21, A_0 = a_20,
                       r = r)$value + # from k_2 to k_3
      stats::integrate(disc_disu2_fun, 
                       lower = k_3, upper = t_2, 
                       t_1 = t_1,
                       DU_0_1 = DU_0_1,
                       DU_0_2 = DU_0_2,
                       DU_0_3 = DU_0_3,
                       DU_0_4 = DU_0_4,
                       DU_0_5 = DU_0_5,
                       b_5_1 = b_5_1,
                       b_5_2 = b_5_2,
                       b_5_3 = b_5_3,
                       b_5_4 = b_5_4,
                       b_5_5 = b_5_5,
                       b_6_1 = b_6_1,
                       b_6_2 = b_6_2,
                       b_6_3 = b_6_3,
                       b_6_4 = b_6_4,
                       b_6_5 = b_6_5, 
                       DU_all = DU_all,
                       A_3 = 0, A_2 = 0, A_1 = a_31, A_0 = a_30,
                       r = r)$value # from k_3 to t_2

    return(output)
  })
}


## -----------------------------------------------------------------------------
DES_cohort_gen <- function(n_simind = 10000, simtime = tm) {
  # Generate cohort
  mysim <- tibble(age_0 = gen_continuous(n_simind, list_age),
                  SIMD_0 = gen_continuous(n_simind, list_SIMD),
                  Diabetes_0 = gen_proportion(n_simind, list_Diabetes),
                  FH_0 = gen_proportion(n_simind, list_FH),
                  CPD_0 = gen_continuous(n_simind, list_CPD),
                  SBP_0 = gen_continuous(n_simind, list_SBP), 
                  TC_0 = gen_continuous(n_simind, list_TC),
                  HDL_0 = gen_continuous(n_simind, list_HDL),
                  sex_0 = gen_proportion(n_simind, list_sex), 
                  status_0 = 0,
                  timeexit = simtime
                  ) %>% 
    mutate(ID = row_number(),
           Diabetes_yes = as.integer(Diabetes_0 == "yes"),
           FH_yes = as.integer(FH_0 == "yes"),
           sex_male = as.integer(sex_0 == "male"),
           time_1 = NA, 
           disc_time_1 = NA, 
           status_1 = NA,
           status_1_star = NA,
           age_1 = NA, 
           time_2 = NA, 
           disc_time_2 = NA, 
           status_2 = NA, 
           time = NA,
           age = NA)
  
  return(mysim)
}


DES_fn <- function(mysim, simtime = tm, list_coef_inuse = list_coef, rndseed_mod_PSA = 0) {

  
  sim_by_tx_fn <- function(treatment = 0) {
    set.seed(615 + rndseed_mod_PSA)
    mysim_tx <- mysim %>%
      mutate(TC_0 = TC_0 - (TC_0 - HDL_0) * treatment * list_tx$ldleffect,
             HDL_0 = HDL_0 * list_tx$hdleffect ^ treatment) # treatment effect
    
    sim_by_sex_fn <- function(sex = "male") {
      
      # select the data frame for single sex
      mysim_sex <- mysim_tx %>%
        filter(sex_0 == sex)
      
      n_sex <- nrow(mysim_sex)
      
      ############################################################################
      ###################### Part I: Life expectancy #############################
      ############################################################################
      
      # first event ----
      
      # store the ID in a vector
      v_ID <- mysim_sex %>% dplyr::select(ID)
      
      mat_risk <- mysim_sex %>%
        dplyr::select(age_0, SIMD_0, Diabetes_yes, FH_yes, CPD_0, SBP_0, TC_0, HDL_0) %>%
        add_column(cons = 1) %>% # for the constant coef
        as.matrix() %*% choose_coef_by_sex(sex, "first_event_coef", list_coef_inuse)[1:9, ] * calib$f1
      
      linpred <- cbind(v_ID, mat_risk) %>%
        as_tibble() %>% 
        arrange(ID) %>%
        rename_with( ~ gsub("first_event", "lb_firstE", .x, fixed = TRUE), 
                     starts_with("first_event"))
  
      p_gamma <- choose_coef_by_sex(sex, "first_event_coef", list_coef_inuse)[10, ] # shape parameter
      
      mysim_sex <- mysim_sex %>%
        left_join(linpred, by = join_by(ID)) %>%
        mutate(t_firstE_nonfatal_CHD = 1/p_gamma["first_event_nonfatal_CHD"] * 
                 log(1 - p_gamma["first_event_nonfatal_CHD"]/exp(lb_firstE_nonfatal_CHD) * log(runif(n()))),
               
               t_firstE_nonfatal_CBVD = 1/p_gamma["first_event_nonfatal_CBVD"] * 
                 log(1 - p_gamma["first_event_nonfatal_CBVD"]/exp(lb_firstE_nonfatal_CBVD) * log(runif(n()))),
               
               t_firstE_CVD_death = 1/p_gamma["first_event_CVD_death"] * 
                 log(1 - p_gamma["first_event_CVD_death"]/exp(lb_firstE_CVD_death) * log(runif(n()))),
               
               t_firstE_nonCVD_death = 1/p_gamma["first_event_nonCVD_death"] * 
                 log(1 - p_gamma["first_event_nonCVD_death"]/exp(lb_firstE_nonCVD_death) * log(runif(n())))) %>% 
        # when shape parameter of gompertz distribution < 0, 
        # S(t) tends to a non-zero probability as t increases, 
        # in which case the time-to-first-event will be 'NaN'. Should adjust to be 'Inf'
        mutate(
          t_firstE_nonfatal_CHD = case_when(
          is.nan(t_firstE_nonfatal_CHD) ~ Inf,
          .default = t_firstE_nonfatal_CHD
        ),
        t_firstE_nonfatal_CBVD = case_when(
          is.nan(t_firstE_nonfatal_CBVD) ~ Inf,
          .default = t_firstE_nonfatal_CBVD
        ),
        t_firstE_CVD_death = case_when(
          is.nan(t_firstE_CVD_death) ~ Inf,
          .default = t_firstE_CVD_death
        ),
        t_firstE_nonCVD_death = case_when(
          is.nan(t_firstE_nonCVD_death) ~ Inf,
          .default = t_firstE_nonCVD_death
        ),
        # check first event if inf follow-up
        time_1 = pmin(t_firstE_nonfatal_CHD, t_firstE_nonfatal_CBVD, 
                      t_firstE_CVD_death, t_firstE_nonCVD_death),
        status_1_star = as.integer(case_when(
          time_1 == Inf ~ 0, # 0: "no event"
          time_1 == t_firstE_nonfatal_CHD ~ 1, # 1: "nonfatal_CHD"
          time_1 == t_firstE_nonfatal_CBVD ~ 2, # 2: "nonfatal_CBVD"
          time_1 == t_firstE_CVD_death ~ 3, # 3: "CVD_death"
          time_1 == t_firstE_nonCVD_death ~ 4 # 4: "nonCVD_death"
          )
        ), 
        # loss of follow-up, possible four first events competing to happen
        time_1 = pmin(timeexit, t_firstE_nonfatal_CHD, t_firstE_nonfatal_CBVD, 
                      t_firstE_CVD_death, t_firstE_nonCVD_death), 
        disc_time_1 = disc_ly1_fn(time_1),
        status_1 = as.integer(case_when(time_1 == t_firstE_nonfatal_CHD ~ 1, # 1: "nonfatal_CHD"
                                        time_1 == t_firstE_nonfatal_CBVD ~ 2, # 2: "nonfatal_CBVD"
                                        time_1 == t_firstE_CVD_death ~ 3, # 3: "CVD_death"
                                        time_1 == t_firstE_nonCVD_death ~ 4, # 4: "nonCVD_death"
                                        .default = 0)), # 0: "exit with no event"
        time = time_1,
        age = age_0 + time
        )
      
      t_ly_event_free <- sum(mysim_sex$disc_time_1)
      
      # second event ----
      
      mysim_sex_post <- mysim_sex %>%
        filter(status_1 == 1 | status_1 == 2) %>%
        mutate(age_1 = age_0 + time_1, # update age
           timeexit = timeexit - time_1) # update time to exit
        
      # store the ID in a vector
      v_ID2 <- mysim_sex_post %>% dplyr::select(ID)
      
      mat_risk <- mysim_sex_post %>%
        dplyr::select(age_1, SIMD_0, FH_yes) %>%
        add_column(cons = 1) %>% 
        as.matrix() %*% choose_coef_by_sex(sex, "post_event_coef", list_coef_inuse)[1:4,]
      
      linpred <- cbind(v_ID2, mat_risk) %>%
        as_tibble() %>% 
        arrange(ID)
      
      p_gamma <- choose_coef_by_sex(sex, "post_event_coef", list_coef_inuse)[5, ]
      
      mysim_sex_post <- mysim_sex_post %>%
        left_join(linpred, by = join_by(ID)) %>%
        mutate(t_postSurv_CHD = 1/p_gamma["post_CHD"] * 
                 log(1 - p_gamma["post_CHD"]/exp(post_CHD) * log(runif(n()))),
               
               t_postSurv_CBVD = 1/p_gamma["post_CBVD"] * 
                 log(1 - p_gamma["post_CBVD"]/exp(post_CBVD) * log(runif(n())))) %>% 
               # when parameter shape of gompertz distribution < 0, 
               # S(t) tends to a non-zero probability as t increases, 
               # in which case the time-to-first-event will be 'NaN'. Should adjust to be 'Inf'
        mutate(
          t_postSurv_CHD = case_when(
          is.nan(t_postSurv_CHD) ~ Inf,
          .default = t_postSurv_CHD
        ),
        t_postSurv_CBVD = case_when(
          is.nan(t_postSurv_CBVD) ~ Inf,
          .default = t_postSurv_CBVD
        ),
        # loss of follow-up and post CHD/CBVD death competing to happen
        time_2 = pmin(timeexit, ifelse(status_1 == 1, t_postSurv_CHD, t_postSurv_CBVD)),
        disc_time_2 = disc_ly2_fn(time_1, time_2),
        status_2 = as.integer(case_when(time_2 == t_postSurv_CHD ~ 1, # 1: "death after nonfatal_CHD"
                                        time_2 == t_postSurv_CBVD ~ 2, # 2: "death after nonfatal_CBVD"
                                        .default = 0)), # 0: "exit without further event"
        time = time + time_2,
        age = age_0 + time
        )
        
      t_ly_CHD <- sum(mysim_sex_post$disc_time_2[mysim_sex_post$status_1 == 1])
    
      t_ly_CBVD <- sum(mysim_sex_post$disc_time_2[mysim_sex_post$status_1 == 2])
      
      t_ly <- t_ly_event_free + t_ly_CHD + t_ly_CBVD
      
      m_ly <- t_ly / n_sex
      
      mysim_pre <- mysim_sex %>% 
        filter(status_1 != 1 & status_1 != 2) %>%
        dplyr::select(ID, age_0, SIMD_0, Diabetes_0, FH_0, CPD_0, SBP_0, TC_0, HDL_0, 
               sex_0, status_0, Diabetes_yes, FH_yes, sex_male, time_1, 
               disc_time_1, status_1, status_1_star, age_1, time_2, disc_time_2, 
               status_2, time, age)
      
      mysim_post <- mysim_sex_post %>%
        dplyr::select(ID, age_0, SIMD_0, Diabetes_0, FH_0, CPD_0, SBP_0, TC_0, HDL_0, 
               sex_0, status_0, Diabetes_yes, FH_yes, sex_male, time_1, 
               disc_time_1, status_1, status_1_star, age_1, time_2, disc_time_2, 
               status_2, time, age)
      
      mysim_sex <- add_row(mysim_pre, mysim_post) %>% arrange(ID)
      
      ############################################################################
      ################################ Part II: Cost #############################
      ############################################################################
      
      mat_coef_c1 <- choose_coef_by_sex(sex, "coef_c1", list_coef_inuse)
      
      # first event ----
      
      mat_cost_v <- mysim_sex %>% 
        dplyr::select(age_0, SIMD_0, FH_yes) %>% 
        add_column(cons = 1) %>% 
        as.matrix() %*% mat_coef_c1[3:6, c(1, 2, 5, 6)]
      
      linpred <- cbind(v_ID, mat_cost_v) %>%
        as_tibble() %>% 
        arrange(ID)
      
      mysim_sex <- mysim_sex %>%
        left_join(linpred, by = join_by(ID)) %>%
        mutate(C_0 = case_when(# ultimately, nonfatal_CHD:
                               status_1_star == 1 ~ C_preCHD,
                               # ultimately, nonfatal_CBVD:
                               status_1_star == 2 ~ C_preCBVD, 
                               # ultimately, CVD_death:
                               status_1_star == 3 ~ C_fatalCVD, 
                               # ultimately, non_CVD death:
                               status_1_star == 4 ~ C_fatalnoncvd, 
                               # ultimately, no event:
                               .default = 0
                               )) %>%
        dplyr::select(-c("C_preCHD", "C_preCBVD", "C_fatalCVD", "C_fatalnoncvd")) %>%
        mutate(b_1_intib = case_when(# ultimately, nonfatal_CHD:
                                     status_1_star == 1 ~ mat_coef_c1[1, "C_preCHD"],
                                 
                                     # ultimately, nonfatal_CBVD:
                                     status_1_star == 2 ~ mat_coef_c1[1, "C_preCBVD"],
                                 
                                     # ultimately, CVD_death:
                                     status_1_star == 3 ~ mat_coef_c1[1, "C_fatalCVD"],
                                 
                                     # ultimately, non_CVD death:
                                     status_1_star == 4 ~ mat_coef_c1[1, "C_fatalnoncvd"],
                                 
                                     # ultimately, no event:
                                     .default = 0
                                     ),
               c_pre_l = disc_cost1_fn(C_0, b_1_intib, time_1),
               b_2_intib = case_when(# ultimately, nonfatal_CHD:
                                     status_1_star == 1 ~ mat_coef_c1[2, "C_preCHD"],
                                 
                                     # ultimately, nonfatal_CBVD:
                                     status_1_star == 2 ~ mat_coef_c1[2, "C_preCBVD"],
                                 
                                     # ultimately, CVD_death:
                                     status_1_star == 3 ~ mat_coef_c1[2, "C_fatalCVD"],
                                 
                                     # ultimately, non_CVD death:
                                     status_1_star == 4 ~ mat_coef_c1[2, "C_fatalnoncvd"],
                                 
                                     # ultimately, no event:
                                     .default = 0
                                     ),
               c_pre_nl = disc_cost1_nl_fn(nl_RCS_pre[[sex]][["knots"]], 
                                           nl_RCS_pre[[sex]][["coef_a_1j"]], 
                                           nl_RCS_pre[[sex]][["coef_a_2j"]], 
                                           nl_RCS_pre[[sex]][["coef_a_3j"]],
                                           b_2_intib,
                                           time_1),
               c_pre = c_pre_l + c_pre_nl,
               c_post = 0,
               # take the treatment until the first event
               c_tx = disc_time_1 * list_tx$cost * treatment
               )
      
      t_cost_pre_CHD <- sum(mysim_sex$c_pre[mysim_sex$status_1_star == 1])
    
      t_cost_pre_CBVD <- sum(mysim_sex$c_pre[mysim_sex$status_1_star == 2])
      
      t_cost_pre_fatalCVD <- sum(mysim_sex$c_pre[mysim_sex$status_1_star == 3])
    
      t_cost_pre_fatalnoncvd <- sum(mysim_sex$c_pre[mysim_sex$status_1_star == 4])
      
      t_cost_pre <- t_cost_pre_CHD + t_cost_pre_CBVD + t_cost_pre_fatalCVD + t_cost_pre_fatalnoncvd
      
      m_cost_pre <- t_cost_pre / n_sex
      
      t_cost_tx <- sum(mysim_sex$c_tx)
        
      m_cost_tx <- t_cost_tx / n_sex
      
      # second event ----
      
      mysim_sex_post <- mysim_sex %>%
        filter(status_1 == 1 | status_1 == 2)
      
      mat_cost_v <- mysim_sex_post %>% 
        dplyr::select(age_1, SIMD_0, FH_yes) %>% 
        add_column(cons = 1) %>% 
        as.matrix() %*% mat_coef_c1[3:6, 3:4]
      
      linpred <- cbind(v_ID2, mat_cost_v) %>%
        as_tibble() %>% 
        arrange(ID)
      
      mysim_sex_post <- mysim_sex_post %>%
        left_join(linpred, by = join_by(ID)) %>%
        mutate(C_0 = case_when(# after nonfatal_CHD:
                               status_1 == 1 ~ C_postCHD,
                               # after nonfatal_CBVD:
                               status_1 == 2 ~ C_postCBVD
                               )) %>% 
        dplyr::select(-c("C_postCHD", "C_postCBVD")) %>%
        mutate(b_3_intib = case_when(# after nonfatal_CHD:
                                     status_1 == 1 ~ mat_coef_c1[1, "C_postCHD"],
                                 
                                     # after nonfatal_CBVD:
                                     status_1 == 2 ~ mat_coef_c1[1, "C_postCBVD"]
                                     ),
               c_post_l = disc_cost2_fn(C_0, b_3_intib, time_1, time_2),
               b_4_intib = case_when(# after nonfatal_CHD:
                                     status_1 == 1 ~ mat_coef_c1[2, "C_postCHD"],
                                 
                                     # after nonfatal_CBVD:
                                     status_1 == 2 ~ mat_coef_c1[2, "C_postCBVD"]
                                     ),
               c_post_nl = case_when(# after nonfatal_CHD:
                                     status_1 == 1 ~ 
                                     disc_cost2_nl_fn(nl_RCS_postCHD[[sex]][["knots"]], 
                                                      nl_RCS_postCHD[[sex]][["coef_a_1j"]], 
                                                      nl_RCS_postCHD[[sex]][["coef_a_2j"]], 
                                                      nl_RCS_postCHD[[sex]][["coef_a_3j"]],
                                                      b_4_intib,
                                                      time_1,
                                                      time_2),
                                    
                                     # after nonfatal_CBVD:
                                     status_1 == 2 ~ 
                                     disc_cost2_nl_fn(nl_RCS_postCBVD[[sex]][["knots"]], 
                                                      nl_RCS_postCBVD[[sex]][["coef_a_1j"]], 
                                                      nl_RCS_postCBVD[[sex]][["coef_a_2j"]], 
                                                      nl_RCS_postCBVD[[sex]][["coef_a_3j"]],
                                                      b_4_intib,
                                                      time_1,
                                                      time_2)
                                     ),
               c_post = c_post_l + c_post_nl
               )
      
      t_cost_post_CHD <- sum(mysim_sex_post$c_post[mysim_sex_post$status_1 == 1])
    
      t_cost_post_CBVD <- sum(mysim_sex_post$c_post[mysim_sex_post$status_1 == 2])
      
      t_cost_post <- t_cost_post_CHD + t_cost_post_CBVD
      
      m_cost_post <- t_cost_post / n_sex
      
      t_cost <- t_cost_pre + t_cost_tx + t_cost_post
      
      m_cost <- t_cost / n_sex
      
      mysim_pre <- mysim_sex %>% 
        filter(status_1 != 1 & status_1 != 2) %>%
        dplyr::select(ID, age_0, SIMD_0, Diabetes_0, FH_0, CPD_0, SBP_0, TC_0, HDL_0, 
               sex_0, status_0, Diabetes_yes, FH_yes, sex_male, time_1, 
               disc_time_1, status_1, status_1_star, age_1, time_2, disc_time_2, 
               status_2, time, age, c_pre, c_tx, c_post)
      
      mysim_post <- mysim_sex_post %>%
        dplyr::select(ID, age_0, SIMD_0, Diabetes_0, FH_0, CPD_0, SBP_0, TC_0, HDL_0, 
               sex_0, status_0, Diabetes_yes, FH_yes, sex_male, time_1, 
               disc_time_1, status_1, status_1_star, age_1, time_2, disc_time_2, 
               status_2, time, age, c_pre, c_tx, c_post)
      
      mysim_sex <- add_row(mysim_pre, mysim_post) %>% arrange(ID)
      
      ############################################################################
      ################################ Part III: QALY ############################
      ############################################################################
      
      mat_coef_q <- choose_coef_by_sex(sex, "second_event_coef", list_coef_inuse)
      rownames(mat_coef_q) <- list_coef_inuse$second_event_coef_m$eqn
      
      v_du <- -choose_coef_by_sex(sex, "coef_u", list_coef_inuse)
      
      v_u_norms <- choose_coef_by_sex(sex, "u_norms", list_coef_inuse)[, 2]
      
      # first event ----
      
      mysim_sex <- mysim_sex %>%
        mutate(q_pre = disc_qaly1_fn(age_0, v_u_norms, time_1),
               # take the treatment until the first event
               q_pre = q_pre - disc_time_1 * list_tx$disu * treatment,
               q_post = 0
               )
      
      t_qaly_pre_CHD <- sum(mysim_sex$q_pre[mysim_sex$status_1_star == 1])
    
      t_qaly_pre_CBVD <- sum(mysim_sex$q_pre[mysim_sex$status_1_star == 2])
      
      t_qaly_pre_fatalCVD <- sum(mysim_sex$q_pre[mysim_sex$status_1_star == 3])
    
      t_qaly_pre_fatalnoncvd <- sum(mysim_sex$q_pre[mysim_sex$status_1_star == 4])
      
      t_qaly_pre <- t_qaly_pre_CHD + t_qaly_pre_CBVD + t_qaly_pre_fatalCVD + t_qaly_pre_fatalnoncvd
      
      m_qaly_pre <- t_qaly_pre / n_sex
      
      # second event ----
      
      mysim_sex_post <- mysim_sex %>%
        filter(status_1 == 1 | status_1 == 2)
      
      mat_qaly_v <- mysim_sex_post %>% 
        dplyr::select(age_1, SIMD_0, FH_yes) %>% 
        add_column(cons = 1) %>% 
        as.matrix() %*% t(mat_coef_q[, 3:6])
      
      linpred <- cbind(v_ID2, mat_qaly_v) %>%
        as_tibble() %>% 
        arrange(ID) %>% 
        rename_with( ~ gsub("Secondary", "sec", .x, fixed = TRUE), 
                     starts_with("Secondary"))
      
      mysim_sex_post <- temp2 <- mysim_sex_post %>%
        left_join(linpred, by = join_by(ID)) %>%
        mutate(DU_0_1 = case_when(# after nonfatal_CHD:
                                  status_1 == 1 ~ secCHD_CHD,
                                  # after nonfatal_CBVD:
                                  status_1 == 2 ~ secCHD_CBVD
                                  ),
               DU_0_2 = case_when(# after nonfatal_CHD:
                                  status_1 == 1 ~ secCBVD_CHD,
                                  # after nonfatal_CBVD:
                                  status_1 == 2 ~ secCBVD_CBVD
                                  ),
               DU_0_3 = case_when(# after nonfatal_CHD:
                                  status_1 == 1 ~ secHF_CHD,
                                  # after nonfatal_CBVD:
                                  status_1 == 2 ~ secHF_CBVD
                                  ),
               DU_0_4 = case_when(# after nonfatal_CHD:
                                  status_1 == 1 ~ secPAD_CHD,
                                  # after nonfatal_CBVD:
                                  status_1 == 2 ~ secPAD_CBVD
                                  ),
               DU_0_5 = case_when(# after nonfatal_CHD:
                                  status_1 == 1 ~ secotherheart_CHD,
                                  # after nonfatal_CBVD:
                                  status_1 == 2 ~ secotherheart_CBVD
                                  )) %>% 
        dplyr::select(-c("secCHD_CHD", "secCHD_CBVD", 
                  "secCBVD_CHD", "secCBVD_CBVD", 
                  "secHF_CHD", "secHF_CBVD", 
                  "secPAD_CHD", "secPAD_CBVD",
                  "secotherheart_CHD", "secotherheart_CBVD")) %>%
        mutate(b_5_intib_1 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["SecondaryCHD_CHD", 1],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["SecondaryCHD_CBVD", 1]
                                       ),
               b_5_intib_2 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["SecondaryCBVD_CHD", 1],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["SecondaryCBVD_CBVD", 1]
                                       ),
               b_5_intib_3 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["SecondaryHF_CHD", 1],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["SecondaryHF_CBVD", 1]
                                       ),
               b_5_intib_4 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["SecondaryPAD_CHD", 1],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["SecondaryPAD_CBVD", 1]
                                       ),
               b_5_intib_5 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["Secondaryotherheart_CHD", 1],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["Secondaryotherheart_CBVD", 1]
                                       ),
               b_6_intib_1 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["SecondaryCHD_CHD", 2],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["SecondaryCHD_CBVD", 2]
                                       ),
               b_6_intib_2 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["SecondaryCBVD_CHD", 2],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["SecondaryCBVD_CBVD", 2]
                                       ),
               b_6_intib_3 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["SecondaryHF_CHD", 2],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["SecondaryHF_CBVD", 2]
                                       ),
               b_6_intib_4 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["SecondaryPAD_CHD", 2],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["SecondaryPAD_CBVD", 2]
                                       ),
               b_6_intib_5 = case_when(# after nonfatal_CHD:
                                       status_1 == 1 ~ mat_coef_q["Secondaryotherheart_CHD", 2],
                                 
                                       # after nonfatal_CBVD:
                                       status_1 == 2 ~ mat_coef_q["Secondaryotherheart_CBVD", 2]
                                       ),
               q_post_bu_d1 = disc_qaly21_fn(age_0, v_u_norms,
                                             time_1, time_2, v_du[status_1]) - q_pre,
               q_post_d2 = case_when(# after nonfatal_CHD & t_2 < k_1:
                                    status_1 == 1 & time_2 < nl_RCS_postCHD[[sex]][["knots"]][1] ~ 
                                    disc_disQ2_1p_fn(time_1, time_2, v_du, 
                                                     DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                                                     b_5_intib_1, b_5_intib_2, b_5_intib_3, b_5_intib_4, b_5_intib_5,
                                                     b_6_intib_1, b_6_intib_2, b_6_intib_3, b_6_intib_4, b_6_intib_5,
                                                     nl_RCS_postCHD[[sex]][["knots"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_1j"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_2j"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_3j"]]),
                                      
                                    # after nonfatal_CBVD & t_2 < k_1:
                                    status_1 == 2 & time_2 < nl_RCS_postCBVD[[sex]][["knots"]][1] ~ 
                                    disc_disQ2_1p_fn(time_1, time_2, v_du, 
                                                     DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                                                     b_5_intib_1, b_5_intib_2, b_5_intib_3, b_5_intib_4, b_5_intib_5,
                                                     b_6_intib_1, b_6_intib_2, b_6_intib_3, b_6_intib_4, b_6_intib_5,
                                                     nl_RCS_postCBVD[[sex]][["knots"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_1j"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_2j"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_3j"]]),
                                       
                                    # after nonfatal_CHD & t_2 < k_2:
                                    status_1 == 1 & time_2 < nl_RCS_postCHD[[sex]][["knots"]][2] ~ 
                                    disc_disQ2_2p_fn(time_1, time_2, v_du, 
                                                     DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                                                     b_5_intib_1, b_5_intib_2, b_5_intib_3, b_5_intib_4, b_5_intib_5,
                                                     b_6_intib_1, b_6_intib_2, b_6_intib_3, b_6_intib_4, b_6_intib_5,
                                                     nl_RCS_postCHD[[sex]][["knots"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_1j"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_2j"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_3j"]]),
                                      
                                    # after nonfatal_CBVD & t_2 < k_2:
                                    status_1 == 2 & time_2 < nl_RCS_postCBVD[[sex]][["knots"]][2] ~ 
                                    disc_disQ2_2p_fn(time_1, time_2, v_du, 
                                                     DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                                                     b_5_intib_1, b_5_intib_2, b_5_intib_3, b_5_intib_4, b_5_intib_5,
                                                     b_6_intib_1, b_6_intib_2, b_6_intib_3, b_6_intib_4, b_6_intib_5,
                                                     nl_RCS_postCBVD[[sex]][["knots"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_1j"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_2j"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_3j"]]),
                                      
                                    # after nonfatal_CHD & t_2 < k_3:
                                    status_1 == 1 & time_2 < nl_RCS_postCHD[[sex]][["knots"]][3] ~ 
                                    disc_disQ2_3p_fn(time_1, time_2, v_du, 
                                                     DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                                                     b_5_intib_1, b_5_intib_2, b_5_intib_3, b_5_intib_4, b_5_intib_5,
                                                     b_6_intib_1, b_6_intib_2, b_6_intib_3, b_6_intib_4, b_6_intib_5,
                                                     nl_RCS_postCHD[[sex]][["knots"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_1j"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_2j"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_3j"]]),
                                      
                                    # after nonfatal_CBVD & t_2 < k_3:
                                    status_1 == 2 & time_2 < nl_RCS_postCBVD[[sex]][["knots"]][3] ~ 
                                    disc_disQ2_3p_fn(time_1, time_2, v_du, 
                                                     DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                                                     b_5_intib_1, b_5_intib_2, b_5_intib_3, b_5_intib_4, b_5_intib_5,
                                                     b_6_intib_1, b_6_intib_2, b_6_intib_3, b_6_intib_4, b_6_intib_5,
                                                     nl_RCS_postCBVD[[sex]][["knots"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_1j"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_2j"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_3j"]]),
                                       
                                       
                                    # after nonfatal_CHD & t_2 >= k_3:
                                    status_1 == 1 & time_2 >= nl_RCS_postCHD[[sex]][["knots"]][3] ~ 
                                    disc_disQ2_4p_fn(time_1, time_2, v_du, 
                                                     DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                                                     b_5_intib_1, b_5_intib_2, b_5_intib_3, b_5_intib_4, b_5_intib_5,
                                                     b_6_intib_1, b_6_intib_2, b_6_intib_3, b_6_intib_4, b_6_intib_5,
                                                     nl_RCS_postCHD[[sex]][["knots"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_1j"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_2j"]], 
                                                     nl_RCS_postCHD[[sex]][["coef_a_3j"]]),
                                      
                                    # after nonfatal_CBVD & t_2 >= k_3:
                                    status_1 == 2 & time_2 >= nl_RCS_postCBVD[[sex]][["knots"]][3] ~ 
                                    disc_disQ2_4p_fn(time_1, time_2, v_du, 
                                                     DU_0_1, DU_0_2, DU_0_3, DU_0_4, DU_0_5, 
                                                     b_5_intib_1, b_5_intib_2, b_5_intib_3, b_5_intib_4, b_5_intib_5,
                                                     b_6_intib_1, b_6_intib_2, b_6_intib_3, b_6_intib_4, b_6_intib_5,
                                                     nl_RCS_postCBVD[[sex]][["knots"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_1j"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_2j"]], 
                                                     nl_RCS_postCBVD[[sex]][["coef_a_3j"]])
                                    ),
               q_post = q_post_bu_d1 + q_post_d2
               )
      
      t_qaly_post_CHD <- sum(mysim_sex_post$q_post[mysim_sex_post$status_1 == 1])
    
      t_qaly_post_CBVD <- sum(mysim_sex_post$q_post[mysim_sex_post$status_1 == 2])
      
      t_qaly_post <- t_qaly_post_CHD + t_qaly_post_CBVD
      
      m_qaly_post <- t_qaly_post / n_sex
      
      t_qaly <- t_qaly_pre + t_qaly_post
      
      m_qaly <- t_qaly / n_sex
      
      mysim_pre <- mysim_sex %>% 
        filter(status_1 != 1 & status_1 != 2) %>%
        dplyr::select(ID, age_0, SIMD_0, Diabetes_0, FH_0, CPD_0, SBP_0, TC_0, HDL_0, 
               sex_0, status_0, Diabetes_yes, FH_yes, sex_male, time_1, 
               disc_time_1, status_1, status_1_star, age_1, time_2, disc_time_2, 
               status_2, time, age, c_pre, c_tx, c_post, q_pre, q_post)
      
      mysim_post <- mysim_sex_post %>%
        dplyr::select(ID, age_0, SIMD_0, Diabetes_0, FH_0, CPD_0, SBP_0, TC_0, HDL_0, 
               sex_0, status_0, Diabetes_yes, FH_yes, sex_male, time_1, 
               disc_time_1, status_1, status_1_star, age_1, time_2, disc_time_2, 
               status_2, time, age, c_pre, c_tx, c_post, q_pre, q_post)
      
      mysim_sex <- add_row(mysim_pre, mysim_post) %>% arrange(ID)
      
      
      mysim_output <- mysim_sex
  
      
      return(list(mysim_output = mysim_output,
                  n = n_sex,
                  m_ly = m_ly,
                  m_cost = m_cost,
                  m_qaly = m_qaly))
    }
    
  sim_2sex <- lapply(c("male", "female"),
                     sim_by_sex_fn)
  
  names(sim_2sex) <- c("male", "female")
  
  return(sim_2sex)

  }
  
sim_2tx <- lapply(c(0, 1),
                  sim_by_tx_fn)
  
names(sim_2tx) <- c("No treatment", "Treatment")
  
return(sim_2tx)

}
  
  



## -----------------------------------------------------------------------------
sim_cohort_size <- 30000
PSA_n_iter <- 100

set.seed(1847)
sim_cohort <- DES_cohort_gen(sim_cohort_size)


## -----------------------------------------------------------------------------

set.seed(2130)
DES_results <- DES_fn(mysim = sim_cohort, tm)

results_no_tx <- DES_results$`No treatment`
results_tx <- DES_results$`Treatment`


## -----------------------------------------------------------------------------

m_cost_notx <- 
  (ifelse(results_no_tx$male$n == 0, 0, results_no_tx$male$m_cost) * results_no_tx$male$n + 
  ifelse(results_no_tx$female$n == 0, 0, results_no_tx$female$m_cost) * results_no_tx$female$n) / 
  (results_no_tx$male$n + results_no_tx$female$n)

m_qaly_notx <- 
  (ifelse(results_no_tx$male$n == 0, 0, results_no_tx$male$m_qaly) * results_no_tx$male$n + 
  ifelse(results_no_tx$female$n == 0, 0, results_no_tx$female$m_qaly) * results_no_tx$female$n) / 
  (results_no_tx$male$n + results_no_tx$female$n)

m_cost_tx <- 
  (ifelse(results_tx$male$n == 0, 0, results_tx$male$m_cost) * results_tx$male$n + 
  ifelse(results_tx$female$n == 0, 0, results_tx$female$m_cost) * results_tx$female$n) / 
  (results_tx$male$n + results_tx$female$n)

m_qaly_tx <- 
  (ifelse(results_tx$male$n == 0, 0, results_tx$male$m_qaly) * results_tx$male$n + 
  ifelse(results_tx$female$n == 0, 0, results_tx$female$m_qaly) * results_tx$female$n) / 
  (results_tx$male$n + results_tx$female$n)

inc_cost <- m_cost_tx - m_cost_notx

inc_qaly <- m_qaly_tx - m_qaly_notx

print(paste0("Incremental cost = ", format_tab_2GBP(inc_cost), 
             ", and Incremental QALY = ", format_tab_4(inc_qaly)))

tx_c <- 
  (sum(results_tx$male$mysim_output$c_tx) + 
  sum(results_tx$female$mysim_output$c_tx)) / 
  (results_tx$male$n + results_tx$female$n)

print(paste0("The cost of statin = ", format_tab_2GBP(tx_c)))

tibble_sim_output_all <- results_tx[["male"]]$mysim_output %>% 
  bind_rows(results_tx[["female"]]$mysim_output) %>% 
  mutate(arm = "Treatment") %>%
  bind_rows(results_no_tx[["male"]]$mysim_output) %>% 
  bind_rows(results_no_tx[["female"]]$mysim_output) %>% 
  mutate(arm = if_else(is.na(arm), "Control", arm)) %>%
  dplyr::select(arm, ID, age_0, sex_0, SIMD_0, time_1, status_1, age_1, 
         time_2, status_2, time, age, c_pre, c_tx, c_post, q_pre, q_post)%>% 
  mutate(total_c = c_pre + c_tx + c_post,
         total_q = q_pre + q_post) %>%
  arrange(arm, ID)

l_sex <- levels(results_tx[["male"]]$mysim_output$sex_0)

CEA_by_arm <- tibble_sim_output_all %>% 
  group_by(arm) %>% 
  summarise(`Statin cost` = mean(c_tx), 
            `pre-event cost` = mean(c_pre), 
            `post-event cost` = mean(c_post),
            `total cost` = mean(total_c),
            `pre-event QALY` = mean(q_pre), 
            `post-event QALY` = mean(q_post),
            `total QALY` = mean(total_q)) %>%
  pivot_longer(-arm, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = arm, values_from = value) %>%
  mutate(Difference = Treatment - Control) %>%
  pivot_longer(cols = c(Control, Treatment, Difference), names_to = "arm", values_to = "value") %>%
  pivot_wider(names_from = variable, values_from = value) 

CEA_by_arm %>%
  mutate(across(6:8, ~ format_tab_3(.))) %>% 
  mutate(across(2:5, ~ format_tab_2GBP(.))) %>% 
  kable(caption = "The cost and QALY results by treatment arm.",
        align = "r")




## -----------------------------------------------------------------------------
tibble_sim_output_tx <- results_tx[["male"]]$mysim_output %>% 
  add_row(results_tx[["female"]]$mysim_output) %>% 
  dplyr::select(ID, age_0, sex_0, time_1, status_1, age_1, 
         time_2, status_2, time, age, c_pre, c_tx, c_post, q_pre, q_post)%>% 
  arrange(ID)
  
tibble_sim_output_tx %>% 
  slice_head(n=30) %>% 
  mutate(across(where(is.double), ~ format_tab_1(.))) %>% 
  kable(caption = "The events and associated time for the first 30 individuals in treatment arm.",
        align = "r")



## -----------------------------------------------------------------------------

tibble_sim_output_tx %>% 
  group_by(sex_0) %>% 
  summarise(age_0_mean = mean(age_0), LE_mean = mean(age)) %>% 
  mutate(undiscounted_LY = LE_mean - age_0_mean) %>%
  add_column(discounted_LY = c(results_tx[[l_sex[1]]]$m_ly, results_tx[[l_sex[2]]]$m_ly)) %>%
  mutate(across(where(is.double), ~ format_tab_1(.))) %>% 
  kable(caption = "The life expectancy and life-year results by sex.",
        align = "r")

## -----------------------------------------------------------------------------

tibble_sim_output_tx %>% 
  group_by(sex_0) %>% 
  summarise(`Statin cost` = mean(c_tx), `pre-event cost` = mean(c_pre), `post-event cost` = mean(c_post)) %>% 
  add_column(`total cost` = c(results_tx[[l_sex[1]]]$m_cost, results_tx[[l_sex[2]]]$m_cost)) %>%
  mutate(across(where(is.double), ~ format_tab_2GBP(.))) %>% 
  kable(caption = "The cost results by sex.",
        align = "r")

## -----------------------------------------------------------------------------

tibble_sim_output_tx %>% 
  group_by(sex_0) %>% 
  summarise(`pre-event QALY` = mean(q_pre), `post-event QALY` = mean(q_post)) %>% 
  add_column(`total QALY` = c(results_tx[[l_sex[1]]]$m_qaly, results_tx[[l_sex[2]]]$m_qaly)) %>%
  mutate(across(where(is.double), ~ format_tab_3(.))) %>% 
  kable(caption = "The QALY results by sex.",
        align = "r")

## -----------------------------------------------------------------------------
tibble_sim_output_no_tx <- results_no_tx[["male"]]$mysim_output %>% 
  add_row(results_no_tx[["female"]]$mysim_output) %>% 
  dplyr::select(ID, age_0, sex_0, time_1, status_1, age_1, 
         time_2, status_2, time, age, c_pre, c_tx, c_post, q_pre, q_post)%>% 
  arrange(ID)

tibble_sim_output_no_tx %>% 
  group_by(sex_0) %>% 
  summarise(age_0_mean = mean(age_0), LE_mean = mean(age)) %>% 
  mutate(undiscounted_LY = LE_mean - age_0_mean) %>%
  add_column(discounted_LY = c(results_no_tx[[l_sex[1]]]$m_ly, results_no_tx[[l_sex[2]]]$m_ly)) %>%
  mutate(across(where(is.double), ~ format_tab_1(.))) %>% 
  kable(caption = "The life expectancy and life-year results by sex.",
        align = "r")

## -----------------------------------------------------------------------------

tibble_sim_output_no_tx %>% 
  group_by(sex_0) %>% 
  summarise(`Statin cost` = mean(c_tx), `pre-event cost` = mean(c_pre), `post-event cost` = mean(c_post)) %>% 
  add_column(`total cost` = c(results_no_tx[[l_sex[1]]]$m_cost, results_no_tx[[l_sex[2]]]$m_cost)) %>%
  mutate(across(where(is.double), ~ format_tab_2GBP(.))) %>% 
  kable(caption = "The cost results by sex.",
        align = "r")

## -----------------------------------------------------------------------------

tibble_sim_output_no_tx %>% 
  group_by(sex_0) %>% 
  summarise(`pre-event QALY` = mean(q_pre), `post-event QALY` = mean(q_post)) %>% 
  add_column(`total QALY` = c(results_no_tx[[l_sex[1]]]$m_qaly, results_no_tx[[l_sex[2]]]$m_qaly)) %>%
  mutate(across(where(is.double), ~ format_tab_3(.))) %>% 
  kable(caption = "The QALY results by sex.",
        align = "r")

## -----------------------------------------------------------------------------

quintile_grps <- c("Q1 (Least deprived)", "Q2", "Q3", "Q4", "Q5 (Most deprived)")

CEA_by_SIMD_all <- tibble_sim_output_all %>% 
  mutate(`SIMD quintile` = ntile(SIMD_0, 5)) %>%
  pivot_wider(id_cols = c(ID, `SIMD quintile`),
              names_from = arm,
              values_from = c(total_c, total_q),
              names_glue = "{.value}_{arm}") %>%
  mutate(`Incremental cost` = total_c_Treatment - total_c_Control,
         `Incremental QALY` = total_q_Treatment - total_q_Control,
         ICER = `Incremental cost` / `Incremental QALY`,
         `INMB at WTP=£20k` = `Incremental QALY` * 20000 - `Incremental cost`)

CEA_overall_summ <- CEA_by_SIMD_all %>%
  summarise(m_inc_c = mean(`Incremental cost`),
            se_inc_c = sd(`Incremental cost`) / sqrt(n()),
            m_inc_q = mean(`Incremental QALY`),
            se_inc_q = sd(`Incremental QALY`) / sqrt(n()),
            m_inmb = mean(`INMB at WTP=£20k`),
            se_inmb = sd(`INMB at WTP=£20k`) / sqrt(n())) %>%
  mutate(ICER = m_inc_c / m_inc_q,
         `SIMD quintile` = "Overall")

CEA_by_SIMD_summ <- CEA_by_SIMD_all %>%
  group_by(`SIMD quintile`) %>%
  summarise(m_inc_c = mean(`Incremental cost`),
            se_inc_c = sd(`Incremental cost`) / sqrt(n()),
            m_inc_q = mean(`Incremental QALY`),
            se_inc_q = sd(`Incremental QALY`) / sqrt(n()),
            m_inmb = mean(`INMB at WTP=£20k`),
            se_inmb = sd(`INMB at WTP=£20k`) / sqrt(n()))  %>%
  mutate(ICER = m_inc_c / m_inc_q) %>%
  arrange(`SIMD quintile`) %>%
  mutate(`SIMD quintile` = quintile_grps[`SIMD quintile`]) %>%
  bind_rows(CEA_overall_summ) %>%
  mutate(lb_inc_c = m_inc_c - 1.96 * se_inc_c,
         ub_inc_c = m_inc_c + 1.96 * se_inc_c,
         lb_inc_q = m_inc_q - 1.96 * se_inc_q,
         ub_inc_q = m_inc_q + 1.96 * se_inc_q,
         lb_inmb = m_inmb - 1.96 * se_inmb,
         ub_inmb = m_inmb + 1.96 * se_inmb) %>%
  mutate(
    `Mean incremental cost` = paste0(format_tab_0GBP(m_inc_c), 
                                " [", 
                                format_tab_0GBP(lb_inc_c), 
                                ", ", 
                                format_tab_0GBP(ub_inc_c), 
                                "]"),
    `Mean incremental QALY` = paste0(format_tab_3(m_inc_q), 
                                " [", 
                                format_tab_3(lb_inc_q), 
                                ", ", 
                                format_tab_3(ub_inc_q), 
                                "]"),
    `Mean INMB at WTP=£20k` = paste0(format_tab_0GBP(m_inmb), 
                                " [", 
                                format_tab_0GBP(lb_inmb), 
                                ", ", 
                                format_tab_0GBP(ub_inmb), 
                                "]"),
    ICER = paste0(format_tab_0GBP(ICER), "/QALY")
  ) %>%
  dplyr::select(`SIMD quintile`, `Mean incremental cost`, `Mean incremental QALY`, ICER, `Mean INMB at WTP=£20k`)
  

CEA_by_SIMD_summ %>%
  kable(caption = "The base case cost-effectiveness analysis results by deprivation group. Presented in mean of 300,000 entities [95% confidence interval of mean].",
        align = "r", format = "html") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(5, extra_css = "border-bottom: 1px solid #333333;")



## -----------------------------------------------------------------------------
PSA_gen_coef_fn <- function(PSA.iter = 100) {
  
  gen_coef_by_sex <- function(sex) {
    psa_pm_firstE_CHD <- MASS::mvrnorm(
      n = PSA.iter, 
      mu = choose_coef_by_sex(sex, "first_event_coef")[, 1], 
      Sigma = choose_coef_by_sex(sex, "cov_firstE_CHD")
    )
          
    psa_pm_firstE_CBVD <- MASS::mvrnorm(
      n = PSA.iter, 
      mu = choose_coef_by_sex(sex, "first_event_coef")[, 2], 
      Sigma = choose_coef_by_sex(sex, "cov_firstE_CBVD")
    )
          
    psa_pm_fatal_CVD <- MASS::mvrnorm(
      n = PSA.iter, 
      mu = choose_coef_by_sex(sex, "first_event_coef")[, 3], 
      Sigma = choose_coef_by_sex(sex, "cov_fatal_CVD")
    )
    
    psa_pm_fatal_nonCVD <- MASS::mvrnorm(
      n = PSA.iter, 
      mu = choose_coef_by_sex(sex, "first_event_coef")[, 4], 
      Sigma = choose_coef_by_sex(sex, "cov_fatal_nonCVD")
    )

    psa_pm_surv_postCHD <- MASS::mvrnorm(
      n = PSA.iter, 
      mu = choose_coef_by_sex(sex, "post_event_coef")[, 1], 
      Sigma = choose_coef_by_sex(sex, "cov_surv_postCHD")
    )

    psa_pm_surv_postCBVD <- MASS::mvrnorm(
      n = PSA.iter, 
      mu = choose_coef_by_sex(sex, "post_event_coef")[, 2], 
      Sigma = choose_coef_by_sex(sex, "cov_surv_postCBVD")
    )
    
    return(list(psa_pm_firstE_CHD = psa_pm_firstE_CHD,
                psa_pm_firstE_CBVD = psa_pm_firstE_CBVD,
                psa_pm_fatal_CVD = psa_pm_fatal_CVD, 
                psa_pm_fatal_nonCVD = psa_pm_fatal_nonCVD, 
                psa_pm_surv_postCHD = psa_pm_surv_postCHD, 
                psa_pm_surv_postCBVD = psa_pm_surv_postCBVD))
  }
  
  list_coef_psa_pm <- lapply(c("male", "female"),
                             gen_coef_by_sex)
  
  names(list_coef_psa_pm) <- c("male", "female")
  
  return(list_coef_psa_pm)
  
}


list_coef_psa_pm <- PSA_gen_coef_fn(PSA_n_iter)

DES_PSA_fn <- function(i, n_simind, simtime = tm, list_coef_inuse = list_coef) {
  
  sim_cohort <- DES_cohort_gen(n_simind, simtime)
  
  list_coef_psa_draw <- list_coef_inuse
  
  list_coef_psa_draw$first_event_coef_m[ ,2] <- list_coef_psa_pm$male$psa_pm_firstE_CHD[i, ]
  list_coef_psa_draw$first_event_coef_m[ ,3] <- list_coef_psa_pm$male$psa_pm_firstE_CBVD[i, ]
  list_coef_psa_draw$first_event_coef_m[ ,4] <- list_coef_psa_pm$male$psa_pm_fatal_CVD[i, ]
  list_coef_psa_draw$first_event_coef_m[ ,5] <- list_coef_psa_pm$male$psa_pm_fatal_nonCVD[i, ]
  list_coef_psa_draw$post_event_coef_m[ ,2] <- list_coef_psa_pm$male$psa_pm_surv_postCHD[i, ]
  list_coef_psa_draw$post_event_coef_m[ ,3] <- list_coef_psa_pm$male$psa_pm_surv_postCBVD[i, ]
  
  list_coef_psa_draw$first_event_coef_f[ ,2] <- list_coef_psa_pm$female$psa_pm_firstE_CHD[i, ]
  list_coef_psa_draw$first_event_coef_f[ ,3] <- list_coef_psa_pm$female$psa_pm_firstE_CBVD[i, ]
  list_coef_psa_draw$first_event_coef_f[ ,4] <- list_coef_psa_pm$female$psa_pm_fatal_CVD[i, ]
  list_coef_psa_draw$first_event_coef_f[ ,5] <- list_coef_psa_pm$female$psa_pm_fatal_nonCVD[i, ]
  list_coef_psa_draw$post_event_coef_f[ ,2] <- list_coef_psa_pm$female$psa_pm_surv_postCHD[i, ]
  list_coef_psa_draw$post_event_coef_f[ ,3] <- list_coef_psa_pm$female$psa_pm_surv_postCBVD[i, ]
  
  each.iter <- DES_fn(sim_cohort, simtime = simtime, list_coef_psa_draw, i)
  
  n_male_no_tx <- each.iter$`No treatment`$male$n
  m_cost_male_no_tx <- each.iter$`No treatment`$male$m_cost
  m_qaly_male_no_tx <- each.iter$`No treatment`$male$m_qaly
  
  n_female_no_tx <- each.iter$`No treatment`$female$n
  m_cost_female_no_tx <- each.iter$`No treatment`$female$m_cost
  m_qaly_female_no_tx <- each.iter$`No treatment`$female$m_qaly
  
  n_male_tx <- each.iter$`Treatment`$male$n
  m_cost_male_tx <- each.iter$`Treatment`$male$m_cost
  m_qaly_male_tx <- each.iter$`Treatment`$male$m_qaly
  
  n_female_tx <- each.iter$`Treatment`$female$n
  m_cost_female_tx <- each.iter$`Treatment`$female$m_cost
  m_qaly_female_tx <- each.iter$`Treatment`$female$m_qaly
  
  m_cost_all_no_tx <- (m_cost_male_no_tx * n_male_no_tx + m_cost_female_no_tx * n_female_no_tx) / (n_male_no_tx + n_female_no_tx)
  m_qaly_all_no_tx <- (m_qaly_male_no_tx * n_male_no_tx + m_qaly_female_no_tx * n_female_no_tx) / (n_male_no_tx + n_female_no_tx)
  
  m_cost_all_tx <- (m_cost_male_tx * n_male_tx + m_cost_female_tx * n_female_tx) / (n_male_tx + n_female_tx)
  m_qaly_all_tx <- (m_qaly_male_tx * n_male_tx + m_qaly_female_tx * n_female_tx) / (n_male_tx + n_female_tx)
  
  # For results by SIMD quintile
  tibble_output_by_SIMD <- each.iter$`Treatment`$male$mysim_output %>% 
    bind_rows(each.iter$`Treatment`$female$mysim_output) %>% 
    mutate(arm = "Treatment") %>%
    bind_rows(each.iter$`No treatment`$male$mysim_output) %>% 
    bind_rows(each.iter$`No treatment`$female$mysim_output) %>% 
    mutate(arm = if_else(is.na(arm), "Control", arm)) %>%
    dplyr::select(arm, ID, sex_0, SIMD_0, c_pre, c_tx, c_post, q_pre, q_post)%>% 
    mutate(total_c = c_pre + c_tx + c_post,
           total_q = q_pre + q_post) %>%
    arrange(arm, ID) %>%
    mutate(`SIMD quintile` = ntile(SIMD_0, 5)) %>%
    group_by(arm, `SIMD quintile`) %>% 
    summarise(`total cost` = mean(total_c),
              `total QALY` = mean(total_q),
              .groups = "drop") %>%
    pivot_wider(names_from = arm, values_from = c(`total cost`, `total QALY`)) %>%
    mutate(
      `Incremental cost` = `total cost_Treatment` - `total cost_Control`,
      `Incremental QALY` = `total QALY_Treatment` - `total QALY_Control`,
      ICER = `Incremental cost` / `Incremental QALY`,
      `INMB at WTP=£20k` = `Incremental QALY` * 20000 - `Incremental cost`)

  
  
  
  return(list(m_cost_all_no_tx = m_cost_all_no_tx,
              m_qaly_all_no_tx = m_qaly_all_no_tx,
              m_cost_all_tx = m_cost_all_tx,
              m_qaly_all_tx = m_qaly_all_tx,
              tibble_output_by_SIMD = tibble_output_by_SIMD))

}


## -----------------------------------------------------------------------------
# profvis({
# PSA_results <- future_lapply(
#   c(1:100),
#   DES_PSA_fn, 
#   n_simind = sim_cohort_size,
#   future.scheduling = 2,
#   future.seed = TRUE
# )
# })

# profvis({
PSA_results <- lapply(
  c(1:PSA_n_iter),
  DES_PSA_fn, 
  n_simind = sim_cohort_size
)
# })

# profvis({
#   PSA_results <- list()
#   for (i in 1:100) {
#     PSA_results[[i]] <- DES_PSA_fn(i,n_simind = sim_cohort_size)
#   }
# })


PSA_tibble <- do.call(bind_rows, lapply(PSA_results, function(x) {
  tibble(
    cost_no = x$m_cost_all_no_tx,
    qaly_no = x$m_qaly_all_no_tx,
    cost_tx = x$m_cost_all_tx,
    qaly_tx = x$m_qaly_all_tx
  )
})) %>%
  mutate(delta_cost = cost_tx - cost_no,
         delta_qaly = qaly_tx - qaly_no,
         `SIMD quintile` = "Overall")


for (i in 1:5) {
  PSA_tibble_SIMD <- do.call(bind_rows, lapply(PSA_results, function(x) {
    tibble(
      cost_no = x$tibble_output_by_SIMD$`total cost_Control`[i],
      qaly_no = x$tibble_output_by_SIMD$`total QALY_Control`[i],
      cost_tx = x$tibble_output_by_SIMD$`total cost_Treatment`[i],
      qaly_tx = x$tibble_output_by_SIMD$`total QALY_Treatment`[i]
    )
  })) %>%
    mutate(
      delta_cost = cost_tx - cost_no,
      delta_qaly = qaly_tx - qaly_no,
      `SIMD quintile` = quintile_grps[i]
    )
  
  PSA_tibble <- PSA_tibble %>%
    bind_rows(PSA_tibble_SIMD)
}




## -----------------------------------------------------------------------------
wtp <- 20000
PSA_tibble <- PSA_tibble %>%
  mutate(inmb = delta_qaly * wtp - delta_cost)

# Overall
inmb_all <- PSA_tibble %>%
  filter(`SIMD quintile` == "Overall") %>%
  pull(inmb)

n <- length(inmb_all)

# 1) Empirical 95% CI (percentiles)
ci_empirical <- quantile(inmb_all, probs = c(0.025, 0.975))

# 2) Normal‐approximation 95% CI
mean_inmb <- mean(inmb_all)
sd_inmb   <- sd(inmb_all)
ci_normal <- mean_inmb + c(-1, 1) * qnorm(0.975) * sd_inmb

# Output results
print(paste0("INMB at WTP = £", wtp, "/QALY"))
print(paste0("  Mean INMB: ", format_tab_2GBP(mean_inmb)))
print(paste0("  Empirical 95% CI: [", 
             format_tab_2GBP(ci_empirical[1]),
             ", ",
             format_tab_2GBP(ci_empirical[2]),
             "]"))
print(paste0("  Normal-approx  95% CI: [", 
             format_tab_2GBP(ci_normal[1]),
             ", ",
             format_tab_2GBP(ci_normal[2]),
             "]"))

# By SIMD quintile

for (i in 1:5) {
  inmb_SIMD <- PSA_tibble %>%
    filter(`SIMD quintile` == quintile_grps[i]) %>%
    pull(inmb)
  
  n <- length(inmb_SIMD)
  
  # 1) Empirical 95% CI (percentiles)
  ci_empirical <- quantile(inmb_SIMD, probs = c(0.025, 0.975))
  
  # 2) Normal‐approximation 95% CI
  mean_inmb <- mean(inmb_SIMD)
  sd_inmb   <- sd(inmb_SIMD)
  ci_normal <- mean_inmb + c(-1, 1) * qnorm(0.975) * sd_inmb
  
  # Output results
  print(paste0("For SIMD quintile: ", quintile_grps[i]))
  print(paste0("INMB at WTP = £", wtp, "/QALY"))
  print(paste0("  Mean INMB: ", format_tab_2GBP(mean_inmb)))
  print(paste0("  Empirical 95% CI: [", 
               format_tab_2GBP(ci_empirical[1]),
               ", ",
               format_tab_2GBP(ci_empirical[2]),
               "]"))
  print(paste0("  Normal-approx  95% CI: [", 
               format_tab_2GBP(ci_normal[1]),
               ", ",
               format_tab_2GBP(ci_normal[2]),
               "]"))
}



## -----------------------------------------------------------------------------

ce_plane_all <- PSA_tibble %>%
  filter(`SIMD quintile` == "Overall") %>%
  ggplot(aes(x = delta_qaly, y = delta_cost)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.6) +
  labs(
    x = expression(Delta~"QALYs"),
    y = expression(Delta~"Costs"),
    title = "PSA Scatters on Cost-Effectiveness Plane"
  ) +
  theme_minimal(base_size = 14)

print(ce_plane_all)

color_map <- c(
  "Q1 (Least deprived)" = "#1a9850",
  "Q2" = "#66bd63",                  
  "Q3" = "#fed00b",     
  "Q4" = "#f46d43",          
  "Q5 (Most deprived)" = "#d73027"  
)

ce_plane_SIMD <- PSA_tibble %>%
  filter(`SIMD quintile` != "Overall") %>%
  ggplot(aes(x = delta_qaly, y = delta_cost, color = `SIMD quintile`)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = color_map) +
  labs(
    x = expression(Delta~"QALYs"),
    y = expression(Delta~"Costs"),
    title = "Cost-Effectiveness Plane by SIMD Quintile",
    color = "SIMD Quintile"
  ) +
  theme_minimal(base_size = 14)

print(ce_plane_SIMD)


## -----------------------------------------------------------------------------
lambda <- seq(0, 50000, by = 200)

ceac_df <- tibble(
  lambda = lambda,
  p_ce = sapply(lambda, function(wtp) {
    nb_tx <- PSA_tibble$delta_qaly * wtp - PSA_tibble$delta_cost
    mean(nb_tx > 0)
  })
)

ceac_plot_all <- ggplot(ceac_df, aes(x = lambda, y = p_ce)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Willingness-to-pay threshold (\u00A3 per QALY)",
    y = "Probability of treatment being cost-effective",
    title = "Cost-Effectiveness Acceptability Curve"
  ) +
  theme_minimal(base_size = 14)

print(ceac_plot_all)


ceac_df <- PSA_tibble %>%
  filter(`SIMD quintile` != "Overall") %>%
  group_by(`SIMD quintile`) %>%
  group_modify(~ {
    map_dfr(lambda, function(wtp) {
      prob_ce <- mean(.x$delta_qaly * wtp - .x$delta_cost > 0)
      tibble(WTP = wtp, p_ce = prob_ce)
    })
  }) %>%
  ungroup()

ceac_plot_SIMD <- ggplot(ceac_df, aes(x = WTP, y = p_ce, color = `SIMD quintile`)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = color_map) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Willingness-to-pay threshold (\u00A3 per QALY)",
    y = "Probability of treatment being cost-effective",
    title = "Cost-Effectiveness Acceptability Curve",
    color = "SIMD Quintile"
  ) +
  theme_minimal(base_size = 14)

print(ceac_plot_SIMD)

