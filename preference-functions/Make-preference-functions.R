rm(list=ls()); gc()

## -----------------------------------------------------------------------------
##
## Set-up
rm(list=ls()); gc()
library(dplyr)
library(tidyr)

pref_params <- read.csv("data/derived/env-pref-parameters.csv")
dir_out <- "./output-for-ecospace/pref-functions"

## Sumarize into Functional groups
fg_pref <- pref_params %>%
  group_by(variable, FG) %>%
  summarise(
    n_species = n(),
    abs_min       = mean(abs_min, na.rm = TRUE),
    pref_min_10   = mean(pref_min_10, na.rm = TRUE),
    pref_max_90   = mean(pref_max_90, na.rm = TRUE),
    abs_max       = mean(abs_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(variable)

## -----------------------------------------------------------------------------
##
## Logistic function

doublelogistic <- function(max = 400, steps = 400, range = 'wide', min_abs, min_prf, max_prf, max_abs){ 
  #j = 42; max = 30; steps = 1200; range = 'nar'; min_abs = fg_pref$SalinityMin[j]; min_prf = fg_pref$SalinityPrefMin[j]; max_prf = fg_pref$SalinityPrefMax[j]; max_abs = fg_pref$SalinityMax[j]
  #j = 39; max = 30; steps = 1200; range = 'wide'; min_abs = fg_pref$DepthMin[j]; min_prf = fg_pref$DepthPrefMin[j]; max_prf = fg_pref$DepthPrefMax[j]; max_abs = fg_pref$DepthMax[j]
  mid_prf <- min_prf + (max_prf - min_prf) / 2 ## Midpoint. Change from increasing to decreasing logistic function
  mid <- ifelse(mid_prf > max, max, mid_prf)
  step_size <-  max / steps
  x1  <- seq(0, mid-step_size, by = step_size)
  x2  <- seq(mid, max, by = step_size)
  r1  <- min_prf - min_abs 
  r2  <- max_abs - max_prf 
  C1  <- min_abs + r1 / 2
  C2  <- max_prf + r2 / 2
  B1  <- ifelse(range == 'wide', 1/sqrt(r1), 1/log10(r1)) ## If range is 'wide', curve shape inversely proportional to range size
  B2  <- ifelse(range == 'wide', 1/sqrt(r2), 1/log10(r2))
  if(B1 < 0) B1 = Inf
  if(B2 < 0) B2 = Inf
  S  = 1; A1 = 0; D1 = 1; A2 = 1
  
  ## Logistic equations
  f1 <- function(x)     1 / (1 + exp(B1*(C1-x)))^S ## Increasing logistic eq.
  f2 <- function(x) 1 - 1 / (1 + exp(B2*(C2-x)))^S ## Decreasing logistic eq.
  ## 1 = value of the horizontal asymptote when x→−∞
  ## 0 = value of the horizontal asymptote when x→+∞
  ## B describes how rapidly the curve makes its transition between the two asymptotes
  ## S describes the asymmetry of the curve. The curve is symmetric when S=1. 
  ## C is a location parameter, which does not have a nice interpretation, 
  ##   unless S=1 when the curve has an inflection point at x=C.
  ##   In the case when S=1, C is the value of x for which f(x) is the midpoint between the two asymptotes
  
  y1 <- f1(x1)
  y2 <- f2(x2)
  out <- data.frame(x = c(x1, x2), y = c(y1, y2))
  out <- out[1:steps, ] 
  return(out)
}

plot_pref_func <- function(p1, p2, p3, p4, fg_num = "", fg_name,
                           max = 400, xmin = 0, scale_xaxis = 'y', range = 'wide', driver = '') {
  pref_func <- doublelogistic(max = max, steps = max, range = range, p1, p2, p3, p4)
  xlim <- ifelse(p4 < max, p4+p4*0.15, max)
  xmax <- ifelse(scale_xaxis == 'y', xlim, max)
  xmin <- ifelse(scale_xaxis == 'y', p1-p1*0.15, 0)
  plot(pref_func$x, pref_func$y, 
       main = paste(driver, fg_num, fg_name),
       type = "l", ylab = "", xlab = "", cex.main = 1, bty = 'n',
       xlim = c(xmin, xmax), ylim = c(0,1), yaxt='n', lwd=2)
  axis(side = 2, at=c(0,0.5,1))
  abline(v = p1,  col = "red", lty = "dashed")
  abline(v = p2,  col = "blue", lty = "dashed")
  abline(v = p3,  col = "blue", lty = "dashed")
  abline(v = p4,  col = "red", lty = "dashed")
}

## -----------------------------------------------------------------------------
##
## Make matrices for Ecospace

## -----------------------------------------------------------------------------
## Salinity 
driver = "Sal"
range = "nar"
max = 40
n_steps = 1200

## Select and filter variable
var_pref <- 
  fg_pref %>% 
  filter(variable == "salinity") %>%
  drop_na() 
var_pref

## Make preference matrix
pref_mat = matrix(nrow = 1203, ncol = nrow(var_pref))
row.names(pref_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
#colnames(pref_mat) = var_pref$FG

## Loop through functional groupos
for (i in seq_len(nrow(var_pref))){
  #  i = 2
  absmin = var_pref$abs_min[i]
  prfmin = var_pref$pref_min_10[i]
  prfmax = var_pref$pref_max_90[i]
  absmax = var_pref$abs_max[i]
  
  pref_func <- doublelogistic(max = max, steps = n_steps, range = range, 
                              absmin, prfmin, prfmax, absmax)
  
  name = paste0(driver, " - ", var_pref$FG[i])
  print(name)
  outvec = c(name, 0, max, pref_func$y)
  pref_mat[ ,i] = outvec[1:1203]
}

## Write out depth matrix for Ecospace
write.csv(pref_mat, file.path(dir_out, paste0("pref-funcs_", driver,  ".csv")), row.names = T)


## -----------------------------------------------------------------------------
## Make plots

#dir_pdf = "./Ecospace-preference-functions/figures/"
n_rows = 4 ## Number of plots per row on a page
n_cols = 3 ## Number of plots per column on a page
#pg_plts = nwide * nhigh
#n_plots = nrow(fg_pref) * 3
#max_depth = 400
#max_temp = 40
max_sal = 40
w = 8.5
h = 11 

par(mfrow=c(n_rows, n_cols))
for (i in 1:nrow(var_pref)){
  plot_pref_func(var_pref$abs_min[i], var_pref$pref_min_10[i], 
                 var_pref$pref_max_90[i], var_pref$abs_max[i], 
                 fg_name = var_pref$FG[i],
                 max = max_sal, scale_xaxis = 'n', 
                 range = 'nar',
                 driver = "Sal")
}
par(mfrow=c(1, 1))
