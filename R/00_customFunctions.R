## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
## UNRAVELLING THE PAIN TIME COURSE IN CHRONIC LOW BACK PAIN PATIENTS DURING
## A PHYSICAL MULTIMODAL TRAINING PROGRAM
## Script -- Custom functions
## 
## Author: Maxime Bergevin, MSc, PhD student 🧠
## At the time of writing, MSc student in the ELPN LAB
## School of kinesiology, Universite de Montreal, Canada
## Research center of Montreal's Geriatric Institutes, Canada
## Supervised by: 
## Benjamin Pageaux (Kinesiology, Universite de Montreal)
## Mathieu Roy (Psychology, McGill University)
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Most packages are not explicitly loaded, save a few exceptions
# Every other package is explicit when calling their methods
library(tidyverse) # Necessary for pipes (%>%)

## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#  CUSTOM FUNCTIONS ====
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Function given by Ben Bolker on Cross Validated to compute CIs from rlmer
# https://stats.stackexchange.com/questions/233800/how-can-i-get-confidence-intervals-for-fixed-effects-using-the-rlmer-function-r
  confint.rlmerMod <- function(object,
                               parm,
                               level=0.95) {
    beta <- fixef(object)
    if (missing(parm)) parm <- names(beta)
    se <- sqrt(diag(vcov(object)))
    z <- qnorm((1+level)/2)
    ctab <- cbind(beta-z*se,beta+z*se)
    # Slight modification to Ben Bolker's code to name columns
    colnames(ctab) <- c(paste0(((1-level)/2)*100, '%'),
                        paste0(((1+level)/2)*100, '%'))
    #return(ctab[parm,])
    return(ctab)
  }
  
## Save function on disk
  save(
    confint.rlmerMod,
    file = 'Data/confint-rlmerMod.RData'
  )


  bootstrap.lmerMod <- function(model, n_bootstrap = 2000, conf_level = 0.95, seed = 123, verbose = TRUE) {
    # Check if the model is of class 'lmerMod'
    if (!inherits(model, "lmerMod")) {
      stop("The model must be of class 'lmerMod'")
    }
    
    # Set seed for reproducibility
    set.seed(seed)
    
    # Define a function to extract fixed effects
    fixef_fun <- function(fit) {
      return(fixef(fit))
    }
    
    # Perform the bootstrap using bootMer
    boot_results <- lme4::bootMer(model, fixef_fun, nsim = n_bootstrap, seed = seed, parallel = "multicore")
    
    # Extract bootstrapped confidence intervals for each coefficient
    boot_ci <- apply(boot_results$t, 2, function(x) {
      boot::boot.ci(boot::boot(x, function(data, indices) mean(data[indices]), R = n_bootstrap), type = "perc", conf = conf_level)
    })
    
    # Convert boot_ci to a more convenient format
    ci_lower <- sapply(boot_ci, function(ci) ci$percent[4])
    ci_upper <- sapply(boot_ci, function(ci) ci$percent[5])
    
    # Compute bootstrapped p-values
    p_values <- apply(boot_results$t, 2, function(x) {
      2 * min(mean(x <= 0), mean(x >= 0))
    })
    
    # Create significance symbols based on p-values
    signif_codes <- cut(p_values, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                        labels = c("***", "**", "*", ".", " "), right = FALSE)
    
    # Create the summary table
    summary_table <- data.frame(
      Term = names(fixef(model)),  # Names of the fixed effects
      `Estimate` = fixef(model),  # Estimates of the fixed effects
      `Std. Error` = sqrt(diag(vcov(model))),  # Standard errors of the estimates
      `CI Lower` = ci_lower,  # Lower bound of the confidence intervals
      `CI Upper` = ci_upper,  # Upper bound of the confidence intervals
      `p.value` = p_values,  # P-values
      `Signif.` = signif_codes  # Significance codes
    )
    
    # Format the summary table
    summary_table[] <- lapply(summary_table, function(x) {
      if (is.numeric(x)) {
        formatC(x, digits = 5, format = "fg")
      } else {
        x
      }
    })
    
    return(summary_table)  # Return the summary table
  }
  
  
  save(
    bootstrap.lmerMod,
    file = 'Data/bootstrap.lmerMod.RData'
  )
  
  
 
  bootstrap.lmerMod <- function(model, n_bootstrap = 2000, conf_level = 0.95, seed = 123, verbose = TRUE) {
    # Check if the model is of class 'lmerMod'
    if (!inherits(model, "lmerMod")) {
      stop("The model must be of class 'lmerMod'")
    }
    
    # Set seed for reproducibility
    set.seed(seed)
    
    # Perform the bootstrap using lme4::bootMer
    boot_results <- bootMer(model, FUN = fixef, nsim = n_bootstrap, seed = seed, parallel = "multicore")
    
    # Use merTools to get bootstrapped confidence intervals
    ci_boot <- confint.merMod(model, method = "boot", nsim = n_bootstrap, boot.type = "perc", parallel = "multicore", ncpus = detectCores() - 1)
    
    # Extract the lower and upper bounds of the confidence intervals
    ci_lower <- ci_boot[,"2.5 %"]
    ci_upper <- ci_boot[,"97.5 %"]
    
    # Compute p-values using merTools::PBmodcomp
    # Fit the null model (without the fixed effect term)
    null_model <- update(model, . ~ . - (group * time))
    
    # Compute parametric bootstrap p-values
    pb_modcomp <- merTools::PBmodcomp(model, null_model, nsim = n_bootstraps)
    p_values <- pb_modcomp$test$p.value
    
    # Create significance symbols based on p-values
    signif_codes <- cut(p_values, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                        labels = c("***", "**", "*", ".", " "), right = FALSE)
    
    # Create the summary table
    summary_table <- data.frame(
      Term = names(fixef(model)),  # Names of the fixed effects
      Estimate = fixef(model),  # Estimates of the fixed effects
      `Std. Error` = sqrt(diag(vcov(model))),  # Standard errors of the estimates
      `CI Lower` = ci_lower,  # Lower bound of the confidence intervals
      `CI Upper` = ci_upper,  # Upper bound of the confidence intervals
      `p.value` = p_values,  # P-values
      `Signif.` = signif_codes  # Significance codes
    )
    
    # Format the summary table
    summary_table[] <- lapply(summary_table, function(x) {
      if (is.numeric(x)) {
        formatC(x, digits = 5, format = "fg")
      } else {
        x
      }
    })
    
    return(summary_table)  # Return the summary table
  }
