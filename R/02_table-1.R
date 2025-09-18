## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
## UNRAVELLING THE PAIN TIME COURSE IN CHRONIC LOW BACK PAIN PATIENTS DURING
## A PHYSICAL MULTIMODAL TRAINING PROGRAM
## Script -- Baseline characteristics (table 1)
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

## LOAD DATA ====
  load(file = 'Data/Table1.RData')

# CONTINUOUS VARIABLE ----

# Set the structure of tabble 1 
table1.tTest <- tibble(
  Characteristics = c(),     # Name of the characteristic
  Exercise = c(),            # Exercise -- Mean (SD)
  Control = c(),             # Control -- Mean (SD)
  p = c()                    # P-value associated with t-test
)

for(rows in 3:7){
  # Runs a t-test on table1.df from col3 to col7
  .m1 <- lm(data = table1.df, formula = table1.df[[rows]] ~ group)
  
  # Create a one-row column with group info and p values
  .df <- tibble(
    Characteristics = gsub('_Pre', '', names(table1.df[rows])),
    Exercise =   paste0(
      round(data.frame(emmeans::emmeans(.m1, ~ group)[2])[2],2), ' (', # Mean
      round(sd(subset(table1.df, group == 'Exercise')[[rows]]),2),')'  # SD
    ),
    Control = paste0(
      round(data.frame(emmeans::emmeans(.m1, ~ group)[1])[2],2), ' (', # Mean
      round(sd(subset(table1.df, group == 'Control')[[rows]]),2),')'   # SD
    ),
    p = round(summary(.m1)[[4]][8],3) # p-value
  )
  
  # Binds summary info at the bottom of table1.tTest
  table1.tTest <- rbind(table1.tTest, .df)
  
  rm(.m1, .df, rows) # Removes .m1 from the environment
}

# CATEGORIAL VARIABLES ----

  ## Pain Duration ----
  painDuration.summary <- table1.df %>%
    group_by(group) %>%
    count(AnthropoPainDuration)
  
  painDuration.chisq <- chisq.test(
    x = table1.df$AnthropoPainDuration,
    y = table1.df$group
  )
  
  
  ## Sex ----
  sex.summary <- table1.df %>%
    group_by(group) %>%
    count(AnthropoSex_Pre)
  
  sex.chisq <- chisq.test(
    x = table1.df$AnthropoSex_Pre,
    y = table1.df$group
  )
  