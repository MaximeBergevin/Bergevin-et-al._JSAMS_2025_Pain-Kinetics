## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
## UNRAVELLING THE PAIN TIME COURSE IN CHRONIC LOW BACK PAIN PATIENTS DURING
## A PHYSICAL MULTIMODAL TRAINING PROGRAM
## Script -- Data preparation
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


data.path <- 'Data/clbpe-pain-time-course.xlsx' # Relative path to spreadsheet

## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#  OUTCOMES ====
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

## Demographics ----
  # Contains demographic measures and basic pain characteristics
    anthropoVar.wide <- readxl::read_excel(
      path = data.path,
      sheet = 'Anthropometric'
    )
  
  ## Physical measures ----
  # Contains physical & functional capacities outcomes
    # Wide format
    physicalVar.wide <- readxl::read_excel(
      path = data.path,
      sheet = 'Physical measures'
      ) %>%
      mutate(group = factor(group, levels = c('Exercise', 'Control')))

    # Long format
    physicalVar.long <- physicalVar.wide %>%
      pivot_longer(
        col = 3:ncol(physicalVar.wide),
        names_to = c('test', 'time'), names_sep = '_', values_to = 'score'
        ) %>%
      mutate(time = factor(time, levels = c('Pre', 'Post', 'Diff')))

## Psychosocial measures ----
  # Contains psychosocial outcomes (i.e., questionnaires)
      # Wide format
      psychosocialVar.wide <- readxl::read_excel(
      path = data.path,
      sheet = 'Psychosocial measures'
      ) %>%
      mutate(group = factor(group, levels = c('Exercise', 'Control')))
    
      # Long format
      psychosocialVar.long <- psychosocialVar.wide %>%
        pivot_longer(
          col = 3:ncol(psychosocialVar.wide),
          names_to = c('test', 'time'), names_sep = '_', values_to = 'score'
          ) %>%
        mutate(time = factor(time, levels = c('Pre', 'Post', 'Diff')))

## All outcomes in one ----
  # Appends all outcome variables in a single data frame
  allOutcomes.long <- physicalVar.long %>%
    full_join(psychosocialVar.long) %>%
           # Removes post-pre deltas that were computed in the xlsx file
    filter(time != 'Diff') %>%
    mutate(
      time = case_when(
        time == 'Pre' ~ 'Pre-program',
        time == 'Post' ~ 'Post-program'),
      time = factor(time, levels = c('Pre-program', 'Post-program'))
      )
  
  # Extract maximal capacity tests (MAP, 1RM)
  maxTest.long <- allOutcomes.long %>%
    filter(test %in% c('Vo2maxMAP', 'Vo2maxMAPkg', 'Training1RMLatPullDownkg',
                       'Training1RMLegPresskg', 'Training1RMChestPresskg'),
           group == 'Exercise')
  
  # Removes maximal capacity tests (MAP, 1RM)
  allOutcomes.long <- allOutcomes.long %>%
    filter(!(test %in% c('Vo2maxMAP', 'Vo2maxMAPkg', 'Training1RMLatPullDown',
                         'Training1RMLegPress', 'Training1RMChestPress',
                         'Training1RMAvg')))
  
## Table 1 data frame ====
  table1.df <- anthropoVar.wide %>%
    full_join(psychosocialVar.wide) %>%
    select(-c('odi_Pre', 'odi_Post',
              'pcs_Pre', 'pcs_Post',
              'tsk_Pre', 'tsk_Post')) %>%
    relocate(AnthropoPainDuration, .after = painAvg7days_Pre) %>%
    relocate(AnthropoSex_Pre, .after = AnthropoPainDuration)
  
## RData on disk ==== 
  
  # Demographic measures in wide format (for table 1)
  save(
    table1.df,
    file = 'Data/Table1.RData'
  )
  
  # All outcomes in long format (for LMM)
  save(
    allOutcomes.long,
    file = 'Data/allOutcomes.RData'
    )
  
  # Maximal tests in long format (for LMM)
  save(
    maxTest.long,
    file = 'Data/maxTest.RData'
  )


## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#  PAIN TIME COURSE ====
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

## Pain time course ----
  # Contains the pain ratings of each participants at each training session
  kinetics.wide <- readxl::read_excel(
    path = data.path,
    sheet = 'Pain kinetics'
  )
  
  kinetics.long <- kinetics.wide %>%
    dplyr::select(- contains(c('ratio', 'group', 'Diff', 'PercentChange'))) %>%
    pivot_longer(cols = 3:86,
                 names_to = c('chronic', 'acute'),
                 names_sep = '_',
                 values_to = 'score') %>%
    left_join(anthropoVar.wide %>% select(id, AnthropoAge_Pre, AnthropoSex_Pre, AnthropoPainDuration, AnthropoBMIScale_Pre)) %>%
    left_join(physicalVar.wide %>% select(id, Vo2maxMAP_Pre, Vo2maxMAPkg_Pre,
                                          Training1RMLegPresskg_Pre, Training1RMChestPresskg_Pre, Training1RMLatPullDown_Pre)) %>%
    left_join(psychosocialVar.wide %>% select(id, painAvg7days_Pre, PainFeelingAvg_Pre, tsk_Pre, odi_Pre, pcs_Pre)) %>%
    mutate(chronic = as.numeric(gsub('Training|_.*', '', chronic))) %>%
    mutate(
      id = factor(id),
      acute = factor(acute, levels = c('Pre', 'Post'),
                     labels = c('Pre-session', 'Post-session')),
      AnthropoSex_Pre = factor(AnthropoSex_Pre)
    )
  
  # Pain kinetics data in long format (for LMM)
  save(
    kinetics.long,
    file = 'Data/painKinetics.RData'
  )

