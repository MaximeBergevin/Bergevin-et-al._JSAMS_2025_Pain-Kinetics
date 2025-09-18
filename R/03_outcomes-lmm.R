## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
## UNRAVELLING THE PAIN TIME COURSE IN CHRONIC LOW BACK PAIN PATIENTS DURING
## A PHYSICAL MULTIMODAL TRAINING PROGRAM
## Script -- Outcomes
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
  library(lme4)       # Necessary to run robustlmm:rlmer
  library(tidyverse)  # Necessary for pipes (%>%)
  library(ggprism)    # For nice graphs
  library(cowplot)    # Had better result than with patchwork...
  
  

## LOAD DATA & CUMSTON FUNCTIONS ====
  load(file = 'Data/allOutcomes.RData')       # Data for all outcomes (long format)
  load(file = 'Data/maxTest.RData')           # Data for maximal capacity tests (long format)

  
  
## SETITING FIXED EFFECT CONTRASTS ----
  ## Set R's default behavior to use treatment contrasts on lmer/rlmer
  ## Zero-sum contrasts are more appropriate in mixed designs
  ## See `An Introduction to Mixed Models for Experimental Psychology`
  afex::set_sum_contrasts()
  
  ## NOTE: for all models: there is no point in trying to fit a maximal random
  ## structure, or even random slopes, as we only have one observations per 
  ## time point. In this case, we directly fitted random-intercepts only model.
  
  
  
## POST-HOC CONTRASTS ====
  ## To be used with emmeans::emmeans to compute post-hoc analyses
  ## 1) Group comparisons at PRE and POST intervention
  ## 2) Time comparisons for EXERC and CTRL
  contr.int <- list(
    "Exerc - Ctrl (pre)"  = c(1,-1,0,0), # Baseline value per group
    "Exerc - Ctrl (post)" = c(0,0,1,-1), # End-protocol value per group
    "Pre - Post (Exerc)"  = c(1,0,-1,0), # Change in EXERC group
    "Pre - Post (Ctrl)"   = c(0,1,0,-1)  # Change in CTRL group
  )
  
  
  
## MAXIMAL CAPACITY TESTS (EXP only) ----
  
  ### 1RM -- Leg Press ----
  legPress.lmer <- lmerTest::lmer(
    data = maxTest.long %>% filter(test == 'Training1RMLegPresskg'),
    formula = score ~ time + (1|id)
  ) # Successfully converged
  summary(legPress.lmer)
  data.frame(emmeans::emmeans(legPress.lmer, ~time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(legPress.lmer)) # OK
  
  # Model table
  sjPlot::tab_model(legPress.lmer,
                    dv.labels = c('1RM - Leg press'),
                    pred.labels = c('Intercept', 'Time'),
                    df.method = 's')
  
  
  ### 1RM -- Chest Press ----
  chestPress.lmer <- lmerTest::lmer(
    data = maxTest.long %>% filter(test == 'Training1RMChestPresskg'),
    formula = score ~ time + (1|id)
  ) # Successfully converged
  summary(chestPress.lmer)
  data.frame(emmeans::emmeans(chestPress.lmer, ~time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(chestPress.lmer)) # Deviation from normality
  chestPress.rlmer <- robustlmm::rlmer(
    data = maxTest.long %>% filter(test == 'Training1RMChestPresskg'),
    formula = score ~ time + (1|id)
  )
  
  # Model table -
  ## Similar results between standard and robust models
  sjPlot::tab_model(chestPress.lmer,chestPress.rlmer,
                    dv.labels = c('1RM - Chest press (standard)', '1RM - Chest press (robust)'),
                    pred.labels = c('Intercept', 'Time'),
                    df.method = 's')
  
  
  ### 1RM -- Lateral Pulldown ----
  latPulldown.lmer <- lmerTest::lmer(
    data = maxTest.long %>% filter(test == 'Training1RMLatPullDownkg'),
    formula = score ~ time + (1|id)
  ) # Sucessfully converged
  summary(latPulldown.lmer)
  data.frame(emmeans::emmeans(latPulldown.lmer, ~time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(latPulldown.lmer)) # OK
  
  # Model table
  sjPlot::tab_model(latPulldown.lmer,
                    dv.labels = c('1RM - Lateral Pulldown'),
                    pred.labels = c('Intercept', 'Time'),
                    df.method = 's')
  
  
  # Maximal Aerobic Power (W/kg)----
  mapRelative.lmer <- lmerTest::lmer(
    data = maxTest.long %>% filter(test == 'Vo2maxMAPkg'),
    formula = score ~ time + (1|id)
  ) # Sucessfully converged
  summary(mapRelative.lmer)
  data.frame(emmeans::emmeans(mapRelative.lmer, ~time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(mapRelative.lmer))
  mapRelative.rlmer <- robustlmm::rlmer(
    data = maxTest.long %>% filter(test == 'Vo2maxMAPkg'),
    formula = score ~ time + (1|id)
  )
  
  # Model table
  ## Similar results between standard and robust models
  sjPlot::tab_model(mapRelative.lmer, mapRelative.rlmer,
                    dv.labels = c('MAP (standard)', 'MAP (robust)'),
                    pred.labels = c('Intercept', 'Time'),
                    df.method = 's')
  
  
  # Maximal Aerobic Power (W) ---- 
  map.lmer <- lmerTest::lmer(
    data = maxTest.long %>% filter(test == 'Vo2maxMAP'),
    formula = score ~ time + (1|id)
  ) # Sucessfully converged
  summary(map.lmer)
  data.frame(emmeans::emmeans(map.lmer, ~time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(map.lmer)) # OK

  # Model table
  sjPlot::tab_model(map.lmer,
                    dv.labels = c('MAP'),
                    pred.labels = c('Intercept', 'Time'),
                    df.method = 's')
  
  
  
## CARDIORESPIRATORY FITNESS ----
  
  ### Submaximal Exercise Test  ----
  submaxPower.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'Vo2Submax85Power'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(submaxPower.lmer)
  data.frame(emmeans::emmeans(submaxPower.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(submaxPower.lmer)) # OK
  
  # Model table & info
  sjPlot::tab_model(submaxPower.lmer,
                    dv.labels = c('Submaximal exercise test'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  submaxPower.emm <- emmeans::emmeans(submaxPower.lmer, ~ group * time,
                                       contr = list(contr.int), lmer.df = 's')
  confint(submaxPower.emm[[2]])
  
  
  ### 6-min Walk Test ----
  sixMin.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% 
              filter(test == 'T6MWDistance') %>%
              filter(id != 'CLBPE033'), # Removed, did POST without shoes...
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(sixMin.lmer)
  data.frame(emmeans::emmeans(sixMin.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(sixMin.lmer)) # OK
  
  # Model table & info
  sjPlot::tab_model(sixMin.lmer,
                    dv.labels = c('6-min walk test'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'))
  
  sixMin.emm <- emmeans::emmeans(sixMin.lmer, ~ group * time,
                                  contr = list(contr.int), lmer.df = 's')
  confint(sixMin.emm[[2]])
  
  
  
## MUSCLE STRENGTH & POWER ----
  
  ### Handgrip ----
  handgrip.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'HandgripBestSum'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(HandgripBestSum)
  data.frame(emmeans::emmeans(handgrip.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(handgrip.lmer))
  handgrip.rlmer <- robustlmm::rlmer(
    data = allOutcomes.long %>% filter(test == 'HandgripBestSum'),
    formula = score ~ group * time + (1|id)
  )
  
  # Model table & info
  # Estimates and globally similar - effect of time no longer significant
  sjPlot::tab_model(handgrip.lmer, handgrip.rlmer,
                    dv.labels = c('Handgrip'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  
  ### Chairstand ----
  chairstand.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'Chairstand30sec'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(chairstand.lmer)
  data.frame(emmeans::emmeans(chairstand.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(chairstand.lmer))
  chairstand.rlmer <- robustlmm::rlmer(
    data = allOutcomes.long %>% filter(test == 'Chairstand30sec'),
    formula = score ~ group * time + (1|id)
  )

  # Model table & info
  ## Similar results between standard and robust models
  sjPlot::tab_model(chairstand.lmer, chairstand.rlmer,
                    dv.labels = c('Chairstand (standard)', 'Chairstand (robust)'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
    
  
## SHORT WALKING TESTS ----
  
  ### Timed Up-and-go (comfortable) ----
  tugComfortable.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'TUGUGAvg'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(tugComfortable.lmer)
  data.frame(emmeans::emmeans(tugComfortable.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(tugComfortable.lmer))
  tugComfortable.rlmer <- robustlmm::rlmer(
    data = allOutcomes.long %>% filter(test == 'TUGUGAvg'),
    formula = score ~ group * time + (1|id)
  )
  
  # Model table & info
  ## Similar results between standard and robust models
  sjPlot::tab_model(tugComfortable.lmer, tugComfortable.rlmer,
                    dv.labels = c('TUG (standard)', 'TUG (robust)'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  
  ### Timed up-and-go (maximal) ----
 tugMaximal.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'TUGFGAvg'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(tugMaximal.lmer)
  data.frame(emmeans::emmeans(tugMaximal.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(tugMaximal.lmer))
  tugMaximal.rlmer <- robustlmm::rlmer(
    data = allOutcomes.long %>% filter(test == 'TUGFGAvg'),
    formula = score ~ group * time + (1|id)
  )
  
  # Model table & info
  ## Similar results between standard and robust models
  sjPlot::tab_model(tugMaximal.lmer, tugMaximal.rlmer,
                    dv.labels = c('TUG (standard)', 'TUG (robust)'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  
  ### 10-min Walk Test (comfortable) ----
  tenMeterComfortable.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'WT10mUGAvg') %>%
      mutate(score = 10/score), # Convert time (s) in velocity (m/s)
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(tenMeterComfortable.lmer)
  data.frame(emmeans::emmeans(tenMeterComfortable.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(tenMeterComfortable.lmer))
  
  # Model table & info
  sjPlot::tab_model(tenMeterComfortable.lmer,
                    dv.labels = c('10-m Walk Test'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  ### 10-min Walk Test (maximal) ----
  tenMeterMaximal.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'WT10mFGAvg') %>%
      mutate(score = 10/score), # Convert time (s) in velocity (m/s)
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(tenMeterMaximal.lmer)
  data.frame(emmeans::emmeans(tenMeterMaximal.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(tenMeterMaximal.lmer))
  
  # Model table & info
  sjPlot::tab_model(tenMeterMaximal.lmer,
                    dv.labels = c('10-m Walk Test'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  

  
## PSYCHOSOCIAL AND PAIN ----
  
  ### Average Pain (last 7 days) ----
  painAvg.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'painAvg7days'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
    summary(painAvg.lmer)
    data.frame(emmeans::emmeans(painAvg.lmer, ~group*time, lmer.df = 'S'))
    
    # Diagnostics
    plot(DHARMa::simulateResiduals(painAvg.lmer))
    
    # Model table & info
    sjPlot::tab_model(painAvg.lmer,
                      dv.labels = c('Average pain (last 7 days)'),
                      pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                      df.method = 's')
    
    painAvg.emm <- emmeans::emmeans(painAvg.lmer, ~ group * time,
                                        contr = list(contr.int), lmer.df = 's')
    confint(painAvg.emm[[2]])
  
  simr::powerSim(painAvg.lmer,
                 simr::fixed("group1:time1", method = c("t")),
                             seed = 123)
  
  ### Test-evoked Pain ----
  evokedPain.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'PainFeelingAvg'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(evokedPain.lmer)
  data.frame(emmeans::emmeans(evokedPain.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(evokedPain.lmer))
  
  # Model table & info
  sjPlot::tab_model(evokedPain.lmer,
                    dv.labels = c('Test-evoked pain'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  evokedPain.emm <- emmeans::emmeans(evokedPain.lmer, ~ group * time,
                                  contr = list(contr.int), lmer.df = 's')
  confint(evokedPain.emm[[2]])
  
  
  ### Oswestry Disability Index ----
  odi.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'odi'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(odi.lmer)
  data.frame(emmeans::emmeans(odi.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(odi.lmer))
  
  # Model table & info
  sjPlot::tab_model(odi.lmer,
                    dv.labels = c('Oswestry Disability Index'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  odi.emm <- emmeans::emmeans(odi.lmer, ~ group * time,
                                     contr = list(contr.int), lmer.df = 's')
  confint(odi.emm[[2]])
  
  ### Tampa Scale of Kinesiophobia ----
  tsk.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'tsk'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(tsk.lmer)
  data.frame(emmeans::emmeans(tsk.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(tsk.lmer))
  
  # Model table & info
  sjPlot::tab_model(tsk.lmer,
                    dv.labels = c('Tampa Scale of Kinesiophobia'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  tsk.emm <- emmeans::emmeans(tsk.lmer, ~ group * time,
                              contr = list(contr.int), lmer.df = 's')
  confint(tsk.emm[[2]])
  
  
  ### Pain Catastrophization Scale ----
  pcs.lmer <- lmerTest::lmer(
    data = allOutcomes.long %>% filter(test == 'pcs'),
    formula = score ~ group * time + (1|id)
  ) # Successfully converged
  summary(pcs.lmer)
  data.frame(emmeans::emmeans(pcs.lmer, ~group*time, lmer.df = 'S'))
  
  # Diagnostics
  plot(DHARMa::simulateResiduals(pcs.lmer))
  
  # Model table & info
  sjPlot::tab_model(pcs.lmer,
                    dv.labels = c('Pain Catastrophizing Scale'),
                    pred.labels = c('Intercept', 'Group', 'Time', 'Group ⨉ Time'),
                    df.method = 's')
  
  pcs.emm <- emmeans::emmeans(pcs.lmer, ~ group * time,
                              contr = list(contr.int), lmer.df = 's')
  confint(pcs.emm[[2]])
  
  
## RAINCLOUD PLOTS ----
  
  ## Group colors
  # Exercise = blue; Control = yellow
  group.colors <- c(Exercise = '#0a4c9e',
                    Control = '#E69F00')
  
  # Group shapes
  # Exercise = squares; Control = triangles
  group.shapes <- c(Exercise = 15,
                    Control = 17)
  
  ### Average Pain (last 7 days) ----
  #### Density plot PRE ----
  nihPainDensity.pre <- ggplot2::ggplotGrob(
    allOutcomes.long %>%
      filter(test == 'painAvg7days', time == 'Pre-program') %>%
      mutate(group = factor(group, levels = c('Control', 'Exercise'))) %>%
      ggplot(aes(x = score, fill = group))+
      geom_density(alpha = 0.5) +
      coord_flip() +
      scale_y_reverse() +
      scale_x_continuous(limits = c(0,10)) +
      scale_fill_manual(values = group.colors) +
      theme_void() +
      theme(legend.position = 'none')
  )
  
  #### Density plot POST ----
  nihPainDensity.post <- ggplot2::ggplotGrob(
    allOutcomes.long %>%
      filter(test == 'painAvg7days', time == 'Post-program') %>%
      mutate(group = factor(group, levels = c('Control', 'Exercise'))) %>%
      ggplot(aes(x = score, fill = group))+
      geom_density(alpha = 0.5) +
      coord_flip() +
      scale_x_continuous(limits = c(0,10)) +
      scale_fill_manual(values = group.colors) +
      theme_void() +
      theme(legend.position = 'none')
  )
  
  #### Raincloud plot ----
  nihPainRaincoud.plot <- ggplot(data.frame(painAvg.emm[1]), aes(x = emmeans.time, y = emmeans.emmean)) +
    geom_point(alpha = 0) + # invisible points to set the X-axis labels...
    ## ESTIMATED MARGINAL MEANS & ERROR BARS
    # Exercise group
    geom_segment(aes(x = 1.1, xend = 2.1, y = 4.93, yend = 2.58)) +
    geom_errorbar(aes(x = 1.1, ymin = 4.3, ymax = 5.55), width = 0.1) +
    geom_point(aes(x = 1.1, y = 4.93), color = '#0a4c9e', shape = 15, size = 3) +
    geom_errorbar(aes(x = 2.1 , ymin = 1.94, ymax = 3.21), width = 0.1) +
    geom_point(aes(x = 2.1, y = 2.58), color = '#0a4c9e', shape = 15, size = 3) +
    # Control group
    geom_segment(aes(x = 0.9, xend = 1.9, y = 5.62, yend = 5.31)) +
    geom_errorbar(aes(x = 0.9, ymin = 5.01, ymax = 6.24), width = 0.1) +
    geom_point(aes(x = 0.9, y = 5.62), color = '#E69F00', shape = 17, size = 3) +
    geom_errorbar(aes(x = 1.9 , ymin = 4.68, ymax = 5.94), width = 0.1) +
    geom_point(aes(x = 1.9, y = 5.31), color = '#E69F00', shape = 17, size = 3) +
    ## DENSITY PLOTS
    annotation_custom(grob = nihPainDensity.pre, xmin = 0.5, xmax = 0.7) +
    annotation_custom(grob = nihPainDensity.post, xmin = 2.3, xmax = 2.5) +
    # SCALE ADJUSTMENTS
    scale_y_continuous(limits = c(0, 10), breaks = seq(0,10, length.out = 5), guide = 'prism_offset') +
    # ANNOTATIONS
    ylab('Pain intensity') +
    geom_segment(aes(x = 1.9, xend = 2.1, y = 6.25, yend = 6.25), color = 'black') +
    annotate('text', x = 2, y = 6.5, label = "***",  color = 'black', size = 5, fontface = 'bold') +
    # AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_line(size = 0.25),
      axis.ticks.y = element_line(size = 0.25),
      axis.text.x = element_text(size = 10),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 13)
    )
  
  #### Exercise plot ----
  nihPainExerc.plot <- allOutcomes.long %>% filter(group == 'Exercise',
                                                   test == 'painAvg7days') %>%
    ggplot(aes(x = time, y = score,
               color = group, shape = group, group = id)) +
    ## INDIVIDUAL POINTS & LINES
    geom_line(size = 0.5, alpha = 0.25) +
    geom_point(alpha = 0.25) +
    ## GROUP MEANS
    geom_errorbar(aes(x = 0.75, ymin = 4.3, ymax = 5.55), width = 0.1, color = 'black') +
    geom_point(aes(x = 0.75, y = 4.93), color = '#0a4c9e', shape = 15, size = 3) +
    geom_errorbar(aes(x = 2.25 , ymin = 1.94, ymax = 3.21), width = 0.1, color = 'black') +
    geom_point(aes(x = 2.25, y = 2.58), color = '#0a4c9e', shape = 15, size = 3) +
    ## GROUP AESTHETHICS
    scale_color_manual(values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0,10, length.out = 5), guide = 'prism_offset') +
    ## TITLES & LABELS
    ggtitle('Exercise') +
    ylab('') +
    # ANNOTATIONS
    annotate('text', x = 1.5, y = 7.5, label = "***",  color = 'black', size = 5, fontface = 'bold') +
    ## AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(), 
      plot.title = element_text(size = 15, face = 'bold'),
      axis.line.x = element_line(size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  #### Waiting List plot ----
  nihPainCtrl.plot <- allOutcomes.long  %>% filter(group == 'Control',
                                                   test == 'painAvg7days') %>%
    ggplot(aes(x = time, y = score,
               color = group, shape = group, group = id)) +
    ## INDIVIDUAL POINTS & LINES
    geom_line(size = 0.5, alpha = 0.25) +
    geom_point(alpha = 0.25) +
    ## GROUP MEANS
    geom_errorbar(aes(x = 0.75, ymin = 5.01, ymax = 6.24), width = 0.1, color = 'black') +
    geom_point(aes(x = 0.75, y = 5.62), color = '#E69F00', shape = 17, size = 3) +
    geom_errorbar(aes(x = 2.25 , ymin = 4.68, ymax = 5.94), width = 0.1, color = 'black') +
    geom_point(aes(x = 2.25, y = 5.31), color = '#E69F00', shape = 17, size = 3) +
    ## GROUP AESTHETHICS
    scale_color_manual(values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_y_continuous(limits = c(0, 10), guide = 'prism_offset') +
    ## TITLES & LABELS
    ggtitle('Control') +
    ylab('') +
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(size = 15, face = 'bold'),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.line.x = element_line(size = 0.25),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  
  ### Oswestry Disability Index ----
  #### Density plot PRE ----
  odiDensity.pre <- ggplot2::ggplotGrob(
    allOutcomes.long %>%
      filter(test == 'odi', time == 'Pre-program') %>%
      mutate(group = factor(group, levels = c('Control', 'Exercise'))) %>%
      ggplot(aes(x = score, fill = group))+
      geom_density(alpha = 0.5) +
      coord_flip() +
      scale_y_reverse() +
      scale_x_continuous(limits = c(0,50)) +
      scale_fill_manual(values = group.colors) +
      theme_void() +
      theme(legend.position = 'none')
  )
  
  #### Density plot POST ----
  odiDensity.post <- ggplot2::ggplotGrob(
    allOutcomes.long %>%
      filter(test == 'odi', time == 'Post-program') %>%
      mutate(group = factor(group, levels = c('Control', 'Exercise'))) %>%
      ggplot(aes(x = score, fill = group))+
      geom_density(alpha = 0.5) +
      coord_flip() +
      scale_x_continuous(limits = c(0,50)) +
      scale_fill_manual(values = group.colors) +
      theme_void() +
      theme(legend.position = 'none')
  )
  
  #### Raindcloud plot ----
  odiRaincoud.plot <- ggplot(data.frame(odi.emm[1]), aes(x = emmeans.time, y = emmeans.emmean)) +
    geom_point(alpha = 0) + # invisible points to set the X-axis labels...
    ## ESTIMATED MARGINAL MEANS & ERROR BARS
    # Exercise group
    geom_segment(aes(x = 1.1, xend = 2.1, y = 20.4, yend = 12.3)) +
    geom_errorbar(aes(x = 1.1, ymin = 15.9, ymax = 25.0), width = 0.1) +
    geom_point(aes(x = 1.1, y = 20.4), color = '#0a4c9e', shape = 15, size = 3) +
    geom_errorbar(aes(x = 2.1 , ymin = 7.7, ymax = 16.9), width = 0.1) +
    geom_point(aes(x = 2.1, y = 12.3), color = '#0a4c9e', shape = 15, size = 3) +
    # Control group
    geom_segment(aes(x = 0.9, xend = 1.9, y = 28.0, yend = 27.0)) +
    geom_errorbar(aes(x = 0.9, ymin = 23.5, ymax = 32.4), width = 0.1) +
    geom_point(aes(x = 0.9, y = 28.0), color = '#E69F00', shape = 17, size = 3) +
    geom_errorbar(aes(x = 1.9 , ymin = 22.5, ymax = 31.6), width = 0.1) +
    geom_point(aes(x = 1.9, y = 27.0), color = '#E69F00', shape = 17, size = 3) +
    ## DENSITY PLOTS
    annotation_custom(grob = odiDensity.pre, xmin = 0.5, xmax = 0.7) +
    annotation_custom(grob = odiDensity.post, xmin = 2.3, xmax = 2.5) +
    # SCALE ADJUSTMENTS
    scale_y_continuous(limits = c(0,50), breaks = seq(0,50, length.out = 5), guide = 'prism_offset') +
    # ANNOTATIONS
    ylab('Disability') +
    geom_segment(aes(x = 0.9, xend = 1.1, y = 34, yend = 34), color = 'black') +
    annotate('text', x = 1, y = 35, label = "*",  color = 'black', size = 5, fontface = 'bold') +
    geom_segment(aes(x = 1.9, xend = 2.1, y = 34, yend = 34), color = 'black') +
    annotate('text', x = 2, y = 35, label = "***",  color = 'black', size = 5, fontface = 'bold') +
    # AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_line(size = 0.25),
      axis.ticks.y = element_line(size = 0.25),
      axis.text.x = element_text(size = 10),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 13)
    )
  
  #### Exercise plot ----
  odiExerc.plot <- allOutcomes.long %>% filter(group == 'Exercise',
                                               test == 'odi') %>%
    ggplot(aes(x = time, y = score,
               color = group, shape = group, group = id)) +
    ## INDIVIDUAL POINTS & LINES
    geom_line(size = 0.5, alpha = 0.25) +
    geom_point(alpha = 0.25) +
    ## GROUP MEANS
    geom_errorbar(aes(x = 0.75, ymin = 15.9, ymax = 25.0), width = 0.1, color = 'black') +
    geom_point(aes(x = 0.75, y = 20.4), color = '#0a4c9e', shape = 15, size = 3) +
    geom_errorbar(aes(x = 2.25 , ymin = 7.7, ymax = 16.9), width = 0.1, color = 'black') +
    geom_point(aes(x = 2.25, y = 12.3), color = '#0a4c9e', shape = 15, size = 3) +
    ## GROUP AESTHETHICS
    scale_color_manual(values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_y_continuous(limits = c(0,50), breaks = seq(0,50, length.out = 5), guide = 'prism_offset') +
    ## TITLES & LABELS
    ylab('') +
    # ANNOTATIONS
    annotate('text', x = 1.5, y = 40, label = "***",  color = 'black', size = 5, fontface = 'bold') +
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(), 
      plot.title = element_text(size = 15, face = 'bold'),
      axis.line.x = element_line(size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  #### Waiting List plot ----
  odiCtrl.plot <- allOutcomes.long  %>% filter(group == 'Control',
                                               test == 'odi') %>%
    ggplot(aes(x = time, y = score,
               color = group, shape = group, group = id)) +
    ## INDIVIDUAL POINTS & LINES
    geom_line(size = 0.5, alpha = 0.25) +
    geom_point(alpha = 0.25) +
    ## GROUP MEANS
    geom_errorbar(aes(x = 0.75, ymin = 23.5, ymax = 32.4), width = 0.1, color = 'black') +
    geom_point(aes(x = 0.75, y = 28.0), color = '#E69F00', shape = 17, size = 3) +
    geom_errorbar(aes(x = 2.25 , ymin = 22.5, ymax = 31.6), width = 0.1, color = 'black') +
    geom_point(aes(x = 2.25, y = 27.0), color = '#E69F00', shape = 17, size = 3) +
    ## GROUP AESTHETHICS
    scale_color_manual(values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_y_continuous(limits = c(0,50), breaks = seq(0,50, length.out = 5), guide = 'prism_offset') +
    ## TITLES & LABELS
    ylab('') +
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(), 
      plot.title = element_text(size = 15, face = 'bold'),
      axis.line.x = element_line(size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  
  ### Pain Catastrophizing Scale ----
  #### Density plot PRE ----
  pcsDensity.pre <- ggplot2::ggplotGrob(
    allOutcomes.long %>%
      filter(test == 'pcs', time == 'Pre-program') %>%
      mutate(group = factor(group, levels = c('Control', 'Exercise'))) %>%
      ggplot(aes(x = score, fill = group))+
      geom_density(alpha = 0.5) +
      coord_flip() +
      scale_y_reverse() +
      scale_x_continuous(limits = c(0,60)) +
      scale_fill_manual(values = group.colors) +
      theme_void() +
      theme(legend.position = 'none')
  )
  
  #### Density plot POST ----
  pcsDensity.post <- ggplot2::ggplotGrob(
    allOutcomes.long %>%
      filter(test == 'pcs', time == 'Post-program') %>%
      mutate(group = factor(group, levels = c('Control', 'Exercise'))) %>%
      ggplot(aes(x = score, fill = group))+
      geom_density(alpha = 0.5) +
      coord_flip() +
      scale_x_continuous(limits = c(0,60)) +
      scale_fill_manual(values = group.colors) +
      theme_void() +
      theme(legend.position = 'none')
  )
  
  #### Raincloud plot ----
  pcsRaincoud.plot <- ggplot(data.frame(pcs.emm[1]), aes(x = emmeans.time, y = emmeans.emmean)) +
    geom_point(alpha = 0) + # invisible points to set the X-axis labels...
    ## ESTIMATED MARGINAL MEANS & ERROR BARS
    # Exercise group
    geom_segment(aes(x = 1.1, xend = 2.1, y = 16.1, yend = 10.1)) +
    geom_errorbar(aes(x = 1.1, ymin = 12.0, ymax = 20.1), width = 0.1) +
    geom_point(aes(x = 1.1, y = 16.1), color = '#0a4c9e', shape = 15, size = 3) +
    geom_errorbar(aes(x = 2.1 , ymin = 6.1, ymax = 14.1), width = 0.1) +
    geom_point(aes(x = 2.1, y = 10.1), color = '#0a4c9e', shape = 15, size = 3) +
    # Control group
    geom_segment(aes(x = 0.9, xend = 1.9, y = 20.5, yend = 20.0)) +
    geom_errorbar(aes(x = 0.9, ymin = 16.5, ymax = 24.5), width = 0.1) +
    geom_point(aes(x = 0.9, y = 20.5), color = '#E69F00', shape = 17, size = 3) +
    geom_errorbar(aes(x = 1.9 , ymin = 16.0, ymax = 24.0), width = 0.1) +
    geom_point(aes(x = 1.9, y = 20.0), color = '#E69F00', shape = 17, size = 3) +
    ## DENSITY PLOTS
    annotation_custom(grob = pcsDensity.pre, xmin = 0.5, xmax = 0.7) +
    annotation_custom(grob = pcsDensity.post, xmin = 2.3, xmax = 2.5) +
    # SCALE ADJUSTMENTS
    scale_y_continuous(limits = c(0, 52), breaks = seq(0,52, length.out = 5), guide = 'prism_offset') +
    # ANNOTATIONS
    ylab('Pain catastrophizing') +
    geom_segment(aes(x = 1.9, xend = 2.1, y = 26, yend = 26), color = 'black') +
    annotate('text', x = 2, y = 27, label = "***",  color = 'black', size = 5, fontface = 'bold') +
    # AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_line(size = 0.25),
      axis.ticks.y = element_line(size = 0.25),
      axis.text.x = element_text(size = 10),
      axis.title.y = element_text(size = 13, vjust = 4.8),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
    )
  
  #### Exercise plot ----
  pcsExerc.plot <- allOutcomes.long %>% filter(group == 'Exercise',
                                               test  == 'pcs') %>%
    ggplot(aes(x = time, y = score,
               color = group, shape = group, group = id)) +
    ## INDIVIDUAL POINTS & LINES
    geom_line(size = 0.5, alpha = 0.25) +
    geom_point(alpha = 0.25) +
    ## GROUP MEANS
    geom_errorbar(aes(x = 0.75, ymin = 12.0, ymax = 20.1), width = 0.1, color = 'black') +
    geom_point(aes(x = 0.75, y = 16.1), color = '#0a4c9e', shape = 15, size = 3) +
    geom_errorbar(aes(x = 2.25 , ymin = 6.1, ymax = 14.2), width = 0.1, color = 'black') +
    geom_point(aes(x = 2.25, y = 10.1), color = '#0a4c9e', shape = 15, size = 3) +
    ## GROUP AESTHETHICS
    scale_color_manual(values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_y_continuous(limits = c(0, 52), breaks = seq(0,52, length.out = 5), guide = 'prism_offset') +
    ## TITLES & LABELS
    ylab('') +
    # ANNOTATIONS
    annotate('text', x = 1.5, y = 39, label = "***",  color = 'black', size = 5, fontface = 'bold') +
    ## AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(), 
      plot.title = element_text(size = 15, face = 'bold'),
      axis.line.x = element_line(size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  #### Waiting List plot ----
  pcsCtrl.plot <- allOutcomes.long  %>% filter(group == 'Control',
                                               test  == 'pcs') %>%
    ggplot(aes(x = time, y = score,
               color = group, shape = group, group = id)) +
    ## INDIVIDUAL POINTS & LINES
    geom_line(size = 0.5, alpha = 0.25) +
    geom_point(alpha = 0.25) +
    ## GROUP MEANS
    geom_errorbar(aes(x = 0.75, ymin = 16.5, ymax = 24.5), width = 0.1, color = 'black') +
    geom_point(aes(x = 0.75, y = 20.5), color = '#E69F00', shape = 17, size = 3) +
    geom_errorbar(aes(x = 2.25 , ymin = 16.0, ymax = 24.0), width = 0.1, color = 'black') +
    geom_point(aes(x = 2.25, y = 20.0), color = '#E69F00', shape = 17, size = 3) +
    ## GROUP AESTHETHICS
    scale_color_manual(values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_y_continuous(limits = c(0, 52), breaks = seq(0,52, length.out = 5), guide = 'prism_offset') +
    ## TITLES & LABELS
    ylab('') +
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(size = 15, face = 'bold'),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.line.x = element_line(size = 0.25),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  
  
  ### Tamp Scale of Kinesiophobia ----
  #### Density plot PRE ----
  tskDensity.pre <- ggplot2::ggplotGrob(
    allOutcomes.long %>%
      filter(test == 'tsk', time == 'Pre-program') %>%
      mutate(group = factor(group, levels = c('Control', 'Exercise'))) %>%
      ggplot(aes(x = score, fill = group))+
      geom_density(alpha = 0.5) +
      coord_flip() +
      scale_y_reverse() +
      scale_x_continuous(limits = c(0,60)) +
      scale_fill_manual(values = group.colors) +
      theme_void() +
      theme(legend.position = 'none')
  )
  
  #### Density plot POST ----
  tskDensity.post <- ggplot2::ggplotGrob(
    allOutcomes.long %>%
      filter(test == 'tsk', time == 'Post-program') %>%
      mutate(group = factor(group, levels = c('Control', 'Exercise'))) %>%
      ggplot(aes(x = score, fill = group))+
      geom_density(alpha = 0.5) +
      coord_flip() +
      scale_x_continuous(limits = c(0,60)) +
      scale_fill_manual(values = group.colors) +
      theme_void() +
      theme(legend.position = 'none')
  )
  
  #### Raincloud plot ----
  tskRaincoud.plot <- ggplot(data.frame(tsk.emm[1]), aes(x = emmeans.time, y = emmeans.emmean)) +
    geom_point(alpha = 0) + # invisible points to set the X-axis labels...
    ## ESTIMATED MARGINAL MEANS & ERROR BARS
    # Exercise group
    geom_segment(aes(x = 1.1, xend = 2.1, y = 23.9, yend = 20.8)) +
    geom_errorbar(aes(x = 1.1, ymin = 21.5, ymax = 26.4), width = 0.1) +
    geom_point(aes(x = 1.1, y = 23.9), color = '#0a4c9e', shape = 15, size = 3) +
    geom_errorbar(aes(x = 2.1 , ymin = 18.3, ymax = 23.3), width = 0.1) +
    geom_point(aes(x = 2.1, y = 20.8), color = '#0a4c9e', shape = 15, size = 3) +
    # Control group
    geom_segment(aes(x = 0.9, xend = 1.9, y = 27.5, yend = 27.9)) +
    geom_errorbar(aes(x = 0.9, ymin = 25.0, ymax = 29.9), width = 0.1) +
    geom_point(aes(x = 0.9, y = 27.5), color = '#E69F00', shape = 17, size = 3) +
    geom_errorbar(aes(x = 1.9 , ymin = 25.4, ymax = 30.3), width = 0.1) +
    geom_point(aes(x = 1.9, y = 27.9), color = '#E69F00', shape = 17, size = 3) +
    ## DENSITY PLOTS
    annotation_custom(grob = tskDensity.pre, xmin = 0.5, xmax = 0.7) +
    annotation_custom(grob = tskDensity.post, xmin = 2.3, xmax = 2.5) +
    # SCALE ADJUSTMENTS
    scale_y_continuous(limits = c(0, 68), breaks = seq(0, 68, length.out = 5), guide = 'prism_offset') +
    # ANNOTATIONS
    ylab('Pain-related fear') +
    geom_segment(aes(x = 1.9, xend = 2.1, y = 34, yend = 34), color = 'black') +
    annotate('text', x = 2, y = 35, label = "***",  color = 'black', size = 5, fontface = 'bold') +
    
    # AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_line(size = 0.25),
      axis.ticks.y = element_line(size = 0.25),
      axis.text.x = element_text(size = 10),
      axis.title.y = element_text(size = 13, vjust = 4.8),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
    )
  
  #### Exercise plot ----
  tskExerc.plot <- allOutcomes.long %>% filter(group == 'Exercise',
                                               test  == 'tsk') %>%
    ggplot(aes(x = time, y = score,
               color = group, shape = group, group = id)) +
    ## INDIVIDUAL POINTS & LINES
    geom_line(size = 0.5, alpha = 0.25) +
    geom_point(alpha = 0.25) +
    ## GROUP MEANS
    geom_errorbar(aes(x = 0.75, ymin = 21.5, ymax = 26.4), width = 0.1, color = 'black') +
    geom_point(aes(x = 0.75, y = 23.9), color = '#0a4c9e', shape = 15, size = 3) +
    geom_errorbar(aes(x = 2.25 , ymin = 18.3, ymax = 23.3), width = 0.1, color = 'black') +
    geom_point(aes(x = 2.25, y = 20.8), color = '#0a4c9e', shape = 15, size = 3) +
    ## GROUP AESTHETHICS
    scale_color_manual(values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_y_continuous(limits = c(0, 68), breaks = seq(0, 68, length.out = 5), guide = 'prism_offset') +
    ## TITLES & LABELS
    ylab('') +
    # ANNOTATIONS
    annotate('text', x = 1.5, y = 45, label = "**",  color = 'black', size = 5, fontface = 'bold') +
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(), 
      plot.title = element_text(size = 15, face = 'bold'),
      axis.line.x = element_line(size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  #### Waiting List plot ----
  tskCtrl.plot <- allOutcomes.long  %>% filter(group == 'Control',
                                               test  == 'tsk') %>%
    ggplot(aes(x = time, y = score,
               color = group, shape = group, group = id)) +
    ## INDIVIDUAL POINTS & LINES
    geom_line(size = 0.5, alpha = 0.25) +
    geom_point(alpha = 0.25) +
    ## GROUP MEANS
    geom_errorbar(aes(x = 0.75, ymin = 25.0, ymax = 29.9), width = 0.1, color = 'black') +
    geom_point(aes(x = 0.75, y = 27.5), color = '#E69F00', shape = 17, size = 3) +
    geom_errorbar(aes(x = 2.25 , ymin = 25.4, ymax = 30.3), width = 0.1, color = 'black') +
    geom_point(aes(x = 2.25, y = 27.9), color = '#E69F00', shape = 17, size = 3) +
    ## GROUP AESTHETHICS
    scale_color_manual(values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_y_continuous(limits = c(0, 68), breaks = seq(0, 68, length.out = 5), guide = 'prism_offset') +
    ## TITLES & LABELS
    ylab('') +
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(), 
      plot.title = element_text(size = 15, face = 'bold'),
      axis.line.x = element_line(size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  
  
  ### Combine plots ----
  outcome.plot <- plot_grid(
    # Average pain in the last 7 days
    nihPainRaincoud.plot, nihPainExerc.plot, nihPainCtrl.plot,
    # Oswestry Disability Index
    odiRaincoud.plot, odiExerc.plot, odiCtrl.plot,
    # Pain Catastrophizing Scale
    pcsRaincoud.plot, pcsExerc.plot, pcsCtrl.plot,
    # Tamp Scale of Kinesiophobia
    tskRaincoud.plot, tskExerc.plot, tskCtrl.plot,
    ncol = 3, align = 'v', axis = 'l'
  )
  
  outcome.plot <- outcome.plot +
    draw_plot_label(label = c('A', 'B', 'C', 'D'),
                    x = rep(0, 4), y = c(1, 0.75, 0.5, 0.25),
                    size = 20)
  
  save_plot('outcomePLot.tiff', outcome.plot,
            base_width = 10, base_height = 11, dpi = 320
         )