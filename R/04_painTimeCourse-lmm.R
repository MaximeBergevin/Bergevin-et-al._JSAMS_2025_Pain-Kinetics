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
library(lme4)       # Necessary to run robustlmm:rlmer
library(tidyverse)  # Necessary for pipes (%>%)
library(ggprism)    # Theme for nice plots
  
  
## LOAD DATA & CUMSTON FUNCTIONS ====
  load(file = 'Data/painKinetics.RData')       # Data pain kinetic (long format)
  
  
## SETTING LMM CONTRATS ----
  # We want the mean of the first training session to be the intercept
  # This way, the β reflect change in pain per additional training session
  # Treatment contrasts are usually better with continuous predictor(s)
  # Note that for this project, orthogonal polynomials were used
  afex::set_treatment_contrasts()
  options(scipen = 999) # Scientific notation is annoying
  
  
# LINEAR MIXED-EFFECT MODELS ----
  kinetics.long <- kinetics.long %>%
    filter(!is.na(score)) %>%
    mutate(
      #painAvg7days_Pre = painAvg7days_Pre - mean(painAvg7days_Pre, na.rm = TRUE),
      Vo2maxMAPkg_Pre = Vo2maxMAPkg_Pre - mean(Vo2maxMAPkg_Pre, na.rm = TRUE),
      Training1RMLegPresskg_Pre = Training1RMLegPresskg_Pre - mean(Training1RMLegPresskg_Pre, na.rm = TRUE),
      Training1RMChestPresskg_Pre = Training1RMChestPresskg_Pre - mean(Training1RMChestPresskg_Pre, na.rm = TRUE),
      Training1RMLatPullDown_Pre = Training1RMLatPullDown_Pre - mean(Training1RMLatPullDown_Pre, na.rm = TRUE),
      AnthropoBMIScale_Pre = AnthropoBMIScale_Pre - mean(AnthropoBMIScale_Pre, na.rm = TRUE),
      #AnthropoAge_Pre = AnthropoAge_Pre - mean(AnthropoAge_Pre, na.rm = TRUE),
      tsk_Pre = tsk_Pre - mean(tsk_Pre, na.rm = TRUE)
    ) %>%
    # Physical fitness composite score
    mutate(
      MAP_z = scale(Vo2maxMAPkg_Pre)[,1],
      legPress_z = scale(Training1RMLegPresskg_Pre)[,1],
      chestPress_z = scale(Training1RMChestPresskg_Pre)[,1],
      latPulldown_z = scale(Training1RMLatPullDown_Pre)[,1],
      physicalFitness = (MAP_z + legPress_z + chestPress_z + latPulldown_z)/4,
      muscleStrength = (legPress_z + chestPress_z + latPulldown_z)/3
    )
  
  ### Linear ----
  linear.lmm <- lmerTest::lmer(
    dat = kinetics.long,
    formula = score ~ chronic * acute + (1|id)
  )
  summary(linear.lmm)
  confint(linear.lmm , parm = 'beta_')
  sjPlot::tab_model(linear.lmm, show.df = TRUE, df.method = 's', show.stat = TRUE)
  plot(DHARMa::simulateResiduals(fittedModel = linear.lmm, n = 1000))
  
  
  ### Quadratic ----
  quadratic.lmm <- lmerTest::lmer(
    dat = kinetics.long,
    formula = score ~ poly(chronic,2) * acute + (1|id)
  )
  summary(quadratic.lmm)
  confint(quadratic.lmm, parm = 'beta_')
  anova(linear.lmm, quadratic.lmm)
  sjPlot::tab_model(quadratic.lmm, show.df = TRUE, df.method = 's', show.stat = TRUE)
  plot(DHARMa::simulateResiduals(fittedModel = quadratic.lmm, n = 1000))
  
  simr::powerSim(quadratic.lmm,
                 test = simr::fixed("poly(chronic, 2)1", method = c("t")))
  simr::powerSim(quadratic.lmm,
                 test = simr::fixed("poly(chronic, 2)2", method = c("t")))
 
   simr::powerSim(quadratic.lmm,
                  test = simr::fixed("acutePost-session", method = c("t")),
                  seed = 123)
  
  ## Moderation analyses ----
  
    ### Baseline pain ----
    moderationPain.lmm <- lmerTest::lmer(
      dat = kinetics.long,
      formula = score ~ poly(chronic, 2) * acute +
        poly(chronic, 2):painAvg7days_Pre +
        acute:painAvg7days_Pre +
        (1|id)
    )
    summary(moderationPain.lmm)
    anova(quadratic.lmm, moderationPain.lmm)
    sjPlot::tab_model(moderationPain.lmm)
    
    ### Pain duration ----
    moderationDuration.lmm <- lmerTest::lmer(
      dat = kinetics.long,
      formula = score ~ poly(chronic, 2) * acute +
      poly(chronic,2):AnthropoPainDuration +
      acute:AnthropoPainDuration +
      (1|id)
    )
    summary(moderationDuration.lmm)
    anova(quadratic.lmm, moderationDuration.lmm)
    sjPlot::tab_model(moderationDuration.lmm)
    
    ### Maximal aerobic power ----
    moderationMAP.lmm <- lmerTest::lmer(
      dat = kinetics.long,
      formula = score ~ poly(chronic, 2) * acute +
      poly(chronic,2):Vo2maxMAPkg_Pre +
      acute:Vo2maxMAPkg_Pre +
      (1|id)
    )
    summary(moderationMAP.lmm)
    anova(quadratic.lmm, moderationMAP.lmm)
    sjPlot::tab_model(moderationMAP.lmm)
    
    ### One maximum repetition ----
    moderation1RM.lmm <-  lmerTest::lmer(
      dat = kinetics.long,
      formula = score ~ poly(chronic, 2) * acute +
        poly(chronic,2):muscleStrength +
        acute:muscleStrength +
        (1|id)
    )
    summary(moderation1RM.lmm)
    anova(quadratic.lmm, moderation1RM.lmm)
    sjPlot::tab_model(moderation1RM.lmm)
    
    ### Physical fitness ----
    moderationFitness.lmm <- lmerTest::lmer(
      dat = kinetics.long,
      formula = score ~ poly(chronic, 2) * acute +
      poly(chronic,2):physicalFitness +
      acute:physicalFitness +
      (1|id)
    )
    summary(moderationFitness.lmm)
    anova(quadratic.lmm, moderationFitness.lmm)
    sjPlot::tab_model(moderationFitness.lmm)
    
    ### Sex ----
    moderationSex.lmm <- lmerTest::lmer(
      dat = kinetics.long,
      formula = score ~ poly(chronic,2) * acute +
      poly(chronic,2):AnthropoSex_Pre +
      acute:AnthropoSex_Pre +
      (1|id)
    )
    summary(moderationSex.lmm)
    anova(quadratic.lmm, moderationSex.lmm)
    sjPlot::tab_model(moderationSex.lmm)
    
    ### Age ----
    moderationAge.lmm <- lmerTest::lmer(
      dat = kinetics.long,
      formula = score ~ poly(chronic,2) * acute +
      poly(chronic,2):AnthropoAge_Pre +
      acute:AnthropoAge_Pre +
      (1|id)
    )
    summary(moderationAge.lmm)
    anova(quadratic.lmm, moderationAge.lmm)
    sjPlot::tab_model(moderationAge.lmm)
    
    ### Changes scores ----
    delta <- kinetics.long %>%
      # Keep only pre-session scores from session 1 & 42
      filter(acute == "Pre-session", chronic %in% c(1,42)) %>%
      # Extract session 1 & 42 in separate cols
      pivot_wider(
        id_cols = c(id, painAvg7days_Pre, AnthropoAge_Pre),
        names_from = chronic,
        values_from = score,
        names_prefix = 'session_'
      ) %>%
      # Calculate change score
      mutate(delta = session_1 - session_42) %>%
      drop_na()
    
    deltaPain.lm <- lm(delta ~ painAvg7days_Pre, data = deltaPain)
    ggplot(delta, aes(x = painAvg7days_Pre, y = delta)) +
      geom_smooth(method = 'lm')
    
    deltaAge.lm <- lm(delta ~ AnthropoAge_Pre, data = delta)
    ggplot(delta, aes(x = AnthropoAge_Pre, y = delta)) +
      geom_smooth(method = 'lm')
    
  ### Exacerbation of pain ----
  adverseAcute <- kinetics.long %>%
      group_by(id, acute) %>%
      summarise(mean_pain = mean(score, na.rm = TRUE)) %>%
      pivot_wider(names_from = acute, values_from = mean_pain) %>%
      mutate(delta = `Post-session` - `Pre-session`) %>%
      summarise(mean_delta = mean(delta, na.rm = TRUE))
    
    # Compute per-session deltas
    delta_sessions <- kinetics.long %>%
      filter(acute %in% c("Pre-session", "Post-session")) %>%
      group_by(id, chronic, acute) %>%
      summarise(pain = mean(score, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = acute, values_from = pain) %>%
      mutate(delta = `Post-session` - `Pre-session`)
    
    # Count number of sessions where pain increased
    n_sessions_exacerbated <- sum(delta_sessions$delta > 0, na.rm = TRUE)
    
    # Number of unique participants who had at least one exacerbated session
    n_participants_exacerbated <- delta_sessions %>%
      filter(delta > 0) %>%
      distinct(id) %>%
      nrow()
    
    # Total number of participants
    total_participants <- n_distinct(kinetics.long$id)
    
    # Output
    list(
      exacerbated_sessions = n_sessions_exacerbated,
      participants_with_exacerbation = n_participants_exacerbated,
      total_participants = total_participants
    )
    
    exacerbation_counts <- delta_sessions %>%
      filter(delta > 0) %>%
      count(id, name = "n_exacerbated")
    
    summary(exacerbation_counts$n_exacerbated)
    
    exacerbation_counts <- exacerbation_counts %>%
      mutate(prop_of_total = (n_exacerbated / 42)*100)
    
    
  ### All moderators ----
  moderation.lmm <- lmerTest::lmer(
   dat = kinetics.long,
   formula = score ~ poly(chronic,2) + acute +
     # Moderation analysis on the training program
     poly(chronic,2):painAvg7days_Pre +         # 7-day pain
     poly(chronic,2):AnthropoPainDuration +     # Pain duration
     poly(chronic,2):Vo2maxMAPkg_Pre +          # Maximal aerobic power
     poly(chronic, 2):Training1RMAvg_Pre +      # 1RM
     poly(chronic, 2):AnthropoBMIScale_Pre +    # BMI
     poly(chronic,2):AnthropoSex_Pre +          # Sex
     poly(chronic, 2):AnthropoAge_Pre +         # Age
     # Moderation analysis on the training session
     acute:painAvg7days_Pre +                   # 7-day pain
     acute:AnthropoPainDuration +               # Pain duration
     acute:Vo2maxMAPkg_Pre +                    # Maximal aerobic power
     acute:Training1RMAvg_Pre +                # 1RM
     acute:AnthropoSex_Pre +                    # Sex
     acute:AnthropoAge_Pre +                    # Age
     (1|id)
  )
 summary(moderation.lmm)
 sjPlot::tab_model(moderation.lmm, show.df = TRUE, df.method = 's', show.stat = TRUE, show.se = TRUE)
 # Correlation between linear and quadratic terms
 moderation.vcov <- vcov(moderation.lmm)
 moderation.cor <- cov2cor(moderation.vcov)
 # Diagnostics
 plot(DHARMa::simulateResiduals(fittedModel = moderation.lmm, n = 100))
 car::vif(moderation.lmm)
 
# PLOTS ----
    
    #### Predicted/Fitted data frames ----
    # Predicted values for every participants
    predicted.lmm <- cbind(
      kinetics.long,
      merTools::predictInterval(quadratic.lmm, kinetics.long)
      ) %>%
     # Mean for pre- and post-workout values for each subjects
     group_by(id, chronic) %>%
     mutate(chronicEffect = mean(fit, na.rm = TRUE)) %>%
     # Values below 0/10 don't make sense.
     # Shortcoming of the model - will need to fin ways to bind model within
     # scale limits in the future.
     mutate(chronicEffect = case_when(
       chronicEffect < 0 ~ 0,
       chronicEffect >= 0 ~ chronicEffect))
 
    # Fitted values for every training sessions
    fitted.lmm <- data.frame(
      effects::Effect(mod = quadratic.lmm,
                      xlevels = list(chronic = seq(1,42,1)),
                      focal.predictors = c('chronic', 'acute'))
    ) %>%
      mutate(acute = ifelse(acute == 'Pre', 'Pre-session', 'Post-session'),
             acute = factor(acute, levels = c('Pre-session', 'Post-session'))) %>%
      # Mean for pre- and post-workout fitted values
      group_by(chronic) %>%
      mutate(chronicEffect = mean(fit, na.rm = TRUE))
    
    
    
    #### Training session ----
    # Extracts EMM for the main effect of acute execise
    acute.emm <- data.frame(emmeans::emmeans(quadratic.lmm, ~ acute))
    
    # Colors for 
    timeSession.colors <- c(`Pre-session` = '#0a4c9e',
                            `Post-session` = '#56B4E9')
    
    # Prepare data for the plot -- averages pain ratings across all 42 sessions
    session.plot <- kinetics.long %>%
      group_by(id, acute) %>%
      summarize(mean = mean(score, na.rm = TRUE)) %>%
      # Plots group and individual data for main effect of acute exercise
      ggplot(aes(x = acute, y = mean, group = id)) +
      ## INDIVIDUAL DATA
      geom_point(aes(color = acute), shape = 'square', alpha = 0.5) +
      geom_line(alpha = 0.5) +
      ## GROUP DATA
      # Pre-session
      geom_errorbar(aes(x = 0.75, ymin = acute.emm[1,5], ymax = acute.emm[1,6]), width = 0.1) +
      geom_point(aes(x = 0.75, y = acute.emm[1,2]), shape = 'square', size = 3, color = '#0a4c9e') +
      # Post-session
      geom_errorbar(aes(x = 2.25, ymin = acute.emm[2,5], ymax = acute.emm[2,6]), width = 0.1) +
      geom_point(aes(x = 2.25, y = acute.emm[2,2]), shape = 'square', size = 3, color = '#56B4E9') +
      ## SCALES
      scale_y_continuous(limits = c(0,7), guide = 'prism_offset') +
      # LABELS
      labs(title = "Effect of training session",
           x = "",
           y = "") +
      # ANNOTATION
      #annotate('text', x = 1.5, y = 6, label = 'β=-0.62 (-0.71, -0.53), p<.001', fontface = 'bold') +
     # ASTHETHETICS
      scale_color_manual(values = timeSession.colors) +
      ggprism::theme_prism() +
      theme(
        plot.title = element_text(size = 17, face = 'bold', vjust = -2),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = 'none'
      )
    
    session.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(session.plot) +
      cowplot::draw_plot_label(label = 'B', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_sessionPlot.tiff',
                       plot = session.plot, 
                       base_height = 3, base_width = 4, dpi = 300)

    #### Training program ----
    training.plot <- sjPlot::plot_model(
      quadratic.lmm, type = 'emm', terms = c('chronic[all]'),
      colors = 'bw', axis.title = "") +
      # INDIVIDUAL LINES
      geom_line(data = predicted.lmm %>% filter(acute == 'Pre-session'),
                aes(x = chronic, y = chronicEffect, group = id), 
                size = 0.5, alpha = 0.25, color = 'black') +
      # SCALES
      scale_x_continuous(limits = c(1,42), breaks = seq(0,40,10)) +
      scale_y_continuous(limits = c(0,7), guide = 'prism_offset') + 
      # LABELS
      labs(title = "Effect of training program",
           x = "",
           y = "Pain intensity") +
      # ANNOTATION
      #annotate('text', x = 13, y = 6, label = 'Linear trend: p<.001', fontface = 'bold') +
      #annotate('text', x = 15.2, y = 5.3, label = 'Quadratric trend: p=.006', fontface = 'bold') +
      # AESTHETHICS
      ggprism::theme_prism() +
      theme(
        plot.title = element_text(size = 17, face = 'bold', vjust = -2),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = 'none'
      )
    
    training.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(training.plot) +
      cowplot::draw_plot_label(label = 'A', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_trainingPlot.tiff',
                       plot = training.plot, 
                       base_height = 3, base_width = 4, dpi = 300)

    ### Session × Training ----
    interaction.plot <- sjPlot::plot_model(
      quadratic.lmm, type = 'emm', terms = c('chronic[all]', 'acute[all'),
      axis.title = "") +
      # SCALES
      scale_x_continuous(limits = c(1,42), breaks = seq(0,40,10)) +
      scale_y_continuous(limits = c(0,7), guide = 'prism_offset') +
      # LABELS
      labs(title = "Session × program interaction",
           x = "",
           y = "") +
      # AESTHETHICS
      scale_color_manual(values = timeSession.colors) +
      scale_fill_manual(values = timeSession.colors) +
      # ANNOTATION
      #annotate('text', x = 13.2, y = 6, label = 'Linear trend: p=.973') +
      #annotate('text', x = 15.8, y = 4.5, label = 'Quadratric trend: p=.585') +
      ggprism::theme_prism() +
      theme(
        plot.title = element_text(size = 17, face = 'bold', vjust = -2),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = 'none'
      )
      #guides(color = guide_legend(override.aes = list(fill = NA)))
    
    interaction.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(interaction.plot) +
      cowplot::draw_plot_label(label = 'C', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_interaction.tiff',
                       plot = interaction.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    ### Session × 7-day pain ----
    # Original mean: 4.928571 | Subtract mean by each level
    sessionPain.plot <- sjPlot::plot_model(moderationPain.lmm, type = 'emm',
                                         terms = c('acute[all]', 'painAvg7days_Pre[-0.928571, 0.071429, 1.071429, 2.071429, 3.071429]'),
                                         se = TRUE, dot.size = 1.5) +
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      # LABELS
      labs(title = "",
           x = "",
           y = "") +
      # AESHETHICS
      scale_color_manual(values = c('-0.928571' = '#330000',
                                    '0.071429'  = '#900000',
                                    '1.071429'  = '#CC0000',
                                    '2.071429'  = '#FF0000',
                                    '3.071429'  = '#FF3366')) +
       # geom_point(aes(x = .25, y = 1), alpha = 0) +
       # geom_point(aes(x = 2.75, y = 1), alpha = 0) +
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(hjust = c(0,1)),
        legend.position = 'none',
        #legend.position = c(.25, .25),
        legend.text = element_text(face = 'bold'),
        legend.key.height = unit(.25, 'cm')
      ) 
    
    sessionPain.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(sessionPain.plot) +
      cowplot::draw_plot_label(label = '', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_sessionPainPlot.tiff',
                       plot = sessionPain.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Session × Pain duration ----
    sessionDuration.plot <- sjPlot::plot_model(moderationDuration.lmm, type = 'emm',
                                               terms = c('acute[all]', 'AnthropoPainDuration')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      # LABELS
      labs(title = "",
           x = "",
           y = "") +
      # Nudge EMMS towards the center of the plot
      # geom_point(aes(x = .25, y = 1), alpha = 0) +
      # geom_point(aes(x = 2.75, y = 1), alpha = 0) +
      # AESTHETHICS
      scale_color_manual(values = c('1-5 years' = '#660000', 
                                    '> 5 years' = '#FF9933')) +
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(hjust = c(0,1)),
        legend.position = 'none'
        # legend.position = c(.25, .3),
        # legend.text = element_text(face = 'bold'),
        # legend.key.height = unit(.5, 'cm')
      ) 
    
    sessionDuration.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(sessionDuration.plot) +
      cowplot::draw_plot_label(label = '', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_sessionDurationPlot.tiff',
                       plot = sessionDuration.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Session × 1RM ----
    session1RM.plot <- sjPlot::plot_model(moderation1RM.lmm, type = 'emm',
                                              terms = c('acute[all]', 'muscleStrength[-1, -0.5, 0, 0.5, 1]')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      # LABELS
      labs(title = "",
           x = "",
           y = "") +
      # AESHETHICS
      scale_color_manual(values = c('-1'    = '#663300',
                                    '-0.5'  = '#993300',
                                    '0'     = '#CC6633',
                                    '0.5'   = '#CC6600',
                                    '1'     = '#FF9933')) +
      # Nudge EMMS towards the center of the plot
      # geom_point(aes(x = .25, y = 1), alpha = 0) +
      # geom_point(aes(x = 2.75, y = 1), alpha = 0) +
      # AESTHETHICS
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(hjust = c(0,1)),
        legend.position = 'none',
        #legend.position = c(.25, .3),
        legend.text = element_text(face = 'bold'),
        legend.key.height = unit(.5, 'cm')
      ) 
    
    session1RM.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(session1RM.plot) +
      cowplot::draw_plot_label(label = '', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_session1RMPlot.tiff',
                       plot = session1RM.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Session × MAP ----
    # Original mean: 2.187721 | Subtract mean by each level
    sessionMAP.plot <- sjPlot::plot_model(moderationMAP.lmm, type = 'emm',
                                          terms = c('acute[all]', 'Vo2maxMAPkg_Pre[-0.187721,0.812279,1.812279,2.812279]')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      # LABELS
      labs(title = "",
           x = "",
           y = "") +
      # AESHETHICS
      scale_color_manual(values = c('-0.187721' = '#00CC00',
                                    '0.812279'  = '#33CC33',
                                    '1.812279'  = '#006600',
                                    '2.812279'  = '#333300')) +
      # Nudge EMMS towards the center of the plot
      # geom_point(aes(x = .25, y = 1), alpha = 0) +
      # geom_point(aes(x = 2.75, y = 1), alpha = 0) +
      # AESTHETHICS
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(hjust = c(0,1)),
        legend.position = 'none',
        #legend.position = c(.25, .3),
        legend.text = element_text(face = 'bold'),
        legend.key.height = unit(.5, 'cm')
      ) 
    
    sessionMAP.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(sessionMAP.plot) +
      cowplot::draw_plot_label(label = '', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_sessionMAPPlot.tiff',
                       plot = sessionMAP.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
 
    
  ### Session × Sex ----
    sessionSex.plot <- sjPlot::plot_model(moderationSex.lmm, type = 'emm',
                       terms = c('acute[all]', 'AnthropoSex_Pre')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      # LABELS
      labs(title = "",
           x = "",
           y = "") +
      # AESHETHICS
      scale_color_manual(values = c('male' = '#660066',
                                    'female' = '#CC33FF')) +
      # Nudge EMMS towards the center of the plot
      # geom_point(aes(x = .25, y = 1), alpha = 0) +
      # geom_point(aes(x = 2.75, y = 1), alpha = 0) +
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(hjust = c(0,1)),
        legend.position = 'none'
        # legend.position = c(.25, .25),
        # legend.text = element_text(face = 'bold'),
        # legend.key.height = unit(.5, 'cm')
      ) 
    
    sessionSex.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(sessionSex.plot) +
      cowplot::draw_plot_label(label = '', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_sessionSexPlot.tiff',
                       plot = sessionSex.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Session × Age ----
    sessionAge.plot <- sjPlot::plot_model(moderationAge.lmm, type = 'emm',
                                               terms = c('acute[all]', 'AnthropoAge_Pre[-25.25, -15.25, -5.25, 4.75, 14.75, 24.75]')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      # LABELS
      labs(title = "",
           x = "Timing of measurement",
           y = "") +
      # Nudge EMMS towards the center of the plot
      # geom_point(aes(x = .25, y = 1), alpha = 0) +
      # geom_point(aes(x = 2.75, y = 1), alpha = 0) +
      # AESTHETHICS
      scale_color_manual(values = c('-25.25'  = '#000033',
                                    '-15.25'  = '#000066',
                                    '-5.25'   = '#000099',
                                    '4.75'    = '#0000CC',
                                    '14.75'   = '#0000FF',
                                    '24.75'   = '#3366CC')) +
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(hjust = c(0,1)),
        legend.position = 'none',
        #legend.position = c(.25, .5),
        legend.text = element_text(face = 'bold'),
        legend.key.height = unit(.5, 'cm')
      ) 
    
    sessionAge.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(sessionAge.plot) +
      cowplot::draw_plot_label(label = '', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_sessionAgePlot.tiff',
                       plot = sessionAge.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
  
    ### Program x 7-day pain ----
    # Original mean: 4.928571 | Subtract mean by each level
    programPain.plot <- sjPlot::plot_model(moderationPain.lmm, type = 'emm', ci.lvl = NA,
                       terms = c('chronic[all]', 'painAvg7days_Pre[-0.928571,0.071429,1.071429,2.071429,3.071429]')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      scale_color_manual(values = c('-0.928571' = '#330000',
                                    '0.071429' = '#900000',
                                    '1.071429' = '#CC0000',
                                    '2.071429' = '#FF0000',
                                    '3.071429' = '#FF3366')) +
      # LABELS
      labs(title = "",
           x = "",
           y = "Pain intensity") +
      # AESHETHICS
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        legend.position = 'none'
        # legend.position = c(.25, .3),
        # legend.text = element_text(face = 'bold'),
        # legend.key.height = unit(.5, 'cm')
      ) 
    
    programPain.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(programPain.plot) +
      cowplot::draw_plot_label(label = 'D', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_programPain.tiff',
                       plot = programPain.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Program x Pain duration ----
    programDuration.plot <- sjPlot::plot_model(moderationDuration.lmm, type = 'emm', ci.lvl = NA,
                                               terms = c('chronic[all]', 'AnthropoPainDuration')) +
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      scale_color_manual(values = c('1-5 years' = '#660000', 
                                    '> 5 years' = '#FF9933')) +
      # LABELS
      labs(title = "",
           x = "",
           y = "Pain intensity") +
      # AESTHETHICS
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        legend.position = 'none'
        # legend.position = c(.25, .3),
        # legend.text = element_text(face = 'bold'),
        # legend.key.height = unit(.5, 'cm')
      ) 
    
    programDuration.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(programDuration.plot) +
      cowplot::draw_plot_label(label = 'E', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_programDurationPlot.tiff',
                       plot = programDuration.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Session × 1RM ----
    program1RM.plot <- sjPlot::plot_model(moderation1RM.lmm, type = 'emm', ci.lvl = NA,
                                              terms = c('chronic[all]', 'muscleStrength[-1, -0.5, 0, 0.5, 1]')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      # LABELS
      labs(title = "",
           x = "",
           y = "") +
      # AESHETHICS
      scale_color_manual(values = c('-1'    = '#663300',
                                    '-0.5'  = '#993300',
                                    '0'     = '#CC6633',
                                    '0.5'   = '#CC6600',
                                    '1'     = '#FF9933')) +
      # LABELS
      labs(title = "",
           x = "",
           y = "Pain intensity") +
      # AESHETHICS
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        legend.position = 'none',
        #legend.position = c(0.5, 1),
        # legend.text = element_text(face = 'bold'),
        # legend.key.height = unit(.5, 'cm')
      ) 
    
    program1RM.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(program1RM.plot) +
      cowplot::draw_plot_label(label = 'G', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_program1RMPlot.tiff',
                       plot = program1RM.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Program x MAP ----
    # Original mean: 2.187721 | Subtract mean by each level
    programMAP.plot <- sjPlot::plot_model(moderationMAP.lmm, type = 'emm', ci.lvl = NA,
                                          terms = c('chronic[all]', 'Vo2maxMAPkg_Pre[-0.187721,0.812279,1.812279,2.812279]')) +
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      scale_color_manual(values = c('-0.187721' = '#00CC00',
                                    '0.812279'  = '#33CC33',
                                    '1.812279'  = '#006600',
                                    '2.812279'  = '#333300')) +
      # LABELS
      labs(title = "",
           x = "",
           y = "Pain intensity") +
      # AESHETHICS
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        legend.position = 'none',
        #legend.position = c(0.5, 1),
        # legend.text = element_text(face = 'bold'),
        # legend.key.height = unit(.5, 'cm')
      ) 
    
    
    programMAP.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(programMAP.plot) +
      cowplot::draw_plot_label(label = 'F', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_programMAPPlot.tiff',
                       plot = programMAP.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Program x Sex ----
    programSex.plot <- sjPlot::plot_model(moderationSex.lmm, type = 'emm', ci.lvl = NA,
                       terms = c('chronic[all]', 'AnthropoSex_Pre')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      scale_color_manual(values = c('male' = '#660066',
                                    'female' = '#CC33FF')) +
      # LABELS
      labs(title = "",
           x = "",
           y = "Pain intensity") +
      # AESHETHICS
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        legend.position = 'none'
        # legend.position = c(.25, .3),
        # legend.text = element_text(face = 'bold'),
        # legend.key.height = unit(.5, 'cm')
      ) 
    
    programSex.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(programSex.plot) +
      cowplot::draw_plot_label(label = 'H', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_programSexPlot.tiff',
                       plot = programSex.plot, 
                       base_height = 3, base_width = 4, dpi = 300)
    
    
    
    ### Program x Age ----
    # Original mean: 45.25 | Subtract mean by each level
    programAge.plot <- sjPlot::plot_model(moderationAge.lmm, type = 'emm', ci.lvl = NA,
                                          terms = c('chronic[all]', 'AnthropoAge_Pre[-25.25, -15.25, -5.25, 4.75, 14.75, 24.75]')) +
      # SCALES
      scale_y_continuous(guide = 'prism_offset', limits = c(-0.5, 4)) +
      scale_color_manual(values = c('-25.25'  = '#000033',
                                    '-15.25'  = '#000066',
                                    '-5.25'   = '#000099',
                                    '4.75'    = '#0000CC',
                                    '14.75'   = '#0000FF',
                                    '24.75'   = '#3366CC')) +
      # LABELS
      labs(title = "",
           x = "Training program sessions",
           y = "Pain intensity") +
      # AESHETHICS
      theme_prism() +
      theme(
        plot.title = element_text(size = 12, face = 'bold', vjust = -5),
        panel.background = element_blank(),    
        axis.line = element_line(linewidth = .75),
        axis.ticks = element_line(linewidth =.75),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 14, face = 'bold'),
        legend.position = 'none'
        # legend.position = c(.25, .3),
        # legend.text = element_text(face = 'bold'),
        # legend.key.height = unit(.5, 'cm')
      ) 
    
    programAge.plot <- cowplot::ggdraw() +
      cowplot::draw_plot(programAge.plot) +
      cowplot::draw_plot_label(label = 'I', x = 0, y = 1, size = 20, fontface = 'bold')
    
    cowplot::save_plot('Plots_painKinetics/painKinetics_programAgePlot.tiff',
                       plot = programAge.plot, 
                       base_height = 3, base_width = 4, dpi = 300)