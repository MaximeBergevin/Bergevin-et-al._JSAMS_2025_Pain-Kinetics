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
load(file = 'Data/allOutcomes.RData')       # Data for all outcomes (long format)


## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Shapes & Colors ====
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ## Group colors ----
  # Exercise = blue; Control = yellow
  group.colors <- c(Exercise = '#0a4c9e',
                    Control = '#E69F00')
  
  # Group shapes ----
  # Exercise = squares; Control = triangles
  group.shapes <- c(Exercise = 15,
                    Control = 17)


## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# OSWESTRY DISABILITY INDEX ====
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ### Density plot PRE ----
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
  
  ### Density plot POST ----
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
  
  ### Raincloud plot ----
  odiRaincoud.plot <- ggplot() +
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
    annotation_custom(grob = odiDensity.pre, xmin = 0.4, xmax = 0.6) +
    annotation_custom(grob = odiDensity.post, xmin = 2.4, xmax = 2.6) +
    # SCALE ADJUSTMENTS
    scale_x_continuous(limits = c(0, 2.6)) +
    scale_y_continuous(limits = c(0,50), breaks = seq(0,50, length.out = 5), guide = 'prism_offset') +
    # AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_line(size = 0.25),
      axis.ticks.y = element_line(size = 0.25),
      axis.text.y = element_text(size = 14),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_blank()
    )
  
  ggsave(filename = 'odi_raincloudPlot.tiff',
         plot = odiRaincoud.plot,
         width = 4, height = 3, dpi = 'retina')
  
  ### Exercise plot ----
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
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  ggsave(filename = 'odi_exercPlot.tiff',
         plot = odiExerc.plot,
         width = 4, height = 3, dpi = 'retina')
  
  ### Waiting List plot ----
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
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  ggsave(filename = 'odi_ctrlPlot.tiff',
         plot = odiCtrl.plot,
         width = 4, height = 3, dpi = 'retina')
  
  
  ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # PAIN CATASTROPHIZING ====
  ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  
  ### Density plot PRE ----
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
  
  ### Density plot POST ----
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
  
  ### Raincloud plot ----
  pcsRaincoud.plot <- ggplot() +
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
    annotation_custom(grob = pcsDensity.pre, xmin = 0.4, xmax = 0.6) +
    annotation_custom(grob = pcsDensity.post, xmin = 2.4, xmax = 2.6) +
    # SCALE ADJUSTMENTS
    scale_x_continuous(limits = c(0, 2.6)) +
    scale_y_continuous(limits = c(0, 52), breaks = seq(0,52, length.out = 5), guide = 'prism_offset') +
    # AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_line(linewidth = 0.25),
      axis.ticks.y = element_line(size = 0.25),
      axis.text.y = element_text(size = 14),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_blank()
    )
  
  ggsave(filename = 'pcs_raincloudPlot.tiff',
         plot = pcsRaincoud.plot,
         width = 4, height = 3, dpi = 'retina')
  
  ### Exercise plot ----
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
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  ggsave(filename = 'pcs_exercPlot.tiff',
         plot = pcsExerc.plot,
         width = 4, height = 3, dpi = 'retina')
  
  ### Waiting List plot ----
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
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  ggsave(filename = 'pcs_ctrlPlot.tiff',
         plot = pcsCtrl.plot,
         width = 4, height = 3, dpi = 'retina')
  

## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# PAIN INTENSITY ====
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  
  ### Density plot PRE ----
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
  
  ### Density plot POST ----
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
  
  ### Raincloud plot ----
  nihPainRaincoud.plot <- ggplot() +
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
    annotation_custom(grob = nihPainDensity.pre, xmin = 0.4, xmax = 0.6) +
    annotation_custom(grob = nihPainDensity.post, xmin = 2.4, xmax = 2.6) +
    # SCALE ADJUSTMENTS
    scale_x_continuous(limits = c(0, 2.6)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0,10, length.out = 5), guide = 'prism_offset') +
    # AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_line(size = 0.25),
      axis.ticks.y = element_line(size = 0.25),
      axis.text = element_text(size = 14),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_blank()
    )
  
  ggsave(filename = '7dayAvg_raincloudPlot.tiff',
         plot = nihPainRaincoud.plot,
         width = 4, height = 3, dpi = 'retina')
  
  ### Exercise plot ----
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
    ylab('') +
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  ggsave(filename = '7dayAvg_exercPlot.tiff',
         plot = nihPainExerc.plot,
         width = 4, height = 3, dpi = 'retina')
  
  ### Waiting List plot ----
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
    ylab('') +
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_blank(),
      #legend.position = 'none'
    )
  
  ggsave(filename = '7dayAvg_ctrlPlot.tiff',
         plot = nihPainCtrl.plot,
         width = 4, height = 3, dpi = 'retina')
  
  
  ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # KINESOPHOBIA ====
  ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  
  ### Density plot PRE ----
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
  
  ### Density plot POST ----
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
  
  ### Raincloud plot ----
  tskRaincoud.plot <- ggplot() +
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
    annotation_custom(grob = tskDensity.pre, xmin = 0.4, xmax = 0.6) +
    annotation_custom(grob = tskDensity.post, xmin = 2.4, xmax = 2.6) +
    # SCALE ADJUSTMENTS
    scale_x_continuous(limits = c(0, 2.6)) +
    scale_y_continuous(limits = c(0, 68), breaks = seq(0, 68, length.out = 5), guide = 'prism_offset') +
    # AESTHETHICS
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_line(linewidth = 0.25),
      axis.ticks.y = element_line(size = 0.25),
      axis.text.y = element_text(size = 14),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_blank()
    )
  
  ggsave(filename = 'tsk_raincloudPlot.tiff',
         plot = tskRaincoud.plot,
         width = 4, height = 3, dpi = 'retina')
  
  ## Exercise plot ----
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
    ## THEMES
    ggprism::theme_prism() +
    theme(
      panel.background = element_blank(),    
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  ggsave(filename = 'tsk_exercPlot.tiff',
         plot = tskExerc.plot,
         width = 4, height = 3, dpi = 'retina')
  
  ## Waiting List plot ----
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
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  ggsave(filename = 'tsk_ctrlPlot.tiff',
         plot = tskCtrl.plot,
         width = 4, height = 3, dpi = 'retina')
  
  allOutcomes.long <- allOutcomes.long %>%
    filter(test == "painAvg7days") %>%
    group_by(group, time) %>%
    summarise(pain = mean(score, na.rm = TRUE))
  
  
  