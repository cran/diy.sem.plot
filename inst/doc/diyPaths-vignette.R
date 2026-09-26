## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(diy.sem.plot)
library(ggplot2)
library(lavaan)

## -----------------------------------------------------------------------------
data_cars <- mtcars

model_cars <- '
  mpg ~  wt + hp
  wt ~~ hp
'

fit_cars <- sem(model_cars, data = data_cars)

## -----------------------------------------------------------------------------
node_list <- list(
  node(name = "wt", x = 1, y = 2, label = "Weight"),
  node(name = "hp", x = 1, y = 1, label = "Horse Power"),
  node(name = "mpg", x = 3, y = 1, label = "Miles Per\n Gallon")
)

## -----------------------------------------------------------------------------

path_list <- list(
  path(from = "wt", to = "mpg", side_from = "right", side_to = "left"),
  path(from = "hp",  to = "mpg", side_from = "right", side_to = "left"),
  path(from = "wt",  to ="hp", side_from = "left", side_to = "left", cov_curve = -0.4) 
)

## ----fig.width = 10, fig.height = 8,out.width = "100%"------------------------
diyPaths(fit = fit_cars, 
         node_positions = node_list, 
         path_positions = path_list, 
         show_grid = TRUE)

## ----fig.width = 10, fig.height = 8,out.width = "100%"------------------------
node_list <- list(
  node(name = "wt", x = 1, y = 3, label = "Weight"),
  node(name = "hp", x = 1, y = 1, label = "Horse Power"),
  node(name = "mpg", x = 4, y = 2, label = "Miles Per \nGallon"))

path_list <- list(
           path(from = "wt", to = "mpg", side_from = "right", side_to = "left"),
           path(from = "hp",  to = "mpg", side_from = "right", side_to = "left"),
           path(from = "wt",  to ="hp", side_from = "left", side_to = "left", cov_curve = 0.4))

diyPaths(fit_cars, 
         node_positions = node_list,
         path_positions = path_list,
         est_stars = TRUE,
         est_ci = TRUE,
         est_p = TRUE,
         sig_linetype = TRUE,
         observed_node_text_size = 8,
         path_text_size = 6,
         line_thickness = 0.8,
         arrow_size = 0.3,
         margin_x = 1,
         margin_y =1
         )


## -----------------------------------------------------------------------------
sem_model <- '
   visual  =~ x1 + x2 + x3
   textual =~ x4 + x5 + x6
   speed   =~ x7 + x8 + x9

   speed ~ visual + textual
   visual ~~ textual
'

# fit the model

fit_sem <- sem(sem_model, data = HolzingerSwineford1939)

## ----fig.width = 10, fig.height = 8,out.width = "100%"------------------------

node_list <- list(
   # main latent variable structure
   node("visual", x = 1, y = 1, label = "Visual"),
   node("textual", x = 1, y = 2.5, label = "Textual"),
   node("speed", x = 4, y = 1.75, label = "Speed"),

   # observed variables that visual perception ability loads onto
   node("x1", x = 0.5, y = 0, label = "Visual\nPerception"),
   node("x2", x = 1, y = 0, label = "Cubes"),
   node("x3", x = 1.5, y = 0, label = "Lozenges"),

   # observed variables that textual ability loads onto
   node("x4", x = 0.5, y = 3.5, label = "Paragraph\nComprehension"),
   node("x5", x = 1, y = 3.5, label = "Sentence\nCompletion"),
   node("x6", x = 1.5, y = 3.5, label = "Word\nMeaning"),

   # observed variables that speeded cognitive processing loads onto
   node("x7", x = 5.5, y = 1, label = "Speeded\nAddition"),
   node("x8", x = 5.5, y = 1.75, label = "Speeded\nCounting"),
   node("x9", x = 5.5, y = 2.5, label = "Speeded\nDiscrimination")
)

# Specify the paths

path_list <- list(
  #Structural 
    path(from = "visual",  to = "speed", side_from = "right", side_to = "left"),
   path(from = "textual", to = "speed", side_from = "right", side_to = "left"),

   path(from = "visual",  to = "textual", side_from = "left", side_to = "left", cov_curve = -0.6),
  
   #visual loadings
   path(from = "visual", to = "x1", side_from = "bottom", side_to = "top"), 
   path(from = "visual", to = "x2", side_from = "bottom", side_to = "top"),
   path(from = "visual", to = "x3", side_from = "bottom", side_to = "top"),

   #textual loadings
   path(from = "textual", to = "x4", side_from = "top", side_to = "bottom"),
   path(from = "textual", to = "x5", side_from = "top", side_to = "bottom"),
   path(from = "textual", to = "x6", side_from = "top", side_to = "bottom"),

   #speed loadings
   path(from = "speed",   to = "x7", side_from = "right", side_to = "left"),
   path(from = "speed",   to = "x8", side_from = "right", side_to = "left"),
   path(from = "speed",   to = "x9", side_from = "right", side_to = "left")
)

# creating the diagram

p <- diyPaths(
   fit = fit_sem,
   node_positions = node_list,
   path_positions = path_list,
   standardised = TRUE,
   observed_node_size_adjust = 0.6, 
   show_grid = TRUE,
   grid_axis_scale = 0.5,
   look_up_table  = TRUE,
   show_variances = TRUE
)

print(p)

## ----fig.width = 10, fig.height = 8,out.width = "100%"------------------------

node_list <- list(
   # main latent variable structure
   node("visual", x = 1, y = 1, label = "Visual"),
   node("textual", x = 1, y = 3, label = "Textual"),
   node("speed", x = 4, y = 2, label = "Speed"),

   # observed variables that visual perception ability loads onto
   node("x1", x = -0.2, y = -0.5, label = "Visual\nPerception"),
   node("x2", x = 1, y = -0.5, label = "Cubes"),
   node("x3", x = 2.2, y = -0.5, label = "Lozenges"),

   # observed variables that textual ability loads onto
   node("x4", x = -0.2, y = 4.5, label = "Paragraph\nComprehension"),
   node("x5", x = 1, y = 4.5, label = "Sentence\nCompletion"),
   node("x6", x = 2.2, y = 4.5, label = "Word\nMeaning"),

   # observed variables that speeded cognitive processing loads onto
   node("x7", x = 6, y = 0.8, label = "Speeded\nAddition"),
   node("x8", x = 6, y = 2, label = "Speeded\nCounting"),
   node("x9", x = 6, y = 3.2, label = "Speeded\nDiscrimination")
)

# Specify the paths

path_list <- list(
  # Structural
  path(from = "visual",  to = "speed",   side_from = "right", side_to = "left"),
  path(from = "textual", to = "speed",   side_from = "right", side_to = "left"),
  path(from = "visual",  to = "textual", side_from = "left",  side_to = "left", cov_curve = -0.6),

  # Visual loadings
  path(from = "visual", to = "x1", side_from = "bottom", side_to = "top"),
  path(from = "visual", to = "x2", side_from = "bottom", side_to = "top"),
  path(from = "visual", to = "x3", side_from = "bottom", side_to = "top"),

  # Textual loadings
  path(from = "textual", to = "x4", side_from = "top", side_to = "bottom"),
  path(from = "textual", to = "x5", side_from = "top", side_to = "bottom"),
  path(from = "textual", to = "x6", side_from = "top", side_to = "bottom"),

  # Speed loadings
  path(from = "speed", to = "x7", side_from = "right", side_to = "left"),
  path(from = "speed", to = "x8", side_from = "right", side_to = "left"),
  path(from = "speed", to = "x9", side_from = "right", side_to = "left"),

  # Latent variance/residual
  path(from = "visual",  to = "visual",  variance_position = "top"),
  path(from = "textual", to = "textual", variance_position = "bottom"),
  path(from = "speed",   to = "speed",   variance_position = "top"),

  # Measurement variances/residuals 
  path(from = "x1", to = "x1", variance_position = "bottom"),
  path(from = "x2", to = "x2", variance_position = "bottom"),
  path(from = "x3", to = "x3", variance_position = "bottom"),

  path(from = "x4", to = "x4", variance_position = "top"),
  path(from = "x5", to = "x5", variance_position = "top"),
  path(from = "x6", to = "x6", variance_position = "top"),

  path(from = "x7", to = "x7", variance_position = "right"),
  path(from = "x8", to = "x8", variance_position = "right"),
  path(from = "x9", to = "x9", variance_position = "right")
)

# creating the diagram

p <- diyPaths(
   fit = fit_sem,
   node_positions = node_list,
   path_positions = path_list,
   standardised = TRUE,
   sig_linetype = TRUE,
   observed_node_size_adjust = 0.6, 
   observed_node_text_size = 3.5,
   latent_node_text_size = 5,
   est_stars = TRUE,
   est_ci = TRUE,
   show_variances = TRUE
)

print(p)

## -----------------------------------------------------------------------------
fit_sem_groups <- sem(sem_model, data = HolzingerSwineford1939, group = "school")

## ----fig.width = 10, fig.height = 8,out.width = "100%"------------------------
node_list <- list(
   # main latent variable structure
   node("visual", x = 1, y = 1, label = "Visual"),
   node("textual", x = 1, y = 3, label = "Textual"),
   node("speed", x = 4, y = 2, label = "Speed"),

   # observed variables that visual perception ability loads onto
   node("x1", x = -0.2, y = -0.5, label = "Visual\nPerception"),
   node("x2", x = 1, y = -0.5, label = "Cubes"),
   node("x3", x = 2.2, y = -0.5, label = "Lozenges"),

   # observed variables that textual ability loads onto
   node("x4", x = -0.2, y = 4.5, label = "Paragraph\nComprehension"),
   node("x5", x = 1, y = 4.5, label = "Sentence\nCompletion"),
   node("x6", x = 2.2, y = 4.5, label = "Word\nMeaning"),

   # observed variables that speeded cognitive processing loads onto
   node("x7", x = 6, y = 0.8, label = "Speeded\nAddition"),
   node("x8", x = 6, y = 2, label = "Speeded\nCounting"),
   node("x9", x = 6, y = 3.2, label = "Speeded\nDiscrimination")
)

# Specify the paths

path_list <- list(
  # Structural
  path(from = "visual",  to = "speed",   side_from = "right", side_to = "left"),
  path(from = "textual", to = "speed",   side_from = "right", side_to = "left"),
  path(from = "visual",  to = "textual", side_from = "left",  side_to = "left", cov_curve = -0.6),

  # Visual loadings
  path(from = "visual", to = "x1", side_from = "bottom", side_to = "top"),
  path(from = "visual", to = "x2", side_from = "bottom", side_to = "top"),
  path(from = "visual", to = "x3", side_from = "bottom", side_to = "top"),

  # Textual loadings
  path(from = "textual", to = "x4", side_from = "top", side_to = "bottom"),
  path(from = "textual", to = "x5", side_from = "top", side_to = "bottom"),
  path(from = "textual", to = "x6", side_from = "top", side_to = "bottom"),

  # Speed loadings
  path(from = "speed", to = "x7", side_from = "right", side_to = "left"),
  path(from = "speed", to = "x8", side_from = "right", side_to = "left"),
  path(from = "speed", to = "x9", side_from = "right", side_to = "left"),

  # Latent variance/residual
  path(from = "visual",  to = "visual",  variance_position = "top"),
  path(from = "textual", to = "textual", variance_position = "bottom"),
  path(from = "speed",   to = "speed",   variance_position = "top"),

  # Measurement variances/residuals 
  path(from = "x1", to = "x1", variance_position = "bottom"),
  path(from = "x2", to = "x2", variance_position = "bottom"),
  path(from = "x3", to = "x3", variance_position = "bottom"),

  path(from = "x4", to = "x4", variance_position = "top"),
  path(from = "x5", to = "x5", variance_position = "top"),
  path(from = "x6", to = "x6", variance_position = "top"),

  path(from = "x7", to = "x7", variance_position = "right"),
  path(from = "x8", to = "x8", variance_position = "right"),
  path(from = "x9", to = "x9", variance_position = "right")
)

# creating the diagram

p <- diyPaths(
   fit = fit_sem_groups,
   node_positions = node_list,
   path_positions = path_list,
   standardised = TRUE,
   sig_linetype = TRUE,
   observed_node_size_adjust = 0.5, 
   observed_node_text_size = 3.5,
   latent_node_text_size = 5,
   est_stars = TRUE,
   est_ci = TRUE,
   show_variances = TRUE,
   show_group_labels = TRUE
)

print(p)

## -----------------------------------------------------------------------------
title_list <- list(
  panel_title(panel_num = 1, title = "SEM Diagram: Pasteur School"),
  panel_title(panel_num = 2, title = "SEM Diagram: Grant-White School")
)

## ----fig.width = 10, fig.height = 16, out.width = "100%"----------------------
node_list <- list(
   # main latent variable structure
   node("visual", x = 1, y = 1, label = "Visual"),
   node("textual", x = 1, y = 3, label = "Textual"),
   node("speed", x = 4, y = 2, label = "Speed"),

   # observed variables that visual perception ability loads onto
   node("x1", x = -0.2, y = -0.5, label = "Visual\nPerception"),
   node("x2", x = 1, y = -0.5, label = "Cubes"),
   node("x3", x = 2.2, y = -0.5, label = "Lozenges"),

   # observed variables that textual ability loads onto
   node("x4", x = -0.2, y = 4.5, label = "Paragraph\nComprehension"),
   node("x5", x = 1, y = 4.5, label = "Sentence\nCompletion"),
   node("x6", x = 2.2, y = 4.5, label = "Word\nMeaning"),

   # observed variables that speeded cognitive processing loads onto
   node("x7", x = 6, y = 0.8, label = "Speeded\nAddition"),
   node("x8", x = 6, y = 2, label = "Speeded\nCounting"),
   node("x9", x = 6, y = 3.2, label = "Speeded\nDiscrimination")
)

# Specify the paths

path_list <- list(
  # Structural
  path(from = "visual",  to = "speed",   side_from = "right", side_to = "left"),
  path(from = "textual", to = "speed",   side_from = "right", side_to = "left"),
  path(from = "visual",  to = "textual", side_from = "left",  side_to = "left", cov_curve = -0.6),

  # Visual loadings
  path(from = "visual", to = "x1", side_from = "bottom", side_to = "top"),
  path(from = "visual", to = "x2", side_from = "bottom", side_to = "top"),
  path(from = "visual", to = "x3", side_from = "bottom", side_to = "top"),

  # Textual loadings
  path(from = "textual", to = "x4", side_from = "top", side_to = "bottom"),
  path(from = "textual", to = "x5", side_from = "top", side_to = "bottom"),
  path(from = "textual", to = "x6", side_from = "top", side_to = "bottom"),

  # Speed loadings
  path(from = "speed", to = "x7", side_from = "right", side_to = "left"),
  path(from = "speed", to = "x8", side_from = "right", side_to = "left"),
  path(from = "speed", to = "x9", side_from = "right", side_to = "left"),

  # Latent variance/residual
  path(from = "visual",  to = "visual",  variance_position = "top"),
  path(from = "textual", to = "textual", variance_position = "bottom"),
  path(from = "speed",   to = "speed",   variance_position = "top"),

  # Measurement variances/residuals 
  path(from = "x1", to = "x1", variance_position = "bottom"),
  path(from = "x2", to = "x2", variance_position = "bottom"),
  path(from = "x3", to = "x3", variance_position = "bottom"),

  path(from = "x4", to = "x4", variance_position = "top"),
  path(from = "x5", to = "x5", variance_position = "top"),
  path(from = "x6", to = "x6", variance_position = "top"),

  path(from = "x7", to = "x7", variance_position = "right"),
  path(from = "x8", to = "x8", variance_position = "right"),
  path(from = "x9", to = "x9", variance_position = "right")
)

# creating the diagram

p <- diyPaths(
   fit = fit_sem_groups,
   node_positions = node_list,
   path_positions = path_list,
   panel_titles = title_list,
   standardised = TRUE,
   sig_linetype = TRUE,
   observed_node_size_adjust = 0.6,
   observed_node_text_size = 3.5,
   path_text_size = 3.5,
   latent_node_text_size = 5,
   est_stars = TRUE,
   est_ci = TRUE,
   show_variances = TRUE,
   panel_cols = 1
)

print(p)

