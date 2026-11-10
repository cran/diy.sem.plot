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
  wt  ~ a * hp
  mpg ~ b * wt + c * hp + disp
  disp ~~ hp

  indirect := a * b
  total := c + a * b
'

fit_cars <- sem(model = model_cars, data = data_cars)

## ----comment = ""-------------------------------------------------------------
template(fit = fit_cars, include_variance_paths = FALSE, diyPaths_arguments = "min", quiet = FALSE)

## -----------------------------------------------------------------------------
node_list <- list(
  #node(s) for observed variable(s)
  node(name = "wt", x = 2, y = 2.5, label = "Weight"),
  node(name = "mpg", x = 4, y = 1, label = "Miles Per\nGallon"),
  node(name = "disp", x = 0, y = -0.5, label = "Engine\nDisplacement"),
  node(name = "hp", x = 0, y = 1, label = "Horsepower")
)

## -----------------------------------------------------------------------------
path_list <- list(
  # regression path(s)
  path(from = "hp", to = "wt", side_from = "top", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
  path(from = "wt", to = "mpg", side_from = "right", side_to = "top", nudge_text_x = 0, nudge_text_y = 0),
  path(from = "hp", to = "mpg", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
  path(from = "disp", to = "mpg", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),

  # covariance/correlation path(s)
  path(from = "disp", to = "hp", side_from = "left", side_to = "left", nudge_text_x = 0, nudge_text_y = 0, cov_curve = -0.4)
)

## -----------------------------------------------------------------------------
title_list <- list(
      panel_title(panel_num = 1, title = "**Figure 1**<br>*Mediation of Horsepower on MPG by Weight*")
)

note_list <- list(
  panel_note(panel_num = 1, note_text = "*Note.* Indirect effect = -0.036 (SE = 0.009, p < .001)")
)

## ----fig.width = 10, fig.height = 8, out.width = "100%"-----------------------
diyPaths(fit = fit_cars,
         node_positions = node_list,
         path_positions = path_list,
         panel_titles = title_list,
         panel_notes = note_list,
         show_grid = TRUE)

## ----fig.width = 10, fig.height = 8,out.width = "100%"------------------------
path_list <- list(
  # regression path(s)
  path(from = "hp", to = "wt", side_from = "top", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
  path(from = "wt", to = "mpg", side_from = "right", side_to = "top", nudge_text_x = 0, nudge_text_y = 0),
  path(from = "hp", to = "mpg", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
  path(from = "disp", to = "mpg", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),

  # covariance/correlation path(s)
     path(from = "disp", to = "hp", side_from = "left", side_to = "left", nudge_text_x = 0, nudge_text_y = 0, cov_curve = 0.4))

diyPaths(fit = fit_cars,
         node_positions = node_list,
         path_positions = path_list,
         panel_titles = title_list,
         panel_notes = note_list,
         est_stars = TRUE,
         est_ci = TRUE,
         est_p = TRUE,
         sig_linetype = TRUE,
         observed_node_text_size = 6,
         path_text_size = 4,
         title_text_size = 18,
         note_text_size = 14,
         font_family = "sans",
         title_note_font_family = "serif",
         line_thickness = 0.8,
         arrow_size = 0.3,
         margin_x = 1,
         margin_y = 0.5)


## -----------------------------------------------------------------------------
sem_model <- '
   visual  =~ x1 + x2 + x3
   textual =~ x4 + x5 + x6
   speed   =~ x7 + x8 + x9

   speed ~ visual + textual
   visual ~~ textual
'

# fit the model

fit_sem <- sem(model = sem_model, data = HolzingerSwineford1939)

## -----------------------------------------------------------------------------
template(fit = fit_sem, include_variance_paths = TRUE, quiet = TRUE, diyPaths_arguments = "min")

## ----fig.width = 10, fig.height = 8, out.width = "100%"-----------------------
node_list <- list(
   #node(s) for latent variable(s)
   node(name = "visual", x = 1, y = 1, label = "Visual"),
   node(name = "textual", x = 1, y = 2.5, label = "Textual"),
   node(name = "speed", x = 4, y = 1.75, label = "Speed"),

   #node(s) for observed variables loading onto latent variable visual
   node(name = "x1", x = 0.5, y = 0, label = "Visual\nPerception"),
   node(name = "x2", x = 1, y = 0, label = "Cubes"),
   node(name = "x3", x = 1.5, y = 0, label = "Lozenges"),

   #node(s) for observed variables loading onto latent variable textual
   node(name = "x4", x = 0.5, y = 3.5, label = "Paragraph\nComprehension"),
   node(name = "x5", x = 1, y = 3.5, label = "Sentence\nCompletion"),
   node(name = "x6", x = 1.5, y = 3.5, label = "Word\nMeaning"),

   #node(s) for observed variables loading onto latent variable speed
   node(name = "x7", x = 5.5, y = 1, label = "Speeded\nAddition"),
   node(name = "x8", x = 5.5, y = 1.75, label = "Speeded\nCounting"),
   node(name = "x9", x = 5.5, y = 2.5, label = "Speeded\nDiscrimination")
)

path_list <- list(
   # regression path(s)
   path(from = "visual", to = "speed", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "textual", to = "speed", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),

   #loading path(s) from visual
   path(from = "visual", to = "x1", side_from = "bottom", side_to = "top", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "visual", to = "x2", side_from = "bottom", side_to = "top", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "visual", to = "x3", side_from = "bottom", side_to = "top", nudge_text_x = 0, nudge_text_y = 0),

   #loading path(s) from textual
   path(from = "textual", to = "x4", side_from = "top", side_to = "bottom", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "textual", to = "x5", side_from = "top", side_to = "bottom", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "textual", to = "x6", side_from = "top", side_to = "bottom", nudge_text_x = 0, nudge_text_y = 0),

   #loading path(s) from speed
   path(from = "speed", to = "x7", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "speed", to = "x8", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "speed", to = "x9", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),

   # covariance/correlation path(s)
   path(from = "textual", to = "visual", side_from = "left", side_to = "left", nudge_text_x = 0, nudge_text_y = 0, cov_curve = -0.6),

   # variance path(s)
   path(from = "x1", to = "x1", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x2", to = "x2", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x3", to = "x3", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x4", to = "x4", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x5", to = "x5", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x6", to = "x6", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x7", to = "x7", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x8", to = "x8", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x9", to = "x9", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "visual", to = "visual", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "textual", to = "textual", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "speed", to = "speed", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top")
)


title_list <- list(
   panel_title(panel_num = 1, title = "**Figure 2**<br>*Visual and Textual Ability Predicting Speeded Cognitive Processing*")
)

p <- diyPaths(
   fit = fit_sem,
   node_positions = node_list,
   path_positions = path_list,
   panel_titles = title_list,
   standardised = TRUE,
   observed_node_size_adjust = 0.6,
   show_grid = TRUE,
   grid_axis_scale = 0.5,
   look_up_table = TRUE,
   show_variances = TRUE
)

print(p)

## ----fig.width = 10, fig.height = 8, out.width = "100%"-----------------------
node_list <- list(
   #node(s) for latent variable(s)
   node(name = "visual", x = 1, y = 1, label = "Visual"),
   node(name = "textual", x = 1, y = 3, label = "Textual"),
   node(name = "speed", x = 4, y = 2, label = "Speed"),

   #node(s) for observed variables loading onto latent variable visual
   node(name = "x1", x = -0.2, y = -0.5, label = "Visual\nPerception"),
   node(name = "x2", x = 1, y = -0.5, label = "Cubes"),
   node(name = "x3", x = 2.2, y = -0.5, label = "Lozenges"),

   #node(s) for observed variables loading onto latent variable textual
   node(name = "x4", x = -0.2, y = 4.5, label = "Paragraph\nComprehension"),
   node(name = "x5", x = 1, y = 4.5, label = "Sentence\nCompletion"),
   node(name = "x6", x = 2.2, y = 4.5, label = "Word\nMeaning"),

   #node(s) for observed variables loading onto latent variable speed
   node(name = "x7", x = 6, y = 0.8, label = "Speeded\nAddition"),
   node(name = "x8", x = 6, y = 2, label = "Speeded\nCounting"),
   node(name = "x9", x = 6, y = 3.2, label = "Speeded\nDiscrimination")
)

path_list <- list(
   # regression path(s)
   path(from = "visual", to = "speed", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "textual", to = "speed", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),

   #loading path(s) from visual
   path(from = "visual", to = "x1", side_from = "bottom", side_to = "top", nudge_text_x = -0.1, nudge_text_y = 0),
   path(from = "visual", to = "x2", side_from = "bottom", side_to = "top", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "visual", to = "x3", side_from = "bottom", side_to = "top", nudge_text_x = 0.1, nudge_text_y = 0),

   #loading path(s) from textual
   path(from = "textual", to = "x4", side_from = "top", side_to = "bottom", nudge_text_x = -0.1, nudge_text_y = 0),
   path(from = "textual", to = "x5", side_from = "top", side_to = "bottom", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "textual", to = "x6", side_from = "top", side_to = "bottom", nudge_text_x = 0.1, nudge_text_y = 0),

   #loading path(s) from speed
   path(from = "speed", to = "x7", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "speed", to = "x8", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
   path(from = "speed", to = "x9", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),

   # covariance/correlation path(s)
   path(from = "textual", to = "visual", side_from = "left", side_to = "left", nudge_text_x = 0, nudge_text_y = 0, cov_curve = -0.6),

   # variance path(s)
   path(from = "x1", to = "x1", nudge_text_x = 0, nudge_text_y = 0, variance_position = "bottom"),
   path(from = "x2", to = "x2", nudge_text_x = 0, nudge_text_y = 0, variance_position = "bottom"),
   path(from = "x3", to = "x3", nudge_text_x = 0, nudge_text_y = 0, variance_position = "bottom"),
   path(from = "x4", to = "x4", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x5", to = "x5", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x6", to = "x6", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "x7", to = "x7", nudge_text_x = 0, nudge_text_y = 0, variance_position = "right"),
   path(from = "x8", to = "x8", nudge_text_x = 0, nudge_text_y = 0, variance_position = "right"),
   path(from = "x9", to = "x9", nudge_text_x = 0, nudge_text_y = 0, variance_position = "right"),
   path(from = "visual", to = "visual", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
   path(from = "textual", to = "textual", nudge_text_x = 0, nudge_text_y = 0, variance_position = "bottom"),
   path(from = "speed", to = "speed", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top")
)

title_list <- list(
   panel_title(panel_num = 1, title = "**Figure 2**<br>*Visual and Textual Ability Predicting Speeded Cognitive Processing*")
)

p <- diyPaths(
   fit = fit_sem,
   node_positions = node_list,
   path_positions = path_list,
   panel_titles = title_list,
   standardised = TRUE,
   sig_linetype = TRUE,
   observed_node_size_adjust = 0.6,
   observed_node_text_size = 3.5,
   latent_node_text_size = 5,
   est_stars = TRUE,
   est_ci = TRUE,
   show_variances = TRUE,
   title_text_size = 16,
   font_family = "sans",
   title_note_font_family = "serif"
)

print(p)

## -----------------------------------------------------------------------------
fit_sem_groups <- sem(model = sem_model, data = HolzingerSwineford1939, group = "school")

## ----fig.width = 10, fig.height = 8, out.width = "100%"-----------------------
p <- diyPaths(
   fit = fit_sem_groups,
   node_positions = node_list,
   path_positions = path_list,
   standardised = TRUE,
   sig_linetype = TRUE,
   observed_node_size_adjust = 0.6,
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
  panel_title(panel_num = 1, title = "**Figure 3a**<br>*SEM Diagram: Pasteur School*"),
  panel_title(panel_num = 2, title = "**Figure 3b**<br>*SEM Diagram: Grant-White School*")
)

note_list <- list(
  panel_note(panel_num = 1, note_text = "*Note.* n = 156"),
  panel_note(panel_num = 2, note_text = "*Note.* n = 145")
)

## ----fig.width = 10, fig.height = 16, out.width = "100%"----------------------
p <- diyPaths(
   fit = fit_sem_groups,
   node_positions = node_list,
   path_positions = path_list,
   panel_titles = title_list,
   panel_notes = note_list,
   standardised = TRUE,
   sig_linetype = TRUE,
   observed_node_size_adjust = 0.6,
   observed_node_text_size = 3.5,
   path_text_size = 3.5,
   latent_node_text_size = 5,
   est_stars = TRUE,
   est_ci = TRUE,
   show_variances = TRUE,
   title_text_size = 16,
   note_text_size = 12,
   font_family = "sans",
   title_note_font_family = "serif",
   panel_cols = 1
)

print(p)

