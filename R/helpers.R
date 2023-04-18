#==========================================================================================
# Helper function to create the ggplot2-graphics for the reference values

#------------------------------------------------------------------------------------------
# Create plotting data frame
#------------------------------------------------------------------------------------------

#' Predict centiles and plot them using ggplot2
#'
#' This function generates the centiles predicted by a model and plots them using ggplot2.
#'
#' @param mod a statistical model object.
#' @return a data.frame containing the predicted centiles.
#' @import ggplot2
#' @examples
#' mod <- lm(mpg ~ wt + cyl + hp, data = mtcars)
#' create_predframe(mod)

create_predframe <- function(mod) {

  #------------------------------------------------------------------------------------------
  # Predict centiles and plot them using ggplot2
  #------------------------------------------------------------------------------------------

  # Generate the percentiles from the models

  centiles_pred <- c(3, 15, 50, 85, 97)

  predmat <- centiles.pred(
    obj = mod
    , type = "centiles"
    , xname = "Age"
    , xvalues = seq(20, 91, by = 0.1)
    , cent = centiles_pred
  )

  names(predmat)[1] <- "Age"
  names(predmat)[2:(length(centiles_pred) + 1)] <- paste0(centiles_pred, " %")

  # Reshape to long format for plotting

  predmat_long <- reshape(
    data = predmat
    , direction = "long"
    , varying = list(value = paste0(centiles_pred, " %"))
    , v.names = "value"
    , idvar = "Age"
    , timevar = "percentile"
    , times = paste0(centiles_pred, " %")
  )

  # Set the linetype for each percentile

  predmat_long$line_ind <- NA
  predmat_long$line_ind[predmat_long$percentile %in% c("50 %")] <- 0
  predmat_long$line_ind[predmat_long$percentile %in% c("15 %", "85 %")] <- 1
  predmat_long$line_ind[predmat_long$percentile %in% c("3 %", "97 %")] <- 2

  predmat_long$line_ind <- factor(predmat_long$line_ind)

  predmat_long

}

#------------------------------------------------------------------------------------------
# Create ggplot2-graph for individual data
#------------------------------------------------------------------------------------------
#' Create a publication-quality plot
#'
#' This function creates a publication-quality plot with two subplots. The first subplot shows the average acceleration in mg over different percentiles of age, and the second subplot shows the intensity gradient over different percentiles of age. The function takes two arguments:
#'
#' \code{mod}: A model object that includes the predictions for the average acceleration and the intensity gradient. The object should have two model components with the strings "avacc" and "ig" in their names, respectively. The function uses \code{create_predframe} to create the data frames for each of the subplots.
#'
#' \code{input_frame}: A data frame that includes the measured values for the average acceleration and the intensity gradient, as well as the age of the participants. The data frame should have three columns: "age", "parameter", and "value".
#'
#' @param mod A model object that includes the predictions for the average acceleration and the intensity gradient.
#' @param input_frame A data frame that includes the measured values for the average acceleration and the intensity gradient, as well as the age of the participants.
#'
#' @return The function returns a publication-quality plot.
#'
#' @examples
#' create_plot_i(mod = my_model, input_frame = my_data_frame)

create_plot_i <- function(mod, input_frame) {

  # gender <- unique(input_frame$gender)
  model_names <- names(mod)

  plot_frame_avacc <- create_predframe(mod[[grep("avacc", model_names)]])
  plot_frame_ig <- create_predframe(mod[[grep("ig", model_names)]])

  # print(plot_frame_avacc)

  #------------------------------------------------------------------------------------------
  # Publication plot
  #------------------------------------------------------------------------------------------

  # Rename levels of percentiles

  my.dl <- list(box.color=NA, "draw.rects")

  theme_set(theme_bw())
  p_avacc <- ggplot(plot_frame_avacc, aes(x = Age, y = value, group = percentile)) +
    geom_line(aes(linetype = line_ind), linewidth = 1) +
    geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
    geom_point(data = subset(input_frame, parameter %in% "avacc"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
    xlab("Age in years") +
    ylab(expression(paste("Average acceleration in m", italic('g')))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
    scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
    scale_color_brewer(palette = "Dark2", name = "Percentile") +
    theme(
      axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
      , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
      ,  axis.title.x = element_text(size = 17, hjust = 0.5)
      , legend.position = "none"
      , axis.text.x = element_text(colour = "black", size=14)
      , axis.text.y = element_text(colour = "black", size=15)
      , legend.text = element_text(size= 15)
      , plot.title = element_text(face = "bold")
      , legend.title = element_text(size = 15)
      , panel.grid.minor = element_line(colour = "gray80")
      , panel.grid.major = element_line(colour = "gray80")
    )

  theme_set(theme_bw())
  p_ig <- ggplot(plot_frame_ig, aes(x = Age, y = value, group = percentile)) +
    geom_line(aes(linetype = line_ind), linewidth = 1) +
    geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
    geom_point(data = subset(input_frame, parameter %in% "ig"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
    xlab("Age in years") +
    ylab(expression(paste("Intensity gradient"))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
    scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
    scale_color_brewer(palette = "Dark2", name = "Percentile") +
    theme(
      axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
      , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
      ,  axis.title.x = element_text(size = 17, hjust = 0.5)
      , legend.position = "none"
      , axis.text.x = element_text(colour = "black", size=14)
      , axis.text.y = element_text(colour = "black", size=15)
      , legend.text = element_text(size= 15)
      , plot.title = element_text(face = "bold")
      , legend.title = element_text(size = 15)
      , panel.grid.minor = element_line(colour = "gray80")
      , panel.grid.major = element_line(colour = "gray80")
    )

  ref_plot <- ggarrange(
    p_avacc
    , p_ig
    , nrow = 2
    , ncol = 1
    , common.legend = FALSE
    , align = c("v")
  )

  ref_plot <- annotate_figure(
    ref_plot
    # , top = text_grob(unique(input_frame$ID), size = 23)
    , bottom = text_grob(paste0("Created on: ", format(Sys.Date(), "%d.%m.%Y")), size = 12, hjust = 1, x = 1)
  )

  return(ref_plot)

}

#------------------------------------------------------------------------------------------
# Create ggplot2-graph for cohort data stratified
#------------------------------------------------------------------------------------------

#' Creates a plot for the input model
#'
#' The function takes a model object and an input frame to create a plot of predicted values.
#' The plot shows the predicted values for two parameters (avacc and ig) for males and females.
#' It also shows the percentiles of the predicted values for each parameter and sex
#'
#' @param mod a model object
#' @param input_frame a data frame containing input data for the model
#' @return A plot of predicted values for avacc and ig parameters for males and females
#' @examples
#' mod <- lm(avacc_m ~ age, data = input_data)
#' input_frame <- data.frame(parameter = c("avacc", "avacc", "ig", "ig"), gender = c("m", "f", "m", "f"),
#' age = c(20, 25, 30, 35), values = c(2.5, 3.1, 1.5, 2.0))
#' create_plot_g(mod, input_frame)

create_plot_g <- function(mod, input_frame) {

  # print(input_frame)

  # gender <- unique(input_frame$gender)
  model_names <- names(mod)

  plot_frame_avacc_m <- create_predframe(mod[[grep("(?=.*avacc)(?=.*_m)", model_names, perl = TRUE)]])
  plot_frame_avacc_f <- create_predframe(mod[[grep("(?=.*avacc)(?=.*_f)", model_names, perl = TRUE)]])

  plot_frame_ig_m <- create_predframe(mod[[grep("(?=.*ig)(?=.*_m)", model_names, perl = TRUE)]])
  plot_frame_ig_f <- create_predframe(mod[[grep("(?=.*ig)(?=.*_f)", model_names, perl = TRUE)]])

  #------------------------------------------------------------------------------------------
  # Publication plot
  #------------------------------------------------------------------------------------------

  # Rename levels of percentiles

  my.dl <- list(box.color=NA, "draw.rects")

  theme_set(theme_bw())
  p_avacc_m <- ggplot(plot_frame_avacc_m, aes(x = Age, y = value, group = percentile)) +
    geom_line(aes(linetype = line_ind), linewidth = 1) +
    geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
    geom_point(data = subset(input_frame, parameter %in% "avacc" & gender %in% "m"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
    # geom_label(data = subset(input_frame, parameter %in% "avacc" & gender %in% "m"), aes(label = ID, x = age, y = values), hover = TRUE)+
    labs(
      x = "Age in years"
      , y = expression(paste("Average acceleration in m", italic('g')))
      , title = "Males"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
    scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
    scale_color_brewer(palette = "Dark2", name = "Percentile") +
    theme(
      axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
      , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
      ,  axis.title.x = element_text(size = 17, hjust = 0.5)
      , legend.position = "none"
      , axis.text.x = element_text(colour = "black", size=14)
      , axis.text.y = element_text(colour = "black", size=15)
      , legend.text = element_text(size= 15)
      , plot.title = element_text(face = "bold")
      , legend.title = element_text(size = 15)
      , panel.grid.minor = element_line(colour = "gray80")
      , panel.grid.major = element_line(colour = "gray80")
    )

  theme_set(theme_bw())
  p_avacc_f <- ggplot(plot_frame_avacc_f, aes(x = Age, y = value, group = percentile)) +
    geom_line(aes(linetype = line_ind), linewidth = 1) +
    geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
    geom_point(data = subset(input_frame, parameter %in% "avacc" & gender %in% "f"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
    labs(
      x = "Age in years"
      , y = expression(paste("Average acceleration in m", italic('g')))
      , title = "Females"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
    scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
    scale_color_brewer(palette = "Dark2", name = "Percentile") +
    theme(
      axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
      , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
      ,  axis.title.x = element_text(size = 17, hjust = 0.5)
      , legend.position = "none"
      , axis.text.x = element_text(colour = "black", size=14)
      , axis.text.y = element_text(colour = "black", size=15)
      , legend.text = element_text(size= 15)
      , plot.title = element_text(face = "bold")
      , legend.title = element_text(size = 15)
      , panel.grid.minor = element_line(colour = "gray80")
      , panel.grid.major = element_line(colour = "gray80")
    )

  theme_set(theme_bw())
  p_ig_m <- ggplot(plot_frame_ig_m, aes(x = Age, y = value, group = percentile)) +
    geom_line(aes(linetype = line_ind), linewidth = 1) +
    geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
    geom_point(data = subset(input_frame, parameter %in% "ig" & gender %in% "m"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
    labs(
      x = "Age in years"
      , y = "Intensity gradient"
      , title = "Males"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
    scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
    scale_color_brewer(palette = "Dark2", name = "Percentile") +
    theme(
      axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
      , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
      ,  axis.title.x = element_text(size = 17, hjust = 0.5)
      , legend.position = "none"
      , axis.text.x = element_text(colour = "black", size=14)
      , axis.text.y = element_text(colour = "black", size=15)
      , legend.text = element_text(size= 15)
      , plot.title = element_text(face = "bold")
      , legend.title = element_text(size = 15)
      , panel.grid.minor = element_line(colour = "gray80")
      , panel.grid.major = element_line(colour = "gray80")
    )

  theme_set(theme_bw())
  p_ig_f <- ggplot(plot_frame_ig_f, aes(x = Age, y = value, group = percentile)) +
    geom_line(aes(linetype = line_ind), linewidth = 1) +
    geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
    geom_point(data = subset(input_frame, parameter %in% "ig" & gender %in% "f"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
    labs(
      x = "Age in years"
      , y = "Intensity gradient"
      , title = "Females"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
    scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
    scale_color_brewer(palette = "Dark2", name = "Percentile") +
    theme(
      axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
      , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
      ,  axis.title.x = element_text(size = 17, hjust = 0.5)
      , legend.position = "none"
      , axis.text.x = element_text(colour = "black", size=14)
      , axis.text.y = element_text(colour = "black", size=15)
      , legend.text = element_text(size= 15)
      , plot.title = element_text(face = "bold")
      , legend.title = element_text(size = 15)
      , panel.grid.minor = element_line(colour = "gray80")
      , panel.grid.major = element_line(colour = "gray80")
    )

  # # If there are no data for any combination of parameter and sex, don't generate a plot for that
  #
  # n_plots <- 0
  # plot_list <- vector("list", length = 0)
  #
  # if (sum(input_frame$gender %in% "m" & input_frame$parameter %in% "avacc") > 0) {
  #   n_plots <- n_plots + 1
  #   plot_list <- append(plot_list, list(p_avacc_m))
  # }
  #
  # if (sum(input_frame$gender %in% "m" & input_frame$parameter %in% "ig") > 0) {
  #   n_plots <- n_plots + 1
  #   plot_list <- append(plot_list, list(p_ig_m))
  # }
  #
  # print(sum(input_frame$gender %in% "f" & input_frame$parameter %in% "avacc"))
  #
  # if (sum(input_frame$gender %in% "f" & input_frame$parameter %in% "avacc") > 0) {
  #   n_plots <- n_plots + 1
  #   plot_list <- append(plot_list, list(p_avacc_f))
  # }
  #
  # if (sum(input_frame$gender %in% "f" & input_frame$parameter %in% "ig") > 0) {
  #   n_plots <- n_plots + 1
  #   plot_list <- append(plot_list, list(p_ig_f))
  # }
  #
  # ref_plot <- ggarrange(
  #   plotlist = plot_list
  #   , nrow = c(1, 2, 2, 2)[n_plots]
  #   , ncol = c(1, 1, 2, 2)[n_plots]
  #   , common.legend = FALSE
  #   , align = c("v")
  #   , heights = 1
  #   , widths = 1
  # )

    ref_plot <- ggarrange(
      p_avacc_m
      , p_avacc_f
      , p_ig_m
      , p_ig_f
      , nrow = 2
      , ncol = 2
      , common.legend = FALSE
      , align = c("v")
      , heights = 1
      , widths = 1
    )

  ref_plot <- annotate_figure(
    ref_plot
    # , top = text_grob(unique(input_frame$ID), size = 23)
    , bottom = text_grob(paste0("Created on: ", format(Sys.Date(), "%d.%m.%Y")), size = 12, hjust = 1, x = 1)
  )

  return(ref_plot)

}

# #------------------------------------------------------------------------------------------
# # Plots for cohort-level data (raw) with all raw data points included
# #------------------------------------------------------------------------------------------
#
#
# create_plot_r <- function(mod, input_frame) {
#
#   print(input_frame)
#   # print(mod)
#
#   # gender <- unique(input_frame$gender)
#   model_names <- names(mod)
#
#   plot_frame_avacc_m <- create_predframe(mod[[grep("(?=.*avacc)(?=.*_m)", model_names, perl = TRUE)]])
#   plot_frame_avacc_f <- create_predframe(mod[[grep("(?=.*avacc)(?=.*_f)", model_names, perl = TRUE)]])
#
#   plot_frame_ig_m <- create_predframe(mod[[grep("(?=.*ig)(?=.*_m)", model_names, perl = TRUE)]])
#   plot_frame_ig_f <- create_predframe(mod[[grep("(?=.*ig)(?=.*_f)", model_names, perl = TRUE)]])
#
#   #------------------------------------------------------------------------------------------
#   # Publication plot
#   #------------------------------------------------------------------------------------------
#
#   # Rename levels of percentiles
#
#   my.dl <- list(box.color=NA, "draw.rects")
#
#   theme_set(theme_bw())
#   p_avacc_m <- ggplot(plot_frame_avacc_m, aes(x = Age, y = value, group = percentile)) +
#     geom_line(aes(linetype = line_ind), linewidth = 1) +
#     geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
#     geom_point(data = subset(input_frame, parameter %in% "avacc" & gender %in% "m"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
#     labs(
#       x = "Age in years"
#       , y = expression(paste("Average acceleration in m", italic('g')))
#       , title = "Males"
#     ) +
#     #geom_point(data = subset(df, parameter %in% "avacc" & gender %in% "m")) +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
#     scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
#     scale_color_brewer(palette = "Dark2", name = "Percentile") +
#     theme(
#       axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
#       , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
#       ,  axis.title.x = element_text(size = 17, hjust = 0.5)
#       , legend.position = "none"
#       , axis.text.x = element_text(colour = "black", size=14)
#       , axis.text.y = element_text(colour = "black", size=15)
#       , legend.text = element_text(size= 15)
#       , plot.title = element_text(face = "bold")
#       , legend.title = element_text(size = 15)
#       , panel.grid.minor = element_line(colour = "gray80")
#       , panel.grid.major = element_line(colour = "gray80")
#     )
#
#   theme_set(theme_bw())
#   p_avacc_f <- ggplot(plot_frame_avacc_f, aes(x = Age, y = value, group = percentile)) +
#     geom_line(aes(linetype = line_ind), linewidth = 1) +
#     geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
#     geom_point(data = subset(input_frame, parameter %in% "avacc" & gender %in% "f"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
#     labs(
#       x = "Age in years"
#       , y = expression(paste("Average acceleration in m", italic('g')))
#       , title = "Females"
#     ) +
#     #geom_point(data = subset(df, parameter %in% "ig" & gender %in% "f")) +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
#     scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
#     scale_color_brewer(palette = "Dark2", name = "Percentile") +
#     theme(
#       axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
#       , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
#       ,  axis.title.x = element_text(size = 17, hjust = 0.5)
#       , legend.position = "none"
#       , axis.text.x = element_text(colour = "black", size=14)
#       , axis.text.y = element_text(colour = "black", size=15)
#       , legend.text = element_text(size= 15)
#       , plot.title = element_text(face = "bold")
#       , legend.title = element_text(size = 15)
#       , panel.grid.minor = element_line(colour = "gray80")
#       , panel.grid.major = element_line(colour = "gray80")
#     )
#
#   theme_set(theme_bw())
#   p_ig_m <- ggplot(plot_frame_ig_m, aes(x = Age, y = value, group = percentile)) +
#     geom_line(aes(linetype = line_ind), linewidth = 1) +
#     geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
#     geom_point(data = subset(input_frame, parameter %in% "ig" & gender %in% "m"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
#     labs(
#       x = "Age in years"
#       , y = "Intensity gradient"
#       , title = "Males"
#     ) +
#     #geom_point(data = subset(df, parameter %in% "ig" & gender %in% "m")) +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
#     scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
#     scale_color_brewer(palette = "Dark2", name = "Percentile") +
#     theme(
#       axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
#       , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
#       ,  axis.title.x = element_text(size = 17, hjust = 0.5)
#       , legend.position = "none"
#       , axis.text.x = element_text(colour = "black", size=14)
#       , axis.text.y = element_text(colour = "black", size=15)
#       , legend.text = element_text(size= 15)
#       , plot.title = element_text(face = "bold")
#       , legend.title = element_text(size = 15)
#       , panel.grid.minor = element_line(colour = "gray80")
#       , panel.grid.major = element_line(colour = "gray80")
#     )
#
#   theme_set(theme_bw())
#   p_ig_f <- ggplot(plot_frame_ig_f, aes(x = Age, y = value, group = percentile)) +
#     geom_line(aes(linetype = line_ind), linewidth = 1) +
#     geom_dl(aes(label = percentile), method = list(list("last.points", "calc.boxes", "enlarge.box", my.dl), dl.trans(x = x + 0.2), cex = 1.2)) +
#     geom_point(data = subset(input_frame, parameter %in% "ig" & gender %in% "f"), aes(x = age, y = values, colour = "green"), size = 5, inherit.aes = FALSE)+
#     labs(
#       x = "Age in years"
#       , y = "Intensity gradient"
#       , title = "Females"
#     ) +
#    # geom_point(data = subset(df, parameter %in% "ig" & gender %in% "f")) +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(NA, NA)) +
#     scale_x_continuous(breaks = seq(0, 90, 10), limits = c(NA, 100)) + # Format x-axis
#     scale_color_brewer(palette = "Dark2", name = "Percentile") +
#     theme(
#       axis.title.y.left = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0))
#       , axis.title.y.right = element_text(size = 17, hjust = 0.5, margin = margin(t = 0, r = 0, b = 0, l = 15))
#       ,  axis.title.x = element_text(size = 17, hjust = 0.5)
#       , legend.position = "none"
#       , axis.text.x = element_text(colour = "black", size=14)
#       , axis.text.y = element_text(colour = "black", size=15)
#       , legend.text = element_text(size= 15)
#       , plot.title = element_text(face = "bold")
#       , legend.title = element_text(size = 15)
#       , panel.grid.minor = element_line(colour = "gray80")
#       , panel.grid.major = element_line(colour = "gray80")
#     )
#
#   ref_plot <- ggarrange(
#     p_avacc_m
#     , p_avacc_f
#     , p_ig_m
#     , p_ig_f
#     , nrow = 2
#     , ncol = 2
#     , common.legend = FALSE
#     , align = c("v")
#     , heights = 1
#     , widths = 1
#   )
#
#   ref_plot <- annotate_figure(
#     ref_plot
#     # , top = text_grob(unique(input_frame$ID), size = 23)
#     , bottom = text_grob(paste0("Created on: ", format(Sys.Date(), "%d.%m.%Y")), size = 12, hjust = 1, x = 1)
#   )
#
#   return(ref_plot)
#
# }

#------------------------------------------------------------------------------------------
# Use a model to calculate how much the inputs have to change to change the predicted
# value by a specified percentage
#------------------------------------------------------------------------------------------

#' Find Delta X
#'
#' This function finds the change in a specific variable \code{fix} in a data frame \code{myvals}
#' needed to achieve a specified percentage increase in the predicted response variable
#' \code{mod}. The function uses the \code{uniroot} function to find the value of \code{x} that
#' satisfies the equation \code{predict(mod, newdata = tmp_frame) - mygoal = 0}.
#'
#' @param mod a fitted model object
#' @param myvals a data frame of predictor variables
#' @param fix a character vector specifying the variable to hold constant while finding delta x
#' @param delta_y_perc a numeric scalar specifying the percentage increase in the predicted response variable
#'
#' @return a numeric scalar specifying the change in \code{fix} needed to achieve the specified percentage
#' increase in the predicted response variable
#'
#' @examples
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' myvals <- data.frame(wt = 2.8, hp = 110)
#' find_delta_x(mod, myvals, fix = "wt", delta_y_perc = 0.1)
#'

find_delta_x <- function(mod, myvals, fix = "ACC_day_mg_pla", delta_y_perc = 0.1) {

  mypred <- predict(mod, newdata = myvals)
  mygoal <- mypred*(1 + delta_y_perc)

  foo <- function(x, mod, myvals, mygoal) {
    tmp_frame <- myvals
    tmp_frame[, setdiff(names(myvals), fix)] <- x
    predict(mod, newdata = tmp_frame) - mygoal
  }

  uniroot(foo, c(-100, 100), extendInt = "yes", mod = mod, myvals = myvals, mygoal = mygoal)$root

}


#------------------------------------------------------------------------------------------
# Use a model to calculate how much the inputs have to change to change the predicted
# value by a specified absolute value
#------------------------------------------------------------------------------------------

#' Find Delta Y
#'
#' This function finds the change in the predicted response variable in a data frame \code{myvals}
#' needed to achieve a specified absolute increase in the predicted response variable
#' \code{mod}. The function uses the \code{uniroot} function to find the value of \code{x} that
#' satisfies the equation \code{predict(mod, newdata = tmp_frame) - mygoal = 0}.
#'
#' @param mod a fitted model object
#' @param myvals a data frame of predictor variables
#' @param fix a character vector specifying the variable to hold constant while finding delta y
#' @param delta_y_abs a numeric scalar specifying the absolute increase in the predicted response variable
#'
#' @return a numeric scalar specifying the change in the predicted response variable needed to achieve
#' the specified absolute increase in the predicted response variable
#'
#' @examples
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' myvals <- data.frame(wt = 2.8, hp = 110)
#' find_delta_y(mod, myvals, fix = "", delta_y_abs = 2)
#'

find_delta_y <- function(mod, myvals, fix = "", delta_y_abs = 1) {

  if (any(grepl("gamlss", class(mod)))) {
    mypred <- predict(mod, newdata = myvals, what = "mu")
    mygoal <- mypred + delta_y_abs

    foo <- function(x, mod, myvals, mygoal) {
      tmp_frame <- myvals
      tmp_frame[, setdiff(names(myvals), fix)] <- x
      predict(mod, newdata = tmp_frame, what = "mu") - mygoal
    }
  } else {
    mypred <- predict(mod, newdata = myvals)
    mygoal <- mypred + delta_y_abs

    foo <- function(x, mod, myvals, mygoal) {
      tmp_frame <- myvals
      tmp_frame[, setdiff(names(myvals), fix)] <- x
      predict(mod, newdata = tmp_frame) - mygoal
    }
  }

  uniroot(foo, c(-100, 100), extendInt = "yes", mod = mod, myvals = myvals, mygoal = mygoal)$root

}

#------------------------------------------------------------------------------------------
# Use a model to calculate how much the inputs have to change to change the predicted
# value by a specified absolute value
#------------------------------------------------------------------------------------------

#' Find Delta Y for intensity gradient
#'
#' Given a fitted GAM or GAMLSS model with inverse Gaussian distribution, and a set of predictor values \code{myvals}, this function finds the change in a specified variable \code{fix} that leads to a target change in the predicted response of the model. The target change is specified by the argument \code{delta_y_abs}, which represents the desired absolute change in the predicted response. If the model is a GAMLSS model, the predicted response is the mean of the response distribution, which is obtained using the \code{what = "mu"} argument of the \code{predict()} function.
#'
#' @param mod A fitted GAM or GAMLSS model with inverse Gaussian distribution, as returned by functions like \code{gam()} or \code{gamlss()}.
#' @param myvals A data frame containing the predictor values at which the predicted response is to be evaluated. The column names of \code{myvals} should correspond to the variable names used in the model formula.
#' @param fix A character vector specifying the names of the variables in \code{myvals} that should be fixed at their original values when searching for the change in \code{fix} that leads to the target change in the predicted response. The default value is an empty character vector, indicating that no variables should be fixed.
#' @param delta_y_abs A numeric value specifying the desired absolute change in the predicted response of the model. The default value is 1.
#'
#' @return A numeric value representing the change in the variable \code{fix} that leads to the target change in the predicted response of the model.
#'
#' @examples
#' # Fit a GAMLSS model with inverse Gaussian distribution
#' data(sunspot.year)
#' mod <- gamlss(log(y) ~ pb(year, 4), data = sunspot.year, family = "IG")
#'
#' # Find the change in the year variable that leads to a 10% increase in the predicted response
#' myvals <- data.frame(year = 1940)
#' find_delta_y_ig(mod, myvals, fix = "year", delta_y_abs = 0.1*exp(predict(mod, newdata = myvals, what = "mu")))
#'

find_delta_y_ig <- function(mod, myvals, fix = "", delta_y_abs = 1) {

  if (any(grepl("gamlss", class(mod)))) {
    mypred <- predict(mod, newdata = myvals, what = "mu")
    mygoal <- mypred + delta_y_abs

    foo <- function(x, mod, myvals, mygoal) {
      tmp_frame <- myvals
      tmp_frame[, setdiff(names(myvals), fix)] <- x
      predict(mod, newdata = tmp_frame, what = "mu") - mygoal
    }
  } else {
    mypred <- predict(mod, newdata = myvals)
    mygoal <- mypred + delta_y_abs

    foo <- function(x, mod, myvals, mygoal) {
      tmp_frame <- myvals
      tmp_frame[, setdiff(names(myvals), fix)] <- x
      predict(mod, newdata = tmp_frame) - mygoal
    }
  }

  uniroot(foo, c(-100, 100), extendInt = "yes", mod = mod, myvals = myvals, mygoal = mygoal)$root

}

#------------------------------------------------------------------------------------------
# Use a model to calculate how much the inputs have to change to change the predicted
# value by a specified absolute value - for cvd risk
#------------------------------------------------------------------------------------------

#' Find Delta CVD
#'
#' This function finds the delta value for the Cardiovascular Disease (CVD) endpoint for a given model and input values using the uniroot function.
#'
#' @param mod A model object to be used for prediction
#' @param myvals A data.frame or matrix containing the input values for prediction
#' @param fix A character vector of column names to be held constant during optimization
#' @param delta_y_abs The desired absolute change in CVD endpoint
#'
#' @return The delta x value that results in a delta y value of delta_y_abs for the CVD endpoint.
#'
#' @examples
#' find_delta_cvd(my_mod, my_vals, fix = "ACC_day_mg_pla", delta_y_abs = 3.5)
#'

find_delta_cvd <- function(mod, myvals, fix = "ACC_day_mg_pla", delta_y_abs = 3.5) {

  mypred <- predict(mod, newdata = myvals)
  mygoal <- mypred + delta_y_abs

  foo <- function(x, mod, myvals, mygoal) {
    tmp_frame <- myvals
    tmp_frame[, setdiff(names(myvals), fix)] <- x
    predict(mod, newdata = tmp_frame) - mygoal
  }

  uniroot(foo, c(-100, 100), extendInt = "yes", mod = mod, myvals = myvals, mygoal = mygoal)$root

}

