#' ---
#' title: "Graphical Determination of Dispersion Coefficient"
#' author: "Stephen Johnson"
#' date: "February 27, 2017"
#' output: html_document
#' ---

#' ## Overview
#' This script was developed to produce a plot needed for the DLC and VISF
#' experiments in CPE 619 Petroleum Engineering Lab I. For detailed instructions,
#' see `Dispersion_instructions.pdf`.  It is written in the `R` programming language.
#' To run the script you will need access to a PC running `R` and `RStudio`.
#' These should already be installed on the computers in LEA 3108.
#' R (https://www.r-project.org/) is a free, open-source statistical
#' programming language; RStudio (https://www.rstudio.com/) is an integrated
#' development environment (IDE) that makes it easy to use R without prior experience.
#'
#' Detailed instructions for using this script are in `Dispersion_instructions.pdf`.
#' Briefly, the data to plot should be in `data.csv` with headings _U_ and _C_D_,
#' and parameters for calculations and for fine-tuning the range of the final
#' graph should be in `parameters.csv` with, at a minimum, columns headed
#' _Parameter_ and _Value_. Both input files need to be saved in the same folder
#' as this script. The parameters `lo_y` and `hi_y` dictate the range
#' of U values plotted on the y-axis. Edit and save the `parameters.csv`
#' by referring to the linear plot produced when the script is first executed
#' and execute the script again.  Repeat as needed until the range of linear
#' data is fully within the range of the y-axis and nearly fills the height of
#' the plot. Similarly, tune `lo_x` and `hi_x`to select the range of `
#' C_D` values to include in the straight line fit.
#'
#' Once the probability plot is correct read off the values of U10
#' (i.e. the U value at CD = 0.10) and U90 (i.e. CD = 0.90) and use
#' U10, U90, V_p, tStar and L to calculate the dispersion coefficient
#' as described in the laboratory procedure.

#+ r setup, include = FALSE
# Remove all objects from the environment for a clean start
rm(list = ls(all = TRUE))

# Install if necessary and attach required packages
for (package in c('ggplot2', 'tibble', 'dplyr', 'pander', 'readr', 'stringr')) {
        if (!require(package, character.only=T, quietly=T)) {
                install.packages(package, repos="http://cran.us.r-project.org")
                # install.packages(package)
                library(package, character.only=T)
        }
}

# Set options for pander
panderOptions('table.style', 'rmarkdown')


# SET UP FUNCTIONS FOR USE LATER

load_parameters <- function(parameters_file) {
        #' ## Parameters
        #+ r load parameters, echo = FALSE
        parameters <- read.csv(parameters_file)
        ### Remove extraneous spaces
        names(parameters) <- str_trim(names(parameters), side = "both")
        parameters$Parameter <- str_trim(parameters$Parameter, side = "both")
        parameters$Description <- str_trim(parameters$Description, side = "both")
        parameters$Units<- str_trim(parameters$Units, side = "both")
        parameters$Value <- as.numeric(parameters$Value)
        # Put all the parameters in a list
        input <- structure(as.list(parameters$Value),
                           names = parameters$Parameter)
}

load_data <- function(data_file) {
        #' ## Data
        #+ r load data
        data <- read.csv(data_file) # Headings "U" and "C_D"
        data$C_D<- data$C_D / 100 # Convert percent to fraction
        return(data)
}


filter_data <- function(data, lo_x, hi_x) {
        # Choose data for linear regression
        data_filtered <- data %>% filter(C_D >= lo_x & C_D <= hi_x)
}

linear_plot <- function(data) {
        #' ## Linear plot
        #' This is just to give an overview of the data to help in tuning x and y parameters for the second plot
        #+ r linear plot
        gg <- ggplot(data = data, aes(x = C_D, y = U)) +
                labs(title = expression(paste("Plot of U vs. ", C[D])),
                     x = expression(C[D]),
                     y = expression(paste("U (", cm^{3/2}, ")"))
                ) +
                geom_point() +
                geom_line() +
                scale_x_continuous() +
                scale_y_continuous() +
                theme_linedraw()
}

dispersion_plot <- function(data, data_filtered, lo_y, lo_x){
        #' ## Probability plot
        #' #' Plot U vs C_D with C_D on a probit scale.
        #' #' Once optimized by tuning x and y parameters, you can read off U10 and U90.
        #' #' These are needed to calculate dispersion coefficient, K_i.
        #' #+ r probability plot

        #' # Set reasonably-sized increments for y axis
        #' # Note that this may give some odd increments depending on lo_y and hi_y
        range_y <- hi_y - lo_y
        incr_y <- case_when(
                range_y > 50 ~ 5,
                range_y > 20 ~ 2,
                range_y > 10 ~ 1,
                range_y > 5 ~ 0.5,
                range_y > 2 ~ 0.2,
                range_y > 1 ~ 0.1,
                range_y <= 1 ~ 0.05
        )

        gg <- ggplot(data = data_filtered, aes(x = C_D, y = U)) +
                labs(title = expression(paste("Probability Plot of U vs. ", C[D])),
                     x = expression(C[D]),
                     y = expression(paste("U (", cm^{3/2}, ")"))
                ) +
                geom_point(data = data, aes(x = C_D, y = U), colour = "grey") +
                geom_point(colour = "orange", fill = "red", size = 3) +
                geom_smooth(method = "lm", se = TRUE, fullrange = TRUE) +
                geom_vline(xintercept = 0.1, linetype = "dashed") +
                geom_vline(xintercept = 0.9, linetype = "dashed") +
                scale_x_continuous(trans = "probit",
                                   breaks = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.5,
                                              0.7, 0.8, 0.9, 0.95, 0.98, 0.99),
                                   limits = c(0.01, 0.99)) +
                scale_y_continuous(limits = c(lo_y, hi_y),
                                   breaks = seq(lo_y, hi_y, incr_y)) +
                theme_linedraw()
}

dispersion_model <- function(U, C_D){
        #' ## Calculate linear model, U10, U90, and dispersion coefficient
        #' #' Calculate linear model with C_D (0-1) converted to z-score
        #' #+ r Linear model
        lm <- glm(U ~ qnorm(C_D), data = data_filtered)
        return(lm)
}

U_values <- function(lm){
        #' Calculate U10 and U90 from a linear model
        U10 <- lm$coefficients[2] * qnorm(0.1) + lm$coefficients[1]
        U90 <- lm$coefficients[2] * qnorm(0.9) + lm$coefficients[1]
        return(list(U10 = U10, U90 = U90))
}

dispersion_coefficient <- function(L, U10, U90, V_p, tStar){
        #' Calculate dispersion coefficient
        K_i <- ((L * (U10 - U90) / 3.625)^2) * (1 / (V_p * tStar))
        return(K_i)
}


# THIS IS WHERE WE DO STUFF

# Load the parameters from a file
parameters <- load_parameters("parameters.csv")

# Pull parameters out to named variables to make calculations clearer
V_p <- parameters$V_p # mL
L <- parameters$L  # cm
tStar <- parameters$tStar  # s
# EDIT THESE IN parameters.csv TO CAPTURE LINEAR DATA
lo_x <- parameters$lo_x
hi_x <- parameters$hi_x
lo_y <- parameters$lo_y
hi_y <- parameters$hi_y

# Load the data from a file
data <- load_data("data.csv")
head(data)

# Display the parameters
set.alignment(c('left', 'left', 'right', 'left'))
pander(parameters, caption = '')

#` Filter data for linear regression
data_filtered <- filter_data(data = data, lo_x = lo_x, hi_x = hi_x)
head(data_filtered)

#' Make and save plots
gg <- linear_plot(data)
ggsave("plot_linear.png", width = 7, height = 5, units = "in")

gg <- dispersion_plot(data, data_filtered, lo_y, hi_y)
ggsave("plot_probability.png", width = 7, height = 5, units = "in")

#` Calculate model and extract parameters
# lm <- dispersion_model(data_filtered$U, data_filtered$C_D)
# pander(lm)
# U <- U_values(lm)
# K_i <- dispersion_coefficient(L = L, U10 = U$U10, U90 = U$U90, V_p = V_p, tStar = tStar)
# pander(tibble(U10 = U$U10, U90 = U$U90, K_i = K_i))




