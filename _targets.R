library(targets)

library(data.table)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(transformr)
source("src/functions.R")

tar_option_set(packages = c("data.table", "foreign", "readxl", "ggplot2"))

list(
    tar_target(raw_data,
        read_excel("data/raw_data.xlsx")
    ),
    tar_target(processed_data, process_data(raw_data)),
    tar_target(line_animation, create_line_gif(processed_data)),
    tar_target(bar_animation, create_bar_gif(processed_data))
)
