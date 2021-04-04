library(targets)
source("src/functions.R")

tar_option_set(packages = c("data.table", "foreign", "readxl", "ggplot2", "gganimate", "gifski", "transformr", "dplyr"))

list(
    tar_target(raw_data, "data/raw_data.xlsx", format = "file"),

    tar_target(processed_data, process_data(read_excel(raw_data))),
    tar_target(line_animation, create_line_gif(processed_data))
)
