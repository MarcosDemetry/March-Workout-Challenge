
process_data <- function(DT) {
    require(data.table)

    setDT(DT, key = "Date")

    # Creating cumulative sums per participant
    cols <- c("Alexander", "Camila", "Fanny", "Marcos", "Mary", "Rose")
    DT[, paste0("Cum.", cols) := cumsum(.SD), .SDcols = cols]

    # Keeping relevant variables
    DT <- DT[, .SD, .SDcols = -cols]

    setnames(DT, c("Cum.Alexander", "Cum.Camila", "Cum.Fanny", "Cum.Marcos", "Cum.Mary", "Cum.Rose"), cols)
    
    # Getting current leader
    DT[, current_leader := colnames(.SD)[max.col(.SD, ties.method = "random")], .SDcols = cols]
    
    # Reshaping wide to long
    DT.long = melt(DT, id.vars = c("Date"),
                measure.vars = cols)

    # Renaming
    setnames(DT.long, c("variable", "value"), c("participant", "nr_workouts"))

    # Getting leader
    DT.long[, current_leader := .SD[which.max(nr_workouts)], by = Date]

    # Getting loser
    DT.long[, current_loser := .SD[which.min(nr_workouts)], by = Date]

    return(DT.long)
}

create_line_gif <- function(DT) {
    require(ggplot2)
    require(gganimate)

    participant_colors <- c("darkorange", "orangered", "violetred", "purple", "black", "darkblue")
    participant_labels <- c("Alexander", "Camila", "Fanny", "Marcos", "Mary", "Rose")

    p <- ggplot(DT,
               aes(x = Date, y = nr_workouts,
               group = participant, color = participant)) +
        geom_path() +
        geom_point() +
        scale_color_manual(name = "Participant",
                          values = participant_colors,
                          labels = participant_labels) +
        transition_reveal(along = Date) +
        labs(title = 'Date: {DT$Date[which.min(abs(DT$Date-frame_along))]}
                      Leader: {DT$current_leader[which.min(abs(DT$Date-frame_along))]}
                      Loser: {DT$current_loser[which.min(abs(DT$Date-frame_along))]}',
            x = 'Date', y = 'Number of Workouts')

    gganimate::animate(p, nframes = 100, fps = 5, renderer = gifski_renderer())
    gganimate::anim_save("animateOutput_line.gif", animation = last_animation(), path = "graphics/")

}

create_bar_gif <- function(DT) {
    require(ggplot2)
    require(gganimate)
    require(dplyr)

    DF <- as.data.frame(DT)

    datos2 <- DF %>%
        group_by(Date) %>%
        arrange(Date, desc(nr_workouts)) %>%
        mutate(ranking = row_number())

    animacion <- datos2 %>%
        ggplot() +
        geom_col(aes(ranking, nr_workouts, fill = participant)) +
        geom_text(aes(ranking, nr_workouts, label = nr_workouts), hjust = -0.1) +
        geom_text(aes(ranking, y = 0, label = participant), hjust = 1.1) + 
        geom_text(aes(x = 7, y = max(nr_workouts), label = as.factor(Date)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 10) +
        labs(x = "Number of workouts") + 
        coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
        theme_minimal() + theme(
            panel.grid = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.margin = margin(1, 4, 1, 3, "cm")
        ) +
        transition_states(Date, state_length = 0, transition_length = 1) +
        enter_fade() +
        exit_fade()

    animate(animacion, width = 700, height = 432, fps = 25, duration = 7, rewind = FALSE)
    anim_save("animateOutput_bar.gif", animation = last_animation(), path = "graphics/")

}