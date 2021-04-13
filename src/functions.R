
process_data <- function(DT) {
    require(data.table)

    setDT(DT, key = "Date")

    # Creating cumulative sums per participant
    ignore <- "Date"
    cols <- setdiff(names(DT), ignore)
    DT[, paste0("Cum.", cols) := cumsum(.SD), .SDcols = cols]

    # Keeping relevant variables
    DT <- DT[, .SD, .SDcols = -cols]

    setnames(DT, paste0("Cum.", cols), cols)

    # Reshaping wide to long
    DT.long = melt(DT, id.vars = c("Date"),
                measure.vars = cols)

    # Renaming
    setnames(DT.long, c("variable", "value"), c("participant", "nr_workouts"))

    # Leader (per day)
    DT.long[, current_leader := .SD[which.max(nr_workouts)], by = Date]

    # Loser (per day)
    DT.long[, current_loser := .SD[which.min(nr_workouts)], by = Date]

    return(DT.long)
}

create_line_gif <- function(DT) {
    require(ggplot2)
    require(gganimate)
    require(data.table)

    participant_colors <- c("darkorange", "orangered", "violetred", "purple", "black", "darkblue")
    participant_labels <- DT[, participant_labels := tstrsplit(participant, ".", fixed = TRUE)]

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

    gganimate::animate(p, nframes = 100, fps = 5, renderer = gifski_renderer(), end_pause = 10)
    gganimate::anim_save("animateOutput_line.gif", animation = last_animation(), path = "graphics/")

}
