
mygantt = function (project, spots = NULL,
                    project_start_date = zoo::as.yearmon(Sys.Date()),
                    colour_palette = wesanderson::wes_palette("Darjeeling1"),
                    font_family = "sans",
                    mark_quarters = FALSE, mark_years = TRUE,
                    size_wp = 6, hide_wp = FALSE, size_activity = 4, size_text_relative = 1,
                    label_wrap = FALSE, month_number_label = TRUE, month_date_label = TRUE,
                    x_axis_position = "top", colour_stripe = "lightgray", alpha_wp = 1,
                    alpha_activity = 1, line_end = "round",
                    show_vertical_lines = FALSE,
                    x_axis_text_align = "left",y_axis_text_align="right")
{
  # Required libraries ----
  require(zoo)
  require(dplyr)
  require(lubridate)

  # Fix some columns ----
  project$wp = as.character(project$wp)
  project$activity = as.character(project$activity)

  # Repeat colors in palette ----
  nwp = length(unique(project$wp))
  np = length(as.character(colour_palette))
  colour_palette = colour_palette[rep(1:np,length.out=nwp)]

  # Wrap labels if desired ----
  if (label_wrap != FALSE) {
    label_wrap = ifelse(isTRUE(label_wrap),32,label_wrap)

    project$wp <- stringr::str_wrap(string = project$wp,
                                    width = label_wrap)
    project$activity <- stringr::str_wrap(string = project$activity,
                                          width = label_wrap)
    if (!is.null(spots)) {
      spots$activity <- stringr::str_wrap(string = spots$activity,
                                          width = label_wrap)
    }
  }

  # Determine dates -----
  project_start_date = zoo::as.yearmon(project_start_date) # Force project to start at month start

  project <- project |>
    dplyr::mutate(start_date_yearmon = project_start_date + (1/12) * (start_date-1),
                  end_date_yearmon = project_start_date + (1/12) * (end_date-1)) |>
    dplyr::mutate(start_date_date = zoo::as.Date(start_date_yearmon, frac = 0),
                  end_date_date = zoo::as.Date(end_date_yearmon, frac = 1))

  # # Get a sequence of calendar year quarters from start to end
  # seq_q <- seq.Date(from = lubridate::floor_date(min(project$start_date_date),unit="quarter"),
  #                   to = lubridate::floor_date(max(project$end_date_date),unit="quarter"), by = "3 months")

  # Get a sequence of project quarters from start to end
  seq_qn = floor(min(project$start_date-1,0) / 3) : floor(max(project$end_date-1,0) / 3)
  seq_qN = seq_qn %% 4 + 1 # For labeling quarters

  seq_qY = floor(seq_qn/4)
  seq_qY = seq_qY + ifelse(seq_qY>=0,1,0) # For labeling years

  seq_q = zoo::as.Date(project_start_date + (1/12) * seq_qn * 3,frac=0) # Actual dates of quarters

  # quarter starts and ends for creating shaded rectangles
  quarts_df = data.frame(start=seq_q,end=seq_q %m+% months(3))
  quarts_df2 = quarts_df[seq(1,nrow(quarts_df),2),] # Every other quarter

  # # Get a sequence of calendar years from start to end - for year lines
  # s = c(seq_q,max(seq_q) %m+% months(3))
  # seq_y = s[lubridate::month(seq_q) == 1]

  # Get a sequence of project years from start to end - for year lines
  seq_y = seq_q[seq_qN == 1]
  # Add a year if the project naturally ends on a year
  if (max(seq_qN)==4) seq_y[length(seq_y)+1] = max(seq_y) %m+% months(12)

  # Create data.frame with all the info for labelling quarters
  yl = paste0("Y",seq_qY)
  ql = paste0("Q",seq_qN)
  seq_q_df = data.frame(d=seq_q,q=seq_qN,y=seq_qY,
                        q.lab = paste0(ifelse(seq_qN==1,yl,""),"\n",ql),
                        d.lab = format(seq_q,"%b\n%Y"))

  # Add WP to activities ----
  project$row = 1:nrow(project) # Add row ID column
  project.sum = project |> dplyr::group_by(wp) |>
    dplyr::summarize(activity = wp[1],
                     start_date_date=min(start_date_date),
                     end_date_date=max(end_date_date),
                     row=min(row)-0.5) # Summarize to just WP

  project.wp = dplyr::bind_rows(
    activity = project |> select(wp,activity,start_date_date,end_date_date,row),
    wp = project.sum,
    .id = "type"
  ) |> dplyr::arrange(wp,row) # Add in WP and arrange so things group by WP

  # If desired remove WP rows
  if (hide_wp)
    project.wp <- project.wp |> dplyr::filter(type != "wp")

  # Set alpha and size of wp and activities segments
  project.wp$alpha <- c(wp=alpha_wp,activity=alpha_activity)[project.wp$type]
  project.wp$size <- c(wp=size_wp,activity=size_activity)[project.wp$type]

  # Turn activity into factor to avoid reordering ----
  project.wp$activity = factor(project.wp$activity,levels = rev(unique(project.wp$activity)))
  # rev essentially reverses y axis of plot.

  # Basic Gantt plot with quarter rectangles ----
  gg_gantt <- ggplot2::ggplot(data = project.wp,
                              mapping = ggplot2::aes(x = start_date_date, y = activity,
                                                     xend = end_date_date, yend = activity,
                                                     colour = wp)) +
    ggplot2::geom_rect(data = quarts_df2, ggplot2::aes(xmin = start,xmax = end,
                                                       ymin = -Inf, ymax = Inf),
                       inherit.aes = FALSE,
                       alpha = 0.4, fill = colour_stripe)

  # Add in lines for quarters and years if desired ----
  if (mark_quarters)
    gg_gantt <- gg_gantt + ggplot2::geom_vline(xintercept = seq_q,colour = "gray50")

  if (mark_years)
    gg_gantt <- gg_gantt + ggplot2::geom_vline(xintercept = seq_y,
                                               colour = "gray50")

  # Add segments ----
  gg_gantt <- gg_gantt +
    ggplot2::geom_segment(lineend = line_end,
                          size=project.wp$size,alpha=project.wp$alpha)

  # Add x axis labels ----
  if (month_number_label && month_date_label) {
    args = list(name="",minor_breaks=NULL,breaks=seq_q_df$d,labels=seq_q_df$d.lab,
                sec.axis = ggplot2::dup_axis(labels=seq_q_df$q.lab))
  } else if (!month_number_label && month_date_label) {
    args = list(name="",minor_breaks=NULL,breaks=seq_q_df$d,labels=seq_q_df$d.lab,
                position = x_axis_position)
  } else if (month_number_label && !month_date_label) {
    args = list(name="",minor_breaks=NULL,breaks=seq_q_df$d,labels=seq_q_df$q.lab,
                position = x_axis_position)
  } else {
    args = list(name="")
  }
  gg_gantt <- gg_gantt + do.call(ggplot2::scale_x_date,args)

  # Text alignment ----
  xn = switch(x_axis_text_align,
              left=0,right=1,
              center=,centre=0.5,
              stop("Unknown x axis alignment"))
  yn = switch(y_axis_text_align,
              left=0,right=1,
              center=,centre=0.5,
              stop("Unknown y axis alignment"))

  # For deciding on bold face for y axis labels
  tt = project.wp |> dplyr::distinct(activity, wp, type) |> dplyr::pull(type)
  tt = rev(ifelse(tt=="wp","bold","plain"))

  gg_gantt <- suppressWarnings(
    gg_gantt + ggplot2::scale_y_discrete("") +
      ggplot2::theme_minimal() +
      ggplot2::scale_colour_manual(values = colour_palette) +
      ggplot2::theme(text = ggplot2::element_text(family = font_family),
                     axis.text.y.left = ggplot2::element_text(face = tt,
                                                              size = ggplot2::rel(size_text_relative),
                                                              hjust = yn),
                     axis.text.x = ggplot2::element_text(size = ggplot2::rel(size_text_relative),
                                                         hjust = xn),
                     legend.position = "none")
  )

  # Spots ----
  if (!is.null(spots)) {
    spots <- spots |> # tidyr::drop_na() |>
      dplyr::mutate(spot_date = as.numeric(spot_date),
                    activity = as.character(activity),
                    spot_type = as.character(spot_type)) |>
      dplyr::mutate(activity = factor(activity, levels = levels(project.wp$activity)),
                    spot_date_date = zoo::as.Date(project_start_date + (1/12) * (spot_date-1), frac = 0.5),
                    end_date_date=as.Date(NA),wp=NA)
    #browser()
    gg_gantt <- gg_gantt +
      ggplot2::geom_label(data = spots,
                          mapping = ggplot2::aes(x = spot_date_date, y = activity,
                                                 label = spot_type),
                          colour = "gray30", alpha = 1,
                          fontface = "bold", size = 3 * size_text_relative,
                          family = font_family)
  }

  if (!show_vertical_lines)
    gg_gantt <- gg_gantt + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0))

  return(gg_gantt)
}
