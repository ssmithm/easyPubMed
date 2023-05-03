library(ggplot2)
library(shiny)
library(bslib)
library(ganttrify)

# data prep work
# check.df <- readr::read_csv("/Users/stevensmith/Dropbox (UFL)/R Projects/cvmedlab.github.io/assets/data/pubmed_query.csv")
# grants <- readr::read_tsv("/Users/stevensmith/Dropbox (UFL)/R Projects/cvmedlab.github.io/assets/data/grant_funding.csv")
check.df <- readr::read_csv("C:/Users/ssmithm/Dropbox (UFL)/R Projects/cvmedlab.github.io/assets/data/pubmed_query.csv")
grants <- readr::read_tsv("C:/Users/ssmithm/Dropbox (UFL)/R Projects/cvmedlab.github.io/assets/data/grant_funding.csv")
grants <- grants |> dplyr::mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
                                  end_date = as.Date(end_date, format = "%m/%d/%Y"))

gs <- c("Desai R", "Keshwani S", "Ndai A", "Smith K", "Morris EJ", "Mehanna M")

pdoc <- c("Piszczatoski CR", "Hwang AY", "Garland SG", "Dawwas GK", "Carris NW", "Monahan C")

authors <-
  stringr::str_split(check.df$Authors, ", ", n = Inf) |>
  unlist() |>
  tibble::as_tibble() |>
  dplyr::mutate(value = dplyr::case_when(value == "Trinkley K" ~ "Trinkley KE",
                                         value == "Walsh M" ~ "Walsh MG",
                                         value == "Pepine C" ~ "Pepine CJ",
                                         value == "Handberg E" ~ "Handberg EM",
                                         value == "Gurka M" ~ "Gurka MJ",
                                         value == "Epstein B" ~ "Epstein BJ",
                                         value == "Dietrich E" ~ "Dietrich EA",
                                         value == "Desai RA" ~ "Desai R",
                                         value == "Dave C" ~ "Dave CV",
                                         value == "Cooper-Dehoff RM" ~ "Cooper-DeHoff RM",
                                         value == "Chen Y" ~ "Chen YE",
                                         value == "Chapman A" ~ "Chapman AB",
                                         value == "Johnson J" ~ "Johnson JA",
                                         TRUE ~ value
  )) |>
  dplyr::group_by(value) |>
  dplyr::summarize(n = dplyr::n()) |>
  dplyr::mutate(status = dplyr::case_when(value %in% gs ~ "Lab GS",
                                          value %in% pdoc ~ "Lab Postdoc",
                                          value == "Smith SM" ~ "PI",
                                          TRUE ~ "Collaborator")) |>
  dplyr::rename(Author = value) |>
  dplyr::mutate(Author1 = paste0(Author, " (", status, ")")) |>
  dplyr::filter(status %in% c("Lab GS", "Lab Postdoc", "PI"))

author.list.split <- check.df |>
  tidyr::separate_longer_delim(Authors, delim = ", ") |>
  dplyr::select(Authors, Year) |>
  dplyr::arrange(Authors, Year) |>
  dplyr::group_by(Authors, Year) |>
  dplyr::add_count() |>
  dplyr::distinct() |>
  dplyr::group_by(Authors) |>
  dplyr::mutate(n = cumsum(n)) |>
  dplyr::ungroup() |>
  dplyr::arrange(Authors, Year) |>
  dplyr::mutate(status = dplyr::case_when(Authors %in% gs ~ "Lab GS",
                                          Authors %in% pdoc ~ "Lab Postdoc",
                                          Authors == "Smith SM" ~ "PI",
                                          TRUE ~ "Collaborator")) |>
  dplyr::filter(status %in% c("Lab GS", "Lab Postdoc", "PI"))

# ui <- fluidPage(
#   fluidRow(
#     column(2,
#            "sidebar"
#     ),
#     column(10,
#            "main"
#     )
#   )
# )

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),

  fluidRow(
    column(12,
           headerPanel("CV Med Lab Productivity")
    )
  ),

  fluidRow(
    column(4,
           sidebarPanel(

             selectInput("author", "Lab Member:",
                         authors$Author,
                         selected = "Smith SM")
           ),
    ),
    column(8,
           mainPanel(

             plotOutput("funding", height = "8in"),

             plotOutput("journalPlot"),

             plotOutput("cumPubs")

           )
    )
  )
)
# ui <- pageWithSidebar(
#
#   headerPanel("CV Med Lab Productivity"),
#
#   sidebarPanel(
#
#     selectInput("author", "Lab Member:",
#                 authors$Author,
#                 selected = "Smith SM")
#   ),
#
#   mainPanel(
#
#     plotOutput("funding", height = "8in"),
#
#     plotOutput("journalPlot"),
#
#     plotOutput("cumPubs")
#
#   )
#
# )


server <- function(input, output) {

  dat <- reactive({
    dplyr::filter(check.df, grepl(input$author, Authors))
  })

  dat1 <- reactive({
    dplyr::filter(author.list.split, status == "PI")
  })

  dat2 <- reactive({
    dplyr::filter(author.list.split, grepl(input$author, Authors))
  })

  output$funding <- renderPlot({
    ganttrify::ganttrify(
      project = grants,
      label_wrap = 70,
      colour_palette = wesanderson::wes_palette("Darjeeling1"),
      by_date = TRUE,
      month_number_label = FALSE,
      alpha_wp = 0
    ) +
      ggplot2::scale_x_date(date_breaks = "1 year") +
      ggplot2::labs(x = "Date") +
      ggplot2::ggtitle("CV Med Lab Funding") +
      theme(
        axis.text = element_text(size = rel(1.15)),
        title = element_text(face = "bold", size = rel(1.4))
      )
  })

  output$journalPlot <- renderPlot({
    ggplot(data = dat(),
           aes(x = Journal)) +
    geom_bar(fill = "lightblue") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "Journal", y = "# Pubs in Journal") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10), n.breaks = 6) +
    theme(
      axis.text = element_text(size = rel(1.3)),
      title = element_text(face = "bold", size = rel(1.4))
    ) +
    ggtitle("Where CV Med Lab Publishes")
  })

  output$cumPubs <- renderPlot({
      ggplot(data = author.list.split,
        aes(x = Year, y = n, colour = status, group = Authors)) +
      geom_area(data = dat1(), fill = "lightblue", colour = "darkblue") +
      geom_area(data = dat2(), fill = "pink", colour = "red") +
      geom_point(data= dat2(), shape = 21, fill = "white", colour = "red", size = 4) +
      theme_classic() +
      labs(y = "Cumulative Lab Publications") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, nrow(check.df) + 20), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
      scale_x_continuous(expand = c(0, 0),
                         limits = c(min(check.df$Year), max(check.df$Year)),
                         n.breaks = ((max(check.df$Year) - min(check.df$Year)) / 2) + 1
      ) +
      theme(
        axis.text = element_text(size = rel(1.3)),
        title = element_text(face = "bold", size = rel(1.4))
      ) +
      ggtitle("Cumulative CV Med Lab Publications Over Time")

  })
}

shinyApp(ui, server)
