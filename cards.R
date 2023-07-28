#All the cards
cards <- list(
  card(
    min_height = 500,
    full_screen = TRUE,
    card_header("Hourly wage distribution as a function of the discriminating variable"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        varSelectInput("yvar", "Select variable", data[, c(5,6,7,9)])
      ),
      plotOutput("wage_density")
    )),
  card(
    min_height = 500,
    full_screen = TRUE,
    card_header("Distribution of workers according to their type of work"),
    plotOutput("work_distribution"), 
    p("Note : Workers may be counted in more than one category.")
  ), 
  card(
    min_height = 500,
    full_screen = TRUE,
    card_header("Distribution of wage according to their type of work"),
    plotOutput("wage_distribution"), 
    p("Note : for workers having several occupations but only one wage, occupation was randomly chosen among those.")
  )
)
