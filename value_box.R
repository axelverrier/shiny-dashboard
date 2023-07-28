#Here are the value box
values <- list(
  value_box(
    title = "Median hourly earnings",
    value = scales::unit_format(unit = "$")(median(data[[1]])),
    showcase = bsicons::bs_icon("currency-dollar")
  ),
  value_box(
    title = "Proportion of women",
    value = scales::unit_format(unit = "%")((count(data[data$female==1,]) / count(data) * 100)[[1]]),
    showcase = bsicons::bs_icon("gender-female"),
    theme_color = "dark"
  ),
  value_box(
    title = "Most represented region",
    value = data %>% 
      mutate(east=ifelse(west==0 & northcen==0 & south==0, 1,0 ))%>%
      summarize(across(c("west", "northcen", "south", "east"), \(x) sum(x, na.rm=TRUE)))%>%
      pivot_longer(cols = everything(), names_to = "region")%>%
      arrange(desc(value) )%>%
      purrr::pluck(1,1),
    showcase = bsicons::bs_icon("pin-map"), 
    theme_color = "secondary"
  )
)
