#dataviz project : dashboard using rshiny and bslib
# 26/07/2023

#Requirements
source("requirements.R")

#Importing data from wooldridge
data <- wage1
data <- data %>% 
  mutate(female=as.factor(female), 
         nonwhite=as.factor(nonwhite),
         married=as.factor(married), 
         smsa=as.factor(smsa))

occupations <- c("construc", "ndurman", "trcommpu", "trade", "services", "profserv", "profocc", "clerocc", "servocc")
tdc_to_normal <- function(df, c_var){
  df$newvar <- NA
  for (i in 1:nrow(df)) {
    keep <- names(df[i,c_var])[which(df[i,c_var] > 0)]
    if(length(keep)!=0){
      df[i, "newvar"] <- sample(keep, 1)
    }else{
      next
      #df[i, "newvar"] <- NA
    }
  }
  df[,c_var] <- NULL
  return(df)
}
data2 <- tdc_to_normal(data, occupations)%>%
  rename(occupation ="newvar")%>%
  mutate(occupation=factor(occupation, labels=c("Construction", "Non-durable Manufacturing", "Trans/Comm/Pub Utilities","Trade (Wholesale/Retail)", 
                                                "Services", "Professional Services","Professional Occupation", "Clerical Occupation", "Service Occupation")))
# cards
source("cards.R")
#value box
source("value_box.R")

#rshiny architecture
ui <- page_sidebar(
  theme = bs_theme(version = 5),
  title="Descriptive statistics of the dataset",
  #sidebar= sidebar(
  #  title = "Plot controls",
  #  varSelectInput(
  #    "yvar", "Select variable", data[, c(5,6,7,9)]
  #  )
  #),
  layout_columns(
    fill=FALSE, 
    !!!values
  ),
  cards[[1]], 
  #cards[[2]], 
  cards[[3]]
)

server <- function(input, output){
  output$wage_density <- renderPlot(
    ggplot(data, aes(x=wage, group=!!input$yvar, fill=!!input$yvar)) +
      geom_density(adjust=1.5, alpha=.4)+
      theme(axis.text = element_text(size = 15), 
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 15))
      
    
    )
  output$work_distribution <- renderPlot(
    data %>% 
      summarize(across(c("construc", "ndurman", "trcommpu", "trade", "services", "profserv", "profocc", "clerocc", "servocc"), \(x) sum(x, na.rm=TRUE)))%>%
      pivot_longer(cols = everything(), names_to = "type")%>%
      arrange(value)%>%
      mutate(type=factor(type, type, labels=c("Construction", "Non-durable Manufacturing", "Trans/Comm/Pub Utilities","Trade (Wholesale/Retail)", 
                                              "Services", "Professional Services","Professional Occupation", "Clerical Occupation", "Service Occupation")
      )) %>%
      ggplot(aes(x=type, y=value)) +
      geom_segment(aes(x=type, xend=type, y=0, yend=value), color="blue") +
      coord_flip() +
      geom_point(color="blue", size=4) +
      theme_light() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 18)
      ) +
      xlab("Type of work") +
      ylab("Headcount")
  )
  
    output$wage_distribution <- renderPlot(
      data2 %>%
        filter(!is.na(occupation)) %>%
        ggplot(aes(x=reorder(occupation,wage) , y=wage, fill=occupation, color=occupation)) +
        geom_boxplot() +
        coord_flip()+
        scale_fill_viridis(discrete=TRUE) +
        scale_color_viridis(discrete=TRUE) +
        theme(
          legend.position="none",
          axis.line.x = element_line(),
          axis.text = element_text(size = 15), 
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 15)
        ) +
        coord_flip() +
        geom_hline(yintercept = 0) +
        xlab("") +
        ylab("Hourly wage ($)")
      
    )
}

shinyApp(ui, server)
