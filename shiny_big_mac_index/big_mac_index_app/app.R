library(shiny)
library(tidyverse)

with_values <- read_csv("with_values.csv")
world <- read_csv("world.csv")

ui <- fluidPage(

    # Application title
    titlePanel("Big mac index"),

    sidebarLayout(
        sidebarPanel(
            selectInput("date",
                        "Choose date of observation:",
                        choices = c('2000-04-01', '2001-04-01', '2002-04-01', '2003-04-01', '2004-05-01', '2005-06-01', '2006-01-01', '2006-05-01', '2007-01-01', '2007-06-01', '2008-06-01', '2009-07-01', '2010-01-01', '2010-07-01', '2011-07-01', '2012-01-01', '2012-07-01', '2013-01-01', '2013-07-01', '2014-01-01', '2014-07-01', '2015-01-01', '2015-07-01', '2016-01-01', '2016-07-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01', '2019-07-09', '2020-01-14', '2020-07-01'),
                        selected = '2020-07-01')
        ),

        
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        world %>%
            fuzzyjoin::regex_full_join(with_values %>% filter(date == input$date),
                                       by = c("region" = "name")) %>% 
            ggplot(aes(long, lat, map_id = region, fill = dollar_ratio)) +
            geom_map(map = world) +
            coord_quickmap() +
            theme_void() +
            scale_fill_gradient(low = "lightblue", high = "tomato") +
            labs(fill = "")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
