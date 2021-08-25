#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
load("minari.rda")
load("gisaeng.rda")
load("title.rda")
load("top100.rda")

# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title을 입력함 
    titlePanel("아카데미 상 수상 두 영화 평론 워드클라우드"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("title", "영화제목:",
                         c("미나리"="minari",
                           "기생충" = "gisaeng"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            wordcloud2Output("moviePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # reactive function 
    dataInput <- reactive({
        temp <- switch(input$title,  #switch로 A = 분노의질주 이런식, 그리고 밑에 t에 titles == temp로 넣어줌
                       "minari"="미나리",
                       "gisaeng"="기생충")
        t<-which(titles==temp)
        paste(url.review, page, sep="")
        
    })
    #
    output$moviePlot <- renderWordcloud2({
        # generate bins based on input$bins from ui.R
        wordcloud2(dataInput(), size=0.7,
                   color='random-dark',
                   backgroundColor = 'white',
                   gridSize = 10)
        # ggplot도 사용해도된다.
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
