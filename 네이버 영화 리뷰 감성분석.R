
library(shiny)
load("titles.rda")

load("predict.rda")
load("reviews.rda")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title을 입력함 
    titlePanel("영화평론 감성분석"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("title", "영화제목:",
                         c("분노의 질주: 더 얼티메이트"="A",
                           "파이프라인" = "B",
                           "보이저스"="C")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3(textOutput("caption")), #server에서 caption이랑 movieplot을 작성해서 입력받는것
           plotOutput("moviePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # reactive function 
    dataInput <- reactive({
        temp <- switch(input$title,  #switch로 A = 분노의질주 이런식, 그리고 밑에 t에 titles == temp로 넣어줌
                       "A"="분노의 질주: 더 얼티메이트",
                       "B"="파이프라인",
                       "C"="보이저스")
        t<-which(titles==temp)
        predict[t]
    
    })
    #
    output$caption <- renderText({
        s1 <- dataInput()
        p <- length(s1[s1==1])
        n <- length(s1[s1==-1])
        polarity <- (p-n)/(p+n)
        paste("polarity => ", round(polarity, 3))})
        
    output$moviePlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        pie(table(dataInput()), main="감성분석 결과", col=c("blue", "red", "green"))
            # ggplot도 사용해도된다.
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
