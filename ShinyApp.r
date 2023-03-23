library(shinydashboard)
library(corrplot)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Projekt Zaliczeniowy"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prezentacja danych", tabName = "Dane", icon = icon("book", lib = "glyphicon")),
      menuItem("Opis", tabName = "Opis", icon = icon("book", lib = "glyphicon")),
      menuItem("Korelacja", icon = icon("stats", lib = "glyphicon"), tabName = "Korelacja"),
      menuItem("Histogram", icon = icon("stats", lib = "glyphicon"), tabName = "Histogram"),
      menuItem("Wykres pudełkowy", icon = icon("stats", lib = "glyphicon"), tabName = "Boxplot"),
      menuItem("Scatterplot", icon = icon("stats", lib = "glyphicon"), tabName = "Scatterplot"),
      menuItem("Wykres kołowy", icon = icon("stats", lib = "glyphicon"), tabName = "Circle")), 
    # dodajemy wejście
    selectInput(
      'vector', h3("Wybierz dane"),
      choices = c("mpg"=2,"cyl"=3,"disp"=4,"hp"=5, "drat"=6, "wt"=7,"qsec"=8,"vs"=9,"am"=10,"gear"=11,"carb"=12), 
      multiple=FALSE, selected=2
    ),
    sliderInput("slider", "Liczba aut:", 1, 32, 32)
  ),
  dashboardBody(
    # Boxes powinny być w wierszach lub kolumnach 
    fluidRow(
      tabItems(
        tabItem(tabName = "Dane",
                h2("Prezentacja danych"),
                box(status = "warning", solidHeader = T,title = "Zbiór danych mtcars",
                tableOutput("dane"),width = 5)
                
        ),
        tabItem(tabName = "Opis",
                h2("Opis"),
                box(title = "Statystyki opisowe",status = "warning",collapsible = TRUE, solidHeader = T,
                  tableOutput("op"),
                  h3(verbatimTextOutput('print1'))
                ),box(status = "warning", solidHeader = T,
                      h3(verbatimTextOutput('print2'))),textOutput("tekst")
        ),
        tabItem(tabName = "Korelacja",
                h2("Korelacja"),box(status = "warning", solidHeader = T,plotOutput("plot8"))
                
                
        ),
        tabItem(tabName = "Histogram",
                h2("Wykresy poczatkowe"),
                box(title="Histogram",status = "warning",solidHeader = T,plotOutput("plot1", height = 550, width=650)),
                box(title="Wykres liniowy",status = "warning",solidHeader = T,plotOutput("plot3", height = 450, width = 650))
        ),
        tabItem(tabName = "Boxplot",selectInput(
          'vector2', h3("Wybierz dane"),
          choices = c("mpg"=2,"disp"=4,"hp"=5, "drat"=6, "wt"=7,"qsec"=8), 
          multiple=FALSE, selected=2
        ),h2("Wykres pudełkowy"),
               box(status = "warning",solidHeader = T,
                plotOutput("plot2", height =450,width = 650))
        ),
        tabItem(tabName = "Scatterplot",h2("Scatterplot"),
                box(status = "warning",solidHeader = T, plotOutput("plot7"))
                
        ),
        tabItem(tabName = "Circle",h2("Wykres kolowy"),
                box(status = "warning",solidHeader = T,plotOutput("plot4", height = 450, width = 400),),
                box(status = "warning",solidHeader = T,plotOutput("plot5", height = 450, width = 400),width = 5),
                plotOutput("plot6"))
      )
    )
  )
)

server <- function(input, output) {
  d<-read.csv(file="https://gist.githubusercontent.com/seankross/a412dfbd88b3db70b74b/raw/5f23f993cd87c283ce766e7ac6b329ee7cc2e1d1/mtcars.csv", 
              head=T)
  output$op <-renderTable({ 
    data.frame(cbind(c("Długość wektora","Minimum","Maksimum","Średnia","Odchylenie standardowe",
                       "Mediana"),
                     c(length(d[seq_len(input$slider),as.numeric(input$vector)]),
                       min(d[seq_len(input$slider),as.numeric(input$vector)]),
                       max(d[seq_len(input$slider),as.numeric(input$vector)]),
                       mean(d[seq_len(input$slider),as.numeric(input$vector)]),
                       sd(d[seq_len(input$slider),as.numeric(input$vector)]),
                       median(d[seq_len(input$slider),as.numeric(input$vector)]))))})
  
  output$print2 <-renderPrint({
    print("Dane")
    str(d)
  })
  
  output$dane<-renderTable({d})
  
  output$tekst <- renderText({"Dane na temat 32 aut z magazynu Motor Trend US z 1974, ktore pokazuja ich specyfikacje"})
  
  output$print1 <- renderPrint({
    print("Kwantyle")
    quantile(d[seq_len(input$slider),as.numeric(input$vector)])
  })
  
  output$plot1 <- renderPlot({
    data <- d[seq_len(input$slider),as.numeric(input$vector)]
    hist(data, col="light blue",border = "darkblue",xlab = "Auta", ylab = "Ilosc")
    grid()
  })
  
  output$plot2 <- renderPlot({
    data <- d[,as.numeric(input$vector2)]
    boxplot(data~mtcars$cyl, main = "Cylindry a wybrane dane",col = "Darkgreen", xlab = "liczba cylindrow", ylab = "dane")
  })

  output$plot3 <- renderPlot({
    data <- d[seq_len(input$slider),as.numeric(input$vector)]
    plot(data, col='brown2', type="b",xlab = "Auta", ylab = "Dane")
    
  })
  
  biegi<-c(sum(d$gear==3),sum(d$gear==4),sum(d$gear==5))
  cylindry<-c(sum(d$cyl==4),sum(d$cyl==6),sum(d$cyl==8))
  automat<-c(sum(d$am==0),sum(d$am==1))
  
  biegi1<-round(100*biegi/32,1)
  cylindry1<-round(100*cylindry/32,1)
  automat1<-round(100*automat/32,1)
  
  
  output$plot4 <- renderPlot({pie(biegi,
    labels = biegi1,
    radius = 1,col = c("brown2","light green","cadetblue1"), main = "Rozkład aut na podstawie biegow(C-3,Z-4,N-5)")
})
  output$plot5 <- renderPlot({pie(cylindry, 
    labels = cylindry1,
    radius = 1,col = c("brown2","light green","cadetblue1"), main = "Rozkład aut na podstawie cylindrow(C-4,Z-6,N-8)" )
  })
  
  output$plot6 <- renderPlot({pie(automat, 
    labels = automat1,
    radius = 1,col = c("light green","cadetblue1"), main = "Proporcja skrzyni biegow(Z-Man,N-Aut)" )
  })
  
  
  output$plot7 <- renderPlot({
    data <- d[,as.numeric(input$vector)]
    plot(d$wt,data, main ="scatter plot",col='darkslateblue', pch=4,xlab = "Waga", ylab = "Dane", lwd=1, cex=2)
    
  })
  M = cor(mtcars)
  
  output$plot8 <- renderPlot({
    corrplot(M)
    
  })
}

shinyApp(ui, server)