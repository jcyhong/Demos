set.seed(700)
x <- c(rnorm(100), rnorm(100, mean = 4))
y <- c(rnorm(100), rnorm(100, mean = 4))
labels <- factor(c(rep(0,100), rep(1,100)))
d <- data.frame(x,y,labels)
pchs <- c(2,1)
plot(x,y, pch = pchs[labels])
md <- ksvm(labels~y+x, data = d, kernel = "polydot", C = 1000000,
           kpar = list(degree = 3))
plot(md, data =d)

ui <- fluidPage(
  
  title = "SVM demo",
  
  fluidRow(
    column(12,
           "",
           fluidRow(
             column(6,
                    plotOutput("dataplot")),
             column(width = 6,
                    plotOutput("SVMplot")))
    )),
  
  hr(),
  
  fluidRow(
    column(5,
           selectInput("kernel", label = h3("Kernel Choice"),
                       choices = list("Linear Kernel" = "vanilladot", 
                                      "Radial Basis Kernel" = "rbfdot",
                                      "Polynomial Kernel (degree 2)" = "polydotdeg2",
                                      "Polynomial Kernel (degree 3)" = "polydotdeg3",
                                      "Polynomial Kernel (degree 4)" = "polydotdeg4",
                                      "Polynomial Kernel (degree 5)" = "polydotdeg5"), selected = 1)),
    column(5,     
           sliderInput("C", label = h3("Capacity Constant C"),
                       min = 0.05, max = 5, value = 0.05))
    )
)

server <- shinyServer(function(input, output) {
  output$dataplot <- renderPlot(function() {
    print(plot(y~x, data = d, pch = pchs[labels],
               main = "Simulated Data"))
  })
  output$SVMplot <- renderPlot(function() {
    if(grepl("deg", input$kernel)){
      temp <- strsplit(input$kernel, "deg")[[1]]
      md <- ksvm(labels~y+x, data = d, kernel = temp[1], 
                 C = input$C, kpar = list(degree = as.integer(temp[2])))
    }else{
      md <- ksvm(labels~y+x, data = d, kernel = input$kernel, C = input$C) 
    }
    print(plot(md,data=d))
  })
})

shinyApp(ui, server)
