shinyUI(fluidPage(
  titlePanel("Final Project "),
  navlistPanel(widths = c(2, 8),
    tabPanel("Content",
             img(src='slide1.JPG', align = "left")
    ),
    tabPanel("Input",
             img(src='slide2.JPG', align = "left"),
             br(),
             a("https://www.kaggle.com/zhangjuefei/birds-bones-and-living-habits",href="https://www.kaggle.com/zhangjuefei/birds-bones-and-living-habits")
    ),
    tabPanel("Input",
             img(src='slide3.JPG', align = "left")
    ),
    tabPanel("Modeling",
             img(src='slide4.JPG', align = "left")
    ),
    tabPanel("Output",
             img(src='slide5.JPG', align = "left")
    ),
    tabPanel("Demo",
             tabsetPanel(
               tabPanel("fold_1",
                        plotOutput('plot1'),
                        tableOutput('table1')
               ),
               tabPanel("fold_2",
                        plotOutput('plot2'),
                        tableOutput('table2')
               ),
               tabPanel("fold_3",
                        plotOutput('plot3'),
                        tableOutput('table3')
               ),
               tabPanel("fold_4",
                        plotOutput('plot4'),
                        tableOutput('table4')
               ),
               tabPanel("fold_5",
                        plotOutput('plot5'),
                        tableOutput('table5')
               ),
               tabPanel("null hypothesis and accuracy",
                        h3("The accuracy of this method: ",textOutput('accuracy')),
                        h3("The accuracy of null model: ",textOutput('null_accuracy')),
                        img(src='slide6.JPG', align = "left")
                        
               )
    )
  )
  
    
  )
  
    
  
))