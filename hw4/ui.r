library(ggvis)
shinyUI(fluidPage(
  titlePanel("Homework 4: A shiny interactive web site to show the evaluation result"),
  tabsetPanel(
    tabPanel("Comparating the performance between methods", 
      sidebarPanel(
        radioButtons("target", label = h3("Select Target"), 
                     choices = list("male" = "male", "female" = "female"), 
                     selected = "male"),
        checkboxGroupInput("files", label = h3("Select Methods"), 
                           choices = list("method 1" = "methods/method1.csv", "method 2" = "methods/method2.csv",
                                          "method 3" = "methods/method3.csv", "method 4" = "methods/method4.csv", 
                                          "method 5" = "methods/method5.csv", "method 6" = "methods/method6.csv", 
                                          "method 7" = "methods/method7.csv", "method 8" = "methods/method8.csv", 
                                          "method 9" = "methods/method9.csv", "method 10" = "methods/method10.csv"),
                           selected = c("methods/method1.csv","methods/method2.csv","methods/method3.csv",
                                        "methods/method4.csv","methods/method5.csv","methods/method6.csv")),
        radioButtons("measure", label = h3("Select Measurement"),
                     choices = list("sensitivity" = 1, "specificity" = 2, "F1 score" = 3, "AUC" = 4), 
                     selected = 1),
        uiOutput("plot_ui")
      ),
      mainPanel(
        ggvisOutput("plot"),
        tableOutput("result_table")
      )
    ),
    tabPanel("ROC curve", 
      sidebarPanel(
        radioButtons("target_2", label = h3("Select Target"), 
                     choices = list("male" = "male", "female" = "female"), 
                     selected = "male"),
        selectInput("files_2", label = h3("Select Methods"), 
                     choices = list("method 1" = "methods/method1.csv", "method 2" = "methods/method2.csv",
                                    "method 3" = "methods/method3.csv", "method 4" = "methods/method4.csv", 
                                    "method 5" = "methods/method5.csv", "method 6" = "methods/method6.csv", 
                                    "method 7" = "methods/method7.csv", "method 8" = "methods/method8.csv", 
                                    "method 9" = "methods/method9.csv", "method 10" = "methods/method10.csv"),
                     selected = "methods/method1.csv"),
        sliderInput("threshold", label = h3("Threshold"), min = 0, max = 1, value = 0.5),
        uiOutput("plot_ui_2")
      ),
      mainPanel(
        plotOutput("plot_2"),
        tableOutput("result_table_2_1"),
        tableOutput("result_table_2_2")
      )
    )
  )
))