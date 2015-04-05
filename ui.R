shinyUI(bootstrapPage(fluidPage(
  titlePanel("TSVM"),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Loading...",id="loadmessage")),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        div(align="center",fileInput(inputId = 'train_data', label = tags$div('train set *',style='color:blue'),accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
        div(align="center",fileInput(inputId = 'test_data', label = tags$div('test set *',style='color:blue'), accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
        div(align="center",checkboxInput("file_param","Show FileReader parameters",FALSE)),
        conditionalPanel(condition = "input.file_param==true",
                         checkboxInput(inputId = 'header',label = 'header',value = TRUE),
                         radioButtons(inputId = 'separator',label = 'sep',choices = c(Comma=',',Semicolon=';',Tab='\t'),',')
        )
      ),

      wellPanel(
        div(align="center",checkboxInput("param","Show parameters",FALSE)),
        conditionalPanel(condition = "input.param==true",
                         div(align="center",selectInput(inputId = 'type',label = tags$div('type',style='color:blue'),choices = c('Classification','Regression')))
        )
      ),

      wellPanel(div(align="center",checkboxInput("graph_save_param","Show Graph Save Options",FALSE)),
                div(align="center",conditionalPanel(condition = "input.graph_save_param==true",
                                                    radioButtons(inputId = "paramdown",label = "",choices=list("JPG"="jpg","PNG"="png","PDF"="pdf"),selected="jpg")
                ))
      ),

      div(align="center",actionButton(inputId = "quit", label = div(align="center",'Quit the app',style='color:blue')))



    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("train set", dataTableOutput('train_data')),
                  tabPanel("test set", dataTableOutput("test_data")),
                  tabPanel("model summary",
                           wellPanel(
                             div(align="center",textInput(inputId = 'title',label = 'title',value = 'title')),
                             div(align="center",textInput(inputId = 'author',label = 'author',value = 'author')),
                             tags$br(),
                             div(align="center",radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE)),
                             tags$br(),
                             div(align="center",downloadButton(outputId = 'downloadReport',label = 'Download Report'))
                           ),
                           tags$br(),
                           verbatimTextOutput("summary")
                  ),
                  tabPanel('plots',
                           plotOutput("pr_curve"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData1","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData2","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData3","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("Cumulative_curve"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData4","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData5","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData6","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("roc_curve"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData7","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData8","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData9","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("lift_curve"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData10","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData11","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData12","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("grid"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData13","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData14","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData15","Download as pdf"),align="center")))

                  ),
                  tabPanel("About",
                           strong('randomForest with Shiny'),
                           p("The goal of this project is to help students and researchers run randomForest analysis as easily as possible."),
                           p('This application is developed with',
                             a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
                             ''),
                           p('The code for this application is available at this',
                             a('GitHub.', href='https://github.com/guanlongtianzi/shinyOfRandomForest', target="_blank")),
                           br(),

                           strong('List of Packages Used'), br(),

                           aceEditor("myEditor1", value = '#R Scripts \nrequire(shiny)\nrequire(RWeka)\nrequire(ggplot2)\nrequire(shinyAce)\nrequire(gridExtra)\nrequire(rmarkdown)', mode="r",height = '200px',fontSize = 15,theme="ambiance"),

                           br(),
                           strong('Authors'),
                           HTML('<div style="clear: left;"><img src="my.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
                           br(),
                           br(),
                           br()

                  )

      )
    )

  )
)
)
)
