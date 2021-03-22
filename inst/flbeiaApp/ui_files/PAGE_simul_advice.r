

tabsetPanel(type = "tabs",
            tabPanel( 
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Stock and Indicator",
                  sliderInput("rangeA", label=h4("Years"), min(as.numeric(adv$year)), max(as.numeric(adv$year)), value=range(as.numeric(adv$year)),step = 1),
                  selectizeInput("stockA", label=h4("Stock"), levels(as.factor(adv$stock)), selected=unique(adv$stock)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorA", label=h4("Indicators"), levels(as.factor(adv$indicator)),selected="tac",multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioA", label=h4("Scenarios"), levels(as.factor(adv$scenario)), selected=unique(adv$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                  ),
                  shinyBS::bsCollapsePanel("Graphs",
                  numericInput('nColA', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100),
                  checkboxInput("fitCIA", h5("Confident intervals"), FALSE),
                  checkboxInput("fitA", h5("Free scales"), FALSE)
                  ),
                  shinyBS::bsCollapsePanel("Download",
                      #  Options for file downloading
                      fluidRow(column(8,textInput('filenmA', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                               column(4,selectInput(inputId = "fileTypeA", label = "File type", selected= "png", 
                                           choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
                      fluidRow(column(3,numericInput('fileWA', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
                               column(3,numericInput('fileHA', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
                               column(3,numericInput('fileScA', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
                      downloadButton(outputId = "downA", label = "Download the plot")))),
                column(9, uiOutput("plotA", inline =TRUE))
                ))
)# end of the tabsetPanel