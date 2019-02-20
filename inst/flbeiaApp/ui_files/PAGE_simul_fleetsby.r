


tabsetPanel(type = "tabs",
            tabPanel(
              title = "Time series",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("rangeFby", label=h4("Years"), min(as.numeric(fltStk$year)), max(as.numeric(fltStk$year)), value=range(as.numeric(fltStk$year)),step = 1),
                 selectizeInput("stockFby", label=h4("Stock"), unique(fltStk$stock), selected=unique(fltStk$stock),multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("fleetFby", label=h4("Fleet"),         unique(fltStk$fleet), selected=unique(fltStk$fleet)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorFby", label=h4("Indicator"), unique(fltStk$indicator), selected="landings", multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioFby", label=h4("Scenarios"),  unique(fltStk$scenario), selected=unique(fltStk$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
    			  hr(),
                  checkboxInput("fitCIFby", h5("Confident intervals"), FALSE),
                  checkboxInput("fitFby", h5("Free scales"), FALSE),
    			        
    			        # Options for file downloading
    			        textInput('filenmFby', h5("File Name"), value = "", width = NULL, placeholder = NULL),
    			        numericInput('fileWFby', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
    			        numericInput('fileHFby', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
    			        numericInput('fileScFby', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
    			        selectInput(inputId = "fileTypeFby", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
    			        downloadButton(outputId = "downFby", label = "Download the plot")
                ),
                mainPanel(
                  plotOutput("plotFby")
                )
              ))
)# end of the tabsetPanel