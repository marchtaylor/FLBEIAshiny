

tabsetPanel(type = "tabs",
            
            #------------------------------------
            # TIME SERIES window
            #------------------------------------
            tabPanel( 
              title = "Time series",
                sidebarLayout(
                sidebarPanel(
                  sliderInput("rangeS", label=h4("Years"), min(bio$year), max(bio$year), value=range(bio$year),step = 1),
                  selectizeInput("stockS", label=h4("Stock"), levels(as.factor(bio$stock)), selected=unique(bio$stock)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorS", label=h4("Indicators"), levels(as.factor(bio$indicator)),selected=unique(bio$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioS", label=h4("Scenarios"), levels(as.factor(bio$scenario)), selected=unique(bio$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  hr(),
                  checkboxInput("fitCIS", h5("Confident interval"), FALSE),
                  checkboxInput("fitS", h5("Free scales"), FALSE),
                  
                  # Options for file downloading
                  textInput('filenmS', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWS', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileHS', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileScS', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                  selectInput(inputId = "fileTypeS", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "down", label = "Download the plot")),
                
            mainPanel(
                  plotOutput("plotS"))
            )),
            
            
            #------------------------------------
            #  KOBE PLOT
            #------------------------------------
            tabPanel(
              title = "Kobe plot",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("rangeK", label=h4("Years"), min(bio$year), max(bio$year), value=range(bio$year),step = 1),
                  selectizeInput("stockK", label=h4("Stock"),        unique(reference_points$stock),  selected=unique(reference_points$stock),       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioK", label=h4("Scenarios"), unique(as.factor(bio$scenario)), selected=unique(reference_points$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                 
                  # Options for file downloading
                  textInput('filenmSK', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWSK', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileHSK', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileScSK', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                  selectInput(inputId = "fileTypeSK", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downSK", label = "Download the plot"),
                  hr()
                ),
                mainPanel(
                  plotOutput("plotK")
                )
              
            )),
            
            
            #------------------------------------
            #  BIOLOGICAL RISK
            #------------------------------------
            tabPanel(
              title = "Biological risk", 
              sidebarLayout(
                sidebarPanel(
                  sliderInput("rangeR", label=h4("Years"), min(as.numeric(risk$year)), max(as.numeric(risk$year)), value=range(as.numeric(risk$year)),step = 1),
                  selectizeInput("stockR", label=h4("Stock"), choices= unique((reference_points$stock)), selected=unique((reference_points$stock))[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioR", label=h4("Scenarios"), levels(as.factor(risk$scenario)), selected= unique((risk$scenario))[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("brpR", label=h4("Biological reference point"), choices=c("pBlim", "pBpa"),selected="pBlim", multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  
                  # Options for file downloading
                  textInput('filenmSR', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWSR', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileHSR', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileScSR', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                  selectInput(inputId = "fileTypeSR", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downSR", label = "Download the plot"),
                  hr()
                ),
                mainPanel(
                  plotOutput("plotR")
                )
            ))
)#end of tabsetPanel