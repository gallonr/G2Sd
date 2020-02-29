
library(shiny)


stat.choose <- c("arithmetic","geometric", "folk.ward")
# Define UI for miles per gallon application
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  headerPanel(strong("G2Sd : Grain-size Statistics and Description of Sediment")),  
  sidebarPanel(
    
    span(p(strong("G2Sd")," is available on", a(img(src = "https://lamachinerie.org/wp-content/uploads/2019/06/github.jpg", height = 70, width = 200),href = "https://github.com/gallonr/G2Sd",target="_blank")),style ="text-align:center"),
    
    wellPanel(
      h3(p(strong("Data"))),
      p(textInput("separator", label = "Sep: ", value = ";"),
      textInput("decimal", label = "Dec: ", value = ","),
      # checkboxInput('header', 'Header', TRUE),
      fileInput('file1', 'Choose txt or csv File',accept=c('.csv','.txt','.CSV','.TXT')),
        em(helpText("For example of databases,",a("click here",href = "http://regisgallon.wordpress.com/r-software/",target="_blank")))
        )
      
    ),
    #     ,
    wellPanel(
      h3(p(strong("Statistics "))),
      h5(em("Methods")),
      h4(strong(em(selectInput('method', '', stat.choose)))),
      h5(em("Results displayed")),
      h4(strong(em(selectInput('gran', '', c('Statistics','Index','Texture','Sedim','All'))))),
      downloadButton('downloadData', 'Download')
    ),
    
    wellPanel(
      h3(p(strong("Graphics"))),
      em(helpText("To save graphics, right-click on the graphic",
                  "and select Save Picture As")),
      h5(em(strong("Histogram plot"))),
      numericInput('from','N° Station from',1),
      numericInput('to','To',1),
      checkboxInput('hist', 'Histogram', TRUE),
      checkboxInput('cum', 'Cumulative curve', TRUE),
      h5(em(strong("Distribution plot"))),
      selectInput('distritype', '', c('fine','large'))
    ),
    
    span(p("For more information, please contact ",  a(span("regis.gallon@lecnam.net", style = "color:blue"), href="mailto:regis.gallon@lecnam.net?subject=[G2Sd]%20Information"),style ="text-align:center")),  
    span(p(img(src = "https://media-exp1.licdn.com/dms/image/C4E0BAQFNO73Lo4I2Ng/company-logo_200_200/0?e=2159024400&v=beta&t=EWsUo-le-krHJWQD48FnEjKubdTTeir_zabBTObMl8Q", height = 100, width = 100),img(src = "cnrs.gif", height = 80, width = 80)),style ="text-align:center")  
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Table", tableOutput('contents')),
      tabPanel("Statistics", tableOutput('stat')),
      tabPanel("Histogram", plotOutput("plot1",width="auto",height=1900)),
      tabPanel("Distribution", plotOutput("plot2",width="auto",height=800))
      
    ))
))

