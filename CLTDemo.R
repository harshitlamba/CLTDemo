# install.packages('shiny')
# install.packages('moments')
library(shiny)
library(moments)

# UI in ui() - This contains input and output elements (as arguments to fluidPage) to be 
# shown on application page - these are reactive inputs/outputs
ui <- fluidPage(
  fluidRow(tags$h2(tags$strong('Central Limit Theorem Demonstration'), align = 'center')),
  tags$br(),
  fluidRow(
    column(3, selectInput(inputId = 'distributionType', label = 'Distribution Type',
                          choices = c('Uniform','Normal','Skewed','Random'), 
                          selected = 'Uniform',
                          multiple = FALSE)),
    column(3, selectInput(inputId = 'numberOfSamples', label = 'Number of Samples',
                          choices = c(5,10,50,100,1000,10000), selected = 5,
                          multiple = FALSE)),
    column(3, selectInput(inputId = 'sampleSize', label = 'Size of a Sample',
                          choices = c(2,10,30,100,200), selected = 2,
                          multiple = FALSE)),
    column(3,div(style='position:relative;top:1.75em;left:3.5em',
                 actionButton(inputId = 'reset', label = 'Reset', width = '50%')))
    ),
  hr(),
  fluidRow(
    column(6,plotOutput(outputId = 'originalDistribution')),
    column(6,plotOutput(outputId = 'samplingDistribution'))
  ),
  fluidRow(column(6, h4(tags$u(tags$strong('Original Distribution Summary'))), 
                  align = 'center'),
           column(6, h4(tags$u(tags$strong('Sampling Distribution Summary'))), 
                  align = 'center')
  ),
  fluidRow(
    column(6,tableOutput(outputId = 'statistics_original'), align = 'center'),
    column(6,tableOutput(outputId = 'statistics_samplingDist'), align = 'center')
  )
)

# Backend script in server() - used to connect inputs to outputs - contains the
# instructions to build the outputs that user sees
server <- function(input, output, session){
  
  sample_size <- reactive({input$sampleSize})
  no_of_samples <- reactive({input$numberOfSamples})
  
  data <- reactive({
    if(input$distributionType=='Uniform'){
      return(runif(10000))
    }else if(input$distributionType=='Normal'){
      return(rnorm(10000, mean = 0, sd = 1))
    }else if(input$distributionType=='Skewed'){
      alpha <- runif(1)
      if(alpha>0.5){
        return(rbeta(10000, 5, 2))
      }else{
        return(rbeta(10000, 2, 5))
      }
    }else{
      alpha <- runif(1)
      if(alpha>0.5){
        tmp <- rbeta(10000, 5, 2)
      }else{
        tmp <- rbeta(10000, 2, 5)
      }
      cut_intervals <- cut(tmp, breaks = 20, labels = 1:20)
      # cut_intervals_df <- as.data.frame(table(cut_intervals))
      which_intervals_blank <- sample(unique(cut_intervals), 
                                      size = sample(6:15,size = 1))
      return(as.numeric(cut_intervals[!cut_intervals %in% which_intervals_blank]))
      }
  })
  
  sampling_data <- reactive({
    if(no_of_samples()!=0 | sample_size()!=0){
      sample_mean <- c()
      for(i in 1:no_of_samples()){
        sample_mean <- append(sample_mean,mean(sample(data(),size = sample_size(),
                                                      replace=TRUE)))
      }
    }
    return(sample_mean)
  })

  output$originalDistribution <- renderPlot({
    title <- 'Original Distribution'
    hist(data(), main = title, xlab = 'Random Variable', breaks = 20, col = TRUE)})
  
  output$samplingDistribution <- renderPlot({
    title_sampling <- 'Sampling Distribution'
    hist(sampling_data(), main = title_sampling, xlab = 'Sample Means (Random Variable)',
         breaks = 20, col = 'blue', border = 'blue')
    # }else{
    #   output$error <- renderText({'Please enter number of samples and sample size'})
    # }
  })
  
  output$statistics_original <- renderTable({
    list('Mean' = mean(data()), 'Std Dev' = sd(data()),
         'Skewness' = skewness(data()), 'Kurtosis' = kurtosis(data()))
  })
  
  output$statistics_samplingDist <- renderTable({
    list('Mean' = mean(sampling_data()), 'Std Dev' = sd(sampling_data()),
         'Skewness' = skewness(sampling_data()), 'Kurtosis' = kurtosis(sampling_data()))
  })
  
  observeEvent(input$reset,{
    updateSelectInput(session, inputId = 'distributionType', selected = 'Uniform')
    updateSelectInput(session, inputId = 'numberOfSamples', selected = 5)
    updateSelectInput(session, inputId = 'sampleSize', selected = 2)
  })
}

shinyApp(ui = ui, server = server)
