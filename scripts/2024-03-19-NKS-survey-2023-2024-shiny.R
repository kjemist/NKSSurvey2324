# Plots the results from the 2023-24 Norwegian Chemical Society Survey in an interactive Shiny application
# Illimar Rekand, 2023-24
# email: illimar.rekand@gmail.com

library(gsheet)
library(shiny)
library(ggplot2)
library(forcats) #fct_infreq
library(dplyr)
library(tidyverse)
library(ggtext) # fix ggtitle width

################################################################################################################
################################################ Extracting data ###############################################
################################################################################################################

#url <- "https://docs.google.com/spreadsheets/d/1V9dOSDzO3R1mZLjk7YfDdpjHgdydSSJtFCRBCYvKORI/edit#gid=901015046"
url <- "https://docs.google.com/spreadsheets/d/15s7zeTEFT-fShou_jVYaHZHT3_9IvF6rgluUUdWy29Q/edit#gid=0"
sheet <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, na.strings=c("","NA")) #emptry rows set to NAhttp://127.0.0.1:38813/graphics/plot_zoom_png?width=1920&height=1009


################################################################################################################
################################################ Testing plots #################################################
################################################################################################################

columnName <- function(ColumnReference) { #from https://stackoverflow.com/a/14771584/11598009, function to retrieve colname as a string
  return(substring(deparse(substitute(ColumnReference)),which(strsplit(deparse(substitute(ColumnReference)),"")[[1]]=="$")[1]+1))
}

col.name.txt <- columnName(sheet$You.are.)
col.name.fill <- columnName(sheet)
col.name.txt.sub <- gsub(pattern = "\\.\\.", replacement = " ", x = col.name.txt)
col.name.txt.sub <- gsub(pattern = "\\.", replacement = " ", x = col.name.txt.sub)

sheet.NA.rm <- subset(sheet, !is.na(sheet[[col.name.txt]]))
nrow(sheet.NA.rm)

scale.question <- grepl(x = col.name.txt, pattern = "skala", ignore.case = TRUE)

ggplot(sheet.NA.rm, aes(x = if (scale.question==TRUE){as.character(sheet.NA.rm[[col.name.txt]])}else {fct_rev(fct_infreq(as.character(sheet.NA.rm[[col.name.txt]])))},
                        )) + 
  geom_bar(aes(fill= sheet.NA.rm[[col.name.fill]])) + 
  theme_classic() + #fct_infreq = order by count, fct_rev = reverse order
  theme(legend.title = element_blank()) +
  facet_wrap(vars(sheet.NA.rm$What.is.the.name.of.the.company.you.work.in.))

################################################################################################################
################################################ Shiny #########################################################
################################################################################################################

ui <- bootstrapPage(
  
  # App title ----
  titlePanel("Choose input below"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against count ----
      # The below variables will be output as character strings
      selectInput("variable", "Variable:",
                  choices = names(sheet) #all column names
                    ),
      selectInput("fill_value", "Fill:",
                  choices = c("None", names(sheet)) #Adds a none for when filling is not necessary
      ),
      checkboxInput("facet.wrap", "Facet Wrap?", value = FALSE),
      conditionalPanel(condition = "input.facet.wrap == true",
        numericInput('n_facet', 'Number of top facets', 4, min = 1, max = 7)
      ),
      downloadButton('downloadPlot')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against count ----
      plotOutput("nks.plot")
      
      
    )
  )
)

server <- function(input, output) { #shiny passes selectInput as a string. To use these variables for subsetting dataframes, use e.g. df$!!sym(input$variable) or df[[input$variable]]
  
  formulaText <- reactive({
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  
  # Generate a plot of the requested variable against count ----
  bar_plot.reactive <- reactive({
    
    #fix caption from column title
    caption.txt <<- input$variable #spaces have been converted to ".". Below we fix this.
    caption.txt <<- input$variable #spaces have been converted to ".". Below we fix this.
    caption.txt <<- gsub(pattern = "1\\.5", replacement = "1 to 5", x = caption.txt) # "." means any character. This is why we use "\\." instead
    caption.txt <<- gsub(pattern = "\\.\\.", replacement = " ", x = caption.txt) # "." means any character. This is why we use "\\." instead
    caption.txt <<- gsub(pattern = "\\.", replacement = " ", x = caption.txt) # "." means any character. This is why we use "\\." instead
    caption.txt <<- paste0(substring(caption.txt, 1, nchar(caption.txt)-1), "?") #it's nice to have a question mark at the end of a question
    
    #scale-question parameters
    scale.question <- grepl(x = input$variable, pattern = "scale", ignore.case = TRUE) #is this a question with a ranking from 1-5?
    
    #unpack lists (cells with multiple values)
    cells.w.multiple.value <- any(grepl(pattern = ",", x = sheet[[input$variable]]))
    if (cells.w.multiple.value){
      sheet.unpackt <- sheet %>% separate_longer_delim(!!sym(input$variable), delim = ", ")
      sheet <- sheet.unpackt
    }
    
    # remove NA
    sheet.NA.rm <- subset(sheet, !is.na(sheet[[input$variable]]))
    #handling counts for facetted plots
    if(input$facet.wrap){
    sheet.w.count <- sheet.NA.rm %>% group_by(!!sym(input$fill_value)) %>% add_count() #find the counts for each fill value
    count_values <- sort(unique(sheet.w.count$n), decreasing=TRUE) #find unique count values, sorts them high->low

    sheet.top.n.subset <- subset(sheet.w.count, subset = n %in% count_values[1:input$n_facet] ) # creates a subset of the dataset which contains only the top n counts (n is defined in the shiny app as n_facet)
    sheet.NA.rm <- sheet.top.n.subset # pipelines the data
    }
    
    # let's plot
    ggplot(
      sheet.NA.rm, aes(x = if (scale.question==TRUE){as.character(.data[[input$variable]])} #numerical values from scale-questions are converted to text
                       else {fct_rev(fct_infreq(as.character(.data[[input$variable]])))}
                       ))+ #fct_infreq = order by count, fct_rev = reverse order. Scale questions should not be ordered by count, and needs to converted to character type
    
      geom_bar(
        if(input$fill_value == "None"){} #if fill is "None", then fill with no color
               else {aes(fill=
                           if(scale.question){as.character(!!sym(input$fill_value))} 
                         else{!!sym(input$fill_value)}
                         ) 
                 }
        ) +
      geom_text(stat='count', aes(label=..count..), vjust=if(scale.question==FALSE){0}else{-1}, hjust=if(scale.question==FALSE){-0.5}else{0}) +
      theme_classic() +
      {if(scale.question==FALSE)coord_flip()} + #scale question should not be coord-flipped.
      theme(text = element_text(size = 24),
            plot.title = element_textbox_simple(),
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            legend.position=if(input$facet.wrap){"none"}else{"right"}, 
            strip.placement = "outside",
            ) +
      ggtitle(caption.txt) +
    {if(input$facet.wrap)facet_wrap(vars(sheet.NA.rm[[input$fill_value]]), ncol = 2)}
  })
  
  output$nks.plot <- renderPlot(
    { 
      bar_plot.reactive()
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename <- function()
    { if (input$facet.wrap){
      paste0("NKS-plot-",input$variable,"-facet-wrap-",input$fill_value, ".png")
    }else
    {
      paste0("NKS-plot-",input$variable,"fill-value", input$fill_value, ".png")
    }
      },
    content <- function(file){
      png(file=file, width = 1000, height = 1000, pointsize = 24) #can we make this work with ggsave? https://stackoverflow.com/questions/14810409/how-to-save-plots-that-are-made-in-a-shiny-app
      plot(bar_plot.reactive())
      dev.off()
    }
  )
  
  
}

shinyApp(ui, server)
