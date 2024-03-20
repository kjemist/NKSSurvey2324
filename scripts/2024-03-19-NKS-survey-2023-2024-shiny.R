# Plots the results from the 2023-24 Norwegian Chemical Society Survey in an interactive Shiny application
# Illimar Rekand, 2023
# email: illimar.rekand@gmail.com

library(gsheet)
library(shiny)
library(ggplot2)
library(dplyr)
library(forcats) #fct

################################################################################################################
################################################ Extracting data ###############################################
################################################################################################################

url <- "https://docs.google.com/spreadsheets/d/1V9dOSDzO3R1mZLjk7YfDdpjHgdydSSJtFCRBCYvKORI/edit#gid=901015046"

sheet <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, na.strings=c("","NA")) #emptry rows set to NA

class(sheet)

################################################################################################################
################################################ Shiny application #############################################
################################################################################################################


sheet.NA.rm <- subset(sheet, !is.na(sheet$Hva.er.navnet.p.U.00E5..bedriften.du.jobber.i....What.is.the.name.of.the.company.you.work.in.))
nrow(sheet.NA.rm)

ggplot(sheet.NA.rm, aes(x = fct_rev(fct_infreq(sheet.NA.rm$Hva.er.navnet.p.U.00E5..bedriften.du.jobber.i....What.is.the.name.of.the.company.you.work.in.)))) + 
  geom_bar() + 
  coord_flip() + 
  theme_classic() #fct_infreq = order by count, fct_rev = reverse order


#ggplot(sheet, aes(x = fct_rev(fct_infreq(sheet[[4]])))) + 
#  geom_bar() + 
#  coord_flip() + 
#  theme_classic() #fct_infreq = order by count, fct_rev = reverse order



ui <- fluidPage(
  
  # App title ----
  titlePanel("Choose input below"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against count ----
      # The below variables will be output as character strings
      selectInput("variable", "Variable:",
                  c("View count" = "fields.view_count" #currently only one input selectable, other countables have missing data
                  )),
      downloadButton('downloadPlot')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against count ----
      plotOutput("ebi.plot")
      
      
    )
  )
)

server <- function(input, output) { #shiny passes selectInput as a string. To use these variables for subsetting dataframes, use e.g. df$!!sym(input$variable) or df[[input$variable]]
  
  formulaText <- reactive({
    paste("Entries to the PRIDE database from Norwegian institutions")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Render slider input for certain plots
  
  
  # Generate a plot of the requested variable against count ----
  bar_plot.reactive <- reactive({
    df.unpackt <- unpack.df(datasets.df, input$variable) #unpacks the lists
    df.unpackt[[input$variable]] <- as.numeric(df.unpackt[[input$variable]]) #convert from char to numeric
    plot.df <- df.unpackt
    ggplot(plot.df, #sort bars after count, lowest to highest
           aes(reorder(x = id, !!sym(input$variable)), y = !!sym(input$variable)))+ #input$variable is a string, !!sym() converts them into symbols
      geom_point() + #fill-component in aes needs to be declared here, because it is not compatible with aes_string ##this is probably not necessary after all with the impl of !!sym(), but we will keep it to make it easier to read
      geom_segment( aes(x=id, xend=id, y=0, yend=!!sym(input$variable))) +
      ylab(paste(str_to_title( #Capitalize first words
        sub("_", " ", #replace underscores with spaces
            substring(input$variable, 8, nchar(input$variable)))))) +
      xlab("PRIDE entry ID") +
      theme_classic() + #remove gridline
      theme(axis.text.x = element_text(angle = -45)) +
      scale_fill_brewer(palette = "Paired")
    #theme(legend.position = "none") # No legend
  })
  
  output$ebi.plot <- renderPlot(
    { #The fields below are sorted chronologically, not after count5
      bar_plot.reactive()
    }
  )
  
  
  output$downloadPlot <- downloadHandler(
    filename <- function()
    {paste0("PRIDE-plot-",input$variable,".png")},
    content <- function(file){
      png(file=file)
      plot(bar_plot.reactive())
      dev.off()
    }
  )
  
  
}

shinyApp(ui, server)