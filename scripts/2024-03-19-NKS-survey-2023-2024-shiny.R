# Plots the results from the 2023-24 Norwegian Chemical Society Survey in an interactive Shiny application
# Illimar Rekand, 2023-24
# email: illimar.rekand@gmail.com

library(gsheet)
library(shiny)
library(ggplot2)
library(forcats) #fct

################################################################################################################
################################################ Extracting data ###############################################
################################################################################################################

url <- "https://docs.google.com/spreadsheets/d/1V9dOSDzO3R1mZLjk7YfDdpjHgdydSSJtFCRBCYvKORI/edit#gid=901015046"

sheet <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, na.strings=c("","NA")) #emptry rows set to NAhttp://127.0.0.1:38813/graphics/plot_zoom_png?width=1920&height=1009


################################################################################################################
################################################ Testing plots #################################################
################################################################################################################


sheet.NA.rm <- subset(sheet, !is.na(sheet$Du.er...You.are.....Annotert.Illimar...Vennligst.spesifiser.tilh.U.00F8.righet.og.arbeidstittel.om.du.velger..Other....Please.specify.your.affiliation.and.job.description.if.you.choose..Other..))
nrow(sheet.NA.rm)

ggplot(sheet.NA.rm, aes(x = fct_rev(fct_infreq(sheet.NA.rm$Du.er...You.are.....Annotert.Illimar...Vennligst.spesifiser.tilh.U.00F8.righet.og.arbeidstittel.om.du.velger..Other....Please.specify.your.affiliation.and.job.description.if.you.choose..Other..)),
                        )) + 
  geom_bar(aes(fill= sheet.NA.rm$Er.du.medlem.i.Norsk.Kjemisk.Selskap...Are.you.a.member.of.the.Norwegian.Chemical.Society.)) + 
  coord_flip() + 
  theme_classic() #fct_infreq = order by count, fct_rev = reverse order

################################################################################################################
################################################ Shiny #########################################################
################################################################################################################

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
                  choices = names(sheet.NA.rm) #all column names
                    ),
      selectInput("fill_value", "Fill:",
                  choices = c("None", names(sheet.NA.rm)) #Adds a none for when filling is not necessary
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
    paste("Placeholder1")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  
  # Generate a plot of the requested variable against count ----
  bar_plot.reactive <- reactive({
    sheet.NA.rm <- subset(sheet, !is.na(sheet[[input$variable]]))
    ggplot(sheet.NA.rm, aes(x = fct_rev(fct_infreq(!!sym(input$variable))))) + 
      geom_bar(
        if(input$fill_value == "None"){} #if fill is "None", then fill with no colour
               else {aes(fill=!!sym(input$fill_value))}
        )+ 
      coord_flip() + 
      theme_classic() #fct_infreq = order by count, fct_rev = reverse order
    
#    df.unpackt <- unpack.df(datasets.df, input$variable) #unpacks the lists
#    df.unpackt[[input$variable]] <- as.numeric(df.unpackt[[input$variable]]) #convert from char to numeric
#    plot.df <- df.unpackt
#    ggplot(plot.df, #sort bars after count, lowest to highest
#           aes(reorder(x = id, !!sym(input$variable)), y = !!sym(input$variable)))+ #input$variable is a string, !!sym() converts them into symbols
#      geom_point() + #fill-component in aes needs to be declared here, because it is not compatible with aes_string ##this is probably not necessary after all with the impl of !!sym(), but we will keep it to make it easier to read
#      geom_segment( aes(x=id, xend=id, y=0, yend=!!sym(input$variable))) +
#      ylab(paste(str_to_title( #Capitalize first words
#        sub("_", " ", #replace underscores with spaces
#            substring(input$variable, 8, nchar(input$variable)))))) +
#      xlab("PRIDE entry ID") +
#      theme_classic() + #remove gridline
#      theme(axis.text.x = element_text(angle = -45)) +
#      scale_fill_brewer(palette = "Paired")
#    #theme(legend.position = "none") # No legend
  })
  
  output$nks.plot <- renderPlot(
    { #The fields below are sorted chronologically, not after count5
      bar_plot.reactive()
    }
  )
  
  
  output$downloadPlot <- downloadHandler(
    filename <- function()
    {paste0("NKS-plot-",input$variable,".png")},
    content <- function(file){
      png(file=file)
      plot(bar_plot.reactive())
      dev.off()
    }
  )
  
  
}

shinyApp(ui, server)
