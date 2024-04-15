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
library(tm)

################################################################################################################
################################################ Functions #####################################################
################################################################################################################

format_header <<- function(header.txt){
  caption.txt <- header.txt #spaces have been converted to ".". Below we fix this.
  caption.txt <- gsub(pattern = "1\\.5", replacement = "1 to 5", x = caption.txt) # "." means any character. This is why we use "\\." instead
  caption.txt <- gsub(pattern = "\\.\\.", replacement = " ", x = caption.txt) # "." means any character. This is why we use "\\." instead
  caption.txt <- gsub(pattern = "\\.", replacement = " ", x = caption.txt) # "." means any character. This is why we use "\\." instead
  caption.txt <- paste0(substring(caption.txt, 1, nchar(caption.txt)-1), "?") #it's nice to have a question mark at the end of a question, also remove space at end of sentenc
  return(caption.txt)
}

################################################################################################################
################################################ Extracting data ###############################################
################################################################################################################

url <- "https://docs.google.com/spreadsheets/d/15s7zeTEFT-fShou_jVYaHZHT3_9IvF6rgluUUdWy29Q/edit#gid=0"
sheet <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, na.strings=c("","NA")) #emptry rows set to NAhttp://127.0.0.1:38813/graphics/plot_zoom_png?width=1920&height=1009
#colnames(sheet) <- format_header(colnames(sheet))

################################################################################################################
################################################ Testing plots #################################################
################################################################################################################

columnName <- function(ColumnReference) { #from https://stackoverflow.com/a/14771584/11598009, function to retrieve colname as a string
  return(substring(deparse(substitute(ColumnReference)),which(strsplit(deparse(substitute(ColumnReference)),"")[[1]]=="$")[1]+1))
}

col.name.txt <- columnName(sheet$On.a.scale.from.1.5..how.interesting.do.you.find.the.publication..Kjemi..is.for.you..)
col.name.fill <- columnName(sheet$You.are..a..)

col.name.txt.sub <- gsub(pattern = "\\.\\.", replacement = " ", x = col.name.txt)
col.name.txt.sub <- gsub(pattern = "\\.", replacement = " ", x = col.name.txt.sub)

sheet.NA.rm <- subset(sheet, !is.na(sheet[[col.name.txt]]))
nrow(sheet.NA.rm)


sheet.individual.means <- sheet.NA.rm %>% group_by(!!sym(col.name.fill)) %>% mutate(avg_score = mean(!!sym(col.name.txt))) %>% group_by(!!sym(col.name.fill))
scale.question = T
ggplot(sheet.individual.means, aes(x = if (scale.question==TRUE){as.character(sheet.individual.means[[col.name.txt]])}else {fct_rev(fct_infreq(as.character(sheet.individual.means[[col.name.txt]])))},
)) + 
  geom_bar(aes(fill= sheet.individual.means[[col.name.fill]])) + 
  theme_classic() +
  theme(legend.title = element_blank()
        ) +
  facet_wrap(vars(sheet.individual.means[[col.name.fill]])) +
  geom_vline(aes(xintercept = sheet.individual.means$avg_score), size = 2, alpha = 0.3) +
  geom_text(aes( x = 2.5, y = Inf, label = paste("Avg:", format(round(sheet.individual.means$avg_score, 2), nsmall = 2))), check_overlap = TRUE, hjust="inward", vjust ="inward")

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
        numericInput('n_facet', 'Number of top facets', 6, min = 1, max = 7),
        numericInput('font_size', 'Font size', 26),
      numericInput('count_font_size', 'Count font size', 9),
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
#  if(grepl(x = caption.txt, pattern = "If you answered ")){
    ##prepare Corpus from words
    #text <- sheet[[input$variable]]
    #print(text)
    #docs <- Corpus(VectorSource(text))
    #print(docs)
    #docs <- tm_map(docs, removeWords, stopwords("Norwegian")) #remove common words
    #dtm <- TermDocumentMatrix(docs) #reformat
    #matrix <- as.matrix(dtm) #reformat
    #words <- sort(rowSums(matrix),decreasing=TRUE) 
    #df <- data.frame(word = names(words),freq=words)
    #wordcloud_rep <- repeatable(wordcloud)   # Make the wordcloud drawing predictable during a session
    #wordcloud(words = df$word,
              #freq = df$freq,
              #min.freq = 1,
              #random.order=FALSE,
              #rot.per=0.35,
              #colors=brewer.pal(8, "Dark2"))
#  }
#  else{
  bar_plot.reactive <- reactive({
    
    caption.txt <- format_header(input$variable)
    
    #scale-question parameters
    scale.question <- grepl(x = input$variable, pattern = "scale", ignore.case = TRUE) #is this a question with a ranking from 1-5?
    
    #unpack lists (cells with comma-separated, multiple values)
    cells.w.multiple.value <- any(grepl(pattern = ",", x = sheet[[input$variable]])) # check if any cells contain a comma
    if (cells.w.multiple.value){
      sheet.unpackt <- sheet %>% separate_longer_delim(!!sym(input$variable), delim = ", ")
      sheet <- sheet.unpackt
    }
    
    # remove NA
    sheet.NA.rm <- subset(sheet, !is.na(sheet[[input$variable]]))
    #handling counts for faceted plots
    if(input$facet.wrap&scale.question){
      sheet.individual.means <- sheet.NA.rm %>% group_by(!!sym(input$fill_value)) %>% mutate(avg_score = mean(!!sym(input$variable))) %>% group_by(!!sym(input$variable))
      sheet.NA.rm <- sheet.individual.means
      sheet.w.count <- sheet.NA.rm %>% group_by(!!sym(input$fill_value)) %>% add_count() #find the counts for each fill value
      count_values <- sort(unique(sheet.w.count$n), decreasing=TRUE) #find unique count values, sorts them high->low
      sheet.top.n.subset <- subset(sheet.w.count, subset = n %in% count_values[1:input$n_facet] ) # creates a subset of the dataset which contains only the top n counts (n is defined in the shiny app as n_facet)
      sheet.NA.rm <- sheet.top.n.subset # pipelines the data
    } else
    if(input$facet.wrap){
      validate(need(input$fill_value != "None", "Please define a fill value"))
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
    
        {if(input$fill_value == "None") #if fill is "None", then fill with no color
          geom_bar(fill="#005691")
          }+
        {if(input$fill_value != "None")geom_bar( #if fill is defined as other than none, fill with fill parameters
            aes(fill=
                  if(scale.question){as.character(!!sym(input$fill_value))}  #convert scale-numbers (1 to 5) to characters
                  else{!!sym(input$fill_value)}
                  ) 
          )
            }+ 
      geom_text(stat='count', size = input$count_font_size, aes(label=after_stat(count)), vjust=if(scale.question==FALSE){"inward"}else{"inward"}, hjust=if(scale.question=="inward"){-0.5}else{0}) +
      theme_classic() +
      {if(scale.question==FALSE)coord_flip()} + #scale question should not be coord-flipped.
      {if(scale.question==TRUE&input$facet.wrap==FALSE)geom_vline(xintercept = mean(sheet.NA.rm[[input$variable]]), size = 3, alpha = 0.4)} +
      {if(scale.question==TRUE&input$facet.wrap==FALSE)geom_text(aes( x = 2.5, y = Inf, label = paste("Avg:", format(round(mean(sheet.NA.rm[[input$variable]]), 2), nsmall = 2))), check_overlap = TRUE, hjust=+1, vjust ="inward", size = (input$font_size/3))} +
      labs(title = caption.txt) +
      theme(text = element_text(size = input$font_size),
            plot.title = element_textbox_simple(margin = margin(0,0,20,0)), #textbox enables line breaks, margin increased distance betwen plot/title
            plot.title.position = if(str_length(caption.txt)>50){ "plot"}else{"panel"}, #if question gets too long, place it further left
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(), #face="bold" tested this, not sure if it's nicer
            legend.position=if(input$facet.wrap){"none"}else{"right"}, 
            strip.placement = "outside",
            ) +
      {if(scale.question==TRUE)xlab("1 = Very little, 5 = Very much")} + #scale question should not be coord-flipped.
      ylab("Count") +
      # aggregate(sheet.NA.rm$`On a scale from 1 to 5 how interesting do you find the publication Kjemi is for you?`, list(sheet.NA.rm$`You are?`), mean) #calculate individual mean values
      {if(input$facet.wrap)facet_wrap(~sheet.NA.rm[[input$fill_value]], ncol = 2)} + #create individual plots
      {if(input$facet.wrap&scale.question)geom_vline(aes(xintercept = sheet.NA.rm$avg_score), size = 2, alpha = 0.3)} +
      {if(input$facet.wrap&scale.question)geom_text(aes( x = 1.5, y = Inf, label = paste("Avg:", format(round(sheet.NA.rm$avg_score, 2), nsmall = 2))), check_overlap = TRUE, hjust="inward", vjust ="inward", size = (input$font_size/3)) }
    
  })
  #}  This bracket is for the else
  
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
      png(file=file, width = 1500, height = 1000, pointsize = 24) #can we make this work with ggsave? https://stackoverflow.com/questions/14810409/how-to-save-plots-that-are-made-in-a-shiny-app probably not worth the hassle (right now)
      plot(bar_plot.reactive())
      dev.off()
    }
  )
  
  
}

shinyApp(ui, server)
