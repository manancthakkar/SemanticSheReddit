library(shiny)
library(wordcloud2)
library(tm)
library(wordcloud)
library(syuzhet)  # For sentiment analysis

# Define server logic
server <- function(input, output) {
  # Reactive expression to fetch data based on subreddit selection
  data_selected <- reactive({
    switch(input$subreddit,
           "Feminism" = posts_women[["Feminism"]],
           "TwoXChromosomes" = posts_women[["TwoXChromosomes"]],
           "AskWomen" = posts_women[["AskWomen"]],
           "TheRedPill" = posts_men[["TheRedPill"]],
           "MensRights" = posts_men[["MensRights"]],
           "AskMen" = posts_men[["AskMen"]],
           "MensLib" = posts_men[["MensLib"]],
           "news" = posts_neutral[["news"]],
           "worldnews" = posts_neutral[["worldnews"]],
           "AskReddit" = posts_neutral[["AskReddit"]],
           NULL)
  })
  
  # Display summary statistics
  output$summaryOutput <- renderPrint({
    req(data_selected())
    data <- data_selected()
    paste("Total Posts: ", nrow(data),
          "\nAverage Score: ", mean(data$score, na.rm = TRUE),
          "\nAverage Number of Comments: ", mean(data$num_comments, na.rm = TRUE))
  })
  

  output$wordCloudOutput <- renderWordcloud2({
    req(data_selected())
    data <- data_selected()
    
    if (is.null(data) || nrow(data) == 0) {
      return("No data to display.")
    }
    
    # Assuming 'title' column has the relevant text and is a character vector
    text <- tolower(data$title) 
    
    # Create a Corpus from the vector of text
    docs <- Corpus(VectorSource(text))
    
    # Preprocess the text
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeWords, stopwords("english"))  # Remove stopwords
    
    # Create a term-document matrix
    tdm <- TermDocumentMatrix(docs)
    
    # Convert the matrix to a data frame
    tdm_df <- as.data.frame(as.matrix(tdm))
    
    # Word frequencies data frame for wordcloud2
    word_freq_df <- data.frame(word = rownames(tdm_df), freq = rowSums(tdm_df, na.rm = TRUE))
    
    # Order by frequency
    word_freq_df <- word_freq_df[order(word_freq_df$freq, decreasing = TRUE), ]
    
    # Create the word cloud
    wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, min.freq = 1,
              max.words = 200, random.order = FALSE, rot.per = 0.35, 
              colors = brewer.pal(8, "Set1"))
  })
  
  
  
  # Sentiment analysis plot
  output$sentimentPlot <- renderPlot({
    req(data_selected())
    data <- data_selected()
    sentiments <- get_nrc_sentiment(paste(data$title, collapse=" "))
    barplot(colSums(sentiments), las = 2, col = rainbow(10),
            main = "Sentiment Analysis of Posts", xlab = "Sentiment", ylab = "Count")
  })
}
