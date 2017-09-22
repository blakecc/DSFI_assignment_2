#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required libraries and clear workspace

rm(list = ls())

library(shiny)
library(tidyverse)
library(tidytext)
library(stringr)
library(topicmodels)
library(plotly)
library(scales)
library(gridExtra)
library(shinythemes)
library(markdown)

# Load complaints data

load("./data/complaints.RData")

# Define the UI using a navbarPage and tabPanel approach

ui <- navbarPage(theme = shinytheme("superhero"), "Assignment 2 App",
                 tabPanel("Existing data",
                          sidebarLayout(
                            sidebarPanel(
                              titlePanel("Input:"),
                              selectInput("filter_product",
                                          "Product:",
                                          c("Mortgage", "Credit card", "Debt collection", "Credit reporting", "Bank account or service")),
                              checkboxInput("filter_comp",
                                            "Compensated?"),
                              submitButton(text = "Filter"),
                              helpText("Filter complaints")),
                            mainPanel(
                              titlePanel("Histogram of product sentiment for selection"),
                              helpText("Net Sentiment Score = Proportion of positive words - Proportion of negative words"),
                              plotlyOutput("hist_sent")))),
                 tabPanel("Topic modeling",
                          sidebarLayout(
                            sidebarPanel(
                              titlePanel("Input:"),
                              sliderInput("num_topics",
                                          "Number of topics:",
                                          min = 2,
                                          max = 5,
                                          value = 3,
                                          step = 1,
                                          round = T),
                              submitButton("Model topics")),
                            mainPanel(
                              titlePanel("Top words of topic"),
                              helpText("Top 20 word beta's per topic - using LDA"),
                              plotOutput("top_term_plot")))),
                 tabPanel("User input",
                          sidebarLayout(
                            sidebarPanel(
                              titlePanel("Input: "),
                              textAreaInput("user_complaint",
                                        "User complaint",
                                        value = "Enter complaint ..."),
                              submitButton("Submit complaint")),
                            mainPanel(
                              titlePanel("New complaint assessment"),
                              helpText("Sentiment score of user complaint shown by red line, against previous complaints for product"),
                              plotlyOutput("user_comp_chart")))),
                 tabPanel("Help",
                          includeMarkdown("./data/instructions.md"))
)

# Define server logic

server <- function(input, output, session) {

  # Get complaints into tidy format at the word level, with Bing sentiments
  tidy_complaints_1 <- reactive({
    filtered_complaints <- complaints %>%
      filter(product == input$filter_product,
             consumer_compensated == input$filter_comp)
    
    
    filtered_complaints <- as.tibble(filtered_complaints)
    
    # parse the date and add some date related variables
    filtered_complaints_1 <- filtered_complaints %>% 
      mutate(date_received = parse_datetime(filtered_complaints$date_received, "%m/%d/%Y"))
    
    # turn into tidy text 
    replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
    unnest_reg <- "([^A-Za-z_\\d#@]|(?![A-Za-z_\\d#@]))" # ensure apostrophe not part of unnesting
    tidy_complaints <- filtered_complaints_1 %>% 
      mutate(consumer_complaint_narrative = str_replace_all(consumer_complaint_narrative, replace_reg, "")) %>% # remove stuff we don't want like links
      unnest_tokens(word, consumer_complaint_narrative, token = "regex", pattern = unnest_reg) %>% # tokenize
      mutate(word = tolower(word)) %>%
      filter(!word %in% stop_words$word, str_detect(word, "[a-z]"), !str_detect(word, "xx")) # remove stop words
    
    tidy_complaints_1 <- tidy_complaints %>% 
      left_join(get_sentiments("bing")) %>% # add sentiments (pos or neg)
      select(word,sentiment,everything()) %>%
      mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment))
    tidy_complaints_1
    
  })

   # Plot a histogram of sentiment for selected complaints
   output$hist_sent <- renderPlotly({
     tidy_complaints_1() %>%
       group_by(id) %>%
       # Use proportion of positives less proportion of negatives as score
       summarise(count_all = n(),
                 count_pos = length(sentiment[sentiment == "positive"]) / count_all,
                 count_neg = length(sentiment[sentiment == "negative"]) / count_all,
                 net_sentiment = count_pos - count_neg) %>%
       select(id, net_sentiment) %>%
       ggplot() +
        geom_histogram(aes(net_sentiment), colour = "grey", fill = "darkblue", binwidth = 0.05) +
        theme_light() + ylab("Count") +
        xlab("Net Sentiment Score") +
        scale_x_continuous(labels= scales::percent, limits = c(-0.6,0.6)) # set a fixed access for easier comparison
   })
   
   # Model the topics based on specified number of topics
   complaints_lda <- reactive({
     complaints_tdf <- tidy_complaints_1() %>%
       select(id, word) %>%
       group_by(id,word) %>%
       count() %>%  
       ungroup() 
     
     dtm_complaints <- complaints_tdf %>% 
       cast_dtm(id, word, n)
     
     set.seed(1234)
     LDA(dtm_complaints, k = input$num_topics)
   })
   
   # Output the plot of the top terms
   output$top_term_plot <- renderPlot({
     compaints_topics <- tidy(complaints_lda(), matrix = "beta")
     
     top_terms <- compaints_topics %>%
       group_by(topic) %>%
       top_n(20, beta) %>%
       ungroup()
     
     # Use an apply approach for each group of topics
     myplots <- lapply(split(top_terms,top_terms$topic), function(x){
       #relevel factor partei by wert inside this subset
       x$term <- factor(x$term, levels=x$term[order(x$beta,decreasing=F)])
       
       #make the plot
       p <- ggplot(x, aes(x = term, y = beta, width=0.75), color = "blue") +
         geom_bar(stat = "identity", fill =  hue_pal()(input$num_topics)[unique(x$topic)]) + # use the ggplot colour wheel approach to deine bar colour
         scale_fill_brewer() +
         theme_light()+
         theme(legend.position="none")+
         labs(y="Beta", x="", title=unique(x$topic))+
         coord_flip()
     })
     
     do.call(grid.arrange,c(myplots, ncol=min(3,input$num_topics)))
   })
   
   # Get the new complaint into the same format as the overall complaints data
   user_tidy_1 <- reactive({
     
     uc_df <- tibble(date_received = Sys.Date(),
                     product = input$filter_product,
                     consumer_complaint_narrative = input$user_complaint,
                     consumer_compensated = input$filter_comp,
                     id = "user123")
     
     replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
     unnest_reg <- "([^A-Za-z_\\d#@]|(?![A-Za-z_\\d#@]))"
     
     user_tidy <- uc_df %>%
       mutate(consumer_complaint_narrative = str_replace_all(consumer_complaint_narrative, replace_reg, "")) %>% # remove stuff we don't want like links
       unnest_tokens(word, consumer_complaint_narrative, token = "regex", pattern = unnest_reg) %>% # tokenize
       mutate(word = tolower(word)) %>%
       filter(!word %in% stop_words$word, str_detect(word, "[a-z]"), !str_detect(word, "xx")) # remove stop words
     
     user_tidy %>% 
       left_join(get_sentiments("bing")) %>% # add sentiments (pos or neg)
       select(word,sentiment,everything()) %>%
       # mutate(sentiment = ifelse(word == "trump", NA, sentiment)) %>% # "trump" is a positive word in the bing lexicon!
       mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment))
   })
   
   # Calculate the sentiment of the new complaint
   user_net_sentiment <- reactive({
     
     user_tidy_1() %>%
       group_by(id) %>%
       summarise(count_all = n(),
                 count_pos = length(sentiment[sentiment == "positive"]) / count_all,
                 count_neg = length(sentiment[sentiment == "negative"]) / count_all,
                 net_sentiment = count_pos - count_neg) %>%
       select(net_sentiment) %>% as.numeric()
     
     
   })
   
   # Find the likely topic of the new complaint using the posterior function
   user_topic_num <- reactive({
     user_tdf <- user_tidy_1() %>%
       select(id, word) %>%
       group_by(id,word) %>%
       count() %>%  
       ungroup() 
     
     dtm_complaints_user <- user_tdf %>% 
       cast_dtm(id, word, n)
     
     user_topic <- posterior(complaints_lda(), dtm_complaints_user)
     
     which(user_topic$topics == (max(user_topic$topics)))
   })
   
   # Plot the sentiment of the new complaint against the original histogram, and also state the likely topic
   output$user_comp_chart <- renderPlotly({
     tidy_complaints_1() %>%
       group_by(id) %>%
       summarise(count_all = n(),
                 count_pos = length(sentiment[sentiment == "positive"]) / count_all,
                 count_neg = length(sentiment[sentiment == "negative"]) / count_all,
                 net_sentiment = count_pos - count_neg) %>%
       select(id, net_sentiment) %>%
       ggplot() +
        geom_histogram(aes(net_sentiment), colour = "grey", fill = "darkblue") +
        theme_light() +
        ggtitle(paste0("The user complaint is topic: ", user_topic_num())) +
        ylab("Count") +
        xlab("Net Sentiment Score") +
        scale_x_continuous(labels= scales::percent) +
        geom_vline(xintercept = user_net_sentiment(), colour = "red", linetype = 2, size = 1) +
        scale_x_continuous(labels= scales::percent, limits = c(-0.6,0.6))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

