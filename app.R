#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinycssloaders)

library(tidyverse)
library(corrplot) 
library(ggvis)
library(GGally)
library(ggExtra)
library(rpart)
library(rpart.plot)
library(RCurl)
library(knitr)
library(qcc)
library(rgl)
library(pca3d)
library(gridExtra)
library(arules)

#Load Data
song_table = read.csv('genre_music.csv',header = TRUE,sep = ",")

#change popularity to Hit or Flop
song_table$popularity_f = as.character(song_table$popularity)
song_table$popularity_f[song_table$popularity_f == 0] = "Flop"
song_table$popularity_f[song_table$popularity_f == 1] = "Hit"

#drop rows not wanted in classification
df_prep = subset(song_table, select = -c(track,artist,decade,duration_s,acousticness))

#factorize genre and popularity_f
df_prep$genre = as.factor(df_prep$genre)
df_prep$popularity_f = as.factor(df_prep$popularity_f)

#move popularity to end
df_prep = df_prep %>% relocate(popularity,.after = last_col())

vardf = subset(df_prep,select = -c(popularity,popularity_f))

#get variable names
vars_song = colnames(vardf)

#Prepare Discrete Value Dataset
num_data = df_prep[,sapply(df_prep,is.numeric)]


num_data_disc = discretizeDF(num_data,default = list(method = "frequency"))

#factorize already discrete values
num_data_disc$key = as.factor(num_data$key)
num_data_disc$mode = as.factor(num_data$mode)
num_data_disc$time_signature = as.factor(num_data$time_signature)
num_data_disc$popularity = as.factor(num_data$popularity)
num_data_disc$genre = as.factor(song_table$genre)

# Define UI for application that allows users to select inputs to model and hyper parameters
ui <- fluidPage(

    # Application title
    titlePanel("Classifying Hit Songs"),
    
    mainPanel(
        tabsetPanel(
        #Tab for Introduction of Data
          tabPanel("Introduction",
                   #includeHTML("Introduction.html")
                   htmlOutput("intro")
                   ),
          
          #Tab for EDA
          tabPanel("EDA",
                   #includeHTML('CS3_EDA.html')
                   htmlOutput("EDA")
                   ),
          
          #Tab for PCA
          tabPanel("PCA",
                   #includeHTML('CS3_PCA.html')
                   htmlOutput("PCA")
                   ),
          
          #Tab with Example Decision Tree
          tabPanel("Classification Example",
                   #includeHTML('CS3_DT_Example.html')
                   htmlOutput("DT_ex")
                   ),
          
          #Tab for Discrete Decision Tree
          tabPanel("Decision Tree Discrete Variables",
                   fluidRow(
                     column(5,
                            wellPanel(
                              fluidRow(
                                column(8,
                                       h3("Inputs")),
                                column(2,
                                       actionButton(inputId = "DTDisc",
                                                    label = "Classify"))),
                              sliderInput(inputId = "TsplitDisc",
                                          label = "Training Split",
                                          min = .05,
                                          max = .95,
                                          value = .7),
                              pickerInput(
                                inputId = "VarPickerDisc", 
                                label = "Select/Deselect Desired Variables", 
                                choices = vars_song, 
                                options = list(
                                  `actions-box` = TRUE, 
                                  size = 10,
                                  `selected-text-format` = "count > 3"
                                ), 
                                multiple = TRUE,
                                selected = vars_song
                              ),
                              h4("Hyperparameter Tuning"),
                              sliderInput(inputId = "MinSplitDisc",
                                          label = "Minimum Number of Objects in Node to Split",
                                          min = 2,
                                          max = 100,
                                          value = 2),
                              sliderInput(inputId = "MaxDepthDisc",
                                          label = "Max Tree Depth",
                                          min = 2,
                                          max = 30,
                                          value = 5),
                              sliderInput(inputId = "MinGainDisc",
                                          label = "Min Gini Information Gain per Split",
                                          min = 0,
                                          max = .5,
                                          value = 0)
                            )),
                     column(5,
                            wellPanel(
                              
                              h3("Results"),
                              h4("Model Confusion Matrix"),
                              tableOutput("CMTrainDisc")%>% withSpinner(color="#0dc5c1",proxy.height = 50,size = .5),
                              h4("Model Accuracy"),
                              textOutput("Train_accDisc")%>% withSpinner(color="#0dc5c1",proxy.height = 50,size = .5),
                              h4("Test Confusion Matrix"),
                              tableOutput("CMTestDisc")%>% withSpinner(color="#0dc5c1",proxy.height = 50,size = .5),
                              h4("Test Accuracy"),
                              textOutput("Test_accDisc")%>% withSpinner(color="#0dc5c1",proxy.height = 50,size = .5),
                              h5("Model Creation Time"),
                              textOutput("SolveTimeDisc")
                            ))
                   )
          ),
          
          #Tab for Continuous Decision Tree
          tabPanel("Decision Tree Continuous Variables",
                   fluidRow(
                     column(5,
                      wellPanel(
                        fluidRow(
                          column(8,
                        h3("Inputs")),
                        column(2,
                        actionButton(inputId = "DTVar",
                                     label = "Classify"))),
                       sliderInput(inputId = "Tsplit",
                                   label = "Training Split",
                                   min = .05,
                                   max = .95,
                                   value = .7),
                       pickerInput(
                         inputId = "VarPicker", 
                         label = "Select/Deselect Desired Variables", 
                         choices = vars_song, 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE,
                         selected = vars_song
                       ),
                       h4("Hyperparameter Tuning"),
                       sliderInput(inputId = "MinSplit",
                                   label = "Minimum Number of Objects in Node to Split",
                                   min = 2,
                                   max = 100,
                                   value = 2),
                       sliderInput(inputId = "MaxDepth",
                                   label = "Max Tree Depth",
                                   min = 2,
                                   max = 30,
                                   value = 5),
                       sliderInput(inputId = "MinGain",
                                   label = "Min Gini Information Gain per Split",
                                   min = 0,
                                   max = .5,
                                   value = 0)
                      )),
                     column(5,
                        wellPanel(
                         
                         h3("Results"),
                         h4("Model Confusion Matrix"),
                         tableOutput("CMTrain")%>% withSpinner(color="#0dc5c1",proxy.height = 50,size = .5),
                         h4("Model Accuracy"),
                         textOutput("Train_acc")%>% withSpinner(color="#0dc5c1",proxy.height = 50,size = .5),
                         h4("Test Confusion Matrix"),
                         tableOutput("CMTest")%>% withSpinner(color="#0dc5c1",proxy.height = 50,size = .5),
                         h4("Test Accuracy"),
                         textOutput("Test_acc")%>% withSpinner(color="#0dc5c1",proxy.height = 50,size = .5),
                         h5("Model Creation Time"),
                         textOutput("SolveTime")
                     ))
                    )
                   )
        )
    )
    
)

# Define server logic required to calculate accuracy
server <- function(input, output) {
  
  getPage<-function(str_html) {
    return(includeHTML(str_html))
  }
  
  output$intro<-renderUI({getPage("Introduction.html")})
  output$EDA<-renderUI({getPage("CS3_EDA.html")})
  output$PCA<-renderUI({getPage("CS3_PCA.html")})
  output$DT_ex<-renderUI({getPage("CS3_DT_Example.html")})
  
  
  #Classification Model Discrete Variables
  observeEvent(input$DTDisc,{
    if(length(input$VarPickerDisc) > 0){
      
      #Train Test split
      Train_split = input$TsplitDisc
      Test_split = 1-Train_split
      
      
      set.seed(1)
      sample = sample(c(TRUE, FALSE), nrow(df_prep), replace=TRUE, prob=c(Train_split,Test_split))
      train = df_prep[sample, ]
      test = df_prep[!sample, ]
      
      #remove popularity
      train_answer = subset(train, select = c(popularity))
      train = subset(train, select = -c(popularity))
      
      test_answer = subset(test,select = c(popularity))
      test = subset(test,select = -c(popularity,popularity_f))
      
      
      #Update Train to include on variables selected
      vars_song_df = as.data.frame(vars_song)
      VarPicker_df = as.data.frame(input$VarPickerDisc)
      
      names(VarPicker_df)[1] = "VarPicker"
      
      
      is_selected = vars_song_df$vars_song %in% VarPicker_df$VarPicker
      
      train_sub = train[,is_selected]
      test_sub = test[,is_selected]
      
      
      #Add back in popularity
      train_sub$popularity_f = train$popularity_f
      
      
      
      control = rpart.control(minsplit = input$MinSplitDisc,
                              maxdepth = input$MaxDepthDisc,
                              cp = input$MinGainDisc)
      startTime = Sys.time()
      
      dt.model = rpart(popularity_f~ .,
                       data = train_sub, method = "class",control = control)
      
      endTime = Sys.time()
      
      solve_time = endTime - startTime
      
      #Train Prediction
      train_prediction = predict(dt.model,train_sub)
      train_prediction = as.data.frame(train_prediction)
      
      train_is_popular = train_prediction$Hit > 0.5
      train_prediction$Popular = as.integer(as.logical(train_is_popular))
      
      #Train Confusion matrix
      train_table_mat = table(train_answer$popularity, train_prediction$Popular,
                              dnn = c("Actual","Predicted"))
      output$CMTrainDisc = renderTable({train_table_mat})
      
      #Train Accuracy
      accuracy_train = sum(diag(train_table_mat))/sum(train_table_mat)
      output$Train_accDisc = renderText({accuracy_train})
      
      #Test Prediction
      test_prediction = predict(dt.model,test_sub)
      test_prediction = as.data.frame(test_prediction)
      
      is_popular = test_prediction$Hit > 0.5
      test_prediction$Popular = as.integer(as.logical(is_popular))
      
      #Test Confusion matrix
      test_table_mat = table(test_answer$popularity, test_prediction$Popular,
                             dnn = c("Actual","Predicted") )
      output$CMTestDisc = renderTable({test_table_mat})
      
      #Test Accuracy
      accuracy_test = sum(diag(test_table_mat))/sum(test_table_mat)
      output$Test_accDisc = renderText({accuracy_test})
      
      output$SolveTimeDisc = renderText({paste(solve_time,"Seconds")})
      
      
      
    }else{output$Var_accDisc = renderText("Error: At Least One Variable Must Be Selected")}
  })

  #Decison Tree Model Continuous Variables
    observeEvent(input$DTVar,{
      if(length(input$VarPicker) > 0){
        
        #Train Test split
        Train_split = input$Tsplit
        Test_split = 1-Train_split
        
        
        set.seed(1)
        sample = sample(c(TRUE, FALSE), nrow(df_prep), replace=TRUE, prob=c(Train_split,Test_split))
        train = df_prep[sample, ]
        test = df_prep[!sample, ]
        
        #remove popularity
        train_answer = subset(train, select = c(popularity))
        train = subset(train, select = -c(popularity))
        
        test_answer = subset(test,select = c(popularity))
        test = subset(test,select = -c(popularity,popularity_f))
        
        
        #Update Train to include on variables selected
        vars_song_df = as.data.frame(vars_song)
        VarPicker_df = as.data.frame(input$VarPicker)
        
        names(VarPicker_df)[1] = "VarPicker"
        
        
        is_selected = vars_song_df$vars_song %in% VarPicker_df$VarPicker
        
        train_sub = train[,is_selected]
        test_sub = test[,is_selected]
        
        
        #Add back in popularity
        train_sub$popularity_f = train$popularity_f
        
        
        
        control = rpart.control(minsplit = input$MinSplit,
                                maxdepth = input$MaxDepth,
                                cp = input$MinGain)
        startTime = Sys.time()
  
        dt.model = rpart(popularity_f~ .,
                         data = train_sub, method = "class",control = control)
        
        endTime = Sys.time()
        
        solve_time = endTime - startTime
        
        #Train Prediction
        train_prediction = predict(dt.model,train_sub)
        train_prediction = as.data.frame(train_prediction)
        
        train_is_popular = train_prediction$Hit > 0.5
        train_prediction$Popular = as.integer(as.logical(train_is_popular))
        
        #Train Confusion matrix
        train_table_mat = table(train_answer$popularity, train_prediction$Popular,
                                dnn = c("Actual","Predicted"))
        output$CMTrain = renderTable({train_table_mat})
        
        #Train Accuracy
        accuracy_train = sum(diag(train_table_mat))/sum(train_table_mat)
        output$Train_acc = renderText({accuracy_train})
        
        #Test Prediction
        test_prediction = predict(dt.model,test_sub)
        test_prediction = as.data.frame(test_prediction)
        
        is_popular = test_prediction$Hit > 0.5
        test_prediction$Popular = as.integer(as.logical(is_popular))
        
        #Test Confusion matrix
        test_table_mat = table(test_answer$popularity, test_prediction$Popular,
                               dnn = c("Actual","Predicted") )
        output$CMTest = renderTable({test_table_mat})
        
        #Test Accuracy
        accuracy_test = sum(diag(test_table_mat))/sum(test_table_mat)
        output$Test_acc = renderText({accuracy_test})
        
        output$SolveTime = renderText({paste(solve_time,"Seconds")})
        
        
        
      }else{output$Var_acc = renderText("Error: At Least One Variable Must Be Selected")}
    })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
