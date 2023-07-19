library(shiny)
library(class)
library(ggplot2)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "KNN Prediction Model"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("KNN Prediction Before S-II", tabName = "KNNpredicts2", icon = icon("line-chart")),
      menuItem("KNN Prediction Before Finals", tabName = "KNNpredictF", icon = icon("line-chart")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab - Predict Before S-II
      tabItem(
        tabName = "KNNpredicts2",
        fluidRow(
          box(
            width = 4,
            title = "K Value",
            sliderInput("k_value_s2", "Select K value:", min = 1, max = 10, value = 5)
          ),
          box(
            width = 8,
            title = "Accuracy",
            plotOutput("KNNaccuracy_plotS2")
          ),
          box(
            width = 8,
            title = "Precision and Recall",
            verbatimTextOutput("KNN_metrics_textS2")
          )
        )
      ),
      # Predict Before Finals
      tabItem(
        tabName = "KNNpredictF",
        fluidRow(
          box(
            width = 4,
            title = "K Value",
            sliderInput("k_value_f", "Select K value:", min = 1, max = 10, value = 5)
          ),
          box(
            width = 8,
            title = "Accuracy",
            plotOutput("KNNaccuracy_plotF")
          ),
          box(
            width = 8,
            title = "Precision and Recall",
            verbatimTextOutput("KNN_metrics_textF")
          )
        )
      ),
      # Second tab - About
      tabItem(
        tabName = "about",
        h2("Mid-II Grade Prediction"),
        p("This app predicts students' grades as 'pass' or 'fail' before the Mid-II exam using the KNN algorithm."),
        p("Use the slider to select the K value for the KNN algorithm."),
        p("The accuracy of the prediction will be displayed in the plot.")
      )
    )
  )
)

# Server
server <- function(input, output) {
  dataset <- reactive({
    data <- filled_data # Replace with your dataset
    data
  })
  
  output$KNNaccuracy_plotS2 <- renderPlot({
    model <- predictMidIIGradesS2(dataset(), input$k_value_s2)
    accuracy <- sum(model == dataset()$Grade) / length(dataset()$Grade)
    
    accuracy_data <- data.frame(Metric = "Accuracy", Value = accuracy)
    ggplot(accuracy_data, aes(x = Metric, y = Value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "", y = "Accuracy") +
      ggtitle("Mid-II Grade Prediction Accuracy") +
      theme_minimal()
  })
  
  output$KNN_metrics_textS2 <- renderPrint({
    model <- predictMidIIGradesS2(dataset(), input$k_value_s2)
    accuracy <- sum(model == dataset()$Grade) / length(dataset()$Grade)
    
    # Calculate precision and recall
    tp <- sum(model == "Pass" & dataset()$Grade == "Pass")
    fp <- sum(model == "Pass" & dataset()$Grade == "Fail")
    fn <- sum(model == "Fail" & dataset()$Grade == "Pass")
    
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    
    cat("Precision:", precision, "\n")
    cat("Recall:", recall, "\n")
  })
  
  predictMidIIGradesS2 <- function(data, k) {
    # Select relevant features for prediction
    predictors <- c("As:1", "As:2", "As:3", "As:4", "Qz:1", "Qz:2", "Qz:3", "Qz:4", "S-I")
    
    # Split the data into training and test datasets
    train_indices <- sample(nrow(data), nrow(data) * 0.8)  # 80% for training
    train_data <- data[train_indices, ]
    test_data <- data[-train_indices, ]
    
    # Train the k-nearest neighbors model using the training data
    model <- knn(train_data[predictors], test_data[predictors], train_data$Grade, k = k)
    
    return(model)
  }
  
  output$KNNaccuracy_plotF <- renderPlot({
    model <- predictMidIIGradesF(dataset(), input$k_value_f)
    accuracy <- sum(model == dataset()$Grade) / length(dataset()$Grade)
    
    accuracy_data <- data.frame(Metric = "Accuracy", Value = accuracy)
    ggplot(accuracy_data, aes(x = Metric, y = Value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "", y = "Accuracy") +
      ggtitle("Mid-II Grade Prediction Accuracy") +
      theme_minimal()
  })
  
  output$KNN_metrics_textF <- renderPrint({
    model <- predictMidIIGradesF(dataset(), input$k_value_f)
    accuracy <- sum(model == dataset()$Grade) / length(dataset()$Grade)
    
    # Calculate precision and recall
    tp <- sum(model == "Pass" & dataset()$Grade == "Pass")
    fp <- sum(model == "Pass" & dataset()$Grade == "Fail")
    fn <- sum(model == "Fail" & dataset()$Grade == "Pass")
    
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    
    cat("Precision:", precision, "\n")
    cat("Recall:", recall, "\n")
  })
  
  predictMidIIGradesF <- function(data, k) {
    # Select relevant features for prediction
    predictors <- c("As:1", "As:2", "As:3", "As:4","As:5", "Qz:1", "Qz:2", "Qz:3","Qz:4", "Qz:5", "S-I","S-II")
    
    # Split the data into training and test datasets
    train_indices <- sample(nrow(data), nrow(data) * 0.8)  # 80% for training
    train_data <- data[train_indices, ]
    test_data <- data[-train_indices, ]
    
    # Train the k-nearest neighbors model using the training data
    model <- knn(train_data[predictors], test_data[predictors], train_data$Grade, k = k)
    
    return(model)
  }
}

# Run the app
shinyApp(ui = ui, server = server)
