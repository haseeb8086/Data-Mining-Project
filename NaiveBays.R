library(shiny)
library(shinydashboard)
library(e1071)
library(plotly)

# Load the dataset
dataset <- filled_data


ui <- dashboardPage(
  dashboardHeader(title = "Grade Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Predict Grade Before Mid-II Exam", tabName = "mid2_exam"),
      menuItem("Predict Grade Before Final Exam", tabName = "final_exam")
    )
  ),
  dashboardBody(
    tabItems(
    
      tabItem(tabName = "mid2_exam",
              fluidRow(
                sliderInput("num_assignments_mid2", "Number of Assignments:", min = 1, max = 5, value = 4),
                sliderInput("num_quizzes_mid2", "Number of Quizzes:", min = 1, max = 7, value = 4),
                actionButton("predict_mid2", "Predict")
              ),
              fluidRow(
                plotlyOutput("metrics_graph_mid2")
              )
      ),
      
      # Tab for predicting grade before Final exam
      tabItem(tabName = "final_exam",
              fluidRow(
                sliderInput("num_assignments_final", "Number of Assignments:", min = 1, max = 5, value = 5),
                sliderInput("num_quizzes_final", "Number of Quizzes:", min = 1, max = 7, value = 5),
                actionButton("predict_final", "Predict")
              ),
              fluidRow(
                plotlyOutput("metrics_graph_final")
              )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Function to preprocess the dataset based on user inputs
  preprocessed_data <- reactive({
    # Variables for Mid-II exam prediction
    num_assignments_mid2 <- input$num_assignments_mid2
    num_quizzes_mid2 <- input$num_quizzes_mid2
    
    # Variables for Final exam prediction
    num_assignments_final <- input$num_assignments_final
    num_quizzes_final <- input$num_quizzes_final
    
    # Select relevant columns based on user inputs
    selected_columns_mid2 <- c(paste0("As:", 1:num_assignments_mid2), paste0("Qz:", 1:num_quizzes_mid2), "S-I", "S-II", "Grade")
    selected_columns_final <- c(paste0("As:", 1:num_assignments_final), paste0("Qz:", 1:num_quizzes_final), "S-I", "S-II", "Grade")
    
    preprocessed_data <- list(mid2 = dataset[selected_columns_mid2], final = dataset[selected_columns_final])
    
    return(preprocessed_data)
  })
  
  # Function to train the Naive Bayes model
  train_naive_bayes <- reactive({
    preprocessed_data <- preprocessed_data()
    
    # Train the Naive Bayes model using e1071
    naive_bayes_mid2 <- naiveBayes(Grade ~ ., data = preprocessed_data$mid2)
    naive_bayes_final <- naiveBayes(Grade ~ ., data = preprocessed_data$final)
    
    trained_models <- list(mid2 = naive_bayes_mid2, final = naive_bayes_final)
    
    return(trained_models)
  })
  
  # Function to generate random test data
  generate_test_data <- function() {
    preprocessed_data <- preprocessed_data()
    dataset_mid2 <- preprocessed_data$mid2
    dataset_final <- preprocessed_data$final
    
    # Generate random test data based on the range of values in the original dataset
    test_data_mid2 <- lapply(dataset_mid2, function(column) {
      if (is.numeric(column)) {
        column_range <- range(column, na.rm = TRUE)
        random_values <- runif(length(column), min = column_range[1], max = column_range[2])
        return(random_values)
      } else {
        return(rep("", length(column)))
      }
    })
    
    test_data_final <- lapply(dataset_final, function(column) {
      if (is.numeric(column)) {
        column_range <- range(column, na.rm = TRUE)
        random_values <- runif(length(column), min = column_range[1], max = column_range[2])
        return(random_values)
      } else {
        return(rep("", length(column)))
      }
    })
    
    # Convert the generated test data into a data frame
    test_data_mid2 <- as.data.frame(test_data_mid2)
    test_data_final <- as.data.frame(test_data_final)
    
    # Set column names of the generated test data
    colnames(test_data_mid2) <- colnames(dataset_mid2)
    colnames(test_data_final) <- colnames(dataset_final)
    
    return(list(mid2 = test_data_mid2, final = test_data_final))
  }
  
  # Function to predict grades before the Mid-II exam
  predict_mid2 <- function() {
    trained_models <- train_naive_bayes()
    test_data <- generate_test_data()$mid2
    
    # Perform prediction on the test data
    predictions <- predict(trained_models$mid2, test_data)
    
    return(predictions)
  }
  
  # Function to predict grades before the Final exam
  predict_final <- function() {
    trained_models <- train_naive_bayes()
    test_data <- generate_test_data()$final
    
    # Perform prediction on the test data
    predictions <- predict(trained_models$final, test_data)
    
    return(predictions)
  }
  
  # Event handler for the "Predict Grade Before Mid-II Exam" button
  observeEvent(input$predict_mid2, {
    predictions <- predict_mid2()
    output$metrics_graph_mid2 <- renderPlotly({
      calculate_metrics(predictions, "mid2")
    })
  })
  
  # Event handler for the "Predict Grade Before Final Exam" button
  observeEvent(input$predict_final, {
    predictions <- predict_final()
    output$metrics_graph_final <- renderPlotly({
      calculate_metrics(predictions, "final")
    })
  })
  
  # Function to calculate model accuracy, precision, and recall
  calculate_metrics <- function(predictions, exam_type) {
    true_labels <- preprocessed_data()[[exam_type]]$Grade
    
    # Calculate confusion matrix
    cm <- table(predictions, true_labels)
    
    # Calculate model metrics
    accuracy <- sum(diag(cm)) / sum(cm)
    precision <- diag(cm) / colSums(cm)
    recall <- diag(cm) / rowSums(cm)
    
    # Create a graph to display the metrics
    metrics_graph <- plot_ly(x = colnames(cm), type = "bar", name = "Precision", y = precision, marker = list(color = "#FFC300")) %>%
      add_trace(y = recall, name = "Recall", marker = list(color = "#FF5733")) %>%
      add_trace(y = accuracy, name = "Accuracy", marker = list(color = "#3366CC")) %>%
      layout(title = "Model Metrics",
             xaxis = list(title = "Grade"),
             yaxis = list(title = "Value"),
             barmode = "group")
    
    return(metrics_graph)
  }
}

# Run the Shiny app
shinyApp(ui, server)
