library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(corrplot)
library(jsonlite)
library(readxl)
library(tidyr)
library(psych)
library(rpart)
library(rpart.plot)
library(caret)
library(lattice)
library(DT)
library(class)


ui <- dashboardPage(
  
  dashboardHeader(title = "My App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Show Dataset", tabName = "features", icon = icon("list-alt"),
               menuSubItem("Dataset Sheet 1", tabName = "Dataset_S1"),
               menuSubItem("Dataset Sheet 2", tabName = "Dataset_S2"),
               menuSubItem("Dataset Sheet 3", tabName = "Dataset_S3"),
               menuSubItem("Dataset Sheet 4", tabName = "Dataset_S4"),
               menuSubItem("Dataset Sheet 5", tabName = "Dataset_S5"),
               menuSubItem("Dataset Sheet 6", tabName = "Dataset_S6"),
               menuSubItem("Dataset Sheet 7", tabName = "Dataset_S7")
      ),
      menuItem("Combine Sheets", tabName = "CAS", icon = icon("list-alt")),
      menuItem("EDA Analysis", tabName = "EDA", icon = icon("list-alt"),
               menuSubItem("Summary of Data", tabName = "SOF"),
               menuSubItem("Check Missing Value", tabName = "MV"),
               menuSubItem("Fill Miss Value", tabName = "FMV"),
               menuSubItem("Show Outliers", tabName = "SOD")
      ),
      
     
      menuItem("About Us", tabName = "about", icon = icon("info-circle")),
      menuItem("Contact Us", tabName = "contact", icon = icon("envelope"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Home Page")
      ),
      tabItem(tabName = "features",
              h2("Dataset")
      ),
      ########### Subfeature of Show Dataset
      tabItem(tabName = "Dataset_S1",
              h2("Dataset Sheet 1"),
              dataTableOutput("Show_Dataset_table1")
      ),
      tabItem(tabName = "Dataset_S2",
              h2("Dataset Sheet 2"),
              dataTableOutput("Show_Dataset_table2")
      ),
      tabItem(tabName = "Dataset_S3",
              h2("Dataset Sheet 3"),
              dataTableOutput("Show_Dataset_table3")
      ),
      tabItem(tabName = "Dataset_S4",
              h2("Dataset Sheet 4"),
              dataTableOutput("Show_Dataset_table4")
      ),
      tabItem(tabName = "Dataset_S5",
              h2("Dataset Sheet 5"),
              dataTableOutput("Show_Dataset_table5")
      ),
      tabItem(tabName = "Dataset_S6",
              h2("Dataset Sheet 6"),
              dataTableOutput("Show_Dataset_table6")
      ),
      tabItem(tabName = "Dataset_S7",
              h2("Dataset Sheet 7"),
              dataTableOutput("Show_Dataset_table7")
      ), 
      tabItem(tabName = "CAS",
              h2("Combine all Sheet"),
              dataTableOutput("CAS")
      ),
      ################################
      ###############################
      tabItem(tabName = "EDA",
              h2("EDA Analysis")
      ),
      ########### Subfeature of Show Dataset
      tabItem(tabName = "SOF",
              h2("Data Summary"),
              verbatimTextOutput("SOF")
      ),
      tabItem(tabName = "MV",
              h2("Data Summary"),
              plotOutput("MV")
      ),
      tabItem(tabName = "FMV",
              h2("Fill Missing Values"),
              tableOutput("FMV")
      ),
      tabItem(tabName = "SOD",
              h2("Outlier Data"),
              plotOutput("SOD")
      ),
      #################
      ####################
      
                
      #######################
      ##################
      tabItem(tabName = "about",
              h2("About Us Page")
      ),
      tabItem(tabName = "contact",
              h2("Contact Us Page")
      )
    )
  )
)

filled_data <- reactiveVal(NULL)
server <- function(input, output) {
  # Read the "D1" sheet from the Excel file
  df1 <- read_excel("DM_DataSet.xlsx", sheet = "D1")
  
  # Render the data table
  output$Show_Dataset_table1 <- renderDataTable({
    df1
  })
  
  # Read the "D2" sheet from the Excel file
  df2 <- read_excel("DM_DataSet.xlsx", sheet = "D2")
  
  # Render the data table
  output$Show_Dataset_table2 <- renderDataTable({
    df2
  })
  # Read the "D3" sheet from the Excel file
  df3 <- read_excel("DM_DataSet.xlsx", sheet = "D3")
  
  # Render the data table
  output$Show_Dataset_table3 <- renderDataTable({
    df3
  })
  # Read the "D4" sheet from the Excel file
  df4 <- read_excel("DM_DataSet.xlsx", sheet = "D4")
  
  # Render the data table
  output$Show_Dataset_table4 <- renderDataTable({
    df4
  })
  # Read the "D5" sheet from the Excel file
  df5 <- read_excel("DM_DataSet.xlsx", sheet = "D5")
  
  # Render the data table
  output$Show_Dataset_table5 <- renderDataTable({
    df5
  })
  # Read the "D6" sheet from the Excel file
  df6 <- read_excel("DM_DataSet.xlsx", sheet = "D6")
  
  # Render the data table
  output$Show_Dataset_table6 <- renderDataTable({
    df6
  })
  # Read the "D7" sheet from the Excel file
  df7 <- read_excel("DM_DataSet.xlsx", sheet = "D7")
  
  # Render the data table
  output$Show_Dataset_table7 <- renderDataTable({
    df7
  })
###########################
###########################
  combined_sheet <- bind_rows(df1, df2,df3,df4,df5,df6,df7)
  output$CAS <- renderDataTable({
    combined_sheet
  })
  ####################
  ###################
  output$SOF <- renderPrint({
    summary(combined_sheet)
  })
  ################
  ###############
  missingValues <- reactive({
    # Calculate the count of missing values
    sum(is.na(combined_sheet))
  })
  
  # Plot the count of missing values using ggplot2
  output$MV <- renderPlot({
    # Create a data frame with the missing values count
    data_summary <- data.frame(Value = c("Missing", "Non-Missing"),
                               Count = c(missingValues(), length(combined_sheet) - missingValues()))
    
    # Create the bar plot using ggplot2
    ggplot(data_summary, aes(x = Value, y = Count, fill = Value)) +
      geom_bar(stat = "identity") +
      labs(title = "Count of Missing Values",
           x = "Value",
           y = "Count") +
      theme_minimal()
  })
  ##############
  #############
  # Update and store the filled data
  observeEvent(combined_sheet, {
    # Create a copy of the original data to preserve the original
    data <- combined_sheet
    
    # Identify numeric columns
    numeric_cols <- sapply(data, is.numeric)
    
    # Iterate over numeric columns and fill missing values with mean
    for (col in names(data)[numeric_cols]) {
      data[is.na(data[[col]]), col] <- mean(data[[col]], na.rm = TRUE)
    }
    
    # Identify categorical columns
    categorical_cols <- sapply(data, is.factor)
    
    # Iterate over categorical columns and fill missing values with the mode
    for (col in names(data)[categorical_cols]) {
      data[is.na(data[[col]]), col] <- Mode(data[[col]])
    }
    
    # Store the filled data in the reactive value
    filled_data(data)
  })
  
  # Display the updated data in a table
  output$FMV <- renderTable({
    filled_data()
    
  })
  
  # Reactive function to handle outliers and generate boxplot
  handleOutliers <- reactive({
    # Create a copy of the filled data to avoid modifying the original
    processed_data <- filled_data()
    
    # Identify numeric columns
    numeric_cols <- sapply(processed_data, is.numeric)
    
    # Iterate over numeric columns and handle outliers
    for (col in names(processed_data)[numeric_cols]) {
      # Detect outliers using appropriate method (e.g., Tukey's fences)
      outliers <- boxplot.stats(processed_data[[col]])$out
      
      # Replace outliers with appropriate handling technique (e.g., NA)
      processed_data[processed_data[[col]] %in% outliers, col] <- NA
    }
    
    # Return the data with handled outliers
    processed_data
  })
  
  # Display the boxplot
  output$SOD <- renderPlot({
    # Create a boxplot of the numeric columns from handleOutliers()
    boxplot(handleOutliers()[, sapply(handleOutliers(), is.numeric)], 
            main = "Boxplot (After Outlier Handling)", 
            col = "steelblue",
            ylab = "Value")
  })
  
  
  #############
  ###########



  ###################
  ###################
}


shinyApp(ui, server)
