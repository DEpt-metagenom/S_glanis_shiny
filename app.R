options(shiny.maxRequestSize = 30 * 1024^2) # Set maximum upload size to 30 MB

library(shiny)
library(wordcloud2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("GOs on a Word Cloud - Silurus glanis reference transcriptome"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select the dataset
      selectInput("dataset_choice", "Choose Dataset",
                  choices = c("stringent_GO.out", "improved_GO.out")),
      numericInput("min_count", "Minimum count", value = 100, min = 1),
      numericInput("max_count", "Maximum count", value = 100000, min = 1),
      # Add an action button to trigger word cloud update
      # This helps prevent re-calculation on every number input change
      actionButton("update_cloud", "Update Word Cloud")
    ),
    mainPanel(
      wordcloud2Output("wordcloud_img", width = "100%", height = "900px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store pre-calculated desc_counts for each file
  # These will be calculated once when the app starts
  stringent_desc_counts <- reactiveVal(NULL)
  improved_desc_counts <- reactiveVal(NULL)
  
  # Load data and pre-calculate counts when the app initializes
  observe({
    # Path to your files (assuming they are in the same directory as app.R)
    stringent_file_path <- "stringent_GO.out"
    improved_file_path <- "improved_GO.out"
    
    # Process stringent_GO.out
    if (file.exists(stringent_file_path)) {
      tryCatch({
        df_stringent <- read.delim(stringent_file_path, sep = "\t", stringsAsFactors = FALSE)
        if ("desc" %in% colnames(df_stringent)) {
          stringent_desc_counts(df_stringent %>% count(desc))
          showNotification("Loaded stringent_GO.out", type = "message")
        } else {
          showNotification("stringent_GO.out does not contain a 'desc' column.", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error reading stringent_GO.out:", e$message), type = "error")
      })
    } else {
      showNotification("stringent_GO.out not found. Please ensure it's in the app directory.", type = "warning")
    }
    
    # Process improved_GO.out
    if (file.exists(improved_file_path)) {
      tryCatch({
        df_improved <- read.delim(improved_file_path, sep = "\t", stringsAsFactors = FALSE)
        if ("desc" %in% colnames(df_improved)) {
          improved_desc_counts(df_improved %>% count(desc))
          showNotification("Loaded improved_GO.out", type = "message")
        } else {
          showNotification("improved_GO.out does not contain a 'desc' column.", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error reading improved_GO.out:", e$message), type = "error")
      })
    } else {
      showNotification("improved_GO.out not found. Please ensure it's in the app directory.", type = "warning")
    }
  })
  
  # Reactive expression to select the appropriate pre-calculated data
  selected_pre_calculated_data <- reactive({
    if (input$dataset_choice == "stringent_GO.out") {
      stringent_desc_counts()
    } else if (input$dataset_choice == "improved_GO.out") {
      improved_desc_counts()
    }
  })
  
  # Render the word cloud, dependent on selected_pre_calculated_data and update_cloud button
  output$wordcloud_img <- renderWordcloud2({
    # Depend on the action button to trigger the update
    input$update_cloud
    
    # Isolate reactive values that should not trigger re-calculation until button is pressed
    isolate({
      current_desc_counts <- selected_pre_calculated_data()
      
      req(current_desc_counts) # Ensure data is available
      
      if (is.null(current_desc_counts) || nrow(current_desc_counts) == 0) {
        showNotification("Selected dataset is empty or not loaded.", type = "warning")
        return(NULL)
      }
      
      # Filter the pre-calculated counts based on user input
      filtered_desc_counts <- current_desc_counts %>%
        filter(n >= input$min_count & n <= input$max_count)
      
      if (nrow(filtered_desc_counts) == 0) {
        showNotification("No terms match the specified count range for the selected dataset.", type = "warning")
        return(NULL)
      }
      
      # Generate word cloud
      wordcloud2(filtered_desc_counts, size = 0.9, color = "random-light", backgroundColor = "#F5F5F5")
    })
  })
}

# Run the app
shinyApp(ui, server)
