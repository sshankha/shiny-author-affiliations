# app.R

library(shiny)
library(readxl)
library(dplyr)
library(officer)
library(pacman)

# Define UI
ui <- fluidPage(
  titlePanel("Author Affiliations and Contributions"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File", accept = c(".xlsx")),
      actionButton("process", "Process Data")
    ),
    mainPanel(
      textOutput("confirmText"),
      downloadButton("downloadAffiliations", "Download Affiliations Document"),
      downloadButton("downloadContributions", "Download Contributions Document")
    )
  )
)

# Define server logic
server <- function(input, output) {
  authors_data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  observeEvent(input$process, {
    req(authors_data())
    
    # Process data for affiliations
    ordered_data <- authors_data() %>%
      arrange(sequence)
    
    names <- ordered_data$name
    affiliations1 <- ordered_data$affiliation1
    affiliations2 <- ordered_data$affiliation2
    
    all_affiliations <- ordered_data %>%
      select(affiliation1, affiliation2) %>%
      unlist() %>%
      na.omit() %>%
      unique()
    
    affiliation_labels1 <- match(affiliations1, all_affiliations)
    affiliation_labels2 <- ifelse(!is.na(affiliations2) & affiliations2 != "", match(affiliations2, all_affiliations), NA)
    
    names_text <- sapply(1:length(names), function(i) {
      if (!is.na(affiliation_labels2[i])) {
        paste(names[i], affiliation_labels1[i], affiliation_labels2[i], sep = ",")
      } else {
        paste(names[i], affiliation_labels1[i], sep = ",")
      }
    })
    
    names_text <- paste(names_text, collapse = ", ")
    
    affiliations_text <- paste0(seq_along(all_affiliations), ": ", all_affiliations, collapse = "\n")
    
    # Create Word document for affiliations
    doc_affiliations <- read_docx() %>%
      body_add_par(names_text, style = "Normal") %>%
      body_add_par("\n\n", style = "Normal") %>%
      body_add_par(affiliations_text, style = "Normal")
    
    print(doc_affiliations, target = "authors_ordered.docx")
    
    # Process data for contributions
    generate_initials <- function(name) {
      initials <- unlist(strsplit(name, " "))
      initials <- toupper(substr(initials, 1, 1))
      paste(initials, collapse = "")
    }
    
    authors_data_with_initials <- authors_data() %>%
      mutate(initials = sapply(name, generate_initials))
    
    categories <- c("Sample processing, quality control and administration", "Data generation", "Data freeze", "Data interpretation", "Data analysis",
                    "Figures", "Manuscript writing", "Supervision", "Funding acquisition")
    
    doc_contributions <- read_docx()
    
    for (category in categories) {
      category_column <- category
      category_authors <- authors_data_with_initials %>%
        filter(.data[[category_column]] == "x")
      
      doc_contributions <- doc_contributions %>%
        body_add_par(category, style = "heading 1")
      
      if (nrow(category_authors) > 0) {
        authors_text <- sapply(1:nrow(category_authors), function(i) {
          author <- category_authors[i, ]
          author$initials
        })
        
        authors_text <- paste(authors_text, collapse = ", ")
        doc_contributions <- doc_contributions %>%
          body_add_par(authors_text, style = "Normal")
      } else {
        doc_contributions <- doc_contributions %>%
          body_add_par("No authors assigned to this category", style = "Normal")
      }
    }
    
    print(doc_contributions, target = "authors_by_category.docx")
    
    output$confirmText <- renderText("The authors' names and affiliations have been written to authors_ordered.docx and authors_by_category.docx")
  })
  
  output$downloadAffiliations <- downloadHandler(
    filename = function() { "authors_ordered.docx" },
    content = function(file) {
      file.copy("authors_ordered.docx", file)
    }
  )
  
  output$downloadContributions <- downloadHandler(
    filename = function() { "authors_by_category.docx" },
    content = function(file) {
      file.copy("authors_by_category.docx", file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
