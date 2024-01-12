library(shiny)
library(shinythemes)
library(readxl)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Claims Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("claims_file", "Import Claims File:", accept = c(".xlsx")),
      sliderInput("tail_factor", "Tail Factor:",
                  min = 0.5, max = 2.0, value = 1.1, step = 0.01, ticks = TRUE),
      tags$hr(),
      tags$p("Adjust the tail factor to modify the projection of claims."),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative Paid Claims Plot",
                 fluidRow(
                   column(12,
                          plotOutput("plot", width = "100%"),
                          downloadButton("download_plot", "Download Plot", class = "btn-primary")
                   )
                 )
        ),
        tabPanel("Cumulative Paid Claims Table",
                 fluidRow(
                   column(12,
                          dataTableOutput("proj_table"),
                          downloadButton("download_table", "Download Table", class = "btn-primary")
                   )
                 )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive expression for loss year
  loss_year <- reactive({
    req(input$claims_file)
    claims_table = read_xlsx(input$claims_file$datapath)
    colnames(claims_table) <- c("Loss Year", "Development Year", "Amount of Claims Paid ($)")
    unique(claims_table$'Loss Year')
  })
  
  # Reactive expression for data processing
  data <- reactive({
    req(input$claims_file)
    claims_table = read_xlsx(input$claims_file$datapath)
    colnames(claims_table) <- c("Loss Year", "Development Year", "Amount of Claims Paid ($)")
    
    # Data processing steps
    paid_claims_table = matrix(0, nrow = length(loss_year()), ncol = length(loss_year()) + 1)
    cumulative_claims_table = matrix(0, nrow = length(loss_year()), ncol = length(loss_year()) + 1)
    
    # Extract and format amount of paid claims
    col_ind = 1
    for (i in 1:length(loss_year())) {
      for (j in 1:nrow(claims_table)) {
        if (claims_table$'Loss Year'[j] == loss_year()[i]) {
          paid_claims_table[i, col_ind] = claims_table$'Amount of Claims Paid ($)'[j]
          col_ind = col_ind + 1
        } else {
          col_ind = 1
        }
      }
    }
    
    # Calculate cumulative claims
    for (i in 1:length(loss_year())) {
      for (j in 1:length(loss_year())) {
        if (paid_claims_table[i, j] != 0) {
          if (j == 1) {
            cumulative_claims_table[i, j] = paid_claims_table[i, j]
          } else {
            cumulative_claims_table[i, j] = sum(paid_claims_table[i, 1:j])
          }
        }
      }
    }
    
    # Calculate development factor
    dev_factor_table = matrix(1, nrow = 1, ncol = length(loss_year()) + 1)
    for (i in 1:length(loss_year())) {
      dev1 = 0
      dev2 = 0
      if (i != 1) {
        dev1 = sum(cumulative_claims_table[1:(length(loss_year()) + 1 - i), i - 1])
        dev2 = sum(cumulative_claims_table[1:(length(loss_year()) + 1 - i), i])
        dev_factor_table[1, i] = dev2 / dev1
      }
    }
    
    dev_factor_table[1, length(loss_year()) + 1] = input$tail_factor
    
    # Calculate projected claims
    projected_claims_table = cumulative_claims_table
    for (i in 1:length(loss_year())) {
      for (j in 1:length(loss_year()) + 1) {
        if (projected_claims_table[i, j] == 0) {
          projected_claims_table[i, j] = projected_claims_table[i, j - 1] * dev_factor_table[j]
        }
      }
    }
    
    round_df <- function(x, digits) {
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <- round(x[numeric_columns], digits)
      x
    }
    round_df(projected_claims_table, 0)
  })
  
  # Output rendering for the table
  output$proj_table <- renderDataTable({
    proj_table = as.data.frame(data())
    
    # Rename columns of projection table
    dev_year_list = c()
    for (i in 1:(length(loss_year()) + 1)) {
      dev_year_list = append(dev_year_list, paste("DY", i, sep = ""))
    }
    proj_table = cbind(loss_year(), proj_table)
    colnames(proj_table) = c("Loss Year", dev_year_list)
    proj_table
  }, options = list(dom = 't', paging = FALSE,searching = FALSE
  ))
  
  
  # Output rendering for the plot
  output$plot <- renderPlot({
    PCT = as.data.frame(data())
    
    development_year = c()
    for (i in (1:(length(loss_year()) + 1))) {
      development_year = append(development_year, i)
    }
    PCT = rbind(development_year, PCT)
    transposedPCT = as.data.frame(t(PCT))
    colnames(transposedPCT) = c("Development Year", loss_year())
    
    p = ggplot() + labs(x = "Development Year", y = "Cumulative Claims ($)")
    for (i in 1:length(loss_year())) {
      aes = aes_string(x = transposedPCT[, 1], y = transposedPCT[,(i + 1)], color = factor(loss_year()[i]))
      p = p + geom_smooth(aes, method = "loess", se = FALSE, linewidth = 0.8) +
        geom_text(aes, label = paste(transposedPCT[,(i + 1)]), size = 3.5, vjust = -0.8, show.legend = FALSE)
    }
    p
  })
  
  # Download handlers
  output$download_table <- downloadHandler(
    filename = function() { paste("cumulative-claims-table", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(as.data.frame(data()), file, row.names = FALSE) }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("cumulative-claims-plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      png(file)
      PCT = as.data.frame(data())
      development_year = 1:(length(loss_year()) + 1)
      PCT = rbind(development_year, PCT)
      transposedPCT = as.data.frame(t(PCT))
      colnames(transposedPCT) = c("Development Year", loss_year())
      p = ggplot() + labs(x = "Development Year", y = "Cumulative Claims ($)")
      for (i in 1:length(loss_year())) {
        aes = aes_string(x = transposedPCT[, 1], y = transposedPCT[,(i + 1)], color = factor(loss_year()[i]))
        p = p + geom_smooth(aes, method = "loess", se = FALSE, linewidth = 0.8) +
          geom_text(aes, label = paste(transposedPCT[,(i + 1)]), size = 3.5, vjust = -0.8, show.legend = FALSE)
      }
      print(p)
      dev.off()
    }
  )
}

shinyApp(ui, server)