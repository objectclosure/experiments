upload_ui <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    fileInput(
      inputId = ns(id = "upload"), 
      label = "Upload csv file", 
      accept = ".csv"
    ), 
    
    tableOutput(outputId = ns(id = "uploaded_table"))
  )
}

upload_server <- function(id) {
  moduleServer(
    id = id, 
    
    module = function(input, output, session) {
      upload <- reactive({
        file <- input$upload
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        # read first 10 rows:
        read.csv(file$datapath, nrows = 10)
      })
      
      output$uploaded_table <- renderTable({
        upload()
      })
      
      # ----return----
      return(upload)
    }
  )
}

ui <- fluidPage(
  upload_ui(id = "theui")
)

server <- function(input, output, session) {
  # call `upload_server()` inside `reactiveValues()`:
  rv_upload <- reactiveValues(
    uploaded = upload_server(id = "theui")
  )
  
  # recall `upload_server()` returns a reactive, so we observe the event 
  # `rv_upload$uploaded()` and NOT `rv_upload$upload`:
  observeEvent(rv_upload$uploaded(), {
    print(rv_upload$uploaded())
  })
}

shinyApp(ui, server)