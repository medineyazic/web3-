library(shiny)

ui <- fluidPage(
  titlePanel("Sınav Takvimi"),
  sidebarLayout(
    sidebarPanel(
      textInput("course_code", "Ders Kodu:"),
      textInput("date", "Tarih (DD-MM-YYYY):"),
      actionButton("add_exam", "Sınav Ekle"),
      actionButton("clear_exams", "Sınavları Temizle")
    ),
    mainPanel(
      uiOutput("exam_list"),
      h4("Tamamlanmış Sınavlar"),
      uiOutput("completed_exams")
    )
  )
)

server <- function(input, output, session) {
  exams <- reactiveVal(data.frame(CourseCode = character(), Date = character(), stringsAsFactors = FALSE))
  completed_exams <- reactiveVal(data.frame(CourseCode = character(), Date = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$add_exam, {
    req(input$course_code, input$date)
    new_course_code <- input$course_code
    new_date <- input$date
    
    new_exam <- data.frame(CourseCode = new_course_code, Date = new_date, stringsAsFactors = FALSE)
    updated_exams <- rbind(exams(), new_exam)
    updated_exams <- updated_exams[order(as.Date(updated_exams$Date, format = "%d-%m-%Y")), ]
    
    exams(updated_exams)
    updateTextInput(session, "course_code", value = "")
    updateTextInput(session, "date", value = "")
  })
  
  output$exam_list <- renderUI({
    req(nrow(exams()) > 0)
    exam_checkboxes <- lapply(1:nrow(exams()), function(i) {
      checkboxInput(paste0("exam_", i), label = paste(exams()$CourseCode[i], " - ", exams()$Date[i]), value = FALSE)
    })
    
    tagList(
      h4("Sınavlar"),
      tags$ul(
        lapply(exam_checkboxes, function(cb) {
          tags$li(cb)
        })
      )
    )
  })
  
  observe({
    completed <- sapply(1:nrow(exams()), function(i) {
      input[[paste0("exam_", i)]]
    })
    
    if (length(completed) > 0 && any(unlist(completed))) {
      new_completed <- exams()[completed, , drop = FALSE]
      completed_exams(rbind(completed_exams(), new_completed))
      updated_exams <- exams()[!completed, , drop = FALSE]
      exams(updated_exams)
    }
    
    output$completed_exams <- renderUI({
      if (nrow(completed_exams()) > 0) {
        tagList(
          tags$ul(
            lapply(1:nrow(completed_exams()), function(i) {
              tags$li(paste(completed_exams()$CourseCode[i], " - ", completed_exams()$Date[i]))
            })
          )
        )
      } else {
        "Tamamlanmış sınav yok."
      }
    })
  })
  
  observeEvent(input$clear_exams, {
    exams(data.frame(CourseCode = character(), Date = character(), stringsAsFactors = FALSE))
    completed_exams(data.frame(CourseCode = character(), Date = character(), stringsAsFactors = FALSE))
  })
}

shinyApp(ui = ui, server = server)

