#Project name: PTO Calculation
#Purpose: Overview PTO
#Control: Current PTO balance, Accrued hrs per month, Yearly carry over limit, Target PTO
#Presentation: Line graph: X: Timeline Y:PTO hr, Horizontal line to set target and show intersection date

library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(scales)
library(datasets)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "PTO Calculator"),
  
  dashboardSidebar(
    #Input() functions
    numericInput(
      inputId = "EOYMT",
      label = "Last Month of Fiscal Year",
      value = 12,
      min = NA,
      max = NA,
      step = NA,
      width = NULL
    ),
    numericInput(
      inputId = "MXHR",
      label = "Max PTO All Time",
      value = 200,
      min = 0,
      max = NA,
      step = NA,
      width = NULL
    ),
    numericInput(
      inputId = "MXCR",
      label = "Max PTO Carry Over",
      value = 160,
      min = 0,
      max = NA,
      step = NA,
      width = NULL
    ),
    numericInput(
      inputId = "HrBL",
      label = "Hours Balance",
      value = 50,
      min = NA,
      max = NA,
      step = NA,
      width = NULL
    ),
    numericInput(
      inputId = "NoPD",
      label = "Pay Period No in a Year",
      value = 24,
      min = 0,
      max = NA,
      step = NA,
      width = NULL
    ),
    numericInput(
      inputId = "HrPD",
      label = "Hours Acrrue per Pay Period",
      value = 5,
      min = 0,
      max = NA,
      step = NA,
      width = NULL
    ),
    numericInput(
      inputId = "YrFT",
      label = "Year(s) Forecast",
      value = 1,
      min = 0,
      max = NA,
      step = NA,
      width = NULL
    ),
    numericInput(
      inputId = "TGHR",
      label = "Target PTO Hours",
      value = 150,
      min = 0,
      max = NA,
      step = NA,
      width = NULL
    )
  ),
  
  dashboardBody(
    #Output() functions
    plotOutput("Hist", click = "plot_click"),
    verbatimTextOutput("info"),
    dataTableOutput("table")
    
  )
  
)

server <- function(input, output) {
  PTO <- reactive({
    data_frame(
      "Date" = (seq.Date(
        as.Date(cut(Sys.Date(), "month")),
        by = "month",
        length.out = (input$YrFT * 12)
      ))
      ,
      "PTOHrs" = (seq(
        input$HrBL,
        by = (input$HrPD * input$NoPD) / 12,
        length.out = (input$YrFT * 12)
      ))
      ,
      "MaxPTO" = (seq(
        input$MXHR,
        by = 0,
        length.out = (input$YrFT * 12)
      ))
      ,
      "MaxCarry" = (seq(
        input$MXCR,
        by = 0,
        length.out = (input$YrFT * 12)
      ))
    )
  })
  
  output$Hist <- renderPlot({
    ggplot(data = PTO(), aes(
      x = PTO()$Date,
      y = PTO()$PTOHrs,
      group = 1
    )) +
      geom_line(size = 1, color = "#009E73") +
      geom_hline(
        yintercept = input$TGHR,
        size = 1,
        color = "#0072B2",
        linetype = 3
      ) +
      geom_hline(
        yintercept = input$MXHR,
        size = 1,
        color = "#E69F00",
        linetype = 3
      ) +
      geom_hline(
        yintercept = input$MXCR,
        size = 1,
        color = "#D55E00",
        linetype = 3
      ) +
      geom_vline(
        xintercept = (seq.Date(
          as.Date(cut(as.Date(
            paste(as.numeric(format(
              Sys.Date(), "%Y"
            )) , input$EOYMT, 1, sep = "/")
          ), "month")),
          by = "year",
          length.out = (input$YrFT * 2)
        )),
        size = 1,
        color = "#800080",
        linetype = 4
      ) +
      geom_point(size = 3, color = "#009E73") +
      geom_text(aes(
        x = as.Date(Sys.Date()),
        y = input$TGHR + 5,
        label = "Target"
      ), hjust = 0) +
      geom_text(aes(
        x = as.Date(Sys.Date()),
        y = input$MXHR + 5,
        label = "Max Hours"
      ), hjust = 0) +
      geom_text(aes(
        x = as.Date(Sys.Date()),
        y = input$MXCR + 5,
        label = "Max Hours Carry Over"
      ),
      hjust = 0) +
      # geom_text(aes(
      #   x = (seq.Date(
      #     as.Date(cut(as.Date(
      #       paste(as.numeric(format(
      #         Sys.Date(), "%Y"
      #       )) + 1, input$EOYMT, 1, sep = "/")
      #     ), "year")),
      #     by = "year",
      #     length.out = (input$YrFT * 2)
      #   )),
      #   y = 0,
    #   label = "Year End"
    # ),
    # angle = 90,
    # hjust = 0) +
    scale_y_continuous(
      name = "PTO Hours",
      breaks = (seq(
        0,
        by = 20,
        length.out = ((input$MXHR/20)+1)
      )),
      limits = c(0, input$MXHR + 10)
    ) +
      scale_x_date(
        name = "Time Line",
        breaks = date_breaks("months"),
        labels = date_format("%b %y")
      ) +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$info <- renderText({
    paste0("Please click on the plot for coordinate",
      "\nDate : ",
      as.Date("1970-01-01") + input$plot_click$x,
      "\nPTO Balance : ",
      input$plot_click$y
    )
  })
  
  output$table <- renderDataTable(datatable(
    PTO()[1:4],
    options = list(
      rowCallback = JS(
        '
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Bold and green cells for conditions
        if (parseFloat(aData[2]) >= aData[3])
        $("td:eq(2)", nRow).css("font-weight", "bold");
        if (parseFloat(aData[2]) >= aData[4])
        $("td:eq(2)", nRow).css("background-color", "#FFA07A");
        }'
),
pageLength = 24,
orderClasses = TRUE,
searching = FALSE
      )
    ))
  
  }

shinyApp(ui = ui, server = server)
