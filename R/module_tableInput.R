
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(rhandsontable)
library(readxl)
library(tidyverse)

## UI --------------------------------------------------------------------------

tableInput_module_ui <- function(id, box_title = "", status = NULL,
                                 background = NULL,
                                 solid = FALSE,
                                 solid_2 = FALSE,
                                 gradient = FALSE,
                                 title_2 = "",
                                 status_2 = NULL,
                                 bttn_style = "material-flat",
                                 collapsed = FALSE,
                                 collapsed_2 = FALSE
) {
    
    tagList(
        fluidRow(
            bs4TabCard(
                collapsed = collapsed,
                id = NS(id, "tabbox"),
                width = 6,
                type = "tabs",
                side = "right",
                title = box_title,
                status = status,
                background = background,
                solidHeader = solid,
                gradient = gradient,
                
                # footer = actionBttn(NS(id, "update"), "Load", style = bttn_style),
                footer = actionButton(NS(id, "update"), "Load",
                                      outline = TRUE, flat = FALSE,
                                      status = "primary"
                ),
                
                bs4TabItem(
                    title = "Excel",
                    tabName = "excel",
                    fileInput(NS(id, "excel_file"), "Excel file"),
                    # textInput(NS(id, "excel_sheet"), "Sheet name", "Sheet1"),
                    selectInput(NS(id, "excel_sheet"), "Sheet name", choices = c()),
                    numericInput(NS(id, "excel_skip"), "Skip", 0)
                ),
                bs4TabItem(
                    title = "Text",
                    tabName = "text",
                    fileInput(NS(id, "file"), "CSV file"),
                    radioButtons(NS(id, "sep"), "Separator",
                                 c(Comma = ",", Semicolon = ";", Tab = "\t"), "\t"),
                    radioButtons(NS(id, "dec"), "Decimal Point",
                                 c(Point = ".", Comma = ","), ".")
                ),
                bs4TabItem(
                    title = "Manual",
                    tabName = "manual",
                    rHandsontableOutput(NS(id, "hot"))
                )
            ),
            bs4Card(
                collapsed = collapsed_2,
                solidHeader = solid_2,
                title = title_2,
                status = status_2,
                width = 6,
                plotOutput(NS(id, "plot"))
            )
        )
    )
    
}

## Server ----------------------------------------------------------------------

tableInput_module_server <- function(id,
                                     col_names = c("time", "logN"),
                                     default_data = data.frame(time = c(0, 1, 2),
                                                               logN = c(6, 5, 4)),
                                     xvar = "time", yvar = "logN", colvar = NULL,
                                     add_points = TRUE, add_lines = FALSE
) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Excel -------------------------------------------------------------------
        
        excelFile <- reactive({
            # validate(need(input$excel_file, label = "Excel"))
            input$excel_file
        })
        
        observeEvent(excelFile(), {  # Update the choices
            
            validate(need(excelFile(), message = ""))
            updateSelectInput(session = session,
                              inputId = "excel_sheet",
                              choices = excel_sheets(excelFile()$datapath) 
            )
            
        })
        
        excel_frame <- reactive({
            read_excel(excelFile()$datapath,
                       sheet = input$excel_sheet,
                       skip = input$excel_skip,
                       col_types = "numeric")
        })
        
        ## Text --------------------------------------------------------------------
        
        userFile <- reactive({
            # validate(need(input$file, label = "Text"))
            input$file
        })
        
        file_frame <- reactive({
            read.table(userFile()$datapath,
                       header = TRUE,
                       sep = input$sep,
                       dec = input$dec,
                       stringsAsFactors = FALSE)
        })
        
        ## Manual ------------------------------------------------------------------
        
        output$hot = renderRHandsontable({
            if (!is.null(input$hot)) {
                # browser()
                DF <- hot_to_r(input$hot)
            } else {
                DF <- default_data
            }
            
            DF %>%
                set_names(col_names) %>%
                rhandsontable() %>%
                hot_table(highlightCol = TRUE, highlightRow = TRUE)
        })
        
        ## Output ------------------------------------------------------------------
        
        my_data <- eventReactive(input$update, {
            
            ## Get the data from the right tab
            
            d <- switch(input$tabbox,
                        Excel = excel_frame(),
                        Text = file_frame(),
                        Manual = hot_to_r(input$hot)
            )
            
            ## Check the columns are right
            
            # print(names(d))
            # print(col_names)
            #
            # print(any(names(d) =! col_names))
            
            if (any(names(d) != col_names)) {
                showModal(
                    modalDialog(title = "Error loading the data",
                                "The columns must be named:",
                                paste(col_names, collapse = ";")
                    )
                )
                
                safeError("Error loading the data")
            }
            
            
            ## Return
            
            d
            
            
        })
        
        output$plot <- renderPlot({
            
            p <- ggplot()
            
            if (isTRUE(add_points)) {
                p <- p + geom_point(aes_string(x = xvar, y = yvar, colour = colvar),
                                    data = my_data()) +
                    scale_color_binned(type = "viridis")
            }
            
            if (isTRUE(add_lines)) {
                p <- p + geom_line(aes_string(x = xvar, y = yvar, colour = colvar),
                                   data = my_data()) +
                    scale_color_binned(type = "viridis")
            }
            
            p
            
        })
        
        my_data
        
    })
    
}

## test ------------------------------------------------------------------------

tableInput_module_test <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                tableInput_module_ui("test", box_title = "Input data"),
                fluidRow(
                    tableOutput("plot2")
                )
            )
        ),
        server = function(input, output) {
            out <- tableInput_module_server("test",
                                            col_names = c("time", "logN", "temperature"),
                                            default_data = data.frame(time = c(0, 1, 2),
                                                                      logN = c(6, 4, 3),
                                                                      temperature = c(20, 3, 5)),
                                            xvar = "time", yvar = "logN", colvar = "temperature"
            )
            
            output$plot2 <- renderTable(out())
        }
    )
    
}




