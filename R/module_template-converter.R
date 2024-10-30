
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_templates_ui <- function(id) {
    
    tagList(
        fluidRow(
            column(12, align = "right",
                   actionBttn(NS(id, "show_help"), "", 
                              icon = icon("info"),
                              style = "simple",
                              color = "success")
            )
            
        ),
        br(),
        fluidRow(
          tags$h3("Plate reader XX")  
        ),
        fluidRow(
            bs4Card(
                title = "Output of the plate reader",
                status = "primary",
                footer = downloadLink(NS(id, "plateReader_input_example"), "Download example"),
                
                fileInput(NS(id, "file_plate_reader"), "Excel file"),
                selectInput(NS(id, "sheet_plate_reader"), "Sheet name", choices = c()),
                # numericInput(NS(id, "plate_reader_skip"), "Skip", 0)
                textInput(NS(id, "range_plate_reader"), label = "Range", value = "A64:EP162")
            ),
            bs4Card(
                title = "",
                status = "primary",
                plotOutput(NS(id, "plot_plateReader"))
            )
        ),
        fluidRow(
            bs4Card(
                title = "Data schema",
                status = "primary",
                footer = downloadLink(NS(id, "plateReader_template_example"), "Download template"),
                
                fileInput(NS(id, "file_template_plate_reader"), "Excel file"),
                selectInput(NS(id, "sheet_template_plate_reader"), "Sheet name", choices = c())
            ),
            bs4Card(
                title = "Template loaded",
                status = "primary",
                tableOutput(NS(id, "schema_plateReader"))
            )
        ),
        fluidRow(
            bs4Card(
                title = "Conversion",
                checkboxInput(NS(id, "convert_units"), "Convert time units?", value = TRUE),
                conditionalPanel(
                    ns = NS(id),
                    condition = "input.convert_units",
                    selectInput(NS(id, "conversion_factor"),
                                "",
                                choices = c(`Seconds to minutes` = 1/60,
                                               `Seconds to hours` = 1/60/60,
                                               `Seconds to days` = 1/60/60/24,
                                               
                                               `Minutes to seconds` = 60,
                                               `Minutes to hours` = 1/60,
                                               `Minutes to days` = 1/60/24,
                                               
                                               `Hours to seconds` = 60*60,
                                               `Hours to minutes` = 60,
                                               `Hours to days` = 24
                                               
                                               ),
                                selected = 1
                                )
                ),
                actionButton(NS(id, "make_output_plateReader"), "Convert"),
                footer = downloadButton(NS(id, "plateReader_output_download"))
            ),
            bs4TabCard(
                title = "Output preview",
                tabPanel(
                    title = "Table view",
                    DTOutput(NS(id, "preview_table"))
                ),
                tabPanel(
                    title = "Plot",
                    plotOutput(NS(id, "preview_plot"))
                )
            )
        )
        
    )
    
}

## Server ----------------------------------------------------------------------

module_templates_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Help page -----------------------------------------------------------
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Tools to support growth calculations", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p("This module includes functions to convert the output of various tools to the serial-dilution template. Namely:"),
                                tags$li("AA")
                            )
                )
            )
            
        })
        
        ## Plate reader XX -----------------------------------------------------
        
        ### Example template
        
        output$plateReader_template_example <- downloadHandler(
            filename = function() {
                "template_platereader.xlsx"
            },
            content = function(file) {
                file.copy("template_platereader.xlsx", file)
            }
        )
        
        ### Example input
        
        output$plateReader_input_example <- downloadHandler(
            filename = function() {
                "input_platereader.xlsx"
            },
            content = function(file) {
                file.copy("input_platereader.xlsx", file)
            }
        )
        
        ## Plate reader input --------------------------------------------------
        
        plateReaderFile <- reactive({
            input$file_plate_reader
        })
        
        observeEvent(plateReaderFile(), {  # Update the choices
            
            validate(need(plateReaderFile(), message = ""))
            
            updateSelectInput(session = session,
                              inputId = "sheet_plate_reader",
                              choices = excel_sheets(plateReaderFile()$datapath)
            )
            
        })
        
        plateReader_data <- reactive({
            
            validate(need(plateReaderFile(), message = ""))
            
            ## Read the input file
            
            d <- read_excel(plateReaderFile()$datapath, 
                            range = input$range_plate_reader,
                            sheet = input$sheet_plate_reader,
                            col_names = FALSE
                            )
            
            ## Convert it to a convenient format
            
            out <- d[,-1] %>% t() %>% as_tibble()
            out <- out %>% 
                set_names(d$...1) %>% 
                select(time = `Time [s]`, everything(), -`Cycle Nr.`, -`Temp. [°C]`)
            
            ## Return
            
            out
            
        })
        
        output$plot_plateReader <- renderPlot({
            
            validate(need(plateReaderFile(), message = ""))
            
            plateReader_data() %>%
                pivot_longer(-time, names_to = "well") %>%
                ggplot(aes(x = time, y = value, colour = well)) +
                geom_line() +
                ylab("OD")
            
        })
        
        ## Plate reader template -----------------------------------------------
        
        plateReaderTemplateFile <- reactive({
            input$file_template_plate_reader
        })
        
        observeEvent(plateReaderTemplateFile(), {  # Update the choices
            
            validate(need(plateReaderTemplateFile(), message = ""))
            
            updateSelectInput(session = session,
                              inputId = "sheet_template_plate_reader",
                              choices = excel_sheets(plateReaderTemplateFile()$datapath)
            )
            
        })
        
        plateReader_template <- reactive({
            
            validate(need(plateReaderTemplateFile(), message = ""))
            
            ## Read the template
            
            read_excel(plateReaderTemplateFile()$datapath, 
                            sheet = input$sheet_template_plate_reader
                       ) %>%
                set_names(c("condition", "direction", "first_letter", "last_letter", "first_number", "last_number"))

        })
        
        output$schema_plateReader <- renderTable({
            
            plateReader_template()
            
        })
        
        ## Reactive for saving the converted data  -----------------------------
        
        converted_data <- reactiveVal()
        
        ## Convert the template ------------------------------------------------

        observeEvent(input$make_output_plateReader, {
            
            validate(need(plateReaderTemplateFile(), message = ""))
            validate(need(plateReaderFile(), message = ""))
            
            ## Get the reactives for easy access
            
            d <- plateReader_data()
            t <- plateReader_template()
            
            ## Loop through each rough of the template

            legend <- lapply(1:nrow(t), function(i) {
                
                this_data <- t[i,]
                
                ## Make a map, assigning an index to each letter and number
                
                letter_map <- tibble(l = LETTERS[which(this_data$first_letter == LETTERS):which(this_data$last_letter == LETTERS)]) %>%
                    mutate(l_index = row_number())
                
                number_map <- tibble(n = this_data$first_number:this_data$last_number) %>%
                    mutate(n_index = row_number())
                
                out <- expand.grid(l = letter_map$l, n = number_map$n) %>%
                    mutate(column = paste0(l, n)) %>%
                    left_join(letter_map) %>%
                    left_join(number_map)
                
                ## Define the column names

                if (this_data$direction == "letters") {  # Letters define the number of dilutions
                    
                    out <- out %>%
                        mutate(new_col = paste0(this_data$condition,
                                                "/R",
                                                n_index,  # the number is considered as repetition n
                                                "_",
                                                l_index - 1  # -1 to account for 0 dilution
                                                )
                               )
                    
                } else if (this_data$direction == "numbers") {
                    
                    out <- out %>%
                        mutate(new_col = paste0(this_data$condition,
                                                "/R",
                                                l_index,  # the letter is considered as repetition n
                                                "_",
                                                n_index - 1  # -1 to account for 0 dilution
                                                )
                        )
                    
                } else {
                    stop("Wrong direction. Must be 'letters' or 'numbers'")
                }
                
                out
            }) %>%
                bind_rows() %>%
                select(column, new_col)
            
            ## Take only columns that appear in the map
            
            d_renamed <- d %>%
                select(t = time, matches(legend$column))
            
            ## Change the column names according to the legend
            
            d_renamed <- tibble(column = names(d_renamed)) %>%
                left_join(legend) %>%
                mutate(new_col = ifelse(column == "t", "t", new_col)) %>%  # the time is not in template
                pull(new_col) %>%
                set_names(d_renamed, .)
            
            ## Check if we need to convert units
            
            if (input$convert_units) {
                
                d_renamed <- d_renamed %>%
                    mutate(t = t * as.numeric(input$conversion_factor))
            }
            
            ## Update the reactive
            
            converted_data(d_renamed)
            
        })
        
        ## Preview of the output
        
        output$preview_table <- renderDT({
            
            validate(need(converted_data(), message = ""))
            
            converted_data()

        })
        
        output$preview_plot <- renderPlot({
            
            validate(need(converted_data(), message = ""))
            
            converted_data() %>%
                pivot_longer(-t, names_to = "well") %>%
                ggplot() + 
                geom_line(aes(x = t, y = value, colour = well))
            
        })
        
        ## Downloading the file
        
        output$plateReader_output_download <- downloadHandler(
            filename = function() {
                paste0("data_for_od_analysis", ".csv")
            },
            content = function(file) {
                
                validate(need(converted_data(), message = ""))
                
                write_tsv(converted_data(), file = file)
            }
        )
        
        

        
    })
}


## test ------------------------------------------------------------------------

test_module_templates <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_templates_ui("test")
            )
        ),
        server = function(input, output) {
            module_templates_server("test")
        }
    )
    
}





















