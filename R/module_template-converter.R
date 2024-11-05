
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
            selectInput(NS(id, "reader_selecter"),
                        label = "OD device",
                        choices = list(
                            `Plate reader XX` = "plate_reader",
                            `Bioscreen C` = "bioscreen"
                            )
                        )
        ),
        conditionalPanel(  # Plate reader XX -----------------------------------
            ns = NS(id),
            condition = "input.reader_selecter == 'plate_reader'",
            fluidRow(
                bs4Card(
                    title = "Output of the plate reader",
                    status = "primary",
                    footer = downloadLink(NS(id, "plateReader_input_example"), "Download example"),
                    
                    fileInput(NS(id, "file_plate_reader"), "Excel file"),
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
                bs4TabCard(
                    title = "Template loaded",
                    status = "primary",
                    tabPanel(
                        title = "Table view",
                        tableOutput(NS(id, "schema_plateReader"))
                    ),
                    tabPanel(
                        title = "plot",
                        plotOutput(NS(id, "schema_plot_plateReader"))
                    )
                    
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
        ),
        conditionalPanel(  # Bioscreen C ---------------------------------------
            ns = NS(id),
            condition = "input.reader_selecter == 'bioscreen'",
            fluidRow(
                bs4Card(
                    title = "Output of the bioscreen",
                    status = "primary",
                    footer = downloadLink(NS(id, "bioscreen_input_example"), "Download example"),
                    
                    fileInput(NS(id, "file_bioscreen"), "Excel file"),
                    radioButtons(NS(id, "sep_bioscreen"), "Separator",
                                 c(Comma = ",", Semicolon = ";", Tab = "\t"), ","),
                    radioButtons(NS(id, "dec_bioscreen"), "Decimal Point",
                                 c(Point = ".", Comma = ","), ".")
                ),
                bs4Card(
                    title = "",
                    status = "primary",
                    plotOutput(NS(id, "plot_bioscreen"))
                )
            ),
            fluidRow(
                bs4Card(
                    title = "Data schema",
                    status = "primary",
                    footer = downloadLink(NS(id, "bioscreen_template_example"), "Download template"),

                    fileInput(NS(id, "file_template_bioscreen"), "Excel file"),
                    selectInput(NS(id, "sheet_template_bioscreen"), "Sheet name", choices = c())
                ),
                # bs4Card(
                #     title = "Template loaded",
                #     status = "primary",
                #     tableOutput(NS(id, "schema_bioscreen"))
                # )
                bs4TabCard(
                    title = "Template loaded",
                    tabPanel(
                        title = "Table",
                        tableOutput(NS(id, "schema_bioscreen"))
                    ),
                    tabPanel(
                        title = "Plot",
                        plotOutput(NS(id, "schema_plot_bioscreen"))
                    )
                )
            ),
            
            fluidRow(
                bs4Card(
                    title = "Conversion",
                    selectInput(NS(id, "bioscreen_output_unit"),
                                "Output time unit",
                                choices = c("seconds", "minutes", "hours", "days"),
                                selected = "hours"
                                ),
                    # checkboxInput(NS(id, "bioscreen_convert_units"), "Convert time units?", value = TRUE),
                    # conditionalPanel(
                    #     ns = NS(id),
                    #     condition = "input.bioscreen_convert_units",
                    #     selectInput(NS(id, "bioscreen_conversion_factor"),
                    #                 "",
                    #                 choices = c(`Seconds to minutes` = 1/60,
                    #                             `Seconds to hours` = 1/60/60,
                    #                             `Seconds to days` = 1/60/60/24,
                    #                             
                    #                             `Minutes to seconds` = 60,
                    #                             `Minutes to hours` = 1/60,
                    #                             `Minutes to days` = 1/60/24,
                    #                             
                    #                             `Hours to seconds` = 60*60,
                    #                             `Hours to minutes` = 60,
                    #                             `Hours to days` = 24
                    #                             
                    #                 ),
                    #                 selected = 2
                    #     )
                    # ),
                    actionButton(NS(id, "make_output_bioscreen"), "Convert"),
                    footer = downloadButton(NS(id, "bioscreen_output_download"))
                ),
                bs4TabCard(
                    title = "Output preview",
                    tabPanel(
                        title = "Table view",
                        DTOutput(NS(id, "preview_table_bioscreen"))
                    ),
                    tabPanel(
                        title = "Plot",
                        plotOutput(NS(id, "preview_plot_bioscreen"))
                    )
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
        
        ## Plot of the template
        
        output$schema_plot_plateReader <- renderPlot({
            
            validate(need(plateReader_template(), message = ""))
            
            # browser()
            
            t <- plateReader_template()
            
            plate_width <- 14
            plate_height <- 10
            
            ## Plates
            
            p <- ggplot() + 
                geom_rect(aes(xmin = 0, 
                              xmax = plate_width,
                              ymin = 0, 
                              ymax = plate_height)
                )
            
            ## Wells
            
            well_positions <- expand_grid(x = 0:11, 
                                          y = 7:0) %>%
                mutate(label = paste0(LETTERS[y+1], x+1))
            # ) %>%
            #     mutate(label = as.character(1:100)) %>%
            #     mutate(y = ifelse(x%%2 == 0, y + .5, y),
            #            label = ifelse(str_length(label) == 1, paste0("0", label), label)
            #            # label = ifelse(label == "1", "01", label)
            #     )
            
            p <- p + 
                geom_point(aes(x = x + 2, y = y + 1),
                           data = well_positions,
                           size = 5,
                           colour = "white")
            
            ## Row labels
            
            label_positions <- tibble(x = 1, 
                                      y = 7:0,
                                      label = LETTERS[1:8])
            
            p <- p + 
                geom_text(aes(x = x, y = y + 1, label = label),
                          data = label_positions
                )
            
            ## Column labels
            
            label_positions <- tibble(y = plate_height - 1, 
                                      x = 0:11,
                                      label = 1:12)
            
            p <- p + 
                geom_text(aes(x = x + 2, y = y, label = label),
                          data = label_positions
                )
            
            ## Get the legend
            
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
                select(label = column, cond = new_col) %>%
                left_join(well_positions) %>%
                mutate(x = as.numeric(x), y = as.numeric(y)) %>%
                separate(cond, into = c("cond"), sep = "_")
            
            # browser()
            
            p <- p + 
                geom_path(aes(x = x +2 , y = y + 1, colour = cond),
                          data = legend,
                          arrow = arrow(length = unit(.1, "inches"), type = "closed")) +
                geom_point(aes(x = x +2 , y = y + 1, colour = cond),
                          data = legend
                          )
                
            
            ## Add the paths
            
            # dil_paths <- lapply(1:nrow(t), function(i) {
            #     
            #     this_data <- t[i,]
            #     
            #     
            #     if (this_data$first > this_data$last) {
            #         
            #         x <- seq(this_data$first, this_data$last, by = -this_data$direction)
            #         
            #     } else {
            #         x <- seq(this_data$first, this_data$last, by = this_data$direction)
            #     }
            #     
            #     out <- tibble(label = x
            #     ) %>%
            #         mutate(label = as.character(label)) %>%
            #         mutate(label = ifelse(str_length(label) == 1, paste0("0", label), label)) %>%
            #         left_join(., well_positions)
            #     
            #     out %>%
            #         mutate(x = x + (this_data$plate - 1)*(plate_width + 1)) %>%
            #         mutate(condition = this_data$condition)
            #     
            #     
            #     
            # }) %>%
            #     bind_rows()
            # 
            # p <- p +
            #     geom_path(aes(x = x + 1, y = y + 1, colour = condition), 
            #               arrow = arrow(length = unit(.1, "inches"), type = "closed"),
            #               data = dil_paths
            #     ) +
            #     geom_point(aes(x = x + 1, y = y + 1, colour = condition), 
            #                data = dil_paths
            #     )
            
            
            
            ## Output
            
            p + theme_void()
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
        
        ## Bioscreen -----------------------------------------------------------
        
        ### Example template
        
        output$bioscreen_template_example <- downloadHandler(
            filename = function() {
                "template_bioscreen.xlsx"
            },
            content = function(file) {
                file.copy("template_bioscreen.xlsx", file)
            }
        )
        
        ### Example input
        
        output$bioscreen_input_example <- downloadHandler(
            filename = function() {
                "input_bioscreen.csv"
            },
            content = function(file) {
                file.copy("input_bioscreen.csv", file)
            }
        )
        
        ## Bioscreen input --------------------------------------------------
        
        bioscreenFile <- reactive({
            input$file_bioscreen
        })
        
        bioscreen_data <- reactive({
            
            validate(need(bioscreenFile(), message = ""))
            
            ## Read the input file
            
            d <- read.table(file = bioscreenFile()$datapath, 
                            sep = input$sep_bioscreen, 
                            dec = input$dec_bioscreen,
                            header = TRUE
                            )
            
            ## Convert the time

            out <- d %>%
                separate(Time, into = c("h", "m", "s"), sep = ":") %>%
                mutate(t = as.numeric(h) + as.numeric(m)/60 + as.numeric(s)/60/60) %>%
                select(t, everything(), -h, -m, -s)

            out
            
        })
        
        output$plot_bioscreen <- renderPlot({
            
            validate(need(bioscreen_data(), message = ""))
            
            bioscreen_data() %>%
                pivot_longer(-t, names_to = "well") %>%
                ggplot(aes(x = t, y = value, colour = well)) +
                geom_line() +
                ylab("OD") +
                theme(legend.position = "none")
            
        })
        
        ## Bioscreen template --------------------------------------------------
        
        bioscreenTemplateFile <- reactive({
            input$file_template_bioscreen
        })
        
        observeEvent(bioscreenTemplateFile(), {  # Update the choices
            
            validate(need(bioscreenTemplateFile(), message = ""))
            
            updateSelectInput(session = session,
                              inputId = "sheet_template_bioscreen",
                              choices = excel_sheets(bioscreenTemplateFile()$datapath)
            )
            
        })
        
        bioscreen_template <- reactive({
            
            validate(need(bioscreenTemplateFile(), message = ""))
            
            ## Read the template
            
            read_excel(bioscreenTemplateFile()$datapath, 
                       sheet = input$sheet_template_bioscreen
                       ) %>%
                set_names(c("condition", "plate", "first", "direction", "last"))
            
        })
        
        output$schema_bioscreen <- renderTable({
            
            bioscreen_template()
            
        })
        
        ## Plot of the template
        
        output$schema_plot_bioscreen <- renderPlot({
            
            validate(need(bioscreenTemplateFile(), message = ""))
            
            t <- bioscreen_template()
            
            plate_width <- 11
            plate_height <- 12
            
            ## Plates
            
            p <- ggplot() + 
                geom_rect(aes(xmin = 0, 
                              xmax = plate_width,
                              ymin = 0, 
                              ymax = plate_height)
                ) + 
                geom_rect(aes(xmin = plate_width + 1, 
                              xmax = plate_width*2 + 1, 
                              ymin = 0, ymax = plate_height)
                ) 
            
            ## Plate labels
            
            p <- p + 
                geom_label(aes(x = plate_width/2, y = -.5, label = "Plate 1")) +
                geom_label(aes(x = plate_width/2 + plate_width + 1, y = -.5, label = "Plate 2"))
            
            ## Wells
            
            well_positions <- expand_grid(x = 0:9, 
                                          y = 9:0,
            ) %>%
                mutate(label = as.character(1:100)) %>%
                mutate(y = ifelse(x%%2 == 0, y + .5, y),
                       label = ifelse(str_length(label) == 1, paste0("0", label), label)
                       # label = ifelse(label == "1", "01", label)
                )
            
            p <- p + 
                geom_point(aes(x = x + 1, y = y + 1), ## Plate 1
                           data = well_positions,
                           size = 5,
                           colour = "white") +
                geom_point(aes(x = x + 1 + plate_width + 1, y = y + 1), ## Plate 2
                           data = well_positions,
                           size = 5,
                           colour = "white")
            
            ## Well labels
            
            label_positions <- tibble(x = 1:10, 
                                      y = plate_height - .5,
                                      label = as.character(1+0:9*10)) %>%
                mutate(label = ifelse(label == "1", "01", label))
            
            p <- p + 
                geom_text(aes(x = x, y = y, label = label),
                          data = label_positions
                ) + 
                geom_text(aes(x = x, y = y, label = label),
                          data = label_positions %>%
                              mutate(x = x + plate_width + 1)
                )
            
            ## Add the paths
            
            dil_paths <- lapply(1:nrow(t), function(i) {
                
                this_data <- t[i,]
                
                
                if (this_data$first > this_data$last) {
                    
                    x <- seq(this_data$first, this_data$last, by = -this_data$direction)
                    
                } else {
                    x <- seq(this_data$first, this_data$last, by = this_data$direction)
                }
                
                out <- tibble(label = x
                              ) %>%
                    mutate(label = as.character(label)) %>%
                    mutate(label = ifelse(str_length(label) == 1, paste0("0", label), label)) %>%
                    left_join(., well_positions)
                
                out %>%
                    mutate(x = x + (this_data$plate - 1)*(plate_width + 1)) %>%
                    mutate(condition = this_data$condition)
                
                
                
            }) %>%
                bind_rows()
            
            p <- p +
                geom_path(aes(x = x + 1, y = y + 1, colour = condition), 
                          arrow = arrow(length = unit(.1, "inches"), type = "closed"),
                          data = dil_paths
                ) +
                geom_point(aes(x = x + 1, y = y + 1, colour = condition), 
                           data = dil_paths
                )
            
            
            
            ## Output
            
            p + theme_void()
        })
        
        ## Reactive for saving the converted data  -----------------------------
        
        converted_data_bioscreen <- reactiveVal()
        
        ## Convert the template ------------------------------------------------
        
        observeEvent(input$make_output_bioscreen, {
            
            validate(need(bioscreenTemplateFile(), message = ""))
            validate(need(bioscreenFile(), message = ""))
            
            ## Get the reactives for easy access
            
            d <- bioscreen_data()
            t <- bioscreen_template()
            
            ## Loop through each rough of the template
            
            legend <- lapply(1:nrow(t), function(i) {
                
                this_data <- t[i,]
                
                if (this_data$first > this_data$last) {
                    
                    x <- seq(this_data$first, this_data$last, by = -this_data$direction)
                    
                } else {
                    x <- seq(this_data$first, this_data$last, by = this_data$direction)
                }
                
                
                
                tibble(
                    column = x + this_data$plate*100,
                ) %>%
                    mutate(
                        column = paste0("Well.", column),
                        new_col = paste0(this_data$condition, "_", row_number()-1)
                    )
                
                
                
            }) %>%
                bind_rows() %>%
                select(column, new_col)
            
            ## Take only columns that appear in the map
            
            d_renamed <- d %>%
                select(t, matches(legend$column))
            
            ## Change the column names according to the legend
            
            d_renamed <- tibble(column = names(d_renamed)) %>%
                left_join(legend) %>%
                mutate(new_col = ifelse(column == "t", "t", new_col)) %>%  # the time is not in template
                pull(new_col) %>%
                set_names(d_renamed, .)
            
            ## Convert the units
            
            conversion_factor <- switch(input$bioscreen_output_unit,
                                        seconds = 60*60,
                                        minutes = 60,
                                        hours = 1,
                                        days = 1/24
                                        )
            
            d_renamed <- d_renamed %>%
                mutate(t = t * conversion_factor)

            ## Update the reactive
            
            converted_data_bioscreen(d_renamed)
            
        })
        
        ## Preview of the output
        
        output$preview_table_bioscreen <- renderDT({
            
            validate(need(converted_data_bioscreen(), message = ""))
            
            converted_data_bioscreen()
            
        })
        
        output$preview_plot_bioscreen <- renderPlot({
            
            validate(need(converted_data_bioscreen(), message = ""))
            
            converted_data_bioscreen() %>%
                pivot_longer(-t, names_to = "well") %>%
                ggplot() + 
                geom_line(aes(x = t, y = value, colour = well))
            
        })
        
        ## Downloading the file
        
        output$bioscreen_output_download <- downloadHandler(
            filename = function() {
                paste0("data_for_od_analysis", ".csv")
            },
            content = function(file) {
                
                validate(need(converted_data_bioscreen(), message = ""))
                
                write_tsv(converted_data_bioscreen(), file = file)
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





















