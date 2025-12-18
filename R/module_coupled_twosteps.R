
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_coupled_two_fit_ui <- function(id) {
    
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
            bs4TabCard(
                id = NS(id, "tabbox"),
                width = 6,
                type = "tabs",
                side = "right",
                title = "Input",
                status = "primary",
                solidHeader = FALSE,
                
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
                solidHeader = FALSE,
                title = "",
                status = "primary",
                width = 6,
                plotOutput(NS(id, "plot"))
            )
        ),
        fluidRow(
            bs4Card(
                title = "Model definition",
                status = "primary",
                width = 6,
                footer = tagList(
                    actionButton(NS(id, "fit_model"), "Fit model",
                                 outline = TRUE, flat = FALSE,
                                 status = "primary")
                ),
                fluidRow(
                    column(width = 6,
                           numericInput(NS(id, "b_guess"), "b (h^-1)", 5e-2, step = 0.01, min = 0)
                    ),
                    column(width = 6,
                           awesomeCheckbox(NS(id, "b_fixed"), "Fixed?", FALSE)
                    )
                ),
                fluidRow(
                    column(width = 6,
                           numericInput(NS(id, "Tmin_guess"), "Tmin (ºC)", 5)
                    ),
                    column(width = 6,
                           awesomeCheckbox(NS(id, "Tmin_fixed"), "Fixed?", FALSE)
                    )
                ),
                fluidRow(
                    column(width = 6,
                           numericInput(NS(id, "logC0_guess"), "logC0 (·)", -4)
                    ),
                    column(width = 6,
                           awesomeCheckbox(NS(id, "logC0_fixed"), "Fixed?", FALSE)
                    )
                )
            ),
            bs4Card(
                width = 6,
                title = "Initial guess",
                plotOutput(NS(id, "plot_guess"))
            )
        ),
        fluidRow(
            bs4Card(
                width = 6,
                title = "Fitted curve",
                status = "success",
                maximizable = TRUE,
                dropdownMenu = boxDropdown(
                    boxDropdownItem(
                        textInput(NS(id, "xlabel"), "x-label", "Time (h)")
                    ),
                    boxDropdownItem(
                        textInput(NS(id, "ylabel"), "y-label", "Microbial concentration (log CFU/g)")
                    )
                ),
                footer = tagList(
                    fluidRow(
                        column(6, align = "right",
                               actionButton(NS(id, "download_pred_plot"), "",
                                            icon = icon("download"),
                                            outline = TRUE,
                                            status = "success",
                                            flat = FALSE
                               )
                        )
                    )
                ),
                plotlyOutput(NS(id, "fitted_curve"))
            ),
            bs4Card(
                width = 6,
                title = "Statistical summary",
                status = "success",
                verbatimTextOutput(NS(id, "result_summary"))
                
            )
          
        )
    )
    
}

## Server ----------------------------------------------------------------------

module_coupled_two_fit_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Help page -----------------------------------------------------------
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Fitting the Baranyi-Ratkowsky model in a two-steps considering coupling", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p(
                                    paste(
                                        "This module is intended to fit the Baranyi primary model with the suboptimal Ratkowsky secondary model.",
                                        "The module applies the approach by Garre et al. (2025; 10.1016/j.ijfoodmicro.2025.111078). Accordingly,",
                                        "the secondary model for lambda is an inverse square root model. Furthermore, the coupling",
                                        "between both model parameters is incorporated, so only 3 parameters are fitted",
                                        "(Tmin, b and logC0)."
                                    )
                                ),
                                p(
                                    paste(
                                        "The module uses a two-steps approach Therefore, it expects that the primary models have already been fitted.",
                                        "A one-step approach (where the secondary models are fitted directly from logN)",
                                        "is available in a different module."
                                    )
                                ),
                                h3("Uploading data"),
                                p(
                                    "The data can be uploaded as an Excel file, a text file or manually using biogrowth.",
                                    "When uploading the data as an Excel file,"
                                    
                                ),
                                tags$li("The data must be included within a single Excel sheet."),
                                tags$li("The Excel sheet cannot have empty rows at the beginning."),
                                tags$li("The sheet must have three columns: 'temp' with the storage temperature"),
                                tags$li("And 'mu' with the estimate of the specific growth rate (in ln scale)."),
                                tags$li("And 'lambda' with the estimate of the lag phase duration."),
                                p(
                                    tagList(
                                        downloadLink(NS(id, "downloadData"), "Download an example"),
                                        "of a typical dataset."
                                    )
                                ),
                                h3("Initial guesses vs parameter estimates"),
                                p(
                                    paste(
                                        "Models are fitted by non-linear regression. This is an iterative algorithm",
                                        "that requires initial guesses for every model parameter.",
                                        "It is important to emphasize that these values are just a need for the algorithm.",
                                        "The algorithm should always converge to the same model, provided that reasonable guesses are defined.",
                                        "They are defined in the 'Model definition' box.",
                                        "The module includes two tools to help you define 'reasonable' guesses.",
                                        "The box 'Initial guess' compares the growth curve corresponding to the initial guess",
                                        "against the experimental data. This way, you can check that the growth curve is",
                                        "'close enough' to the data. The module also includes an 'Automagic'",
                                        "button that uses some heuristic rules to get reasonable estimates.",
                                        "Regardless of what method is used, it is generally recommended to repeat the fits",
                                        "for different guesses, checking that the algorithm always converges to the same model."
                                    )
                                )
                            )
                )
            )
            
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                "example_coupled_twosteps.xlsx"
            },
            content = function(file) {
                file.copy("example_coupled_twosteps.xlsx", file)
            }
        )
        
        ## Loading the data ----------------------------------------------------
        
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
                DF <- hot_to_r(input$hot)
            } else {
                data("example_coupled_twosteps")
                DF <- example_coupled_twosteps 
            }
            
            DF %>%
                set_names(c("temp", "mu", "lambda")) %>%
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
            
            if (any(names(d) != c("temp", "mu", "lambda"))) {
                showModal(
                    modalDialog(title = "Error loading the data",
                                "The columns must be named:",
                                paste(c("temp", "mu", "lambda"), collapse = ";")
                    )
                )
                
                safeError("Error loading the data")
            }
            
            ## Return
            
            d
            
        })
        
        output$plot <- renderPlot({
            
            validate(need(my_data(), message = ""))
            
            my_data() %>%
                pivot_longer(-temp) %>%
                ggplot() +
                geom_point(aes(x = temp, y = value)) +
                facet_wrap("name", scales = "free")
            
        })
        
        ## Initial guess
        
        output$plot_guess <- renderPlot({
            
            validate(need(my_data(), message = ""))
            
            p <- c(logC0 = input$logC0_guess, b = input$b_guess, Tmin = input$Tmin_guess)
            
            tibble(temp = seq(min(my_data()$temp, na.rm = TRUE),
                              max(my_data()$temp, na.rm = TRUE),
                              length = 100)
                   ) %>%
                mutate(sqmu = biogrowth:::pred_sqmu(p, temp),
                       mu = sqmu^2,
                       lambda = biogrowth:::pred_lambda(p, temp)) %>%
                select(-sqmu) %>%
                pivot_longer(-temp) %>%
                ggplot() +
                geom_line(aes(x = temp, y = value)) +
                geom_point(aes(x = temp, y = value),
                           data = my_data() %>% pivot_longer(-temp)
                           ) +
                facet_wrap("name", scales = "free")
            
        })
        
        ## Model fitting
        
        my_fit <- eventReactive(input$fit_model, {
            
            validate(need(my_data(), message = ""))
            
            ## Get the parameters
            
            guess <- c()
            known <- c()

            if (input$logC0_fixed) {
                known <- c(known, logC0 = input$logC0_guess)
            } else {
                guess <- c(guess, logC0 = input$logC0_guess)
            }
            
            if (input$b_fixed) {
                known <- c(known, b = input$b_guess)
            } else {
                guess <- c(guess, b = input$b_guess)
            }
            
            if (input$Tmin_fixed) {
                known <- c(known, Tmin = input$Tmin_guess)
            } else {
                guess <- c(guess, Tmin = input$Tmin_guess)
            }
            
            ## Fit the model
            
            fit_coupled_growth(my_data(),
                               start = guess,
                               known = known)
        })
        
        ## Output of the fits
        
        output$fitted_curve <- renderPlotly({
            
            validate(need(my_fit(), message = ""))
            
            p <- plot(my_fit(), label_x = input$xlabel, label_y = input$ylabel)
            
            ggplotly(p)
        })
        
        output$result_summary <- renderPrint({
            
            validate(need(my_fit(), message = ""))
            
            summary(my_fit())
            
        })
        
        ## Download the plot
        
        
        observeEvent(input$download_pred_plot, {
            
            showModal(
                session = session,
                dataModal()
            )
            
        })
        
        output$download_sim_text <- downloadHandler(
            filename = "biogrowth_curves.csv",
            content = function(file) {
                
                summary(my_fit())$par %>%
                    as_tibble(rownames = "parameter") %>%
                    write_delim(.,
                                delim = input$delim_sim_download,
                                file = file)
                
            }
        )
        
        output$download_sim_png <- downloadHandler(
            filename = "biogrowth_curves.png",
            content = function(file){
                
                my_theme <- switch(input$theme_sim_png,
                                   grey = theme_grey,
                                   bw = theme_bw,
                                   linedraw = theme_linedraw,
                                   light = theme_light,
                                   dark = theme_dark,
                                   minimal = theme_minimal,
                                   classic = theme_classic,
                                   test = theme_test,
                                   base = theme_base,
                                   calc = theme_calc,
                                   economist = theme_economist,
                                   excel = theme_excel,
                                   few = theme_few,
                                   fivethirtyeight = theme_fivethirtyeight,
                                   gdocs = theme_gdocs,
                                   hc = theme_hc,
                                   pander = theme_pander,
                                   solarized = theme_solarized,
                                   stata = theme_stata,
                                   tufte = theme_tufte,
                                   wsj = theme_wsj,
                )
                
                p <- plot(my_fit(), label_x = input$xlabel, label_y = input$ylabel) +
                    my_theme(base_size = input$baseSize_sim_png)
                
                ggsave(file, plot=p, device = "png",
                       width = input$width_sim_png,
                       height = input$height_sim_png,
                       units = "px",
                       bg = "white"
                )
            }
        )
        
        dataModal <- function() {
            modalDialog(
                easyClose = TRUE,
                fluidRow(
                    column(6,  # Text file 
                           # h3("Download as text"),
                           downloadBttn(NS(id, "download_sim_text"), "Download as CSV",
                                        style = "bordered",
                                        color = "primary",
                                        block = TRUE),
                           br(),
                           radioButtons(NS(id, "delim_sim_download"), "Separator",
                                        choices = list(`tabulation` = "\t",
                                                       `comma (,)` = ",",
                                                       `semicolon (;)` = ";" 
                                        ))
                    ),
                    column(6,  # Figure
                           # h3("Download as figure"),
                           downloadBttn(NS(id, "download_sim_png"), "Download as PNG",
                                        style = "bordered",
                                        color = "primary",
                                        block = TRUE),
                           br(),
                           numericInput(NS(id, "width_sim_png"), "Width (pixels)", 900),
                           numericInput(NS(id, "height_sim_png"), "height (pixels)", 600),
                           pickerInput(NS(id, "legendPos_sim_png"), "Legend position",
                                       choices = c("top", "right", "left", "bottom", "none")),
                           textInput(NS(id, "legendTitle_sim_png"), "Legend title", "model"),
                           numericInput(NS(id, "baseSize_sim_png"), "Base size", 7),
                           selectInput(NS(id, "theme_sim_png"), "Theme",
                                       choices = c(
                                           "grey",
                                           "bw",
                                           "linedraw",
                                           "light",
                                           "dark",
                                           "minimal",
                                           "classic",
                                           "base",
                                           "calc",
                                           "economist",
                                           "excel",
                                           "few",
                                           "fivethirtyeight",
                                           "gdocs",
                                           "hc",
                                           "pander",
                                           "solarized",
                                           "stata",
                                           "tufte",
                                           "wsj"
                                       ))
                    )
                )
            )
        }
        
    })
    
}

## test ------------------------------------------------------------------------

test_module_coupled_two_fit <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_coupled_two_fit_ui("test")
            )
        ),
        server = function(input, output) {
            module_coupled_two_fit_server("test")
        }
    )
    
}







