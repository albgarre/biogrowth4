
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_gamma_fit_ui <- function(id) {
    
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
            bs4Card(
                title = "Input data",
                status = "primary",
                footer = downloadLink(NS(id, "download_example"), "Download example"),
                fileInput(NS(id, "excel_file"), "Excel file"),
                selectInput(NS(id, "excel_sheet"), "Sheet name", choices = c()),
                numericInput(NS(id, "excel_skip"), "Skip", 0)
            ),
            bs4Card(
                title = "",
                status = "primary",
                plotOutput(NS(id, "plot_input"))
            )
        ),
        fluidRow(
            bs4Card(
                title = "Initial guesses",
                status = "primary",
                hr(),
                uiOutput(NS(id, "secondary_guess")),
                fluidRow(
                    column(8,
                           numericInput(NS(id, "muopt"), "mu_opt (same unit as input)", 0.5, min = 0)
                    ),
                    column(4,
                           awesomeCheckbox(NS(id, "muopt_fix"), "Fixed?", value = FALSE)
                    )
                ),
                fluidRow(
                    column(12,
                           actionButton(NS(id, "automagic"), "Automagic",
                                        outline = TRUE, flat = FALSE,
                                        status = "secondary")
                    )
                )
            ),
            bs4Card(
                title = "Fitting algorithm",
                status = "primary",
                footer = tagList(
                    textInput(NS(id, "fit_name"), "Model name", "Model I"),
                    actionButton(NS(id, "go_fit"), "Add/Edit fit",
                                 outline = TRUE, flat = FALSE,
                                 status = "primary")
                ),
                selectInput(NS(id, "transformation"), "Transformation mu",
                            choices = list(`Square root` = "sq",
                                           `Log-transform` = "log",
                                           `None` = "none"))
                # plotOutput(NS(id, "plot_guess"))
            )
        ),
        fluidRow(
            column(6, style='padding:0px;',
                   bs4Card(
                       width = 12,
                       title = "Model fits",
                       status = "success",
                       maximizable = TRUE,
                       footer = tagList(
                           fluidRow(
                               column(6,
                                      actionButton(NS(id, "cleanup"), "Clear",
                                                   outline = TRUE, flat = FALSE,
                                                   status = "secondary"
                                      )
                               ),
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
                       plotlyOutput(NS(id, "plot_fits")),
                       hr(),
                       verbatimTextOutput(NS(id, "table_AIC"))
                   )
            ),
            column(6, style='padding:0px;',
                   fluidRow(
                       bs4TabCard(
                           solidHeader = TRUE, collapsed = TRUE,
                           width = 12,
                           status = "warning",
                           title = "Estimates", side = "right",
                           maximizable = TRUE,
                           type = "tabs",
                           footer = tagList(
                               fluidRow(
                                   column(12, align = "right",
                                          actionButton(NS(id, "download_pars_plot"), "",
                                                       icon = icon("download"),
                                                       outline = TRUE,
                                                       status = "success",
                                                       flat = FALSE
                                          )
                                   )
                               )
                           ),
                           tabPanel(
                               title = "Plot",
                               plotOutput(NS(id, "plot_pars"))
                           ),
                           tabPanel(
                               title = "Table",
                               tableOutput(NS(id, "table_pars"))
                           )
                       )
                   )
            )
        )
        # fluidRow(
        #     bs4Card(
        #         title = "Model fits",
        #         status = "success",
        #         plotOutput(NS(id, "plot_fits")),
        #         hr(),
        #         tableOutput(NS(id, "table_AIC"))
        # 
        #     ),
        #     bs4Card(
        #         title = "Parameter estimates",
        #         status = "warning",
        #         plotOutput(NS(id, "plot_pars")),
        #         hr(),
        #         tableOutput(NS(id, "table_pars"))
        #     )
        # )
    )
    
}

## Server ----------------------------------------------------------------------

module_gamma_fit_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Help page -----------------------------------------------------------
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Fitting secondary models based on the gamma concept", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p(
                                    paste("This module includes functions for fitting several secondary growth models to experimental data according to the gamma approach.",
                                          "The data should consists of growth rates (mu) estimated from independent experiments with constant environmental conditions.",
                                          "This module can consider an arbitrary number of environmental factors.",
                                          "Although it can also be used to analyse data considering a single factor, the 'Secondary' module provides a simpler interface for that."
                                    )
                                ),
                                h3("Uploading data"),
                                p(
                                    paste(
                                        "The data must be uploaded as an Excel file:"
                                    )
                                    
                                ),
                                tags$li("The data must be included within a single Excel sheet."),
                                tags$li("The Excel sheet cannot have empty rows at the beginning."),
                                tags$li("The sheet must have two columns: 'mu' with the estimate of the growth rate"),
                                tags$li("It can have as many additional columns with the values of the environmental conditions."),
                                p(
                                    tagList(
                                        downloadLink(NS(id, "downloadData"), "Download an example"),
                                        "of a typical example."
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
                "example_gamma.xlsx"
            },
            content = function(file) {
                file.copy("example_gamma.xlsx", file)
            }
        )
        
        output$download_example <- downloadHandler(
            filename = function() {
                "example_gamma.xlsx"
            },
            content = function(file) {
                file.copy("example_gamma.xlsx", file)
            }
        )
        
        ## Input data ------ ---------------------------------------------------
        
        excelFile <- reactive({
            input$excel_file
        })
        
        observeEvent(excelFile(), {  # Update the choices
            
            validate(need(excelFile(), message = ""))
            updateSelectInput(session = session,
                              inputId = "excel_sheet",
                              choices = excel_sheets(excelFile()$datapath)
            )
            
        })
        
        my_data <- reactive({
            read_excel(excelFile()$datapath,
                       sheet = input$excel_sheet,
                       skip = input$excel_skip,
                       col_types = "numeric")
        })
        
        output$plot_input <- renderPlot({
            
            validate(need(excelFile(), message = ""))
            
            my_data() %>%
                gather(var, value, -mu) %>%
                ggplot(aes(x = value, y = mu)) +
                # geom_line() +
                geom_point() +
                facet_wrap("var", scales = "free_x") +
                ylab("")
            
        })
        
        ## Dynamic secondary model selector ------------------------------------
        
        output$secondary_guess <- renderUI({
            
            validate(need(excelFile(), message = ""))
            
            my_names <- my_data() %>%
                select(-mu) %>%
                names()
            
            lapply(my_names, function(each_name) {
                
                tagList(
                    tags$h3(paste("Factor:", each_name)),
                    selectInput(NS(id, paste0(each_name, "_model")),
                                "Model type",
                                secondary_model_data()
                    ),
                    fluidRow(
                        column(8,
                               numericInput(NS(id, paste0(each_name, "_xmin")), "Xmin", 0)
                        ),
                        column(4,
                               awesomeCheckbox(NS(id,
                                                  paste0(each_name, "_xmin", "_fixed")
                               ),
                               "Fixed?", FALSE
                               )
                        )
                    ),
                    conditionalPanel(
                        ns = NS(id),
                        condition = paste0("input.", each_name, "_model != 'fullRatkowsky'"),
                        fluidRow(
                            column(8,
                                   numericInput(NS(id, paste0(each_name, "_xopt")), "Xopt", 37)
                            ),
                            column(4,
                                   awesomeCheckbox(NS(id,
                                                      paste0(each_name, "_xopt", "_fixed")),
                                                   "Fixed?", FALSE
                                                   
                                   )
                            )
                        )
                    ),
                    conditionalPanel(
                        ns = NS(id),
                        condition = paste0("input.", each_name, "_model != 'Zwietering'"),
                        fluidRow(
                            column(8,
                                   numericInput(NS(id, paste0(each_name, "_xmax")), "Xmax", 45)
                            ),
                            column(4,
                                   awesomeCheckbox(
                                       NS(id, paste0(each_name, "_xmax", "_fixed")),
                                       "Fixed?", FALSE
                                   )
                            )
                        )
                        
                    ),
                    conditionalPanel(
                        ns = NS(id),
                        condition = paste0("input.", each_name, "_model != 'fullRatkowsky'"),
                        fluidRow(
                            column(8,
                                   numericInput(NS(id, paste0(each_name, "_n")), "n", 1)
                            ),
                            column(4,
                                   awesomeCheckbox(
                                       NS(id, paste0(each_name, "_n", "_fixed")),
                                       "Fixed?", TRUE
                                   )
                            )
                        )
                    ),
                    conditionalPanel(
                        ns = NS(id),
                        condition = paste0("input.", each_name, "_model == 'fullRatkowsky'"),
                        fluidRow(
                            column(8,
                                   numericInput(NS(id, paste0(each_name, "_c")), "c", 1)
                            ),
                            column(4,
                                   awesomeCheckbox(
                                       NS(id, paste0(each_name, "_c", "_fixed")),
                                       "Fixed?", FALSE
                                   )
                            )
                        )
                    ),
                    tags$hr()
                )
                
            })
            
        })
        
        ## Automagic initial guess ---------------------------------------------
        
        observeEvent(input$automagic, {
            
            validate(need(my_data(), message = ""))
            
            ## Extract the models for each factor
            
            sec_models <- list()
            
            my_names <- my_data() %>%
                select(-mu) %>%
                names()
            
            for (each_factor in my_names) {
                
                sec_models[[each_factor]] <- input[[paste0(each_factor, "_model")]]
            }
            
            ## Make the guess
            
            my_guess <- make_guess_secondary(my_data(), sec_models)
            
            ## Update the input for mu_opt
            
            updateNumericInput(session = session,
                               inputId = "muopt",
                               value = my_guess[["mu_opt"]]
            )
            
            ## Update the input for the cardinal parameters
            
            sec_pars <- my_guess[names(my_guess) != "mu_opt"]
            
            for (each_par in names(sec_pars)) {
                
                updateNumericInput(session = session,
                                   inputId = each_par,
                                   value = sec_pars[[each_par]]
                )
                
            }
            
        })
        
        ## Model fitting -------------------------------------------------------
        
        my_fits <- reactiveVal()
        
        observeEvent(input$cleanup, {
            my_fits(NULL)
        })
        
        observeEvent(input$go_fit, withProgress(message = "Fitting model...", {
            
            validate(need(my_data(), message = ""))
            
            ## Extract the models for each factor
            
            sec_models <- list()
            
            my_names <- my_data() %>%
                select(-mu) %>%
                names()
            
            for (each_factor in my_names) {
                
                sec_models[[each_factor]] <- input[[paste0(each_factor, "_model")]]
            }
            
            ## Extract the guess for mu_opt
            
            known_pars <- list()
            my_start <- list()
            
            if (input$muopt_fix) {
                known_pars$mu_opt <- input$muopt
            } else {
                my_start$mu_opt <- input$muopt
            }
            
            ## Extract the secondary model parameters
            
            for (each_factor in my_names) {
                
                my_model <- input[[paste0(each_factor, "_model")]]
                
                for (each_par in secondary_model_data(my_model)$pars) {
                    
                    par_key <- paste0(each_factor, "_", each_par)
                    
                    if (input[[ paste0(par_key, "_fixed") ]]) {
                        known_pars[[par_key]] <- input[[par_key]]
                    } else {
                        my_start[[par_key]] <- input[[par_key]]
                    }
                    
                }
            }
            
            ## Extract the model parameters
            
            this_fit <- fit_secondary_growth(my_data(), 
                                             my_start, 
                                             known_pars, 
                                             sec_models,
                                             transformation = input$transformation
            )
            
            ## Save the fit
            
            out <- my_fits()
            out[[input$fit_name]] <- this_fit
            my_fits(out)
            
        })
        )
        
        ## Output of the fit ---------------------------------------------------
        
        output$plot_fits <- renderPlotly({
            
            validate(need(my_fits(), message = ""))
            
            comparison <- compare_secondary_fits(my_fits())
            p <- plot(comparison) +
                theme(legend.title = element_blank())
            
            ggplotly(p) %>% config(displayModeBar = FALSE)
            
        })
        
        output$table_AIC <- renderPrint({
            
            validate(need(my_fits(), message = ""))
            
            comparison <- compare_secondary_fits(my_fits())
            summary(comparison)
            
        })
        
        ## Download plot -------------------------------------------------------
        
        dataModal <- function() {
            modalDialog(
                easyClose = TRUE,
                fluidRow(
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
        
        observeEvent(input$download_pred_plot, {
            
            showModal(
                session = session,
                dataModal()
            )
            
        })
        
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
                
                
                comparison <- compare_secondary_fits(my_fits())
                
                p <- plot(comparison) +
                    my_theme(base_size = input$baseSize_sim_png) +
                    theme(
                        legend.position = input$legendPos_sim_png
                    ) +
                    labs(colour = input$legendTitle_sim_png)
    
                
                ggsave(file, plot=p, device = "png",
                       width = input$width_sim_png,
                       height = input$height_sim_png,
                       units = "px",
                       bg = "white"
                )
            }
        )
        
        ## Output of the parameters --------------------------------------------
        
        output$plot_pars <- renderPlot({
            
            validate(need(my_fits(), message = ""))
            
            comparison <- compare_secondary_fits(my_fits())
            plot(comparison, type = 2) + 
                theme_gray() +
                theme(axis.text = element_text(size = 14),
                      axis.title = element_text(size = 16),
                      strip.text = element_text(size = 14)
                ) +
                xlab("") + ylab("Estimate")
            
        })
        
        output$table_pars <- renderTable({
            
            validate(need(my_fits(), message = ""))
            
            comparison <- compare_secondary_fits(my_fits())
            coef(comparison)
            
        })
        
        ## Download plot -------------------------------------------------------
        
        dataModal_pars <- function() {
            modalDialog(
                easyClose = TRUE,
                fluidRow(
                    column(6,  # Text file 
                           # h3("Download as text"),
                           downloadBttn(NS(id, "download_pars_text"), "Download table",
                                        style = "bordered",
                                        color = "primary",
                                        block = TRUE),
                           br(),
                           radioButtons(NS(id, "delim_pars_download"), "Separator",
                                        choices = list(`tabulation` = "\t",
                                                       `comma (,)` = ",",
                                                       `semicolon (;)` = ";" 
                                        ))
                    ),
                    column(6,  # Figure
                           # h3("Download as figure"),
                           downloadBttn(NS(id, "download_pars_png"), "Download as PNG",
                                        style = "bordered",
                                        color = "primary",
                                        block = TRUE),
                           br(),
                           numericInput(NS(id, "width_pars_png"), "Width (pixels)", 900),
                           numericInput(NS(id, "height_pars_png"), "height (pixels)", 600),
                           numericInput(NS(id, "baseSize_pars_png"), "Base size", 7),
                           selectInput(NS(id, "theme_pars_png"), "Theme",
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
        
        observeEvent(input$download_pars_plot, {
            
            showModal(
                session = session,
                dataModal_pars()
            )
            
        })
        
        output$download_pars_text <- downloadHandler(
            filename = "biogrowth_estimates.csv",
            content = function(file) {
                
                comparison <- compare_secondary_fits(my_fits())
                coef(comparison) %>%
                    write_delim(.,
                                delim = input$delim_pars_download,
                                file = file)
                
            }
        )
        
        output$download_pars_png <- downloadHandler(
            filename = "biogrowth_estimates.png",
            content = function(file){
                
                my_theme <- switch(input$theme_pars_png,
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
                
                comparison <- compare_secondary_fits(my_fits())
                p <- plot(comparison, type = 2) + 
                    xlab("") + ylab("Estimate") +
                    my_theme(base_size = input$baseSize_pars_png)
                
                ggsave(file, plot=p, device = "png",
                       width = input$width_pars_png,
                       height = input$height_pars_png,
                       units = "px",
                       bg = "white"
                )
            }
        )

    })
    
}

## test ------------------------------------------------------------------------

test_module_gamma_fit <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_gamma_fit_ui("test")
            )
        ),
        server = function(input, output) {
            module_gamma_fit_server("test")
        }
    )
    
}




