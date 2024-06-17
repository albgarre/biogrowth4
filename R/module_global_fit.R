
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_global_fit_ui <- function(id) {
    
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
            bs4Card(title = "Input microbial counts",
                    status = "primary",
                    fileInput(NS(id, "excel_file_count"), "Excel file"),
                    downloadLink(NS(id, "download_example_count"), "Download example")
            ),
            box(status = "primary",
                plotlyOutput(NS(id, "plot_input_count"))
            )
        ),
        fluidRow(
            box(title = "Input environmental conditions",
                status = "primary",
                fileInput(NS(id, "excel_file_env"), "Excel file"),
                downloadLink(NS(id, "download_example_env"), "Download example")
            ),
            box(status = "primary",
                plotlyOutput(NS(id, "plot_input_env"))
            )
        ),
        fluidRow(
            bs4Card(
                title = "Initial guess - primary",
                status = "primary",
                pickerInput(NS(id, "scale_mu"), 
                            label = "Scale for mu: ",
                            choices = c("log10", "ln", "base2"),
                            width = "50%"),
                fluidRow(
                    column(8,
                           numericInput(NS(id, "muopt"), "mu_opt (log10 CFU/h)", 0.5, min = 0)
                    ),
                    column(4,
                           awesomeCheckbox(NS(id, "muopt_fix"), "Fixed?", value = FALSE)
                    )
                ),
                fluidRow(
                    column(8,
                           numericInput(NS(id, "logNmax"), "logNmax (log10 CFU/g)", 8)
                    ),
                    column(4,
                           awesomeCheckbox(NS(id, "logNmax_fix"), "Fixed?", value = FALSE)
                    )
                ),
                fluidRow(
                    column(8,
                           numericInput(NS(id, "logN0"), "logN0 (log10 CFU/g)", 0)
                    ),
                    column(4,
                           awesomeCheckbox(NS(id, "logN0_fix"), "Fixed?", value = FALSE)
                    )
                ),
                fluidRow(
                    column(8,
                           numericInput(NS(id, "Q0"), "Q0 (·)", 1e-3, min = 0)
                    ),
                    column(4,
                           awesomeCheckbox(NS(id, "Q0_fix"), "Fixed?", value = FALSE)
                    )
                )
            ),
            bs4Card(
                title = "Initial guess - secondary",
                status = "primary",
                uiOutput(NS(id, "secondary_guess"))
            )
        ),
        fluidRow(
            bs4Card(
                title = "Fitting algorithm",
                status = "primary",
                footer = tagList(
                    textInput(NS(id, "fit_name"), "Model name", "Model I"),
                    actionButton(NS(id, "go_fit"), "Add/Edit fit")
                ),
                selectInput(NS(id, "algorithm"), "Algorithm",
                            choices = list(`Nonlinear regression` = "regression",
                                           `Adaptive Monte Carlo` = "MCMC")
                ),
                conditionalPanel(
                    ns = NS(id),
                    condition = "input.algorithm == 'MCMC'",
                    numericInput(NS(id, "niter"), "Iterations", 100, min = 10),
                    actionButton(NS(id, "resetSeed"), "Reset seed",
                                 outline = TRUE, flat = FALSE,
                                 status = "secondary")
                )
            ),
            bs4Card(
                title = "Initial guess",
                status = "success",
                plotOutput(NS(id, "plot_guess"))
            )
        ),
        fluidRow(
            column(6, style='padding:0px;',
                   bs4Card(
                       title = "Fitted curves",
                       status = "success",
                       width = 12,
                       maximizable = TRUE,
                       dropdownMenu = boxDropdown(
                           boxDropdownItem(
                               textInput(NS(id, "xlabel"), "x-label", "Time (h)")
                           ),
                           boxDropdownItem(
                               textInput(NS(id, "ylabel"), "y-label", "Microbial concentration (log CFU/ml)")
                           )
                       ),
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
                       verbatimTextOutput(NS(id, "model_comparison"))
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
                               plotOutput(NS(id, "plot_parameters"))
                           ),
                           tabPanel(
                               title = "Table",
                               tableOutput(NS(id, "table_parameters"))
                           )
                       )
                   ),
                   fluidRow(
                       bs4TabCard(
                           solidHeader = TRUE, collapsed = TRUE,
                           width = 12,
                           status = "warning",
                           title = "Residuals", side = "right",
                           maximizable = TRUE,
                           type = "tabs",
                           tabPanel(
                               title = "Plot",
                               plotOutput(NS(id, "plot_residuals"))
                           ),
                           tabPanel(
                               title = "Table",
                               tableOutput(NS(id, "table_residuals"))
                           )
                       )
                   )
            )
        )
        # fluidRow(
        #     
        #     bs4Card(
        #         title = "Parameter estimates",
        #         status = "warning",
        #         plotOutput(NS(id, "plot_parameters")),
        #         hr(),
        #         tableOutput(NS(id, "table_parameters"))
        #     )
        # )
    )
}

## Server ----------------------------------------------------------------------

module_global_fit_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Resetting the seed --------------------------------------------------
        
        observeEvent(input$resetSeed, {
            set.seed(12424)
            print("Seed back to normal")
        })
        
        ## Help page -----------------------------------------------------------
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Global fitting to several (dynamic) experiments", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p(
                                    paste("This module includes functions for fitting growth models combining primary and secondary models to experimental data.",
                                          "The module uses 'global fitting', where a single model is fitted to a batch of experiments with different environmental conditions",
                                          "These experiments can be either at constant or dynamic conditions.",
                                          "For the case where the environmental conditions are the same in every experiment, the 'Dynamic' module provides a simpler interface.")
                                ),
                                h3("Uploading data"),
                                p(
                                    paste(
                                        "Two pieces of data must be uploaded: the microbial concentrations and the environmental conditions.",
                                        "The microbial concentrations can be uploaded as an Excel file.",
                                        "When uploading the data as an Excel file,"
                                    )
                                    
                                ),
                                tags$li("Each sheet within the Excel file describes one experiment."),
                                tags$li("The sheets must be named. No additional sheet (i.e., without experimental data) can be included"),
                                tags$li("The Excel sheets cannot have empty rows at the beginning."),
                                tags$li("One column must be named 'time' and included the elapsed time."),
                                tags$li("A second column must be named 'logN' and include the microbial log-concentration."),
                                p("The data describing the environmental conditions be uploaded as an Excel file."),
                                tags$li("The data must be included within a simple Excel sheet."),
                                tags$li("The Excel sheet cannot have empty rows at the beginning."),
                                tags$li("One column must be named 'time' and included the elapsed time."),
                                tags$li("Then, as many additional columns as environmental factors must be defined."),
                                p(
                                    paste(
                                        "For values of the elapsed time not included in 'time', the environmental factors are calculated by linear interpolation.",
                                        "Therefore, both ideal and measured profiles can be simulated."
                                    )
                                ),
                                p(
                                    tagList(
                                        "These links include an ",
                                        downloadLink(NS(id, "downloadData"), "Example of microbial concentrations "),
                                        "and an ",
                                        downloadLink(NS(id, "downloadEnvData"), "Example environmental conditions "),
                                        "to be used in combination for global fitting.",
                                        "They simulate two dynamic experiments considering two environmental factors (pH and temperature)."
                                    )
                                ),
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
            
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                "example_global_count.xlsx"
            },
            content = function(file) {
                file.copy("example_global_count.xlsx", file)
            }
        )
        
        output$download_example_count <- downloadHandler(
            filename = function() {
                "example_global_count.xlsx"
            },
            content = function(file) {
                file.copy("example_global_count.xlsx", file)
            }
        )
        
        output$downloadEnvData <- downloadHandler(
            filename = function() {
                "example_global_env.xlsx"
            },
            content = function(file) {
                file.copy("example_global_env.xlsx", file)
            }
        )
        
        output$download_example_env <- downloadHandler(
            filename = function() {
                "example_global_env.xlsx"
            },
            content = function(file) {
                file.copy("example_global_env.xlsx", file)
            }
        )
        
        ## Input microbial counts ----------------------------------------------
        
        excelFile_count <- reactive({
            validate(need(input$excel_file_count, message = ""))
            input$excel_file_count
        })
        
        excel_frame_count <- reactive({
            
            my_path <- excelFile_count()$datapath
            
            excel_sheets(my_path) %>%
                set_names(., .) %>%
                map(., ~ read_excel(my_path, sheet = ., col_types = "numeric"))
        })
        
        output$plot_input_count <- renderPlotly({
            
            p <- excel_frame_count() %>%
                imap_dfr(., ~ mutate(.x, exp = .y)) %>%
                ggplot(aes(x = time, y = logN, colour = exp)) +
                geom_point() +
                ylab("") +
                theme(legend.title = element_blank())
            
            ggplotly(p) %>% config(displayModeBar = FALSE)
            
        })
        
        # output$download_example_count <- downloadHandler(
        #     filename = "example_global_count.xlsx",
        #     content = function(file) {
        #         file.copy("example_global_count.xlsx", file)
        #     }
        # )
        
        ## Input environmental conditions
        
        excelFile_env <- reactive({
            validate(need(input$excel_file_env, message = ""))
            input$excel_file_env
        })
        
        excel_frame_env <- reactive({
            
            my_path <- excelFile_env()$datapath
            
            excel_sheets(my_path) %>%
                set_names(., .) %>%
                map(., ~ read_excel(my_path, sheet = ., col_types = "numeric"))
        })
        
        output$plot_input_env <- renderPlotly({
            
            p <- excel_frame_env() %>%
                map(., ~ gather(., var, value, -time)) %>%
                imap_dfr(., ~ mutate(.x, exp = .y)) %>%
                ggplot(aes(x = time, y = value, colour = exp)) +
                geom_line() +
                geom_point() +
                facet_wrap("var", scales = "free_y") +
                ylab("") +
                theme(legend.title = element_blank())
            
            ggplotly(p)  %>% config(displayModeBar = FALSE)
            
        })
        
        # output$download_example_env <- downloadHandler(
        #     filename = "example_global_env.xlsx",
        #     content = function(file) {
        #         file.copy("example_global_env.xlsx", file)
        #     }
        # )
        
        
        ## Dynamic secondary model selector ------------------------------------
        
        output$secondary_guess <- renderUI({
            
            validate(need(excelFile_env(), message = ""))
            
            my_names <- excel_frame_env()[[1]] %>%
                select(-time) %>%
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
        
        ## Initial guess -------------------------------------------------------
        
        output$plot_guess <- renderPlot({
            
            count_data <- excel_frame_count()
            env_conditions <- excel_frame_env()
            
            validate(need(count_data, message = ""))
            validate(need(env_conditions, message = ""))
            
            ## Extract the models for each factor
            
            sec_models <- list()
            
            my_names <- env_conditions[[1]] %>%
                select(-time) %>%
                names()
            
            for (each_factor in my_names) {
                
                sec_models[[each_factor]] <- input[[paste0(each_factor, "_model")]]
            }
            
            ## Extract the primary model parameters
            
            guess <- list(Nmax = 10^input$logNmax,
                          N0 = 10^input$logN0,
                          Q0 = input$Q0,
                          mu_opt = input$muopt
            )
            
            ## Extract the secondary model parameters
            
            for (each_factor in my_names) {
                
                my_model <- input[[paste0(each_factor, "_model")]]
                
                for (each_par in secondary_model_data(my_model)$pars) {
                    
                    par_key <- paste0(each_factor, "_", each_par)
                    guess[[par_key]] <- input[[par_key]]
                    
                }
            }
            
            ## Calculate the guess
            
            scale_mu <- switch(input$scale_mu,
                               `log10` = 10,
                               `ln` = exp(1),
                               `base2` = 2
            )
            
            check_growth_guess(count_data, sec_models, guess,
                               "dynamic",
                               env_conditions,
                               approach = "global",
                               logbase_mu = scale_mu)
            
        })
        
        ## Model fitting -------------------------------------------------------
        
        model_fits <- reactiveVal(list())
        
        observeEvent(input$cleanup, {
            model_fits(list())
        })
        
        observeEvent(input$go_fit, withProgress(
            message = "Fitting model...", {
                
                # browser()
                
                count_data <- excel_frame_count()
                env_conditions <- excel_frame_env()
                
                validate(need(count_data, message = ""))
                validate(need(env_conditions, message = ""))
                
                ## Extract the models for each factor
                
                sec_models <- list()
                
                my_names <- env_conditions[[1]] %>%
                    select(-time) %>%
                    names()
                
                for (each_factor in my_names) {
                    
                    sec_models[[each_factor]] <- input[[paste0(each_factor, "_model")]]
                }
                
                ## Extract primary model parameters
                
                known_pars <- list()
                my_start <- list()
                
                if (input$logNmax_fix) {
                    known_pars$Nmax <- 10^input$logNmax
                } else {
                    my_start$Nmax <- 10^input$logNmax
                }
                
                if (input$logN0_fix) {
                    known_pars$N0 <- 10^input$logN0
                } else {
                    my_start$N0 <- 10^input$logN0
                }
                
                if (input$Q0_fix) {
                    known_pars$Q0 <- input$Q0
                } else {
                    my_start$Q0 <- input$Q0
                }
                
                if (input$muopt_fix) {
                    known_pars$mu_opt <- input$muopt
                } else {
                    my_start$mu_opt <- input$muopt
                }
                
                # browser()
                
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
                
                ## Fit the model
                
                scale_mu <- switch(input$scale_mu,
                                   `log10` = 10,
                                   `ln` = exp(1),
                                   `base2` = 2
                )
                
                new_fit <- fit_growth(count_data,
                                      sec_models,
                                      my_start, known_pars,
                                      environment = "dynamic",
                                      env_conditions = env_conditions,
                                      check = FALSE,
                                      niter = input$niter,
                                      algorithm = input$algorithm,
                                      approach = "global",
                                      logbase_mu = scale_mu
                )
                
                ## Update the reactive
                
                out <- model_fits()
                out[[input$fit_name]] <- new_fit
                model_fits(out)
                
            })
        )
        
        ## Output of the model fits --------------------------------------------
        
        output$plot_fits <- renderPlotly({
            
            validate(need(length(model_fits()) > 0, message = ""))
            
            comparison <- compare_growth_fits(model_fits())
            
            p <- plot(comparison) +
                xlab(input$xlabel) + ylab(input$ylabel)
            
            ggplotly(p)  %>% config(displayModeBar = FALSE)
            
        })
        
        output$model_comparison <- renderPrint({
            
            validate(need(length(model_fits()) > 0, message = ""))
            
            comparison <- compare_growth_fits(model_fits())
            summary(comparison)
            
        })
        
        ## Download plot -------------------------------------------------------
        
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
        
        observeEvent(input$download_pred_plot, {
            
            showModal(
                session = session,
                dataModal()
            )
            
        })
        
        output$download_sim_text <- downloadHandler(
            filename = "biogrowth_curves.csv",
            content = function(file) {
                
                model_fits() %>%
                    map(fitted) %>%
                    imap_dfr(., ~ mutate(.x, model = .y)) %>%
                    select(model, everything()) %>%
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
                
                comparison <- compare_growth_fits(model_fits())
                
                p <- plot(comparison) +
                    xlab(input$xlabel) + ylab(input$ylabel) +
                    my_theme(base_size = input$baseSize_sim_png) +
                    theme(legend.position = input$legendPos_sim_png) +
                    labs(colour = input$legendTitle_sim_png)
                
                ggsave(file, plot=p, device = "png",
                       width = input$width_sim_png,
                       height = input$height_sim_png,
                       units = "px",
                       bg = "white"
                )
            }
        )
        
        
        ## Output of the model parameters --------------------------------------
        
        output$table_parameters <- renderTable({
            validate(need(length(model_fits()) > 0, message = ""))
            
            comparison <- compare_growth_fits(model_fits())
            coef(comparison)
        })
        
        output$plot_parameters <- renderPlot({
            validate(need(length(model_fits()) > 0, message = ""))
            
            comparison <- compare_growth_fits(model_fits())
            plot(comparison, type = 2) + 
                theme_gray() +
                theme(axis.text = element_text(size = 14),
                      axis.title = element_text(size = 16),
                      strip.text = element_text(size = 14)
                ) +
                xlab("") + ylab("Estimate")
            
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
                
                comparison <- compare_growth_fits(model_fits())
                
                comparison %>%
                    coef() %>%
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
                
                
                comparison <- compare_growth_fits(model_fits())
                
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
        
        ## Output of the residuals -------------------------------------------------
        
        output$plot_residuals <- renderPlot({
            
            validate(need(model_fits(), message = ""))
            
            comparison <- compare_growth_fits(model_fits())
            plot(comparison, type = 3) + 
                theme_gray() +
                xlab("Time") + ylab("Residual") +
                theme(legend.title = element_blank())
            
        })
        
        output$table_residuals <- renderTable({
            
            validate(need(model_fits(), message = ""))
            
            model_fits() %>%
                map(., residuals) %>%
                imap_dfr(., ~ mutate(.x, model = .y)) %>%
                select(model, everything())
            
        })
        
    })
    
}

## test ------------------------------------------------------------------------

test_module_global_fit <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_global_fit_ui("test")
            )
        ),
        server = function(input, output) {
            module_global_fit_server("test")
        }
    )
    
}




