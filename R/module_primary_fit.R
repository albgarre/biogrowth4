

library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_primary_fit_ui <- function(id) {
    
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
        tableInput_module_ui(NS(id, "input_data"), box_title = "Input data",
                             status = "primary", status_2 = "primary"),
        fluidRow(
            bs4Card(
                title = "Model definition",
                status = "primary",
                width = 6,
                footer = tagList(
                    textInput(NS(id, "fit_name"), "Fit name", "Model I"),
                    actionButton(NS(id, "fit_model"), "Fit model",
                                 outline = TRUE, flat = FALSE,
                                 status = "primary"
                    )
                ),
                # footer = actionBttn(NS(id, "fit"), "Fit model", style = "material-flat"),
                pickerInput(NS(id, "model_name"), "Model",
                            choices = primary_model_data() %>% set_names(., .) %>% as.list()
                ),
                pickerInput(NS(id, "scale_mu"), 
                            label = "Scale for mu: ",
                            choices = c("log10", "ln", "base2"),
                            width = "50%"),
                tags$h4("Initial guesses", display = 1),
                uiOutput(NS(id, "parameters")),
                actionButton(NS(id, "make_guess"), "Automagic",
                             outline = TRUE, flat = FALSE,
                             status = "secondary")
            ),
            bs4Card(
                title = "Initial guess",
                status = "primary",
                width = 6,
                plotOutput(NS(id, "initial_guess"))
            )
        ),
        fluidRow(
            column(6, style='padding:0px;',
                   bs4Card(
                       width = 12,
                       title = "Model fits",
                       status = "success",
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
                       plotlyOutput(NS(id, "fitted_curves")),
                       hr(),
                       verbatimTextOutput(NS(id, "results_comparison"))
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
                           ),
                           footer = downloadBttn(NS(id, "download_residuals"), "",
                                        style = "bordered",
                                        color = "primary",
                                        block = TRUE)
                       )
                   )
            )
        )
    )
    
}

## Server ----------------------------------------------------------------------

module_primary_fit_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Help page -----------------------------------------------------------
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Fitting primary models to experimental data", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p(
                                    paste("This module includes functions for fitting primary models to logN vs time data.",
                                          "They are intended for situations where the environmental conditions are constant or their effect is disregarded.",
                                          "Otherwise, it is recommended to use the 'Dynamic' or 'Global' modules, which also include secondary models.")
                                ),
                                h3("Uploading data"),
                                p(
                                    paste(
                                        "The data can be uploaded as an Excel file, a text file or manually using biogrowth.",
                                        "When uploading the data as an Excel file,"
                                    )
                                    
                                ),
                                tags$li("The data must be included within a simple Excel sheet."),
                                tags$li("The Excel sheet cannot have empty rows at the beginning."),
                                tags$li("One column must be named 'time' and included the elapsed time."),
                                tags$li("A second column must be named 'logN' and include the microbial log-concentration."),
                                p(
                                    tagList(
                                        downloadLink(NS(id, "downloadData"), "Download an example"),
                                        "of a typical growth curve."
                                    )
                                    # downloadLink(NS(id, "download_example"), 
                                    #              "Example dataset"
                                    #              )
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
                "example_primary.xlsx"
            },
            content = function(file) {
                file.copy("example_primary.xlsx", file)
            }
        )
        
        ## Input -------------------------------------------------------------------
        
        my_data <- tableInput_module_server("input_data",
                                            col_names = c("time", "logN"),
                                            xvar = "time", yvar = "logN",
                                            default_data = tibble(time = c(0, 25, 50, 75, 100),
                                                                  logN = c(2, 2.5, 7, 8, 8)
                                            )
        )
        
        ## Dynamic parameter selection -----------------------------------------
        
        par_map <- tribble(
            ~par, ~label, ~value, ~fixed,
            "logN0", "logN0 (log CFU/g)", 2, FALSE,
            "lambda", "lambda (h)", 3, FALSE,
            "mu", "mu (log CFU/h)", .1, FALSE,
            "C", "C (log CFU)", 6, FALSE,
            "logNmax", "log Nmax (log CFU)", 8, FALSE,
            "nu", "nu (·)", 1, TRUE
        )
        
        make_input <- function(par_name) {
            
            par_data <- par_map %>%
                filter(par == par_name)
            
            fluidRow(
                column(width = 6,
                       numericInput(NS(id, par_name), par_data$label, par_data$value)
                ),
                column(width = 6,
                       awesomeCheckbox(NS(id, paste0(par_name, "_fixed")), "Fixed?", par_data$fixed)
                )
            )
            
        }
        
        output$parameters <- renderUI({
            
            # par_names <- model_map[[input$model]]
            par_names <- primary_model_data(input$model_name)$par
            
            par_names %>%
                map(.,
                    ~ make_input(.)
                )
            
        })
        
        ## Initial guess -------------------------------------------------------
        
        output$initial_guess <- renderPlot({
            
            validate(need(my_data(), message = ""))
            
            ## Extract the parameters
            
            par_names <- primary_model_data(input$model_name)$par
            
            p <- list()
            # known <- list()
            
            for (i in par_names) {
                
                p[[i]] <- input[[i]]
                
            }
            
            p <- unlist(p)
            
            ## Show the guess
            
            scale_mu <- switch(input$scale_mu,
                               `log10` = 10,
                               `ln` = exp(1),
                               `base2` = 2
            )
            
            check_growth_guess(my_data(),
                               list(primary = input$model_name),
                               p,
                               logbase_mu = scale_mu
            )
            
        })
        
        ## Automatic guess
        
        observeEvent(input$make_guess, {
            
            validate(need(my_data(), message = ""))
            
            ## Make the guess
            
            scale_mu <- switch(input$scale_mu,
                               `log10` = 10,
                               `ln` = exp(1),
                               `base2` = 2
            )
            
            my_guess <- make_guess_primary(my_data(), input$model_name,
                                           logbase_mu = scale_mu)
            
            ## Update the numeric inputs
            
            for (i in 1:length(my_guess)) {
                
                updateNumericInput(session = session,
                                   inputId = names(my_guess)[i],
                                   value = my_guess[[i]]
                )
                
            }
            
        })
        
        ## Model fitting -------------------------------------------------------
        
        all_fits <- reactiveVal()
        
        observeEvent(input$fit_model, {
            
            ## Extract the model parameters
            
            par_names <- primary_model_data(input$model_name)$par
            
            p <- list()
            known <- list()
            
            for (i in par_names) {
                
                if (isTRUE(input[[paste0(i, "_fixed")]])) {
                    known[[i]] <- input[[i]]
                } else {
                    p[[i]] <- input[[i]]
                }
                
            }
            
            p <- unlist(p)
            known <- unlist(known)
            
            ## Fit the model
            
            models <- list(primary = input$model_name)
            
            scale_mu <- switch(input$scale_mu,
                               `log10` = 10,
                               `ln` = exp(1),
                               `base2` = 2
            )
            
            primary_fit <- fit_growth(my_data(), 
                                      models, 
                                      p, 
                                      known,
                                      environment = "constant",
                                      logbase_mu = scale_mu
            )
            
            ## Save the results
            
            out <- all_fits()
            
            if (is.null(out)) {
                new_model <- list(primary_fit)
                names(new_model) <- input$fit_name
                all_fits(new_model)
            } else {
                out[[input$fit_name]] <- primary_fit
                all_fits(out)
            }
            
            # out <- all_fits()
            # 
            # new_model <- list(primary_fit)
            # names(new_model) <- input$fit_name
            # 
            # if (is.null(out)) {
            #     all_fits(new_model)
            # } else {
            # 
            #     all_fits(c(out, new_model))
            # }
            
        })
        
        observeEvent(input$cleanup, {
            all_fits(NULL)
        })
        
        ## Fitted models -----------------------------------------------------------
        
        output$fitted_curves <- renderPlotly({
            validate(need(all_fits(), message = ""))
            
            comparison <- compare_growth_fits(all_fits())
            
            p <- plot(comparison) + 
                xlab(input$xlabel) + ylab(input$ylabel) +
                theme_gray() +
                theme(legend.title = element_blank()) 
            
            ggplotly(p) %>% config(displayModeBar = FALSE)
            
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
                
                all_fits() %>%
                    map(., ~.$best_prediction$simulation) %>%
                    imap_dfr(., ~ mutate(.x, sim = .y)) %>%
                    pivot_wider(names_from = sim, values_from = logN) %>%
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
                
                comparison <- compare_growth_fits(all_fits())
                
                p <- plot(comparison) + 
                    xlab(input$xlabel) + ylab(input$ylabel) +
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
        
        ## Summary of the comparison -----------------------------------------------
        
        output$results_comparison <- renderPrint({
            validate(need(all_fits(), message = ""))
            
            comparison <- compare_growth_fits(all_fits())
            print(comparison)
            
        })
        
        ## Output of parameter estimates -------------------------------------------
        
        output$table_parameters <- renderTable({
            validate(need(all_fits(), message = ""))
            
            comparison <- compare_growth_fits(all_fits())
            coef(comparison)
        })
        
        output$plot_parameters <- renderPlot({
            
            validate(need(all_fits(), message = ""))
            
            comparison <- compare_growth_fits(all_fits())
            
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
                
                comparison <- compare_growth_fits(all_fits())
                
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
                
                
                comparison <- compare_growth_fits(all_fits())
                
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
            
            validate(need(all_fits(), message = ""))
            
            comparison <- compare_growth_fits(all_fits())
            plot(comparison, type = 3) + 
                theme_gray() +
                xlab("Time") + ylab("Residual") +
                theme(legend.title = element_blank())
            
        })
        
        output$table_residuals <- renderTable({
            
            validate(need(all_fits(), message = ""))
            
            comparison <- compare_growth_fits(all_fits())
            
            residuals(comparison) %>%
                map(., 
                    ~ .$residuals
                ) %>%
                imap_dfr(., ~ mutate(.x, model = .y, i = row_number())) %>%
                select(model, time = x, observed = obs, fitted = mod, residual = res)
        })
        
        output$download_residuals <- downloadHandler(
            filename = function() {
                "primary_residuals.csv"
            },
            content = function(file) {
                validate(need(all_fits(), message = ""))
                
                comparison <- compare_growth_fits(all_fits())
                
                residuals(comparison) %>%
                    map(., 
                        ~ .$residuals
                    ) %>%
                    imap_dfr(., ~ mutate(.x, model = .y, i = row_number())) %>%
                    select(model, time = x, observed = obs, 
                           fitted = mod, residual = res) %>%
                    write_tsv(., file)
            }
        )
        
    })
    
}

## test ------------------------------------------------------------------------

test_module_primary_fit <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_primary_fit_ui("test")
            )
        ),
        server = function(input, output) {
            module_primary_fit_server("test")
        }
    )
    
}




