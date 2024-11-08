
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_secondary_fit_ui <- function(id) {
    
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
                             status = "primary", status_2 = "primary"
        ),
        fluidRow(
            bs4Card(
                title = "Model definition",
                status = "primary",
                width = 6,
                footer = tagList(
                    textInput(NS(id, "fit_name"), "Fit name", "Model I"),
                    selectInput(NS(id, "fit_scale"), "Scale", 
                                choices = c("sqrt", "log", "orig")
                    ),
                    actionButton(NS(id, "fit_model"), "Fit model",
                                 outline = TRUE, flat = FALSE,
                                 status = "primary")
                ),
                pickerInput(NS(id, "model_name"), "Model",
                            choices = c("Ratkowsky", "full Ratkowsky", "Cardinal", 
                                        "Saramago", "full Saramago", "Zwietering",
                                        "Aryani", "Rosso aw", "Inhibitor")
                ),
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
                           column(12,
                                  textInput(NS(id, "xlabel"), "x-label", "Temperature (ºC)"),
                                  textInput(NS(id, "ylabel"), "y-label", "Growth rate (log CFU/h)"),
                                  radioButtons(NS(id, "plot_scale"), "Scale", 
                                               choices = c("sqrt", "log", "orig")
                                  )
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
                       verbatimTextOutput(NS(id, "results_comparison")),
                       br(),
                       span("* Indexes calculated in the same scale as the fitting for each model")
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
        
    )
    
}

## Server ----------------------------------------------------------------------

module_secondary_fit_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Help page -----------------------------------------------------------
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Fitting secondary growth models to experimental data", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p(
                                    paste("This module includes functions for fitting secondary growth models to experimental data.",
                                          "The data should consists of growth rates (mu) estimated from independent experiments with constant environmental conditions.",
                                          "For this module, the data should consider a single environmental factor.",
                                          "To consider more than one, please use the 'Gamma' module."
                                          )
                                ),
                                h3("Uploading data"),
                                p(
                                    paste(
                                        "The data can be uploaded as an Excel file, a text file or manually using biogrowth.",
                                        "When uploading the data as an Excel file,"
                                    )
                                    
                                ),
                                tags$li("The data must be included within a single Excel sheet."),
                                tags$li("The Excel sheet cannot have empty rows at the beginning."),
                                tags$li("The sheet must have two columns: 'X' with the value of the environmental condition"),
                                tags$li("And 'mu' witht he estimate of the growth rate."),
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
                "example_secondary.xlsx"
            },
            content = function(file) {
                file.copy("example_secondary.xlsx", file)
            }
        )
        
        ## Input -------------------------------------------------------------------
        
        my_data <- tableInput_module_server("input_data",
                                            col_names = c("X", "mu"),
                                            xvar = "X", yvar = "mu",
                                            default_data = tibble(X = seq(0, 40, length = 10),
                                                                  mu = c(.9e-6, 2.1e-5, 8.7e-5, 2.2e-4, 2.8e-4, 5.5e-4, 7.5e-4, 1.1e-3, 7.9e-4, 5.5e-6)*1e3
                                            )
        )
        
        ## Dynamic parameter selection -----------------------------------------
        
        
        sec_model_map <- list(
            Ratkowsky = c("b", "Xmin"),
            `full Ratkowsky` = c("b", "Xmin", "Xmax", "c"),
            Cardinal = c("Xmin", "Xopt", "Xmax", "muopt", "n"),
            Saramago = c("b", "Xmin", "k"),
            `full Saramago` = c("b", "Xmin", "Xmax", "c", "k"),
            Zwietering = c("Xmin", "Xopt", "muopt", "n"),
            Aryani = c("Xmin", "Xhalf", "muopt"),
            `Rosso aw` = c("Xmin", "muopt"),
            Inhibitor = c("MIC", "alpha", "muopt")
        )
        
        sec_par_map <- tribble(
            ~par, ~label, ~value, ~fixed,
            "b", "b-value", 1e-3, FALSE,
            "Xmin", "Xmin", 0, FALSE,
            "Xopt", "Xopt", 35, FALSE,
            "Xmax", "Xmax", 40, FALSE,
            "c", "c-value", 1, FALSE,
            "muopt", "mu at Xopt", 1.1, FALSE,
            "n", "n (order of the model)", 2, TRUE,
            "k", "k-exponent", 1, TRUE,
            "Xhalf", "X where mu=muopt/2", 5, FALSE,
            "Xref", "Xref", 12, FALSE,
            "MIC", "MIC", 50, FALSE,
            "alpha", "Curvature parameter", 1, FALSE
        )
        
        make_input <- function(par_name) {

            par_data <- sec_par_map %>%
                filter(par == par_name)
            
            fluidRow(
                column(6,
                       numericInput(NS(id, par_name), par_data$label, par_data$value)
                ),
                column(6,
                       awesomeCheckbox(NS(id, paste0(par_name, "_fixed")), "Fixed?", par_data$fixed)
                )
            )
            
        }
        
        output$parameters <- renderUI({
            
            par_names <- sec_model_map[[input$model_name]]
            
            par_names %>%
                map(.,
                    ~ make_input(.)
                )
            
        })
        
        ## Models --------------------------------------------------------------
        
        prediction_map <- list(
            
            Ratkowsky = function(p, X) {
                
                p <- as.list(p)
                
                sq_mu <- p$b*(X - p$Xmin)
                
                sq_mu[X < p$Xmin] <- 0
                
                sq_mu^2
                
            },
            
            `full Ratkowsky` = function(p, X) {
                
                p <- as.list(p)
                
                sq_mu <- p$b*(X - p$Xmin)*(1 - exp(p$c*(X - p$Xmax)))
                
                sq_mu[X < p$Xmin] <- 0
                sq_mu[X > p$Xmax] <- 0
                
                sq_mu^2
            },
            
            Cardinal = function(p, X) {
                
                p <- as.list(p)
                
                num <- (X-p$Xmax)*(X-p$Xmin)^p$n
                den <- (p$Xopt-p$Xmin)^(p$n-1)*( (p$Xopt-p$Xmin)*(X-p$Xopt) - (p$Xopt-p$Xmax)*((p$n-1)*p$Xopt + p$Xmin-p$n*X) )
                gamma <- num/den
                
                gamma[X < p$Xmin] <- 0
                gamma[X > p$Xmax] <- 0
                
                mu <- gamma*p$muopt
                
                mu
                
            },
            
            Saramago = function(p, X) {
                
                p <- as.list(p)
                
                sq_mu <- p$b*(X - p$Xmin)^p$k
                
                sq_mu[X < p$Xmin] <- 0
                
                sq_mu^2
                
            },
            
            `full Saramago` = function(p, X) {
                
                p <- as.list(p)
                
                sq_mu <- p$b*(X - p$Xmin)^p$k*(1 - exp(p$c*(X - p$Xmax)))
                
                sq_mu[X < p$Xmin] <- 0
                sq_mu[X > p$Xmax] <- 0
                
                sq_mu^2
                
            },
            
            Zwietering = function(p, X) {
                
                p <- as.list(p)
                
                gamma <- ((X-p$Xmin)/(p$Xopt-p$Xmin))^p$n
                
                gamma[X < p$Xmin] <- 0
                gamma[X > p$Xopt] <- 0
                
                mu <- gamma*p$muopt
                mu
                
            },
            
            Aryani = function(p, X) {

                p <- as.list(p)

                gamma <- 1 - 2^( (X - p$Xmin)/(p$Xmin - p$Xhalf) )

                gamma[X < p$Xmin] <- 0
                
                mu <- gamma*p$muopt
                mu
                
            },
            
            `Rosso aw` = function(p, X) {
                
                p <- as.list(p)
                
                gamma <- (X - p$Xmin)/(1 - p$Xmin)
                
                gamma[X < p$Xmin] <- 0
                mu <- gamma*p$muopt
                mu
                
            },
            
            Inhibitor = function(p, X) {

                p <- as.list(p)
                
                gamma <- 1 - (X/p$MIC)^p$alpha

                mu <- gamma*p$muopt
                mu
                
            } 
            
            
        )
        
        ## Automagic guess -----------------------------------------------------
        
        smart_guy <- list(
            b = function(d) {
                
                Xmin <- smart_guy[["Xmin"]](d)
                muopt <- smart_guy[["muopt"]](d)
                Xopt <- smart_guy[["Xopt"]](d)
                
                (sqrt(muopt) - 0)/(Xopt - Xmin)
            },
            c = function(d) {
                
                1
            },
            Xmin = function(d) {
                
                min(d$X, na.rm = TRUE)
                
            },
            Xopt = function(d) {
                
                max_i <- which(d$mu == max(d$mu, na.rm = TRUE))
                d$X[[max_i]]
                
            },
            Xmax = function(d) {
                
                max(d$X, na.rm = TRUE)
                
            },
            muopt = function(d) {
                
                max(d$mu, na.rm = TRUE)
                
            },
            n = function(d) {
                2
            },
            k = function(d) {
                1
            },
            Xref = function(d) {
                max(d$X, na.rm = TRUE)*1.1
            },
            Xhalf = function(d) {
                Xmin <- smart_guy[["Xmin"]](d)
                Xopt <- smart_guy[["Xopt"]](d)
                
                (Xmin + Xopt)/2
                
            },
            MIC = function(d) {
                smart_guy[["Xmax"]](d)
            },
            alpha = function(d) {
                1
            }
            
        )
        
        observeEvent(input$make_guess, {
            
            validate(need(my_data(), message = ""))
            
            ## Get the parameters
            
            par_names <- sec_model_map[[input$model_name]]
            
            ## Make the guess
            
            d <- my_data()
            
            my_guess <- list()
            
            # browser()
            
            for (each_par in par_names) {
                
                my_guess[[each_par]] <- smart_guy[[each_par]](d)
                
            }
            
            ## Update the numeric inputs
            
            for (i in 1:length(my_guess)) {
                
                updateNumericInput(session = session,
                                   inputId = names(my_guess)[i],
                                   value = my_guess[[i]]
                )
                
            }
            
        })
        
        ## Initial guess -------------------------------------------------------
        
        output$initial_guess <- renderPlot({
            
            validate(need(my_data(), message = ""))

            ## Extract the model parameters
            
            par_names <- sec_model_map[[input$model_name]]
            
            p <- list()
            
            for (i in par_names) {
                
                p[[i]] <- input[[i]]
                
            }
            
            ## Make the prediction
            
            d <- my_data()
            
            x_range <- range(d$X)
            
            p <- tibble(
                x = seq(x_range[1], x_range[2], length = 100),
                y = prediction_map[[input$model_name]](p, x)
            ) %>%
                ggplot() + 
                geom_line(aes(x, y)) +
                geom_point(aes(x = X, y = mu), data = d)
            
            ## Adjust the scale
            
            if (input$fit_scale == "sqrt") {
                
                p + scale_y_sqrt()
                
            } else if (input$fit_scale == "log") {
                
                p + scale_y_log10()
                
            } else {
                
                p
            }
            
            
            
        })
        
        ## Model fitting -------------------------------------------------------
        
        get_residuals <- function(parameters, data, model, known, scale) {
            
            pred <- prediction_map[[model]](c(parameters, known), data$X)
            
            ## Convert to the scale
            
            if (scale == "sqrt") {
                pred <- sqrt(pred)
                y <- sqrt(data$mu)
            } else if (scale == "log") {
                pred <- log(pred)
                y <- log(data$mu)
            } else {
                pred <- pred
                y = data$mu
            }
            
            ## Calculate residuals
            
            res <- pred - y
            
            res
            
        }
        
        all_fits <- reactiveVal()
        
        observeEvent(input$fit_model, {
            
            ## Get the model parameters
            
            par_names <- sec_model_map[[input$model_name]]
            
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
            
            new_fit <- modFit(get_residuals, p, data = my_data(), 
                              model = input$model_name, known = known,
                              scale = input$fit_scale)
            
            ## Save it
            
            out <- all_fits()
            out[[input$fit_name]] <- list(model = input$model_name,
                                          fit = new_fit,
                                          known = known,
                                          scale = input$fit_scale
            )
            all_fits(out)
        })
        
        ## Cleaning up ---------------------------------------------------------
        
        observeEvent(input$cleanup, {
            
            all_fits(NULL)
            
        })
        
        ## Output --------------------------------------------------------------
        
        output$fitted_curves <- renderPlotly({
            
            validate(need(all_fits(), message = ""))
            
            d <- my_data()
            
            x_range <- range(d$X)
            
            p <- all_fits() %>%
                map(.,
                    ~ tibble(
                        x = seq(x_range[1], x_range[2], length = 100),
                        y = prediction_map[[.$model]](c(.$known, coef(.$fit)), x)
                    )
                ) %>%
                imap_dfr(., ~ mutate(.x, sim = .y)) %>%
                ggplot(.) +
                geom_line(aes(x = x, y = y, colour = sim)) +
                geom_point(aes(x = X, y = mu), data = d) +
                xlab(input$xlabel) + ylab(input$ylabel)
            
            if (input$plot_scale == "sqrt") {
                
                p <- p + scale_y_sqrt()
                
            } else if (input$plot_scale == "log") {
                
                p <- p + scale_y_log10()
                
            } else {
                p <- p
            }
            
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
                
                d <- my_data()
                
                x_range <- range(d$X)
                
                all_fits() %>%
                    map(.,
                        ~ tibble(
                            X = seq(x_range[1], x_range[2], length = 100),
                            y = prediction_map[[.$model]](c(.$known, coef(.$fit)), X)
                        )
                    ) %>%
                    imap_dfr(., ~ mutate(.x, sim = .y)) %>%
                    pivot_wider(., names_from = sim, values_from = y) %>%
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
                
                
                validate(need(all_fits(), message = ""))
                
                d <- my_data()
                
                x_range <- range(d$X)
                
                p <- all_fits() %>%
                    map(.,
                        ~ tibble(
                            x = seq(x_range[1], x_range[2], length = 100),
                            y = prediction_map[[.$model]](c(.$known, coef(.$fit)), x)
                        )
                    ) %>%
                    imap_dfr(., ~ mutate(.x, sim = .y)) %>%
                    ggplot(.) +
                    geom_line(aes(x = x, y = y, colour = sim)) +
                    geom_point(aes(x = X, y = mu), data = d) +
                    xlab(input$xlabel) + ylab(input$ylabel) +
                    my_theme(base_size = input$baseSize_sim_png) +
                    theme(
                        legend.position = input$legendPos_sim_png
                    ) +
                    labs(colour = input$legendTitle_sim_png)
                
                if (input$plot_scale == "sqrt") {
                    
                    p <- p + scale_y_sqrt()
                    
                } else if (input$plot_scale == "log") {
                    
                    p <- p + scale_y_log10()
                    
                } else {
                    p <- p
                }
                
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
            
            # browser()
            
            d <- my_data()
            n <- nrow(d)
            
            my_fits <- all_fits()
            
            ## AIC calculation
            
            my_AIC <- lapply(my_fits, function(this_model) {
                
                ## Normal AIC
                
                p <- length(coef(this_model$fit))
                
                sigma <- sqrt(this_model$fit$ssr/this_model$fit$df.residual)
                
                lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*this_model$fit$ssr
                
                AIC <- 2*p - 2*lL
                
                ## Calculate the penalty
                
                k <- 2
                penalty <- (k*p^2 + k*p)/(n - p - 1)
                
                ## Return
                
                AIC + penalty
                
            }) %>%
                unlist()
            
            ## df
            
            dfs <- my_fits %>%
                map_dbl(., ~ n - length(coef(.$fit)))
            
            ## scales 
            
            my_scales <- my_fits %>%
                map_chr(., ~ .$scale)
            
            ## ME and RMSE
            
            my_fits %>%
                map(., ~ tibble(res = residuals(.$fit))) %>%
                map(., ~ mutate(., res2 = res^2)) %>%
                imap_dfr(., ~ mutate(.x, model = .y)) %>%
                group_by(model) %>%
                summarize(ME = mean(res),
                          MSE = mean(res2),
                          RMSE = sqrt(MSE),
                          SSE = sum(res2)
                ) %>%
                ungroup() %>%
                mutate(df = dfs, AIC = my_AIC) %>%
                mutate(SER = SSE/df) %>%
                select(model, df, AIC, ME, RMSE) %>%
                arrange(AIC) %>%
                print()
            
            
        })
        
        ## Parameter estimates -------------------------------------------------
        
        output$plot_parameters <- renderPlot({
            
            validate(need(all_fits(), message = ""))
            
            all_fits() %>%
                map(., 
                    ~ summary(.$fit)$par
                ) %>%
                map(as.data.frame) %>%
                map(., ~ rownames_to_column(., "par")) %>%
                imap_dfr(.,
                         ~ mutate(.x, model = .y)
                ) %>%
                select(model, par, estimate = Estimate, std.err = `Std. Error`) %>%
                ggplot(aes(x = model, y = estimate)) +
                geom_point() +
                geom_errorbar(aes(ymin = estimate - std.err, ymax = estimate + std.err)) +
                facet_wrap("par", scales = "free") +
                theme_gray() +
                theme(axis.text = element_text(size = 14),
                      axis.title = element_text(size = 16),
                      strip.text = element_text(size = 14)
                ) +
                xlab("") + ylab("Estimate")
            
        })
        
        output$table_parameters <- renderTable({
            
            validate(need(all_fits(), message = ""))
            
            all_fits() %>%
                map(., 
                    ~ summary(.$fit)$par
                ) %>%
                map(as.data.frame) %>%
                map(., ~ rownames_to_column(., "par")) %>%
                imap_dfr(.,
                         ~ mutate(.x, model = .y)
                ) %>%
                select(model, par, estimate = Estimate, std.err = `Std. Error`)
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
                
                validate(need(all_fits(), message = ""))
                
                all_fits() %>%
                    map(., 
                        ~ summary(.$fit)$par
                    ) %>%
                    map(as.data.frame) %>%
                    map(., ~ rownames_to_column(., "par")) %>%
                    imap_dfr(.,
                             ~ mutate(.x, model = .y)
                    ) %>%
                    select(model, par, estimate = Estimate, std.err = `Std. Error`) %>%
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
                
                p <- all_fits() %>%
                    map(., 
                        ~ summary(.$fit)$par
                    ) %>%
                    map(as.data.frame) %>%
                    map(., ~ rownames_to_column(., "par")) %>%
                    imap_dfr(.,
                             ~ mutate(.x, model = .y)
                    ) %>%
                    select(model, par, estimate = Estimate, std.err = `Std. Error`) %>%
                    ggplot(aes(x = model, y = estimate)) +
                    geom_point() +
                    geom_errorbar(aes(ymin = estimate - std.err, ymax = estimate + std.err)) +
                    facet_wrap("par", scales = "free") +
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
            
            d <- my_data()
            
            all_fits() %>%
                map(., ~ tibble(X = d$X, res = residuals(.$fit))) %>%
                imap_dfr(., ~ mutate(.x, model = .y)) %>%
                ggplot(aes(x = X, y = res, colour = model)) +
                geom_point() +
                geom_smooth(se = FALSE) +
                xlab("X") + ylab("Residual") +
                theme_gray() +
                theme(legend.title = element_blank()) +
                geom_hline(yintercept = 0, linetype = 2)
            
        })
        
        output$table_residuals <- renderTable({
            
            validate(need(all_fits(), message = ""))
            
            d <- my_data()
            
            all_fits() %>%
                map(., ~ tibble(X = d$X, res = residuals(.$fit))) %>%
                imap_dfr(., ~ mutate(.x, model = .y, i = row_number())) %>%
                select(model, X, residual = res, i) %>%
                pivot_wider(names_from = "model", values_from = "residual", id_expand = TRUE) %>%
                na.omit() %>%
                select(-i)
        })
        
        
    })
    
}

## test ------------------------------------------------------------------------

test_module_secondary_fit <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_secondary_fit_ui("test")
            )
        ),
        server = function(input, output) {
            module_secondary_fit_server("test")
        }
    )
    
}







