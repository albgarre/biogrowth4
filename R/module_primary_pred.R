
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_primary_pred_ui <- function(id) {
    
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
                title = "Growth model", # tagList(h3("Model parameters")),
                status = "primary",
                solidHeader = FALSE,
                headerBorder = TRUE,
                collapsible = FALSE,
                footer = tagList(
                    textInput(NS(id, "sim_name"), "Simulation name", "Model I"),
                    actionButton(NS(id, "go"), "Add/Edit prediction",
                                 outline = TRUE, flat = FALSE,
                                 status = "primary"
                    )
                    # actionButton(NS(id, "clean"), "Clear",
                    #              outline = TRUE, flat = FALSE,
                    #              status = "secondary"
                    # )
                ),
                pickerInput(NS(id, "model_name"), "", # "Primary model",
                            choices = primary_model_data() %>% set_names(., .) %>% as.list()
                ),
                uiOutput(NS(id, "parameters")),
                pickerInput(NS(id, "scale_mu"), 
                            label = "Scale for mu: ",
                            choices = c("log10", "ln", "base2"),
                            width = "50%"),
                pickerInput(NS(id, "scale_logN"), 
                            label = "Scale for logN: ",
                            choices = c("log10", "ln", "base2"),
                            width = "50%"),
                hr(),
                numericInput(NS(id, "sim_time"), "Max. time", 80)
                # pickerInput(NS(id, "scale_mu"), 
                #             label = "Scale for mu",
                #             choices = c("log10", "ln", "base2"))
            ),
            bs4Card(
                title = NULL,
                status = "success",
                solidHeader = FALSE,
                headerBorder = FALSE,
                maximizable = TRUE,
                # icon = icon("info"),
                footer = tagList(
                    fluidRow(
                        column(6,
                               actionButton(NS(id, "clean"), "Clear",
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
                plotlyOutput(NS(id, "plot_curves")) %>% withSpinner(type = 7, hide.ui = FALSE),
                dropdownMenu = boxDropdown(
                    boxDropdownItem(
                        textInput(NS(id, "xlabel"), "x-label", "Time (h)")
                    ),
                    boxDropdownItem(
                        textInput(NS(id, "ylabel"), "y-label", "Microbial concentration (log CFU/ml)")
                    )
                )
                
            )
        ),
        fluidRow(
            bs4Card(
                title = "Time to X log concentration",
                solidHeader = TRUE,
                collapsed = TRUE,
                status = "warning",
                numericInput(NS(id, "target_count"), "Microbial count (log CFU/g)", 4),
                tableOutput(NS(id, "table_times"))
            ),
            bs4Card(
                title = "log concentration at time X",
                solidHeader = TRUE,
                collapsed = TRUE,
                status = "warning",
                numericInput(NS(id, "target_time"), "Time (h)", 35),
                tableOutput(NS(id, "table_counts"))
            )
        ),
        hr(),
        tableInput_module_ui(NS(id, "val_data"), box_title = "Independent data",
                             status = "primary", status_2 = "primary",
                             solid = FALSE, solid_2 = FALSE,
                             collapsed = FALSE, collapsed_2 = FALSE
                             ),
        fluidRow(
            bs4TabCard(
                solidHeader = TRUE, collapsed = TRUE,
                width = 6,
                status = "warning",
                title = "Visualizations", side = "right",
                maximizable = TRUE,
                type = "tabs",
                tabPanel(
                    "Predictions",
                    status = "warning",
                    plotOutput(NS(id, "pred_vs_data"))
                ),
                tabPanel(
                    "Residuals plot",
                    status = "warning",
                    plotOutput(NS(id, "res_plot"))
                ),
                tabPanel(
                    "Histograms",
                    status = "warning",
                    plotOutput(NS(id, "res_hist"))
                )
            ),
            bs4TabCard(
                solidHeader = TRUE, collapsed = TRUE,
                width = 6,
                status = "warning",
                # solidHeader = TRUE,
                type = "tabs",
                title = "Numerical analysis", side = "right",
                tabPanel(
                    "Residuals table",
                    status = "warning",
                    tableOutput(NS(id, "res_table"))
                ),
                tabPanel(
                    "Residuals indexes",
                    status = "warning",
                    tableOutput(NS(id, "res_indexes"))
                )
            )
        )
    )
    
}

## Server ----------------------------------------------------------------------

module_primary_pred_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Help page -----------------------------------------------------------
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Model predictions based on primary growth models", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p(
                                    paste("This module includes functions for making growth predictions using only primary growth models.",
                                          "They are intended for situations where the environmental conditions are constant or their effect is disregarded.",
                                          "Otherwise, it is recommended to use the 'Dynamic' module, which also includes secondary models.")
                                ),
                                h3("Comparing model predictions against independent data"),
                                p(
                                    paste(
                                        "The module can also compare the model predictions.",
                                        "This can be used, for instance, for model validation.",
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
        
        ## Dynamic parameter selection -----------------------------------------
        
        par_map <- tribble(
            ~par, ~label, ~value, 
            "logN0", "logN0 (log CFU/g)", 2, 
            "lambda", "lambda (h)", 20,
            "mu", "mu (log CFU/h)", .5,
            "C", "C (log CFU)", 6,
            "logNmax", "log Nmax (log CFU)", 8, 
            "nu", "nu (·)", 1
        )
        
        make_input <- function(par_name) {
            
            par_data <- par_map %>%
                filter(par == par_name)
            
            fluidRow(
                column(width = 6,
                       numericInput(NS(id, par_name), par_data$label, par_data$value)
                )
            )
        }
        
        output$parameters <- renderUI({
            
            par_names <- primary_model_data(input$model_name)$par
            
            par_names %>%
                map(.,
                    ~ make_input(.)
                )
            
        })
        
        ## Growth simulations --------------------------------------------------
        
        growth_curves <- reactiveVal()
        
        observeEvent(input$clean, {
            growth_curves(NULL)
        })
        
        observeEvent(input$go, {
            
            out <- growth_curves()
            
            ## Get the parameters
            
            par_names <- primary_model_data(input$model_name)$pars

            p <- list()
            
            for (i in par_names) {
                
                p[[i]] <- input[[i]]
                
            }
            
            ## Make the prediction
            
            p$model <- input$model_name
            
            my_time <- seq(0, input$sim_time, length = 1000)
            
            scale_mu <- switch(input$scale_mu,
                               `log10` = 10,
                               `ln` = exp(1),
                               `base2` = 2
                               )
            
            scale_logN <- switch(input$scale_logN,
                                 `log10` = 10,
                                 `ln` = exp(1),
                                 `base2` = 2
            )

            my_prediction <- predict_growth(my_time, p, environment = "constant",
                                            logbase_mu = scale_mu,
                                            logbase_logN = scale_logN
                                            )
            
            ## Add it to the output
            
            out[[input$sim_name]] <- my_prediction$simulation %>%
                mutate(sim = input$sim_name)
            
            
            growth_curves(out)
            
        })
        
        ## Output
        
        output$plot_curves <- renderPlotly({
            
            validate(
                need(growth_curves(), "")
            )
            
            p <- growth_curves() %>%
                bind_rows() %>%
                ggplot() +
                geom_line(aes(x = time, y = logN, colour = sim)) +
                xlab(input$xlabel) + ylab(input$ylabel) +
                theme(legend.title = element_blank())
            
            ggplotly(p) %>% config(displayModeBar = FALSE)
        })
        
        ## Time to size --------------------------------------------------------
        
        output$table_times <- renderTable({
            
            
            validate(
                need(growth_curves(), "")
            )
            
            growth_curves() %>%
                map(.,
                    ~ approx(x = .$logN, y = .$time, xout = input$target_count)$y
                ) %>%
                # print()
                imap_dfr(.,
                         ~ tibble(condition = .y, `log count (log CFU/g)` = input$target_count,
                                  Time = .x)
                )
            
        })
        
        ## Size at time --------------------------------------------------------
        
        output$table_counts <- renderTable({
            
            
            validate(
                need(growth_curves(), "")
            )
            
            growth_curves() %>%
                map(.,
                    ~ approx(x = .$time, y = .$logN, xout = input$target_time)$y
                ) %>%
                # print()
                imap_dfr(.,
                         ~ tibble(condition = .y, Time = input$target_time, `log count (log CFU/g)` = .x)
                )
            
        })
        
        ## Validation data -----------------------------------------------------
        
        my_data <- tableInput_module_server("val_data",
                                            col_names = c("time", "logN"),
                                            xvar = "time", yvar = "logN",
                                            default_data = tibble(time = c(0, 25, 50, 75, 100),
                                                                  logN = c(2, 2.5, 7, 8, 8)
                                            )
        )
        
        ## Residuals calculation -----------------------------------------------
        
        my_res <- eventReactive(my_data(), {
            
            validate(
                need(growth_curves(), ""),
                need(my_data(), "")
            )
            
            growth_curves() %>%
                map(., ~ select(., time, logN)) %>%
                map(as.data.frame) %>%
                map(.,
                    ~ modCost(model = .,
                              obs = as.data.frame(my_data())
                    )
                )
            
        })
        
        ## Residuals plots -----------------------------------------------------
        
        output$pred_vs_data <- renderPlot({
            
            validate(
                need(growth_curves(), ""),
                need(my_data(), "")
            )
            
            growth_curves() %>%
                bind_rows() %>%
                ggplot() +
                geom_line(aes(x = time, y = logN, colour = sim)) +
                xlab(input$xlabel) + ylab(input$ylabel) +
                theme(legend.title = element_blank()) +
                geom_point(aes(x = time, y = logN),
                           data = my_data(),
                           inherit.aes = FALSE)
            
        })
        
        output$res_hist <- renderPlot({
            validate(
                need(my_res(), "")
            )
            
            my_res() %>%
                map(., ~ .$residuals) %>%
                imap_dfr(., ~ mutate(.x, condition = .y)) %>%
                ggplot() +
                geom_histogram(aes(res, fill = condition), position = "dodge") +
                geom_vline(xintercept = 0, linetype = 2) +
                theme(legend.title = element_blank()) +
                xlab("Residual (log CFU/g)") +
                ylab("Count")
            
        })
        
        output$res_plot <- renderPlot({
            
            validate(
                need(my_res(), "")
            )
            
            my_res() %>%
                map(., ~ .$residuals) %>%
                imap_dfr(., ~ mutate(.x, condition = .y)) %>%
                ggplot() +
                geom_point(aes(x = x, y = res, colour = condition)) +
                geom_hline(yintercept = 0, linetype = 2) +
                theme(legend.title = element_blank()) +
                xlab("Time (h)") + ylab("Residual (log CFU/g)")
        })
        
        ## Residual tables
        
        output$res_table <- renderTable({
            
            validate(
                need(my_res(), "")
            )
            
            my_res() %>%
                map(., ~ .$residuals) %>%
                imap_dfr(., ~ mutate(.x, condition = .y)) %>%
                select(condition, time = x, observed = obs, predicted = mod, residual = res)
        })
        
        output$res_indexes <- renderTable({
            
            validate(
                need(my_res(), "")
            )
            
            my_res() %>%
                map(., ~ .$residuals) %>%
                imap_dfr(., ~ mutate(.x, condition = .y)) %>%
                mutate(res2 = res^2) %>%
                group_by(condition) %>%
                summarize(
                    ME = mean(res),
                    RMSE = sqrt(mean(res2))
                )
        })
        
        ## Download plot -------------------------------------------------------
        
        dataModal <- function() {
            modalDialog(
                easyClose = TRUE,
                fluidRow(
                    column(6,  # Text file 
                           # h3("Download as text"),
                           downloadBttn(NS(id, "download_sim_text"), "Download as text",
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
            filename = "biogrowth_simulations.csv",
            content = function(file) {
                
                growth_curves() %>%
                    bind_rows() %>%
                    pivot_wider(names_from = sim, values_from = logN) %>%
                    write_delim(., 
                                delim = input$delim_sim_download,
                                file = file)
                
            }
        )
        
        output$download_sim_png <- downloadHandler(
            filename = "biogrowth_curve.png",
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

                p <- growth_curves() %>%
                    bind_rows() %>%
                    ggplot() +
                    geom_line(aes(x = time, y = logN, colour = sim)) +
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
        
    })
    
}

## test ------------------------------------------------------------------------

test_module_primary_pred <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_primary_pred_ui("test")
            )
        ),
        server = function(input, output) {
            module_primary_pred_server("test")
        }
    )
    
}




