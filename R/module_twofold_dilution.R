library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)

library(plotly)
library(DT)
library(shinyWidgets)

## ui --------------------------------------------------------------------------

module_twofold_ui <- function(id) {
    
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
                title = "OD data",
                status = "primary",
                footer = downloadLink(NS(id, "download_example"), "Download example"),
                fileInput(NS(id, "file"), "CSV file"),
                radioButtons(NS(id, "sep"), "Separator",
                             c(Comma = ",", Semicolon = ";", Tab = "\t"), ","),
                radioButtons(NS(id, "dec"), "Decimal Point",
                             c(Point = ".", Comma = ","), ".")
            ),
            bs4Card(
                title = "",
                status = "primary",
                plotOutput(NS(id, "plot_input"))
            )
        ),
        fluidRow(
            bs4Card(
                title = "Pre-filters",
                status = "primary",
                numericInput(NS(id, "target_OD"), "Target OD", .25, min = 0, step = .1),
                hr(),
                awesomeCheckbox(NS(id, "add_time_filter"), "Filter by a minimum time?", value = FALSE),
                conditionalPanel(
                    ns = NS(id),
                    condition = "input.add_time_filter",
                    # fluidRow(
                    numericInput(NS(id, "time_filter"), "Min. time", 0, min = 0)
                    # )
                ),
                awesomeCheckbox(NS(id, "add_dil_filter"), "Filter by a maximum dilution?", value = FALSE),
                conditionalPanel(
                    ns = NS(id),
                    condition = "input.add_dil_filter",
                    # fluidRow(
                    numericInput(NS(id, "dil_filter"), "Max. dil", 6, min = 1)
                    # )
                ),
                awesomeCheckbox(NS(id, "exclude_cond"), "Exclude conditions?", value = FALSE),
                conditionalPanel(
                    ns = NS(id),
                    condition = "input.exclude_cond",
                    pickerInput(NS(id, "excluded_conditions"),
                                label = "",
                                choices = c("aa", "bb", "ab"),
                                multiple = TRUE,
                                options = list(`live-search` = TRUE)
                                )
                )
            ),
            bs4TabCard(
                maximizable  = TRUE,
                tabPanel(
                    "Conditions",
                    tableOutput(NS(id, "table_conditions"))
                ),
                tabPanel(
                    "OD-curves",
                    awesomeCheckbox(NS(id, "separate_cond"), "Separate by condition?", value = FALSE),
                    plotOutput(NS(id, "cuts_od_curves"))
                ),
                tabPanel(
                    "TTDs", 
                    plotOutput(NS(id, "pre_TTDs"))
                ),
                tabPanel(
                    "Linear approx",
                    actionButton(NS(id, "go_premodel"), label = "Calculate"),
                    DTOutput(NS(id, "prelim_TTDS"))
                )
            )
        ),
        # fluidRow(
        #     bs4Card(
        #         title = "Preliminary results",
        #         status = "success",
        #         actionButton(NS(id, "go_premodel"), label = "Calculate"),
        #         tableOutput(NS(id, "prelim_TTDS"))
        #     ),
        #     bs4Card(
        #         title = "Selection",
        #         status = "primary"
        #     )
        # ),
        fluidRow(
            bs4Card(
                title = "Model fitting",
                status = "primary",
                footer = tagList(
                    numericInput(NS(id, "min_point"), "Minimum number of TTDs", 4, min = 3),
                    actionButton(NS(id, "fit_model"), "Fit model",
                                 outline = TRUE, flat = FALSE,
                                 status = "primary"
                    )
                ),
                radioButtons(NS(id, "model_type"), "Type of model",
                             choices = list(`serial dilution` = "serial",
                                            `serial dilution with lambda` = "serial_lambda"
                                            )
                             ),
                numericInput(NS(id, "dil_base"), "Dilution factor", 2, min = 0),
                conditionalPanel(
                    ns = NS(id),
                    condition = "input.model_type == 'serial_lambda'",
                    # fluidRow(
                    numericInput(NS(id, "logC"), "Concentration at well with dilution 0 (log CFU/mL)", 
                                 4, min = 0, step = .1),
                    numericInput(NS(id, "target_logN"), "Concentration at detection OD (log CFU/mL)", 
                                 6.2, min = 0, step = .1)
                    
                    # )
                )
            ),
            bs4TabCard(
                title = "Estimates",
                footer = tagList(
                    fluidRow(
                        column(12, align = "right",
                               downloadButton(NS(id, "download_par_estimates"), "",
                                              icon = icon("download"),
                                              outline = TRUE,
                                              status = "success",
                                              flat = FALSE
                               )
                        )
                    )
                ),
                tabPanel(
                    "TTDs",
                    tableOutput(NS(id, "out_TTDs"))
                ),
                tabPanel(
                    "Parameter table", 
                    tableOutput(NS(id, "out_pars"))
                ),
                tabPanel(
                    "Parameter plot", 
                    plotOutput(NS(id, "plot_pars"))
                )
            )
        )
        
    )
    
}

## helpers ---------------------------------------------------------------------

serial_dilution_method <- function(my_ttds, dil_base = 2, 
                                   start_a = 15, start_mu = .3,
                                   min_points = 6
                                   ) {
    
    # browser()
    
    my_ttds %>%
        mutate(x = log10(dil_base)*dil) %>%
        filter(!is.na(TTD)) %>%
        mutate(n = n(), .by = cond) %>%
        filter(n >= min_points) %>%
        split(.$cond) %>% 
        map(.,
            ~   nls(TTD ~ a + x/mu, data = .,
                    start = list(a = start_a, mu = start_mu)
            )
        )
}

serial_dilution_method_lambda <- function(my_ttds, dil_base = 2, 
                                          start_lambda = 15, start_mu = 1e-3, 
                                          log_Ndet = 2.5, logC = 4, min_points = 6) {
    my_ttds %>%
        mutate(x = log10(dil_base)*dil) %>%
        filter(!is.na(TTD)) %>%
        mutate(n = n(), .by = cond) %>%
        filter(n > min_points) %>%
        split(.$cond) %>%
        map(.,
            ~   nls(TTD ~ (lambda + log_Ndet/mu - logC/mu) + x/mu, data = .,
                    start = list(lambda = start_lambda, mu = start_mu)
            )
        )
}

## server ----------------------------------------------------------------------

module_twofold_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## help page
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Estimation of the growth rate from OD using the serial dilution method", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p(
                                    paste("This module implements the serial dilution method for estimating growth parameters from optical density data.",
                                          "This method is based on paralel experiments where the OD of several wells is measured for different initial concentrations.",
                                          "Please see Biesta-Peters et al. 2010; 10.1128/AEM.02336-09 for further details.")
                                ),
                                h3("Uploading data"),
                                p(
                                    paste("The data must be uploaded as a .csv file with an arbitrary number of columns.",
                                          "One column must be named 't' and include the elapsed time of the experiment.",
                                          "The other must be named as 'condition_dilution-number'. It is important to include",
                                          "the dilution number so the serial dilution method can be calculated.",
                                          "For instance 'cond1_0' and 'cond1_1' would be considered as the 0-fold and 1-fold",
                                          "dilution for condition 'cond1'.")
                                ),
                                p(downloadLink(NS(id, "downloadData"), "Download an example"))
                            )
                )
            )
            
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                "example_od.csv"
            },
            content = function(file) {
                file.copy("example_od.csv", file)
            }
        )
        
        output$download_example <- downloadHandler(
            filename = function() {
                "example_od.csv"
            },
            content = function(file) {
                file.copy("example_od.csv", file)
            }
        )
        
        ## Loading the od data

        od_data <- reactive({
            
            # browser()
            
            file <- input$file
            ext <- tools::file_ext(file$datapath)
            
            validate(need(ext == "csv", "Please upload a csv file"))
            
            d <- read_delim(file$datapath,
                            delim = input$sep,
                            locale = locale(decimal_mark = input$dec)
                            )
            
            # d <- read.table(file$datapath,
            #            header = TRUE,
            #            sep = input$sep,
            #            dec = input$dec,
            #            stringsAsFactors = FALSE)
            
            if (!"t" %in% names(d)) {
                
                showModal(
                    modalDialog(title = "Error loading the data",
                                "A column must be named 't'"
                    )
                )
                
                safeError("Error loading the data")
                
            }
            
            if (ncol(d) < 2) {
                
                showModal(
                    modalDialog(title = "Error loading the data",
                                "The data must contain at least 2 columns"
                    )
                )
                
                safeError("Error loading the data")
                
            }
            
            d
        })
        
        ## Update the pickerInput for conditions when the data is loaded
        
        observeEvent(od_data(), {

            validate(need(od_data(), message = ""))

            my_conds <- names(od_data())
            my_conds <- str_split(my_conds, pattern = "_", n = 2, simplify = TRUE)[,1]
            my_conds <- my_conds[-1]
            
            updatePickerInput(session = session,
                              "excluded_conditions",
                              choices = unique(my_conds)
                              )
            
            
        })
        
        ## plotting of the OD data
        
        output$plot_input <- renderPlot({
            
            validate(need(od_data(), message = ""))
            
            od_data() %>%
                pivot_longer(-t, names_to = "cond", values_to = "od") %>%
                ggplot() + 
                geom_line(aes(x = t, y = od, colour = cond)) +
                theme(legend.position = "none")
        })
        
        ## summary of the conditions
        
        output$table_conditions <- renderTable({
            
            validate(need(od_data(), message = ""))
            
            tibble(x = names(od_data())) %>%
                separate(x, into = c("Condition", "dil"), sep = "_") %>%
                summarize(Wells = n(), .by = Condition) %>%
                filter(Condition != "t")
            
        })
        
        ## pre-filters
        
        output$cuts_od_curves <- renderPlot({
            
            validate(need(od_data(), message = ""))
            
            if (input$separate_cond) {
                p <- od_data() %>%
                    pivot_longer(-t, names_to = "cond", values_to = "od") %>%
                    separate(cond, into = c("cond", "dil"), sep = "_") %>%
                    ggplot() + 
                    geom_line(aes(x = t, y = od, colour = dil)) +
                    # geom_hline(yintercept = input$target_OD) +
                    facet_wrap("cond") +
                    theme(legend.position = "none")
            } else {
                
                p <- od_data() %>%
                    pivot_longer(-t, names_to = "cond", values_to = "od") %>%
                    ggplot() + 
                    geom_line(aes(x = t, y = od, colour = cond)) +
                    # geom_hline(yintercept = input$target_OD) +
                    theme(legend.position = "none")
                
            }
            
            if (input$add_dil_filter) {
                
                l <- od_data() %>%
                    pivot_longer(-t, names_to = "cond", values_to = "od") %>%
                    separate(cond, into = c("cond", "dil"), sep = "_") %>%
                    filter(dil > input$dil_filter) %>%
                    geom_point(aes(x = t, y = od), 
                               colour = "darkgrey", size = 2, shape = 1,
                               data = .)
                
                p <- p + l
                
            }
            
            if (input$add_time_filter) {

                p <- p + geom_vline(xintercept = input$time_filter, linetype = 2)
            }
            

            
            p + geom_hline(yintercept = input$target_OD, linetype = 2)
            
        })
        
        ## pre-ttds
        
        output$pre_TTDs <- renderPlot({
            
            validate(need(od_data(), message = ""))
            
            d <- od_data() 
            
            if (input$add_time_filter) {
                d <- d %>% filter(t > input$time_filter)
            }
            
            my_ttds <-  d %>%
                pivot_longer(-t, names_to = "cond", values_to = "od") %>%
                split(.$cond) %>%
                imap_dfr(.,
                         ~ tibble(cond = .y,
                                  TTD = approx(x = .x$od, y = .x$t, xout = input$target_OD)$y
                         )
                ) %>%
                separate(cond, into = c("cond", "dil"), sep = "_") %>%
                mutate(dil = as.numeric(dil))
            
            if (input$add_dil_filter) {
                my_ttds <- my_ttds %>%
                    mutate(out = ifelse(dil > input$dil_filter, "out", "in"))
            } else {
                my_ttds <- my_ttds %>%
                    mutate(out = "in")
            }
            
            p <- my_ttds %>%
                ggplot() +
                geom_point(aes(x = dil, y = TTD, colour = out)) +
                facet_wrap("cond") +
                scale_colour_manual(values = c("black", "red"))
            
            p + geom_smooth(aes(x = dil, y = TTD),
                            data = filter(my_ttds, out == "in"),
                            method = "lm")
            
        })
        
        ## Preliminary mus
        
        prelim_models <- reactiveVal()
        
        observeEvent(input$go_premodel, {
            
            validate(need(od_data(), message = ""))
            
            # browser()
            
            d <- od_data() 
            
            if (input$add_time_filter) {
                d <- d %>% filter(t > input$time_filter)
            }
            
            my_ttds <-  d %>%
                pivot_longer(-t, names_to = "cond", values_to = "od") %>%
                split(.$cond) %>%
                imap_dfr(.,
                         ~ tibble(cond = .y,
                                  TTD = approx(x = .x$od, y = .x$t, xout = input$target_OD)$y
                         )
                ) %>%
                separate(cond, into = c("cond", "dil"), sep = "_") %>%
                mutate(dil = as.numeric(dil))
            
            out <- my_ttds %>%
                filter(!is.na(dil)) %>%
                split(.$cond) %>%
                map(.,
                    ~ lm(TTD ~ dil, data = .)
                )
            
            prelim_models(out)
            
        })
        
        output$prelim_TTDS <- renderDT({
            
            models <- prelim_models()
            
            models %>% 
                map(., ~ coef(.)[["dil"]]) %>% 
                imap_dfr(., 
                         ~ tibble(cond = .y, mu = .x)
                         )
            

            
        })
        
        ## Model fitting -------------------------------------------------------

        my_ttds <- reactiveVal()
        my_models <- reactiveVal()
        
        observeEvent(input$fit_model, {
            
            validate(need(od_data(), message = ""))
            
            # browser()
            
            ## Calculate ttds
            
            d <- od_data()
            
            # browser()
            
            if (input$add_time_filter) {
                d <- d %>% filter(t > input$time_filter)
            }
            
            if (input$exclude_cond) {
                
                out_conds <- input$excluded_conditions
                
                if (length(out_conds) > 0) {

                    d <- select(d, -matches(out_conds))
                        
                }
                
            }
            
            my_ttds <-  d %>%
                pivot_longer(-t, names_to = "cond", values_to = "od") %>%
                split(.$cond) %>%
                imap_dfr(.,
                         ~ tibble(cond = .y,
                                  TTD = approx(x = .x$od, y = .x$t, xout = input$target_OD)$y
                         )
                ) %>%
                separate(cond, into = c("cond", "dil"), sep = "_") %>%
                mutate(dil = as.numeric(dil)) %>%
                filter(!is.na(dil))
            
            if (input$add_dil_filter) {
                my_ttds <- my_ttds %>% filter(dil <= input$dil_filter)
            } 
            
            my_ttds(my_ttds)
            
            ## Estimate mu

            if (input$model_type == "serial") {
                
                out <- serial_dilution_method(my_ttds, dil_base = input$dil_base, 
                                              start_a = 100, start_mu = 1e-3,
                                              min_points = input$min_point
                                              )
                
            } else if (input$model_type == "serial_lambda") {
                
                out <- serial_dilution_method_lambda(my_ttds, dil_base = input$dil_base, 
                                                     start_lambda = 15, start_mu = 1e-3, 
                                                     log_Ndet = input$target_logN, 
                                                     logC = input$logC,
                                                     min_points = input$min_point)  
                
            } else {
                safeError("I made a mess")
            }
            
            my_models(out)
            
        })
        
        ## Output of TTDs
        
        output$out_TTDs <- renderTable({
            
            validate(need(my_ttds(), message = ""))
            
            my_ttds() %>%
                pivot_wider(names_from = "dil", values_from = "TTD") %>%
                rename(Condition = "cond")
            
        })
        
        ## Output of the model
        
        output$out_pars <- renderTable({
            
            validate(need(my_models(), message = ""))
            
            my_models() %>% 
                map(., ~ summary(.)$parameters) %>%
                map(., ~ as_tibble(., rownames = "Parameter")) %>%
                imap_dfr(., ~ mutate(.x, cond = .y)) %>%
                select(-`t value`, -`Pr(>|t|)`) %>%
                select(Condition = "cond", everything())
            
        })
        
        output$plot_pars <- renderPlot({
            
            
            validate(need(my_models(), message = ""))

            my_models() %>% 
                map(., ~ summary(.)$parameters) %>%
                map(., ~ as_tibble(., rownames = "par")) %>%
                imap_dfr(., ~ mutate(.x, cond = .y)) %>%
                ggplot(aes(x = cond, y = Estimate)) +
                geom_point() +
                geom_errorbar(aes(ymin = Estimate - `Std. Error`,
                                 ymax = Estimate + `Std. Error`)) +
                facet_wrap("par", scales = "free") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
        })
        
        ## Download parameter table
        
        output$download_par_estimates <- downloadHandler(
            filename = function() {
                "biogrowth_par_estimates.csv"
            },
            content = function(file) {

                my_models() %>% 
                    map(., ~ summary(.)$parameters) %>%
                    map(., ~ as_tibble(., rownames = "Parameter")) %>%
                    imap_dfr(., ~ mutate(.x, cond = .y)) %>%
                    select(-`t value`, -`Pr(>|t|)`) %>%
                    select(Condition = "cond", everything()) %>%
                    write_tsv(., file = file)
                
            }
        )
        
    })
    
}

## test ------------------------------------------------------------------------

test_module_twofold <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_twofold_ui("test")
            )
        ),
        server = function(input, output) {
            module_twofold_server("test")
        }
    )
    
}







