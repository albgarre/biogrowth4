
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_pred_dynamic_ui <- function(id) {
  
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
        title = "Environmental conditions",
        status = "primary",
        footer = downloadLink(NS(id, "download_example"), "Download example"),
        fileInput(NS(id, "excel_file"), "Excel file"),
        # textInput(NS(id, "excel_sheet"), "Sheet name", "Sheet1"),
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
        title = "Primary model",
        status = "primary",
        numericInput(NS(id, "muopt"), "mu_opt (log10 CFU/h)", 0.5, min = 0),
        numericInput(NS(id, "logNmax"), "logNmax (log10 CFU/g)", 8),
        numericInput(NS(id, "logN0"), "logN0 (log10 CFU/g)", 0),
        numericInput(NS(id, "Q0"), "Q0 (·)", 1e-3, min = 0),
        br(),
        pickerInput(NS(id, "scale_mu"), 
                    label = "Scale for mu: ",
                    choices = c("log10", "ln", "base2"),
                    width = "50%"),
        pickerInput(NS(id, "scale_logN"), 
                    label = "Scale for logN: ",
                    choices = c("log10", "ln", "base2"),
                    width = "50%"),
        hr(),
        numericInput(NS(id, "maxtime"), "Total time (h)", 50, min = 0)
      ),
      bs4Card(
        title = "Secondary model",
        status = "primary",
        uiOutput(NS(id, "secondary_pars"))
      )
    ),
    fluidRow(
      bs4Card(
        title = "Simulations",
        status = "success",
        maximizable = TRUE,
        footer = tagList(
          fluidRow(
            column(6,
                   actionButton(NS(id, "clean"), "Clean up",
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
        textInput(NS(id, "sim_name"), "Simulation name", "Model I"),
        actionButton(NS(id, "go"), "Add/Edit simulation",
                     outline = TRUE, flat = FALSE,
                     status = "primary"
                     ),
        hr(),
        plotlyOutput(NS(id, "plot_curves")),
        dropdownMenu = boxDropdown(
          tagList(
            column(12,
                   textInput(NS(id, "xlabel"), "x-label", "Time (h)"),
                   textInput(NS(id, "ylabel"), "y-label", "Microbial concentration (log CFU/ml)")
                   )
          )
          # boxDropdownItem(
          #   textInput(NS(id, "xlabel"), "x-label", "Time (h)")
          # ),
          # boxDropdownItem(
          #   textInput(NS(id, "ylabel"), "y-label", "Microbial concentration (log CFU/ml)")
          # )
        )
      ),
      bs4Card(
        title = "Gamma factors",
        status = "success",
        maximizable = TRUE,
        plotlyOutput(NS(id, "plot_gammas")),
        footer = actionButton(NS(id, "download_gamma_plot"), "",
                              icon = icon("download"),
                              outline = TRUE,
                              status = "success",
                              flat = FALSE
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
                         status = "primary", status_2 = "primary"
    ),
    fluidRow(
      bs4TabCard(
        width = 6,
        collapsed = TRUE,
        solidHeader = TRUE,
        status = "warning",
        type = "tabs",
        title = "Visualizations", side = "right",
        tabPanel(
          "Predictions",
          status = "warning",
          plotOutput(NS(id, "pred_vs_data"))
        ),
        tabPanel(
          "Residual plot",
          status = "warning",
          plotOutput(NS(id, "res_plot"))
        ),
        tabPanel(
          "Histogram",
          status = "warning",
          plotOutput(NS(id, "res_hist"))
        )
      ),
      bs4TabCard(
        width = 6,
        collapsed = TRUE,
        solidHeader = TRUE,
        status = "warning",
        # solidHeader = TRUE,
        type = "tabs",
        title = "Numerical analysis", side = "right",
        tabPanel(
          "Residual table",
          status = "warning",
          tableOutput(NS(id, "res_table"))
        ),
        tabPanel(
          "Residual indexes",
          status = "warning",
          tableOutput(NS(id, "res_indexes"))
        )
      )
    )
    
  )
}

## Server ----------------------------------------------------------------------

module_pred_dynamic_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ## Help page -----------------------------------------------------------
    
    observeEvent(input$show_help, {
      
      showModal(
        session = session,
        modalDialog(title = "Model predictions combining primary and secondary growth models", 
                    easyClose = TRUE,
                    size = "xl",
                    tagList(
                        p(
                            paste("This module includes functions for making growth predictions combining primary and secondary models.",
                                  "The main use case for this module is to simulate the effect of varying environmental conditions.",
                                  "Nonetheless, it can also simulate constant environmental conditions. This can be beneficial in some situations,",
                                  "as models can be defined from the secondary models (see vignettes within the biogrowth package).")
                        ),
                        p(
                            paste(
                                "The module can also compare the model predictions.",
                                "This can be used, for instance, for model validation."
                            )
                            
                        ),
                        h3("Uploading data on the environmental profile"),
                        p("The data must be uploaded as an Excel file."),
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
                                downloadLink(NS(id, "downloadData"), "Download an example"),
                                "with 3 different profiles: one with a single factor, one with 2 factors, and one with constant conditions."
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
              "example_dyna.xlsx"
              # paste("data-", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
              # data <- mtcars
              # write.csv(data, file)
              file.copy("examples_dyna.xlsx", file)
          }
      )
      
      output$download_example <- downloadHandler(
          filename = function() {
              "example_dyna.xlsx"
              # paste("data-", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
              # data <- mtcars
              # write.csv(data, file)
              file.copy("examples_dyna.xlsx", file)
          }
      )
    
    ## Data input ----------------------------------------------------------
    
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
      
      validate(need(excelFile(), message = ""))
      
      read_excel(excelFile()$datapath,
                 sheet = input$excel_sheet,
                 skip = input$excel_skip,
                 col_types = "numeric")
    })
    
    output$plot_input <- renderPlot({
      
      validate(need(excelFile(), message = ""))
      
      excel_frame() %>%
        gather(var, value, -time) %>%
        ggplot(aes(x = time, y = value)) +
        geom_line() +
        geom_point() +
        facet_wrap("var", scales = "free_y") +
        ylab("")
      
    })
    
    # output$download_example <- downloadHandler(
    #     filename = "example_dyna.xlsx",
    #     content = function(file) {
    #         file.copy("example_dyna.xlsx", file)
    #     }
    # )
    
    ## Dynamic secondary model selector ------------------------------------
    
    output$secondary_pars <- renderUI({
      
      validate(need(excelFile(), message = "Upload the environmental conditions first"))
      
      my_names <- excel_frame() %>%
        select(-time) %>%
        names()
      
      lapply(my_names, function(each_name) {
        
        tagList(
          tags$h3(paste("Factor:", each_name)),
          selectInput(NS(id, paste0(each_name, "_model")),
                      "Model type",
                      secondary_model_data()
          ),
          numericInput(NS(id, paste0(each_name, "_xmin")), "Xmin", 0),
          conditionalPanel(
            ns = NS(id),
            condition = paste0("input.", each_name, "_model != 'fullRatkowsky'"),
            numericInput(NS(id, paste0(each_name, "_xopt")), "Xopt", 37)
          ),
          conditionalPanel(
            ns = NS(id),
            condition = paste0("input.", each_name, "_model != 'Zwietering'"),
            numericInput(NS(id, paste0(each_name, "_xmax")), "Xmax", 45),
          ),
          conditionalPanel(
            ns = NS(id),
            condition = paste0("input.", each_name, "_model != 'fullRatkowsky'"),
            numericInput(NS(id, paste0(each_name, "_n")), "n", 1)
          ),
          conditionalPanel(
            ns = NS(id),
            condition = paste0("input.", each_name, "_model == 'fullRatkowsky'"),
            numericInput(NS(id, paste0(each_name, "_c")), "c", 1)
          ),
          tags$hr()
        )
        
      })
      
    })
    
    ## Calculation ---------------------------------------------------------
    
    my_simulations <- reactiveVal(list())
    
    observeEvent(input$clean, {
      
      my_simulations(list())
      my_res(NULL)
      
      
    })
    
    observeEvent(input$go, {
      
      ## Extract primary parameters
      
      primary_pars <- list(mu_opt = input$muopt,
                           Nmax = 10^input$logNmax,
                           N0 = 10^input$logN0,
                           Q0 = input$Q0)
      
      
      ## Extract secondary models
      
      my_factors <- excel_frame() %>%
        select(-time) %>%
        names()
      
      sec_models <- list()
      
      for (i in 1:length(my_factors)) {
        
        factor_name <- my_factors[[i]]
        
        new_model <- list()
        new_model$model <- input[[paste0(factor_name, "_model")]]
        new_model$xmin <- input[[paste0(factor_name, "_xmin")]]
        new_model$xopt <- input[[paste0(factor_name, "_xopt")]]
        new_model$xmax <- input[[paste0(factor_name, "_xmax")]]
        new_model$n <- input[[paste0(factor_name, "_n")]]
        new_model$c <- input[[paste0(factor_name, "_c")]]
        
        sec_models[[factor_name]] <- new_model
        
      }
      
      out <- my_simulations()
      
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
      
      new_sim <- predict_growth(environment = "dynamic",
                                seq(0, input$maxtime, length = 1000),
                                primary_pars,
                                sec_models,
                                excel_frame(),
                                logbase_mu = scale_mu,
                                logbase_logN = scale_logN,
                                check = FALSE  # cheater
      )
      
      # browser()
      
      out[[input$sim_name]] <- new_sim
      my_simulations(out)
      
    })
    
    output$plot_curves <- renderPlotly({
      
      validate(need(length(my_simulations()) > 0, message = ""))
      
      p <- my_simulations() %>%
        map(., ~.$simulation) %>%
        imap_dfr(., ~ mutate(.x, sim = .y)) %>%
        ggplot() +
        geom_line(aes(x = time, y = logN, colour = sim)) +
        theme_gray() +
        theme(legend.title = element_blank()) +
        xlab(input$xlabel) + ylab(input$ylabel)
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
      
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
        
        my_simulations() %>%
          map(., ~.$simulation) %>%
          imap_dfr(., ~ mutate(.x, sim = .y)) %>%
          select(time, sim, logN) %>%
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
        
        p <- my_simulations() %>%
          map(., ~.$simulation) %>%
          imap_dfr(., ~ mutate(.x, sim = .y)) %>%
          select(time, sim, logN) %>%
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

    ## Gamma factors -------------------------------------------------------
    
    output$plot_gammas <- renderPlotly({
      
      validate(need(length(my_simulations()) > 0, message = ""))
      
      p <- my_simulations() %>%
        map(., ~.$gammas) %>%
        map(.,
            ~ pivot_longer(., -time, names_to = "factor", values_to = "gamma")
        ) %>%
        imap_dfr(., ~ mutate(.x, sim = .y)) %>%
        ggplot() +
        geom_line(aes(x = time, y = gamma, colour = sim)) +
        facet_wrap("factor") +
        theme(legend.title = element_blank())
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
      
      
    })
    
    ## Download gamma plot -------------------------------------------------------
    
    dataModal_gamma <- function() {
      modalDialog(
        easyClose = TRUE,
        fluidRow(
          column(6,  # Text file 
                 # h3("Download as text"),
                 downloadBttn(NS(id, "download_gamma_text"), "Download as text",
                              style = "bordered",
                              color = "primary",
                              block = TRUE),
                 br(),
                 radioButtons(NS(id, "delim_gamma_download"), "Separator",
                              choices = list(`tabulation` = "\t",
                                             `comma (,)` = ",",
                                             `semicolon (;)` = ";" 
                              ))
          ),
          column(6,  # Figure
                 # h3("Download as figure"),
                 downloadBttn(NS(id, "download_gamma_png"), "Download as PNG",
                              style = "bordered",
                              color = "primary",
                              block = TRUE),
                 br(),
                 numericInput(NS(id, "width_gamma_png"), "Width (pixels)", 900),
                 numericInput(NS(id, "height_gamma_png"), "height (pixels)", 600),
                 pickerInput(NS(id, "legendPos_gamma_png"), "Legend position",
                             choices = c("top", "right", "left", "bottom", "none")),
                 textInput(NS(id, "legendTitle_gamma_png"), "Legend title", "model"),
                 numericInput(NS(id, "baseSize_gamma_png"), "Base size", 7),
                 selectInput(NS(id, "theme_gamma_png"), "Theme",
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
    
    observeEvent(input$download_gamma_plot, {
      
      showModal(
        session = session,
        dataModal_gamma()
      )
      
    })
    
    output$download_gamma_text <- downloadHandler(
      filename = "biogrowth_gammas.csv",
      content = function(file) {
        
        my_simulations() %>%
          map(., ~.$gammas) %>%
          map(.,
              ~ pivot_longer(., -time, names_to = "factor", values_to = "gamma")
          ) %>%
          imap_dfr(., ~ mutate(.x, sim = .y)) %>%
          write_delim(., 
                      delim = input$delim_gamma_download,
                      file = file)
        
      }
    )
    
    output$download_gamma_png <- downloadHandler(
      filename = "biogrowth_gammas.png",
      content = function(file){
        
        my_theme <- switch(input$theme_gamma_png,
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
        
        p <- my_simulations() %>%
          map(., ~.$gammas) %>%
          map(.,
              ~ pivot_longer(., -time, names_to = "factor", values_to = "gamma")
          ) %>%
          imap_dfr(., ~ mutate(.x, sim = .y)) %>%
          ggplot() +
          geom_line(aes(x = time, y = gamma, colour = sim)) +
          facet_wrap("factor") +
          my_theme(base_size = input$baseSize_gamma_png) +
          theme(
            legend.position = input$legendPos_gamma_png
          ) +
          labs(colour = input$legendTitle_gamma_png)
        
        ggsave(file, plot=p, device = "png",
               width = input$width_gamma_png,
               height = input$height_gamma_png,
               units = "px",
               bg = "white"
        )
      }
    )
    
    ## Time to size --------------------------------------------------------
    
    output$table_times <- renderTable({
      
      
      validate(
        need(my_simulations(), message = "")
      )
      
      my_simulations() %>%
        map(., ~.$simulation) %>%
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
        need(my_simulations(), message = "")
      )
      
      my_simulations() %>%
        map(., ~.$simulation) %>%
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
    
    my_res <- reactiveVal()
    
    update_res <- function() {
      
      validate(
        need(my_simulations(), ""),
        need(my_data(), "")
      )
      
      out <- my_simulations() %>%
        map(., ~.$simulation) %>%
        map(., ~ select(., time, logN)) %>%
        map(as.data.frame) %>%
        map(.,
            ~ modCost(model = .,
                      obs = as.data.frame(my_data())
            )
        )
      
      my_res(out)
      
    }
    
    observeEvent(my_simulations(), {
      update_res()
    })
    
    observeEvent(my_data(), {
      
      update_res()
      
    })
    
    ## Residuals plots -----------------------------------------------------
    
    output$pred_vs_data <- renderPlot({
      
      validate(
        need(my_simulations(), ""),
        need(my_data(), "")
      )
      
      my_simulations() %>%
        map(., ~.$simulation) %>%
        imap_dfr(., ~ mutate(.x, sim = .y)) %>%
        ggplot() +
        geom_line(aes(x = time, y = logN, colour = sim)) +
        # xlab(input$xlabel) + ylab(input$ylabel) +
        theme(legend.title = element_blank()) +
        geom_point(aes(x = time, y = logN),
                   data = my_data(),
                   inherit.aes = FALSE)
      
    })
    
    output$res_hist <- renderPlot({
      validate(
        need(!is.null(my_res()), "")
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
        need(!is.null(my_res()), "")
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
    
    ## Residual tables -----------------------------------------------------
    
    output$res_table <- renderTable({
      
      validate(
        need(!is.null(my_res()), "")
      )
      
      my_res() %>%
        map(., ~ .$residuals) %>%
        imap_dfr(., ~ mutate(.x, condition = .y)) %>%
        select(condition, time = x, observed = obs, predicted = mod, residual = res)
    })
    
    output$res_indexes <- renderTable({
      
      validate(
        need(!is.null(my_res()), "")
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
    
    
  })
  
}

## test ------------------------------------------------------------------------

test_module_pred_dynamic <- function(id) {
  
  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        module_pred_dynamic_ui("test")
      )
    ),
    server = function(input, output) {
      test_module_pred_dynamic("test")
    }
  )
  
}




