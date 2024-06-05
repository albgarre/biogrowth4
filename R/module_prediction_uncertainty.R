
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_pred_unc_ui <- function(id) {
  
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
        title = "Growth model",
        status = "primary",
        footer = tagList(
          numericInput(NS(id, "n_sims"), "Number of simulations", 100),
          actionButton(NS(id, "resetseed"), "Reset seed",
                       outline = TRUE, flat = FALSE,
                       status = "secondary"
          ),
          actionButton(NS(id, "calculate"), "Calculate",
                       outline = TRUE, flat = FALSE,
                       status = "primary"
          )
        ),
        pickerInput(NS(id, "model_name"), "",
                    choices = primary_model_data() %>% set_names(., .) %>% as.list()
        ),
        uiOutput(NS(id, "parameters")),
        fluidRow(
            pickerInput(NS(id, "scale_mu"), 
                        label = "Scale for mu: ",
                        choices = c("log10", "ln", "base2"),
                        width = "50%")
            ),
        fluidRow(
            pickerInput(NS(id, "scale_logN"), 
                        label = "Scale for logN: ",
                        choices = c("log10", "ln", "base2"),
                        width = "50%"),
        ),
        hr(),
        numericInput(NS(id, "sim_time"), "Max. time", 800)
      ),
      bs4TabCard(
        title = "Inputs",
        status = "primary",
        bs4TabItem(
          title = "Parameters",
          tabName = "pars_tab",
          plotOutput(NS(id, "plot_pars")) %>% withSpinner(type = 6)
        ),
        bs4TabItem(
          title = "Curves",
          tabName = "curves_tab",
          plotOutput(NS(id, "plot_curves")) %>% withSpinner(type = 6)
        )
      )
    ),
    fluidRow(
      column(6, style='padding:0px;',
             bs4Card(
               width = 12,
               title = "Prediction",
               status = "success",
               maximizable = TRUE,
               plotlyOutput(NS(id, "plot_simulation")) %>% withSpinner(type = 7, hide.ui = FALSE),
               dropdownMenu = boxDropdown(
                 column(12,
                        textInput(NS(id, "xlabel"), "x-label", "Time (h)"),
                        textInput(NS(id, "ylabel"), "y-label", "Microbial concentration (log CFU/ml)"),
                        numericInput(NS(id, "min_y"), "Min. y", 2),
                        numericInput(NS(id, "max_y"), "Max. y", 8),
                        colourInput(NS(id, "line_col"), "Line colour", value = "black"),
                        colourInput(NS(id, "ribbon80"), "Inner ribbon colour", value = "steelblue"),
                        colourInput(NS(id, "ribbon90"), "Outer ribbon colour", value = "steelblue"),
                        numericInput(NS(id, "alpha80"), "Alpha inner ribbon", value = .5),
                        numericInput(NS(id, "alpha90"), "Alpha outer ribbon", value = .4)
                 )
               ),
               footer = tagList(
                 fluidRow(
                   column(12, align = "right",
                          actionButton(NS(id, "download_pred_plot"), "",
                                       icon = icon("download"),
                                       outline = TRUE,
                                       status = "success",
                                       flat = FALSE
                          )
                   )
                 )
               )
             )
             ),
      column(6, style='padding:0px;',
             fluidRow(
               bs4Card(
                 width = 12,
                 title = "Log-concentration at time X",
                 status = "warning",
                 solidHeader = TRUE,
                 collapsed = TRUE,
                 numericInput(NS(id, "target_time"), "Time (h)", 400),
                 plotOutput(NS(id, "plot_sizeAtTime")),
                 tableOutput(NS(id, "table_sizeAtTime"))
               ),
             ),
             fluidRow(
               bs4Card(
                 width = 12,
                 title = "Time to log-concentration",
                 status = "warning",
                 solidHeader = TRUE,
                 collapsed = TRUE,
                 numericInput(NS(id, "target_logN"), "Target (log CFU/g)", 5),
                 plotOutput(NS(id, "plot_time2size")),
                 tableOutput(NS(id, "table_time2size"))
               )
             )
             )
    )
  )
}

## Server ----------------------------------------------------------------------

module_pred_unc_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ## Help page -----------------------------------------------------------
    
    observeEvent(input$show_help, {
      
      showModal(
        session = session,
        modalDialog(title = "Model predictions based on primary growth models with parameter uncertainty", 
                    easyClose = TRUE,
                    size = "xl",
                    tagList(
                        p(
                            paste("This module includes functions for making growth predictions considering parameter uncertainty.",
                                  "Calculations are done my Monte Carlo simulations based on normal distributions of the model parameters",
                                  "Although simulations are limited to normal distributions, these can be defined in transformed parameters.",
                                  "That is"),
                            tags$li("the parameter can follow a normal distribution"),
                            tags$li("the square root of the parameter can follow a normal distribution"),
                            tags$li("or the logarithm of the parameter can follow a normal distribution")
                        ),
                        p(
                            paste("It is important to note that the parameters of the normal distribution",
                                  "are defined on the transformed scale. For instance, let's assume that the square root of the",
                                  "growth rate (mu) follows a normal distribution. Then defining a mean of 0.1 would be defined on",
                                  "square-root scale. That is in units of h^-2. So, its equivalent in h^-1 would be 0.1^2 = 0.01"
                                  )
                        ),
                        h3("Convergence of the simulations"),
                        p(
                            paste(
                                "Please note that the module doesn't do any convergence check,",
                                "so that is left to the user.",
                                "The module provides several tools to do so. The 'Inputs' pannel",
                                "includes a histogram of the draws of the parameters. It is recommended",
                                "to always check that the histograms are 'smooth', properly representing the distribution.",
                                "This panel also includes a plot of every growth curve used for the calculations. It can be used to",
                                "check ensure a proper coverage of the predictions."
                            )
                            
                        )
                    )
        )
      )
      
    })
    
    ## Dynamic parameter selection -----------------------------------------
    
    par_map <- tribble(
      ~par, ~label, ~mean, ~sd, ~scale, 
      "logN0", "logN0 (log CFU/g)", 2, .1, "original",
      "lambda", "lambda (h)", 20, 1, "sqrt",
      "mu", "mu (log CFU/h)", .2, .1, "sqrt",
      "C", "C (log CFU)", 6, 0, "original",
      "logNmax", "log Nmax (log CFU)", 8, 0, "original",
      "nu", "nu (·)", 1, 0, "original"
    )
    
    make_input <- function(par_name) {
      
      par_data <- par_map %>%
        filter(par == par_name)
      
      tagList(
        
        fluidRow(
          column(12,
                 tags$h5(par_data$label)
                 # tags$span(par_data$label)
          )
        ),
        fluidRow(
          column(12,
                 pickerInput(NS(id, paste0(par_name, "_scale")), 
                             "scale",
                             # choices = c("sqrt", "log", "original"),
                             list(`Square-root` = "sqrt",
                                  `log-transform` = "log",
                                  `No transformation` = "original"),
                             selected = par_data$scale
                 )
          )
        ),
        fluidRow(
          column(width = 6,
                 numericInput(NS(id, paste0(par_name, "_mean")), 
                              "mean (in 'scale' units)", par_data$mean)
          ),
          column(width = 6,
                 numericInput(NS(id, paste0(par_name, "_sd")), 
                              "sd (in 'scale' units)", par_data$sd)
          )
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
    
    ## Seed ----------------------------------------------------------------
    
    observeEvent(input$resetseed, {
      
      set.seed(12421)
      print("Seed back to normal")
      
    })
    
    ## Simulation ----------------------------------------------------------
    
    my_simulation <- reactiveVal()
    
    observeEvent(input$calculate, withProgress(
      message = "Calculating...",
      {
        
        ## Extract the parameters
        
        my_model <- input$model_name
        par_names <- primary_model_data(my_model)$pars
        
        my_pars <- par_names %>%
          map_dfr(.,
                  ~ tibble(par = .,
                           mean = input[[paste0(., "_mean")]],
                           sd = input[[paste0(., "_sd")]],
                           scale = input[[paste0(., "_scale")]])
          )
        
        ## Make the calculation
        
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
        
        sim <- predict_growth_uncertainty(
          my_model,
          seq(0, input$sim_time, length = 100),
          input$n_sims,
          my_pars,
          logbase_mu = scale_mu,
          logbase_logN = scale_logN,
        )
        
        my_simulation(sim)
        
      }) 
    )
    
    ## Plot of the inputs ------------------------------------------------------
    
    output$plot_pars <- renderPlot({
      validate(need(my_simulation(), message = ""))
      
      my_simulation()$sample %>%
        pivot_longer(everything()) %>%
        ggplot() +
        geom_histogram(aes(value)) +
        facet_wrap("name", scales = "free")
      
    })
    
    output$plot_curves <- renderPlot({
      validate(need(my_simulation(), message = ""))
      
      my_simulation()$simulations %>%
        ggplot() +
        geom_line(aes(x = time, y = logN, colour = iter)) +
        theme(legend.position = "none")
      
    })
    
    ## Plot prediction band ------------------------------------------------
    
    output$plot_simulation <- renderPlotly({
      
      validate(need(my_simulation(), message = ""))
      p <- plot(my_simulation(),
                line_col = input$line_col,
                ribbon80_fill = input$ribbon80,
                ribbon90_fill = input$ribbon90,
                alpha80 = input$alpha80,
                alpha90 = input$alpha90) +
        theme_gray() +
        xlab(input$xlabel) + ylab(input$ylabel) +
        coord_cartesian(ylim = c(input$min_y, input$max_y))
      
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
      filename = "biogrowth_bands.csv",
      content = function(file) {
        
        my_simulation()$quantiles %>%
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
        
        p <- plot(my_simulation(),
                  line_col = input$line_col,
                  ribbon80_fill = input$ribbon80,
                  ribbon90_fill = input$ribbon90,
                  alpha80 = input$alpha80,
                  alpha90 = input$alpha90) +
          theme_gray() +
          xlab(input$xlabel) + ylab(input$ylabel) +
          coord_cartesian(ylim = c(input$min_y, input$max_y)) +
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
    
    ## Time to reach a size ------------------------------------------------
    
    my_time2size <- reactive({
      
      validate(need(my_simulation(), message = ""))
      
      out_time <- time_to_size(type = "distribution", my_simulation(), input$target_logN)
      
    })
    
    output$plot_time2size <- renderPlot({
      
      validate(need(my_time2size(), message = ""))
      plot(my_time2size()) + theme_gray(base_size = 14)
      
    })
    
    output$table_time2size <- renderTable({
      
      validate(need(my_time2size(), message = ""))
      summary(my_time2size())
    })
    
    ## Size at time t ------------------------------------------------------
    
    my_sizeAtTime <- reactive({
      
      validate(need(my_simulation(), message = ""))
      
      my_simulation()$simulations %>%
        split(.$iter) %>%
        map_dfr(.,
                ~ tibble(logN = approx(x = .$time, y = .$logN, xout = input$target_time)$y)
        )
    })
    
    output$plot_sizeAtTime <- renderPlot({
      
      validate(need(my_sizeAtTime(), message = ""))
      
      
      my_sizeAtTime() %>%
        summarize(mean_logN = mean(logN, na.rm = TRUE),
                  med_logN = median(logN, na.rm = TRUE),
                  q05 = quantile(logN, .05, na.rm = TRUE),
                  q95 = quantile(logN, .95, na.rm = TRUE)) %>%
        ggplot() +
        geom_histogram(aes(logN), data = my_sizeAtTime()) +
        geom_vline(aes(xintercept = mean_logN), linetype = 2, colour = "maroon") +
        geom_vline(aes(xintercept = med_logN), linetype = 3, colour = "maroon") +
        geom_vline(aes(xintercept = q05), linetype = 1, colour = "maroon") +
        geom_vline(aes(xintercept = q95), linetype = 1, colour = "maroon") +
        theme_gray(base_size = 14)
      
    })
    
    output$table_sizeAtTime <- renderTable({
      
      validate(need(my_sizeAtTime(), message = ""))
      
      
      my_sizeAtTime() %>%
        summarize(mean_logN = mean(logN, na.rm = TRUE),
                  med_logN = median(logN, na.rm = TRUE),
                  q05 = quantile(logN, .05, na.rm = TRUE),
                  q95 = quantile(logN, .95, na.rm = TRUE))
      
      
    })
    
    
    
  })
  
}

## test ------------------------------------------------------------------------

test_module_pred_unc <- function(id) {
  
  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        module_pred_unc_ui("test")
      )
    ),
    server = function(input, output) {
      module_pred_unc_server("test")
    }
  )
  
}




