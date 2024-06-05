
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
library(biogrowth)
library(plotly)

## UI --------------------------------------------------------------------------

module_other_ui <- function(id) {
    
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
                title = "Q0 to lambda (in ln scale)",
                status = "primary",
                solidHeader = TRUE,
                collapsed = TRUE,
                numericInput(NS(id, "q0_mode1"), "Q0 (unitless)", 1e-3, step = 1e-3),
                numericInput(NS(id, "mu_mode1"), "mu (ln CFU/h)", .8, step = 1e-1),
                footer = tagList(
                    infoBoxOutput(NS(id, "lambda_mode1"), width = 12)
                )
                
            ),
            bs4Card(
                title = "Q0 to lambda (in log10 scale)",
                status = "primary",
                solidHeader = TRUE,
                collapsed = TRUE,
                numericInput(NS(id, "q0_mode2"), "Q0 (unitless)", 1e-3, step = 1e-3),
                numericInput(NS(id, "mu_mode2"), "mu (log CFU/h)", .8, step = 1e-1),
                footer = tagList(
                    infoBoxOutput(NS(id, "lambda_mode2"), width = 12)
                )
                
            )
        ),
        fluidRow(
            bs4Card(
                title = "lambda to Q0 (in ln scale)",
                status = "primary",
                solidHeader = TRUE,
                collapsed = TRUE,
                numericInput(NS(id, "lambda_mode3"), "lambda (h)", 30, step = 1),
                numericInput(NS(id, "mu_mode3"), "mu (ln CFU/h)", .8, step = 1e-1),
                footer = tagList(
                    infoBoxOutput(NS(id, "Q0_mode3"), width = 12)
                )
                
            ),
            bs4Card(
                title = "lambda to Q0 (in log10 scale)",
                status = "primary",
                solidHeader = TRUE,
                collapsed = TRUE,
                numericInput(NS(id, "lambda_mode4"), "lambda (h)", 30, step = 1),
                numericInput(NS(id, "mu_mode4"), "mu (log10 CFU/h)", .8, step = 1e-1),
                footer = tagList(
                    infoBoxOutput(NS(id, "Q0_mode4"), width = 12)
                )
                
            )
        )
    )
    
}

## Server ----------------------------------------------------------------------

module_other_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ## Help page -----------------------------------------------------------
        
        observeEvent(input$show_help, {
            
            showModal(
                session = session,
                modalDialog(title = "Tools to support growth calculations", 
                            easyClose = TRUE,
                            size = "xl",
                            tagList(
                                p("This module includes functions to convert between the parameters of the Baranyi models. Namely:"),
                                tags$li("Calculation of the lag phase duration (lambda) from the values of Q0 and mu. The calculation can be done defining mu in either ln or log10 scale"),
                                tags$li("Calculation of the value of Q0 from the lag phase duration (lambda) and mu. The calculation can be done defining mu in either ln or log10 scale"),
                            )
                )
            )
            
        })
        
        ## Mode 1 (Q0 to lambda in ln scale)
        
        output$lambda_mode1 <- renderInfoBox({
            
            lambda <- Q0_to_lambda(input$q0_mode1, input$mu_mode1, logbase_mu = exp(1))
            
            infoBox(
                value = prettyNum(lambda, digits = 2),
                title = "Lag phase duration",
                subtitle = "(hours)",
                width = 12,
                color = "success",
                icon = icon("chart-bar")
            )
        })
        
        ## Mode 2 (Q0 to lambda in log10 scale)
        
        output$lambda_mode2 <- renderInfoBox({
            
            lambda <- Q0_to_lambda(input$q0_mode2, input$mu_mode2)
            
            infoBox(
                value = prettyNum(lambda, digits = 2),
                title = "Lag phase duration",
                subtitle = "(hours)",
                width = 12,
                color = "success",
                icon = icon("chart-bar")
            )
        })
        
        ## Mode 3 (lambda to Q0 in ln scale)
        
        output$Q0_mode3 <- renderInfoBox({
            
            Q0 <- lambda_to_Q0(input$lambda_mode3, input$mu_mode3, logbase_mu = exp(1))
            
            infoBox(
                value = prettyNum(Q0, digits = 4),
                title = "Q0",
                subtitle = "(unitless)",
                width = 12,
                color = "success",
                icon = icon("chart-bar")
            )
        })
        
        ## Mode 4 (lambda to Q0 in log10 scale)
        
        output$Q0_mode4 <- renderInfoBox({
            
            Q0 <- lambda_to_Q0(input$lambda_mode4, input$mu_mode4)
            
            infoBox(
                value = prettyNum(Q0, digits = 4),
                title = "Q0",
                subtitle = "(unitless)",
                width = 12,
                color = "success",
                icon = icon("chart-bar")
            )
        })
        
   
    
    })
}


## test ------------------------------------------------------------------------

test_module_other <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_other_ui("test")
            )
        ),
        server = function(input, output) {
            module_other_server("test")
        }
    )
    
}
