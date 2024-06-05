
library(shiny)
library(bs4Dash)

library(tidyverse)

## UI --------------------------------------------------------------------------

welcome_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             bs4Jumbotron(
               status = "warning",
               title = "bioinactivation",
               lead = h4("Now with a dark mode!"),
               btnName = NULL
             )
      )
    ),
    fluidRow(
      column(6,
             carousel(
               id = "mycarousel",
               carouselItem(
                 caption = "Isothermal predictions",
                 tags$img(src = "/img/pic_isopred.png")
               ),
               carouselItem(
                 caption = "Dynamic fitting",
                 tags$img(src = "/img/fit_dynamic.png")
               ),
               carouselItem(
                 caption = "Dynamic predictions",
                 tags$img(src = "/img/dynamic_predictions.png")
               ),
               carouselItem(
                 caption = "Fit primary model",
                 tags$img(src = "/img/fit_primary.png")
               ),
               carouselItem(
                 caption = "One-step fitting",
                 tags$img(src = "/img/fit_secondary.png")
               ),
               carouselItem(
                 caption = "Stochastic predictions",
                 tags$img(src = "/img/stoc_pred.png")
               )
             )
             ),
      column(6,
             bs4Card(title = "",
                     width = 12,
                     gradient = TRUE,
                     background = "primary",
                     collapsible = FALSE,
                     p("Bioinactivation was developed within the Technical University of Cartagena (Spain) with the aim to ease modelling of microbial inactivation."),
                     p("It includes functions for fitting inactivation models commonly used in predictive microbiology to data gathered under static or dynamic conditions."),
                     p("It can also be used to make predictions under static or dynamic conditions."),
                     p("Do not hesitate to contact us if you have any comment!")
                     )
             )
    )
  )

}

## Server ----------------------------------------------------------------------

welcome_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

welcome_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        welcome_module_ui("test")
      )
    ),
    server = function(input, output) {
      welcome_module_server("test")
    }
  )

}
