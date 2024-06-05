library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

module_about_ui <- function(id) {
    
    tagList(
        fluidRow(
            column(6,
                   userBox(
                       title = userDescription(
                           title = "Methodology",
                           subtitle = "",
                           type = 2,
                           image = "https://images-na.ssl-images-amazon.com/images/I/71-A3eC0XsL.png"
                       ),
                       width = 12,
                       status = "success",
                       p(
                           paste("The statistical methods implemented in biogrowth are based on classical methods",
                                 "from predictive microbiology.", 
                                 "They can be found in classical boooks such as 'Predictive Microbiology in Foods'",
                                 "by Perez-Rodriguez and Valero (2012)."
                                 )
                       ),
                       p(
                           paste("In particular, the calculations are based on the biogrowth package for R.",
                                 "It is described in 'Modeling Population Growth in R with the biogrowth Package'",
                                 "Garre et al. (2023).")
                       ),
                       p("The software documentation provides additional (practical) details not covered in those other documents.")
                   )
            ),
            column(6,
                   userBox(
                       title = userDescription(
                           title = "The biogrowth package",
                           subtitle = "",
                           type = 2,
                           image = "https://docs.microsoft.com/nl-nl/azure/architecture/data-guide/images/logo_r.svg",
                       ),
                       width = 12,
                       status = "success",
                       # gradient = TRUE,
                       # background = "primary",
                       p("The calculations are based on the biogrowth package for R. It is available from CRAN in"),
                       p("https://cran.r-project.org/package=biogrowth")
                   )
            )
        ),
        fluidRow(
            column(6,
                   userBox(
                       title = userDescription(
                           title = "Open code",
                           subtitle = "",
                           type = 2,
                           image = "https://www.influxdata.com/wp-content/uploads/GitHub-logo.jpg"
                       ),
                       width = 12,
                       status = "warning",
                       # gradient = TRUE,
                       # background = "primary",
                       p("The complete biogrowth project is developed in Open Source."),
                       p("The latest version of the code for the biogrowth package can be found in: https://github.com/albgarre/biogrowth"),
                       p("The latest version of the code for the application can be found in https://github.com/albgarre/biogrowth4")
                   )
            ),
            column(6,
                   userBox(
                       title = userDescription(
                           title = "Citation",
                           subtitle = "",
                           type = 2,
                           image = "https://w7.pngwing.com/pngs/304/33/png-transparent-red-and-black-chat-box-dialog-box-computer-icons-message-box-material-miscellaneous-painted-text.png",
                       ),
                       width = 12,
                       status = "warning",
                       # gradient = TRUE,
                       # background = "primary",
                       p("We would really appreciate if you could include a citation to biogrowth when using it in scientific studies."),
                       p("Please cite it as "),
                       p("Garre, A., Koomen, J., Besten, H. M. W. den, & Zwietering, M. H. (2023). Modeling Population Growth in R with the biogrowth Package. Journal of Statistical Software, 107, 1-51. https://doi.org/10.18637/jss.v107.i01")
                   )
            )
        ),
        fluidRow(
            column(6,
                   userBox(
                       title = userDescription(
                           title = "Contact & bug reports",
                           subtitle = "",
                           type = 2,
                           image = "https://w7.pngwing.com/pngs/304/33/png-transparent-red-and-black-chat-box-dialog-box-computer-icons-message-box-material-miscellaneous-painted-text.png"
                       ),
                       width = 12,
                       status = "danger",
                       gradient = FALSE,
                       # background = "warning",
                       p("For bug reports, please open an issue in GitHub: https://github.com/albgarre/biogrowth4/issues. This makes error tracking easier for everyone"),
                       p("For any other queries, please contact Alberto Garre at alberto.garre(at)upct.es")
                   )
            )
        )
    )
    
}

## Server ----------------------------------------------------------------------

module_about_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
    })
    
}

## test ------------------------------------------------------------------------

module_about_test <- function(id) {
    
    shinyApp(
        ui = dashboardPage(
            title = "",
            header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            controlbar = dashboardControlbar(),
            footer = dashboardFooter(),
            body = dashboardBody(
                module_about_ui("test")
            )
        ),
        server = function(input, output) {
            module_about_server("test")
        }
    )
    
}