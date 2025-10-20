
library(shiny)
library(bs4Dash)
library(tidyverse)

library(thematic)
thematic_shiny()

library(fresh)
library(ggthemes)
library(cowplot)
library(shinycssloaders)
library(colourpicker)

library(shinydisconnect)

## Source the modules

list.files("./R") %>%
  map(., ~ paste0("./R/", .)) %>%
  map(source)

## The Page

ui <- bs4DashPage(
    dark = NULL,
    fullscreen = TRUE,

    header = bs4DashNavbar(title = dashboardBrand(title = "biogrowth4")
                           # skin = "dark",
                           # status = "primary"
    ),
    footer = dashboardFooter(
        left = a(
          href = "https://github.com/albgarre/biogrowth4",
          target = "_blank", "@AlbertoGarre"
        ),
        right = "alpha version - May 2024"
    ),
    sidebar = dashboardSidebar(
        sidebarMenu(
            id = "welcome_menu",
            menuItem(
                "Welcome",
                tabName = "welcome_tab",
                icon = icon("igloo")
            )
        ),
        sidebarMenu(
            id = "pred_menu",
            sidebarHeader("Predictions"),
            menuItem(
                "Isothermal",
                tabName = "pred_iso",
                icon = icon("vial")
            ),
            menuItem(
                "Dynamic",
                tabName = "pred_dyna",
                icon = icon("plane")
            ),
            menuItem(
                "Uncertainty",
                tabName = "pred_stoc",
                icon = icon("dice")
            )
        ),
        
        sidebarMenu(
            id = "fit_menu",
            sidebarHeader("Model fitting"),
            menuItem(
                "Primary model",
                tabName = "fit_primary",
                icon = icon("wand-sparkles")
            ),
            menuItem(
                "Dynamic",
                tabName = "fit_dyna",
                icon = icon("plane-slash")
            ),
            menuItem(
                "Global",
                tabName = "fit_global",
                icon = icon("spaghetti-monster-flying")
            ),
            menuItem(
                "Secondary",
                tabName = "fit_secondary",
                icon = icon("hand-peace")
            ),
            menuItem(
                "Gamma",
                tabName = "fit_gamma",
                icon = icon("hands-bound")
            ),
            menuItem(
                "Coupled Two-steps",
                tabName = "coupled_two",
                icon = icon("hands-bound")
            ),
            menuItem(
                "Coupled One-step",
                tabName = "coupled_one",
                icon = icon("hands-bound")
            )
        ),
        
        sidebarMenu(
            id = "fit_menu",
            sidebarHeader("Other tools"),
            menuItem(
                "Lag and Q0",
                tabName = "lag_Q0",
                icon = icon("circle")
            ),
            menuItem(
                "Serial dilution method",
                tabName = "two_fold",
                icon = icon("arrows-turn-to-dots")
            ),
            menuItem(
                "OD-data converter",
                tabName = "templates",
                icon = icon("home")
                
            )
        ),
        
        sidebarMenu(
            id = "doc_menu",
            sidebarHeader("Documentation"),
            menuItem(
                "Documentation",
                tabName = "manual",
                icon = icon("book"),
                # href = "https://www.google.com/",
                newTab = TRUE
            ),
            menuItem(
                "About",
                tabName = "other_page",
                icon = icon("microchip")
            )
        ),
        
        
        
    ),
    body = dashboardBody(
        disconnectMessage(),
        tabItems(
            tabItem(
                tabName = "manual",
                fluidRow(
                    bs4Card(title = "Online documentation", width = 6,
                            solidHeader = TRUE,
                            status = "info",
                            collapsible = FALSE,
                            icon = icon("book"),
                            # label = bs4CardLabel(text = "new", status = "white"),
                            a("User manual", href="https://docs.google.com/document/d/1HTaftyLDDiGjZFeUWe9mfoST1COF5QOc5SAWae4A6pU/edit?usp=sharing")
                    ),
                    bs4Card(title = "Training material", width = 6,
                            solidHeader = TRUE,
                            status = "info",
                            collapsible = FALSE,
                            icon = icon("book")
                            # label = bs4CardLabel(text = "new", status = "white"),
                            # a("User manual", href="https://www.google.com/")
                    ),
                )
                
            ),
            tabItem(
                tabName = "pred_iso",
                module_primary_pred_ui("module_primary_pred")
            ),
            tabItem(
                tabName = "pred_dyna",
                module_pred_dynamic_ui("module_dynamic_pred")
            ),
            tabItem(
                tabName = "pred_stoc",
                module_pred_unc_ui("module_pred_uncertainty")
            ),
            tabItem(
                tabName = "fit_primary",
                module_primary_fit_ui("module_primary_fit")
            ),
            tabItem(
                tabName = "fit_secondary",
                module_secondary_fit_ui("module_secondary_fit")
            ),
            tabItem(
                tabName = "fit_gamma",
                module_gamma_fit_ui("module_gamma_fit")
            ),
            tabItem(
                tabName = "coupled_two",
                module_coupled_two_fit_ui("module_coupled_two")
            ),
            tabItem(
                tabName = "coupled_one",
                module_coupled_one_fit_ui("module_coupled_one")
            ),
            # tabItem(
            #     tabName = "fit_1step",
            #     # fit1step_module_ui("module_1step")
            # ),
            tabItem(
                tabName = "fit_dyna", 
                module_dynamic_fit_ui("module_dynamic_fit")
            ),
            tabItem(
                tabName = "fit_global",
                module_global_fit_ui("module_global_fit")
            ),
            tabItem(
                tabName = "lag_Q0",
                module_other_ui("module_other")
            ),
            tabItem(
                tabName = "two_fold",
                module_twofold_ui("module_twofold_dilution")
            ),
            tabItem(
                tabName = "templates",
                module_templates_ui("module_templates")
            ),
            tabItem(
                tabName = "welcome_tab",
                fluidRow(
                    column(12,
                           bs4Jumbotron(
                               status = "warning",
                               title = "biogrowth",
                               lead = h4("Let it grow!"),
                               btnName = NULL
                           )
                    )
                ),
                fluidRow(
                    bs4Card(title = "",
                            width = 6,
                            gradient = TRUE,
                            background = "primary",
                            collapsible = FALSE,
                            p(
                                paste0(
                                    "Biogrowth is intended the facilitate modelling microbial growth using predictive microbiology",
                                    "It is developed and maintained by the Universidad Politecnica de Cartagena (Spain) and Wageningen University (the Netherlands)."
                                )
                            ),
                            p(
                                paste0(
                                    "It includes functions for fitting inactivation models commonly used in predictive microbiology to data gathered under static or dynamic conditions.",
                                    "It can also be used to make predictions under static or dynamic conditions."
                                )
                            ),
                            p("Do not hesitate to contact us (see the 'About' section) if you have any comment!")
                    ),
                    carousel(
                        width = 6,
                        id = "mycarousel",
                        carouselItem(
                            caption = "Fitting secondary models",
                            tags$img(src = "pic-secondary-fit.png")
                        ),
                        carouselItem(
                            caption = "Predictions from primary models",
                            tags$img(src = "pic-primary-pred.png")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "other_page",
                module_about_ui("module_about")
            )
        )
        
    )
)

















