library('networkD3')
library('rhandsontable')
library('shiny')
library('shinydashboard')
library('dplyr')
library('visNetwork')
source('error.bar.R')
library('shinyWidgets')
library('shinyalert')
library('shinycssloaders')
library('shinythemes')

dashboardPage(skin = "blue",
              dashboardHeader(title = "Child & Maternal Health",
                              titleWidth = 200
                              ),
              dashboardSidebar(width = 200,
                               sidebarMenu(id = "sidebarMenu",
                                           menuItem("Home",
                                                    tabName = "Home",
                                                    icon = icon("home")
                                                    ),
                                           menuItem("Structure",
                                                    icon = shiny::icon("globe"),
                                                    tabName = "structure"
                                                    ),
                                           menuItem("Documentation",
                                                    icon = icon("book"),
                                                    tabName = "Documentation"
                                                    )
                                           )
                               ),
              dashboardBody(id ="dashboardBody",
                            useShinyalert(),
                            tags$script(HTML("$('body').addClass('fixed');")),
                            shinydashboard::tabItems(
                              shinydashboard::tabItem(tabName = "Home",
                                                      fluidRow(
                                                        box(title = "Status summary",
                                                            width = 12 ,
                                                            status="primary",
                                                            helpText("About the project and dashboard")
                                                            )
                                                        )
                                                      ),
                              shinydashboard::tabItem(tabName = "structure",
                                                      fluidRow(shiny::column(width = 3,
                                                                             offset = -1,
                                                                             style='padding:0px;margin:0px',
                                                                             tabBox(width = 12,
                                                                                    id = "control_box",
                                                                                    tabPanel("Graph",
                                                                                             helpText("Important modules"),
                                                                                             shiny::fluidRow(shiny::column(12,
                                                                                                                           selectInput("module",
                                                                                                                                       "Module Selection:",
                                                                                                                                       "Module 1"
                                                                                                                                       )
                                                                                                                           )
                                                                                                             ),
                                                                                             helpText("Update graph to visualize the seleted chain"),
                                                                                             shiny::fluidRow(shiny::column(12,actionButton('graphBtn', 'Update Graph'))),
                                                                                             helpText("Slide to adjust degree of visible neighbors on highlighting"),
                                                                                             sliderInput("degree", "chain of neighbors",
                                                                                                         min = 1, max = 5,
                                                                                                         value = 2
                                                                                                         ),
                                                                                             shiny::selectInput('graph_layout','Layout',"layout_nicely")
                                                                                             ),
                                                                                    tabPanel("Inference",
                                                                                             helpText("Select evidence to add to the model"),
                                                                                             shiny::fluidRow(shiny::column(6,actionButton('insertBtn', 'Insert')),
                                                                                                             shiny::column(6,actionButton('removeBtn', 'Remove'))
                                                                                             ),
                                                                                             shiny::fluidRow(shiny::column(6,tags$div(id = 'placeholder1')),
                                                                                                             shiny::column(6,tags$div(id = 'placeholder2'))
                                                                                             ),
                                                                                             helpText("Select an event of interest"),
                                                                                             shiny::selectInput("event",
                                                                                                                "Event Node:",
                                                                                                                ""),
                                                                                             helpText("Produce a simple/confidence inference plot"),
                                                                                             shiny::fluidRow(shiny::column(5,actionButton('plotBtn', 'Simple Plot')),shiny::column(5,actionButton('plotStrengthBtn', 'Confidence Plot'))),
                                                                                             helpText("Slide to select the no. of iteration for confiedence plot"),
                                                                                             sliderInput("numInterval", "No. of confidence intervals",
                                                                                                         min = 1, max = 500,
                                                                                                         value = 25
                                                                                                         )
                                                                                             )
                                                                                    )
                                                                             ),
                                                      shiny::column(width = 9,
                                                                    offset = -1,
                                                                    style='padding:0px;margin:0px',
                                                                    tabBox(width = 12,
                                                                           id = "visula",
                                                                           tabPanel("Network Graph",
                                                                                    withSpinner(visNetworkOutput("netPlot",height = "700px"), color= "#0099CC")
                                                                                    ),
                                                                           tabPanel("Inference Plot",
                                                                                    withSpinner(shiny::plotOutput("distPlot"), color= "#0099CC")
                                                                                    )
                                                                           )
                                                                    )
                                                      )
                                                      ),
                              shinydashboard::tabItem(tabName = "Documentation",
                                                      tabBox(width = 12,
                                                             tabPanel("Codebook",
                                                                      uiOutput("pdf1")

                                                                      ),
                                                             tabPanel("Documentation",
                                                                      uiOutput("pdf2")

                                                                      ),
                                                             tabPanel("Description",
                                                                      uiOutput("pdf3")

                                                                      )
                                                             )

                                                      )
                              )
                            )
              )
