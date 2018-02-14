library('networkD3')
library('rhandsontable')
library('shiny')
library('shinydashboard')
#library('dplyr')
library('visNetwork')
source('error.bar.R')

dashboardPage(skin = "blue",
              dashboardHeader(title = "Child & Maternal Health",
                              titleWidth = 240
                              ),
              dashboardSidebar(width = 240,
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
                                                                                                         )
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
                                                                    tabBox(width = 12,
                                                                           id = "visula",
                                                                           tabPanel("Network Graph",
                                                                                    visNetworkOutput("netPlot",height = "700px")
                                                                                    ),
                                                                           tabPanel("Inference Plot",
                                                                                    shiny::plotOutput("distPlot")
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
