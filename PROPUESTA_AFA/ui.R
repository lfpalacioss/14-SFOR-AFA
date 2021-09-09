#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


source("C:/Users/lfpal/Google Drive/4) R/1) Scripts/1) OEFA/14) SFOR-AFA/PROPUESTA_AFA/global.R")

dashboardPage(
    # Configuración de la página ----------------------------------------------------
    skin  = "blue",
    
    dashboardHeader(
        title ="Tableros de control SFOR",
        titleWidth = 500
    ),
    # Fin ----
    
    
    # Configuración del Sidebar (listado de Dashboards) -------------------------------------------------------
    dashboardSidebar(
        sidebarMenu(
            menuItem("SFOR-AFA",
                     tabName = "AFA",
                     icon = icon("tasks")
            # menuSubItem("EFA", tabName = "tab_EFA", icon = icon("undo-alt"))
            ),
            
            menuItem("SFOR-EFA",
                     tabName = "EFA",
                     icon = icon("tasks")
                     # menuSubItem("EFA", tabName = "tab_EFA", icon = icon("undo-alt"))
            )            
        )
    ),
    # Fin ----
    
    
    
    # Configuración del cuerpo del Dashboard -----------------------------------------------
    dashboardBody(
        tabItems(

            # Input AFA -----------------------------------------------
            tabItem(tabName = "AFA",
                    style = "height:1000px; overflow-y: scroll;overflow-x: scroll;",
                    
                    fluidRow(
                        
                        box(width = 12,
                            selectInput("PERIODO_1", 
                                        label = h3("Seleccionar periodo"), 
                                        choices = list("2020" = 2020, "2021" = 2021, "2022" = 2022), 
                                        selected = 1)),
                        
                        box(width = 12, formattableOutput(outputId = "GRAF_AFA_1")),
                        
                        box(width = 12, formattableOutput(outputId = "GRAF_AFA_2")),  ####
                        
                        box(width = 6, plotOutput(outputId = "GRAF_AFA_3")),  #, height = "500px"
                        
                        box(width = 6, plotOutput(outputId = "GRAF_AFA_4")))  #, height = "500px"
            ),
            # Fin ----
            



            # Input EFA -----------------------------------------------
            tabItem(tabName = "EFA",
                    style = "height:1000px; overflow-y: scroll;overflow-x: scroll;",
                    
                    fluidRow(
                        
                        box(width = 4,
                            selectInput("PERIODO_2", 
                                        label = h3("Seleccionar periodo"), 
                                        choices = list("2020" = 2020, "2021" = 2021, "2022" = 2022), 
                                        selected = 1)),
                        
                        box(width = 4,
                            selectInput("AMBITO_2", 
                                        label = h3("Seleccionar ámbito"), 
                                        choices = (unique(BD_EFA$ambitoEntidadMatricula)), 
                                        selected = 1)),
                        
                        box(width = 4,
                            selectInput("NOMBRE_2",
                                        label = h3("Seleccionar nombre"),
                                        choices = (unique(BD_EFA$NombreFinalGrupo)),
                                        selected = 1)),
                        
                        
                        box(width = 12, formattableOutput(outputId = "GRAF_EFA_1")),
                        
                        box(width = 12, formattableOutput(outputId = "GRAF_EFA_2")),  ####
                        
                        box(width = 6, plotOutput(outputId = "GRAF_EFA_3")),  #, height = "500px"
                        
                        box(width = 6, plotOutput(outputId = "GRAF_EFA_4")))  #, height = "500px"
            )
            # Fin ----


        )
    )
)

    # Fin ---
    


########################################################################################################################################################################

