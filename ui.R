library(shiny)
library(shinydashboard)
source("groups.R")

displayGroup <- function(group) {
  
  indices <- matrix(c(1, 2, 3, 4,
                      1, 3, 2, 4,
                      4, 1, 2, 3), nrow = 3, ncol = 4, byrow = TRUE)
  
  id_list <- c("first", "second", "third")
  
  res <- shiny::actionButton(inputId = "saveScores", label = "Save", class = "btn-success")
  
  for (i in 1:length(id_list)) {
    
    res <- paste0(res, shiny::div(
        shiny::h3(paste("Matchday",i)),
        shiny::div(
          shiny::textInput(inputId = paste0(group[indices[i,1]],"_",id_list[i]), label = group[indices[i,1]], width = "42px"),
          style = "display : inline-block;"
        ),
        shiny::div(style = "width : 60px; display : inline-block;"),
        shiny::div(
          shiny::textInput(inputId = paste0(group[indices[i,2]],"_",id_list[i]), label = group[indices[i,2]], width = "42px"),
          style = "display : inline-block;"
        ),
        shiny::br(),
        shiny::div(
          shiny::textInput(inputId = paste0(group[indices[i,3]],"_",id_list[i]), label = group[indices[i,3]], width = "42px"),
          style = "display : inline-block;"
        ),
        shiny::div(style = "width : 60px; display : inline-block;"),
        shiny::div(
          shiny::textInput(inputId = paste0(group[indices[i,4]],"_",id_list[i]), label = group[indices[i,4]], width = "42px"),
          style = "display : inline-block;"
        ),
        shiny::br()
      )
    )
    
  }
  
  return(HTML(res))
  
}

ui <- dashboardPage(
  dashboardHeader(title = "Euro 2020 Scoreboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Group A", tabName = "group_a", selected = TRUE),
      menuItem("Group B", tabName = "group_b"),
      menuItem("Group C", tabName = "group_c"),
      menuItem("Group D", tabName = "group_d"),
      menuItem("Group E", tabName = "group_e"),
      menuItem("Group F", tabName = "group_f")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "group_a", div(class = "row",
                                       div(class = "col-md-3",
                                           h1("Group A"),
                                           displayGroup(group = group_a)
                                       ),
                                       tableOutput("group_a_table")
      )
      ),
      tabItem(tabName = "group_b", div(class = "row",
                                       div(class = "col-md-3",
                                           h1("Group B"),
                                           displayGroup(group = group_b)
                                       ),
                                       tableOutput("group_b_table")
      )
      ),
      tabItem(tabName = "group_c", div(class = "row",
                                       div(class = "col-md-3",
                                           h1("Group C"),
                                           displayGroup(group = group_c)
                                       ),
                                       tableOutput("group_c_table")
      )
      ),
      tabItem(tabName = "group_d", div(class = "row",
                                       div(class = "col-md-3",
                                           h1("Group D"),
                                           displayGroup(group = group_d)
                                       ),
                                       tableOutput("group_d_table")
      )
      ),
      tabItem(tabName = "group_e", div(class = "row",
                                       div(class = "col-md-3",
                                           h1("Group E"),
                                           displayGroup(group = group_e)
                                       ),
                                       tableOutput("group_e_table")
      )
      ),
      tabItem(tabName = "group_f", div(class = "row",
                                       div(class = "col-md-3",
                                           h1("Group F"),
                                           displayGroup(group = group_f)
                                       ),
                                       tableOutput("group_f_table")
      )
      )
    )
  )
)