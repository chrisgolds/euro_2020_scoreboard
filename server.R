library(shiny)
source("groups.R")

headers <- c("Country", "Won", "Drawn", "Lost", "Goals For", "Goals Against", "Goal Difference", "Points")

initTable <- function(group) {
  
  res <- matrix(c(group,
                  c(0,0,0,0),
                  c(0,0,0,0),
                  c(0,0,0,0),
                  c(0,0,0,0),
                  c(0,0,0,0),
                  c(0,0,0,0),
                  c(0,0,0,0)), nrow = 4, ncol = 8)
  colnames(res) <- headers
  rownames(res) <- group 
  return(res)
  
}

server <- function(input, output) {
  
  GROUP_TABLES <- reactiveValues()
  GROUP_TABLES$A <- initTable(group_a)
  GROUP_TABLES$B <- initTable(group_b)
  GROUP_TABLES$C <- initTable(group_c)
  GROUP_TABLES$D <- initTable(group_d)
  GROUP_TABLES$E <- initTable(group_e)
  GROUP_TABLES$F <- initTable(group_f)
  
  output$group_a_table <- renderTable(GROUP_TABLES$A)
  output$group_b_table <- renderTable(GROUP_TABLES$B)
  output$group_c_table <- renderTable(GROUP_TABLES$C)
  output$group_d_table <- renderTable(GROUP_TABLES$D)
  output$group_e_table <- renderTable(GROUP_TABLES$E)
  output$group_f_table <- renderTable(GROUP_TABLES$F)
  
  result <- function(team_a, score_a, team_b, score_b, table) {
    
    print("Here")
    
    if (score_a > score_b) {
      table[team_a,"Points"] <- as.integer(table[team_a,"Points"]) + 3
    } else if (score_b > score_a) {
      table[team_b,"Points"] <- as.integer(table[team_b,"Points"]) + 3
    }
    
    return(table)
    
  }
  
  observe({
    
    # Group A
    output$group_a_table <- renderTable(result(group_a[1],
                                               eval(parse(text=paste0("input$",group_a[1],"_first"))),
                                               group_a[2],
                                               eval(parse(text=paste0("input$",group_a[2],"_first"))),
                                               GROUP_TABLES$A))
    output$group_a_table <- renderTable(result(group_a[3],
                                               eval(parse(text=paste0("input$",group_a[3],"_first"))),
                                               group_a[4],
                                               eval(parse(text=paste0("input$",group_a[4],"_first"))),
                                               GROUP_TABLES$A))
    
    # output$group_a_table <- renderTable(result(group_a[1],
    #                                            eval(parse(text=paste0("input$",group_a[1],"_second"))),
    #                                            group_a[3],
    #                                            eval(parse(text=paste0("input$",group_a[3],"_second"))),
    #                                            GROUP_TABLES$A))
    # output$group_a_table <- renderTable(result(group_a[2],
    #                                            eval(parse(text=paste0("input$",group_a[2],"_second"))),
    #                                            group_a[4],
    #                                            eval(parse(text=paste0("input$",group_a[4],"_second"))),
    #                                            GROUP_TABLES$A))
    # 
    # output$group_a_table <- renderTable(result(group_a[4],
    #                                            eval(parse(text=paste0("input$",group_a[4],"_third"))),
    #                                            group_a[1],
    #                                            eval(parse(text=paste0("input$",group_a[1],"_third"))),
    #                                            GROUP_TABLES$A))
    # output$group_a_table <- renderTable(result(group_a[2],
    #                                            eval(parse(text=paste0("input$",group_a[2],"_third"))),
    #                                            group_a[3],
    #                                            eval(parse(text=paste0("input$",group_a[3],"_third"))),
    #                                            GROUP_TABLES$A))
    
  })
  
}