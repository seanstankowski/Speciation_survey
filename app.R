
## Packages
{
  
  library(shinydashboard)
  
  library(readxl)
  
  library(shinyWidgets)
  
  library(plotly)
  
  library(shinycssloaders)
  
  library(RColorBrewer)
  
  library(DT)
  
}


## UI
{
  
  ui <- dashboardPage(
    
    
    dashboardHeader(title = "Speciation survey"),
    
    
    dashboardSidebar(
      
      
      uiOutput("columns_to_plot_Y_ui"),
      uiOutput("columns_to_plot_X_ui"),
     
      
      
      
      hr(),
      
      uiOutput("subset_factors_title_ui"),
      
      uiOutput("survey_questions_UI"),
      
      
      tags$style(".skin-blue .sidebar a { color: #444; }")
      
    ),
    
    
    dashboardBody(
      
      fluidRow(
        
        
        box(width = 12,
            
            withSpinner( plotlyOutput("horizontal_bar_plot_ui", height = "700px") )
            
            ),
        
        # box( width = 12,
        #      
        #      DTOutput("filtered_data_table")
        #      
        # ),
        
      )
      
    )
  )
  
}


server <- function(input, output) {
  
  ## Reactive Values
  {
    
    RV = reactiveValues(
      
      survey = NULL,
      
      survey_questions = NULL,
      
      filtered_survey = NULL,
      
      filtered_survey_table = NULL
      
      
    )
    
  }
  
  
  ## Read survey xlsx file
  observe({
    
    # survey = read_xlsx("20201130_survey_clean.xlsx")
    survey = read_xlsx("20_dec_survey_cleaner_without_taxa.xlsx")
    
    RV$survey_questions = colnames(survey)
    
    survey[survey == "N/A"] <- NA
    
    # survey <- as.data.frame(unclass(survey), stringsAsFactors= TRUE, optional = TRUE)
    survey <- as.data.frame(unclass(survey), stringsAsFactors= TRUE)
    
    RV$survey = survey
    
  })
  
  
  ## Subset title
  output$subset_factors_title_ui =   renderUI({
    
    column(width = 12, align="left", h4("Filter Responses"))
    
  })
  
  
  output$survey_questions_UI <- renderUI({
    
    
    lapply(1:ncol(req(RV$survey)), function(i) {
      
      
      ## Dropdown for Questions
      pickerInput(paste0("subset_factor_",i), RV$survey_questions[i],
                  options = list(pickerOptions(dropupAuto = TRUE), 'actions-box' = T),
                  multiple = TRUE, choices = levels(req(RV$survey)[,i]))
      
      
    })
    
    
  })
  
  
  ## Filter survey data
  observe({
    
    survey <- req(RV$survey)
    
    for(i in 1:ncol(survey)){
      
      
      subset_factor <- input[[paste0('subset_factor_', i)]]
      
      if(!is.null(subset_factor)){
        
        survey = survey[survey[,i] %in% subset_factor, ]
        
      }
      
      
    }
    
    survey <- as.data.frame(lapply(survey, function (x) if (is.factor(x)) factor(x) else x)) 
    
    RV$filtered_survey = survey
    
  })
  
  
  ## Columns to plot X
  output$columns_to_plot_X_ui = renderUI({
    
    ## Dropdown for Q3
    pickerInput("columns_to_plot_X", "Select Group Variable",
                options = pickerOptions(dropupAuto = TRUE, maxOptions = 1),
                multiple = TRUE, choices = setNames(colnames(req(RV$survey)), RV$survey_questions))
    
  })
  
  ## Columns to plot Y
  output$columns_to_plot_Y_ui = renderUI({
    
    ## Dropdown for Q3
    pickerInput("columns_to_plot_Y", "Select Answer Variable",
                options = pickerOptions(dropupAuto = TRUE, maxOptions = 1),
                multiple = TRUE, choices = setNames(colnames(req(RV$survey)), RV$survey_questions))
    
  })
  
  
  ## Horizontal Bar plot
  output$horizontal_bar_plot_ui = renderPlotly({
    
    
    filtered_survey <- req(RV$filtered_survey)
    
    survey_questions <- req(RV$survey_questions)
    
    xaxis <- input$columns_to_plot_X
    
    yaxis <- req(input$columns_to_plot_Y)
    
    
    
    
    
    if(is.null(xaxis)){
      
      filtered_survey_plot_df <- data.frame(y = filtered_survey[ , c(yaxis)])
      
      filtered_survey_plot_df = data.frame( y = filtered_survey_plot_df[ complete.cases(filtered_survey_plot_df), ])
      
      filtered_survey_plot_df <- as.data.frame(lapply(filtered_survey_plot_df, 
                                                      function (x) if (is.factor(x)) factor(x) else x)) 
      
      colnames(filtered_survey_plot_df) = c("y")
      
      
      
      annotation_plot_df  = as.data.frame(unclass(table(filtered_survey_plot_df)))
      
      colnames(annotation_plot_df) = paste0("x", 1:(ncol(annotation_plot_df)))
      
      annotation_plot_df <- data.frame(y = row.names(annotation_plot_df), annotation_plot_df)
      
      
      
      
      plot_df = as.data.frame(unclass(prop.table(table(filtered_survey_plot_df))*100))
      
      plot_df = as.data.frame(t(plot_df))
      
      plot_df = cbind( data.frame( Response = yaxis ), plot_df )
      
      
      
      color_list = brewer.pal(n = ncol(plot_df)-1, name = "Reds")
      
      
      
      fig <- plot_ly(plot_df, y = ~ Response, 
                     name = colnames(plot_df)[2],
                     x = plot_df[,2], 
                     type = 'bar', 
                     orientation = 'h',
                     hoverinfo = 'text',
                     text = ~paste0(colnames(plot_df)[2],"<br>n: ",annotation_plot_df[1,2], "<br>%: ",plot_df[,2]),
                     marker = list(color = color_list[1],
                                   line = list(color = 'rgb(248, 248, 249)', width = 1))) 
      
      
      
      if(ncol(plot_df) > 2){
        
        for(i in 3:(ncol(plot_df))){
          
          fig <- fig %>% add_trace(x = plot_df[,i],
                                   name = colnames(plot_df)[i],
                                   hoverinfo = 'text',
                                   text = paste0(colnames(plot_df)[i],"<br>n: ",annotation_plot_df[i-1,2], "<br>%: ",plot_df[,i]),
                                   marker = list(color = color_list[i-1])) 
          
        }
        
      }
      
      
      fig  %>% layout(yaxis = list(title = survey_questions[which(colnames(filtered_survey) %in% yaxis)],
                                   showline = FALSE,
                                   zeroline = FALSE,
                                   showticklabels = FALSE), 
                      barmode = 'stack',
                      showlegend = TRUE) 
      
      
    } else {
      
      filtered_survey_plot_df <- filtered_survey[ ,c(xaxis, yaxis)]
      
      filtered_survey_plot_df = filtered_survey_plot_df[ complete.cases(filtered_survey_plot_df), ]
      
      filtered_survey_plot_df <- as.data.frame(lapply(filtered_survey_plot_df, 
                                                      function (x) if (is.factor(x)) factor(x) else x)) 
      
      colnames(filtered_survey_plot_df) = c("x", "y")
      
      
      
      
      annotation_plot_df  = as.data.frame(unclass(table(filtered_survey_plot_df)))
      
      colnames(annotation_plot_df) = paste0("x", 1:(ncol(annotation_plot_df)))
      
      annotation_plot_df <- data.frame(y = row.names(annotation_plot_df), annotation_plot_df)
      
      

      
      
      
      plot_df = as.data.frame(unclass(prop.table(table(filtered_survey_plot_df), margin = 1)*100))
      
      top_labels = colnames(plot_df)
      
      
      if(is.null(xaxis)){
        
        top_labels = "|||"
        
      }
      
      
      top_labels_x_position = NULL
      
      colnames(plot_df) = paste0("x", 1:(ncol(plot_df)))
      
      plot_df <- data.frame(y = row.names(plot_df), plot_df)
      
      color_list = brewer.pal(n = ncol(plot_df)-1, name = "Reds")
      
      
      
      fig <- plot_ly(plot_df, y = ~ y, x = ~x1, type = 'bar', 
                     orientation = 'h', name = top_labels[1],
                     hoverinfo = 'text',
                     text = ~paste0(top_labels[1],"<br>n: ",annotation_plot_df[,2], "<br>%: ",x1),
                     marker = list(color = color_list[1],
                                   line = list(color = 'rgb(248, 248, 249)', width = 1))) 
      
      
      if(ncol(plot_df) > 2){
        
        for(i in 2:(ncol(plot_df)-1)){
          
          fig <- fig %>% add_trace(x = plot_df[[paste0('x',i)]], name = top_labels[i], 
                                   hoverinfo = 'text',
                                   # text = paste0(top_labels[i],":",annotation_plot_df[,i+1]),
                                   text = paste0(top_labels[i],"<br>n: ",annotation_plot_df[,i+1], "<br>%: ",plot_df[[paste0('x',i)]]),
                                   marker = list(color = color_list[i])) 
          
        }
        
      }
      
      fig <- fig %>% layout(xaxis = list(title = paste0('<b>',survey_questions[which(colnames(filtered_survey) %in% yaxis)], '</b>'),
                                         showgrid = FALSE,
                                         showline = FALSE,
                                         showticklabels = FALSE,
                                         zeroline = FALSE,
                                         domain = c(0.15, 1)),
                            yaxis = list(title = '',
                                         showgrid = FALSE,
                                         showline = FALSE,
                                         showticklabels = FALSE,
                                         zeroline = FALSE),
                            barmode = 'stack',
                            paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
                            margin = list(l = 120, r = 10, t = 140, b = 80),
                            showlegend = TRUE) %>%
        add_annotations(xref = 'paper', yref = 'paper', 
                        x = 0, y = 1.05, 
                        text = paste0('<b>',survey_questions[which(colnames(filtered_survey) %in% xaxis)], '</b>'),
                        showarrow = FALSE,
                        align = 'left')
      
      # labeling the y-axis
      fig <- fig %>% add_annotations(xref = 'paper', yref = 'y', x = 0.14, y =~ y,
                                     xanchor = 'right',
                                     text =~ y,
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(67, 67, 67)'),
                                     showarrow = FALSE, align = 'right') 
      
      
      
      if((ncol(plot_df)-1) >= 1){
        
        
        # labeling the percentages of each bar (x_axis)
        fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                                       x = plot_df[,'x1'] / 2, y =~ y,
                                       text =ifelse(plot_df[['x1']] > 0, paste(round(plot_df[['x1']],1), '%'), ""),
                                       font = list(family = 'Arial', size = 12,
                                                   color = '#000000'),
                                       showarrow = FALSE) 
        
        top_labels_x_position = c(top_labels_x_position, max(plot_df[,'x1']) / 2)
        
      }
      
      
      if((ncol(plot_df)-1) >= 2){
        
        
        # labeling the percentages of each bar (x_axis)
        fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                                       x = plot_df[,'x1'] + (plot_df[,'x2'] / 2), y =~ y,
                                       text =ifelse(plot_df[['x2']] > 0, paste(round(plot_df[['x2']],1), '%'), ""),
                                       font = list(family = 'Arial', size = 12,
                                                   color = '#000000'),
                                       showarrow = FALSE) 
        
        top_labels_x_position = c(top_labels_x_position, max(plot_df[,'x1']) + (max(plot_df[,'x2']) / 2))
      }
      
      
      if((ncol(plot_df)-1) > 2){
        
        
        for(i in 3:(ncol(plot_df)-1)){
          
          # labeling the percentages of each bar (x_axis)
          fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                                         x = rowSums(plot_df[,paste0('x',(1:(i-1)))]) + (plot_df[,paste0('x',i)] / 2), y =~ y,
                                         text = ifelse(plot_df[[paste0('x',i)]] > 0, paste(round(plot_df[[paste0('x',i)]],1), '%'), ""),
                                         font = list(family = 'Arial', size = 12,
                                                     color = '#000000'),
                                         showarrow = FALSE) 
          
          top_labels_x_position = c(top_labels_x_position, max(rowSums(plot_df[,paste0('x',(1:(i-1)))])) + (max(plot_df[1,paste0('x',i)]) / 2))
        }
        
      }
      
      
      fig
      
    }
    
  })
  
  
  ## Process Table
  observe({
    
    filtered_survey <- req(RV$filtered_survey)
    
    colnames(filtered_survey) = req(RV$survey_questions)
    
    RV$filtered_survey_table = filtered_survey
    
  })
  
  ## Table
  output$filtered_data_table = renderDT(req(RV$filtered_survey_table),options = list(scrollX = TRUE))
  
  
}







shinyApp(ui, server)








