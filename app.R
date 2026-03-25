library(shiny)
library(tidyverse)
library(bslib)
library(miscTools)

load('../data/lm_acc.RDS')
load('../data/HAV_acc.RDS')
load('../data/rf_acc.RDS')
load('../data/svm_acc.RDS')
load('../data/robust_vol.RDS')
load('../data/robust_area.RDS')
load('../data/area_ranking.Rds')
load('../data/survey.RData')
load('../data/inter_score.RData')

# Accuracy score computation
median_acc = data.frame(colMedians(acc.lm, na.rm=TRUE),
                      colMedians(acc.HAV, na.rm=TRUE),
                      colMedians(acc.rf, na.rm=TRUE),
                      colMedians(acc.svm, na.rm=TRUE))
colnames(median_acc) = c('Linear Regression', 'HAV-WLS', 'Random Forest', 'Support Vector Machine')

error_normalised = as.data.frame(lapply(median_acc, function(x) x/rowSums(median_acc)))
acc_score = as.data.frame(t(1/colMeans(error_normalised)))
colnames(acc_score) = c('Linear Regression', 'HAV-WLS', 'Random Forest', 'Support Vector Machine')

# Robustness score computation
data_to_normalise = combined_area[, 2:5]
combined_area_normalised = as.data.frame(lapply(data_to_normalise, function(x) x / rowSums(data_to_normalise)))
robustness.scores_per_area = colSums(combined_area_normalised)
robust_score = as.data.frame(t(1/robustness.scores_per_area))
colnames(robust_score) = c('Linear Regression', 'HAV-WLS', 'Random Forest', 'Support Vector Machine')

# Normalised metric scores
acc_score1 = as.numeric(acc_score)
robust_score1 = as.numeric(robust_score)
tech_inter_score1 = as.numeric(inter_score[1,2:5])
general_inter_score1 = as.numeric(inter_score[2,2:5])

norm_acc_score = (acc_score1 - min(acc_score1))/(max(acc_score1)-min(acc_score1))
norm_robust_score = (robust_score1 - min(robust_score1))/
                        (max(robust_score1)-min(robust_score1))
norm_tech_inter_score = (tech_inter_score1 - min(tech_inter_score1))/
                        (max(tech_inter_score1)- min(tech_inter_score1))
norm_general_inter_score = (general_inter_score1 - min(general_inter_score1))/
                        (max(general_inter_score1)- min(general_inter_score1))

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = bs_theme(bootswatch = "journal"),
    "DATA3888 Optiver Project: Realised Volatility Prediction",
  
    tabPanel(
      title = "Accuracy",
      titlePanel("Prediction Errors"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "acc_metric",
                      label = "Accuracy metric:",
                      choices = c('MSE', 'QLIKE', 'MAE', 'RMSPE'),
                      selected = "MSE")
        ),
          mainPanel(
            tabsetPanel(
              tabPanel(title = 'Linear Regession',
                       plotOutput('lm_acc')),
              tabPanel(title = 'HAV-WLS',
                       plotOutput('hav_acc')),
              tabPanel(title = 'Random Forest',
                       plotOutput('rf_acc')),
              tabPanel(title = 'Support Vector Machine',
                       plotOutput('svm_acc')),
              tabPanel(title = 'Overview',
                       br(),
                       selectInput(inputId = "acc_table",
                                    label = "Summary Table: ",
                                    choices = c('Median Errors', 'Ranking', 'Accuracy Score'),
                                    selected = "Median Errors"),
                       tableOutput('acc_table'))
              )
          )
      )
    ),
    tabPanel(
      title = "Robustness",
      titlePanel("Adversarial Area"),
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(inputId = "replace_level",
                       label = "Level of Data Contamination",
                       choices = c('5%', '10%', '15%'),
                       selected = c('5%', '10%', '15%'))
        ),
        mainPanel(
            tabsetPanel(
              tabPanel(title = 'Linear Regession',
                       plotOutput('lm_robust')),
              tabPanel(title = 'HAV-WLS',
                       plotOutput('hav_robust')),
              tabPanel(title = 'Random Forest',
                       plotOutput('rf_robust')),
              tabPanel(title = 'Support Vector Machine',
                       plotOutput('svm_robust')),
              tabPanel(title = 'Overview',
                       br(),
                       selectInput(inputId = "robust_table",
                                    label = "Summary Table: ",
                                    choices = c('Area of Difference', 'Ranking', 'Robustness Score'),
                                    selected = "Area of Difference"),
                       tableOutput('robust_table'))
              )
        )
      )
    ),
    tabPanel(
      title = "Interpretability",
      titlePanel("Survey Data"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = 'understanding',
                      label = "Understanding of Model:",
                      choices = c('Understanding Confidence', 'Tested Understanding'),
                      selected = 'Understanding Confidence')
        ),
        mainPanel(
            tabsetPanel(
              tabPanel(title = 'Linear Regession',
                       plotOutput('lm_inter')),
              tabPanel(title = 'HAV-WLS',
                       plotOutput('hav_inter')),
              tabPanel(title = 'Random Forest',
                       plotOutput('rf_inter')),
              tabPanel(title = 'Support Vector Machine',
                       plotOutput('svm_inter')),
              tabPanel(title = 'Overview',
                       align = 'center',
                       br(),
                       h5('Weighted Interpretability Score'),
                       br(),
                       tableOutput('inter_table'))
              )
        )
      )
    ),
    
    tabPanel(
      title = "Model Recommendation",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "stakeholder",
                      label = 'Stakeholder:',
                      choices = c('Technical', 'General'),
                      selected = 'Technical'),
          selectInput(inputId = "metric1",
                      label = "Metric 1:",
                      choices = c('Accuracy', 'Robustness', 'Interpretability'),
                      selected = "Accuracy"),
          sliderInput(inputId = "weight1",
                      label='Weight:',
                      min=0, max=1, value=0.5, step=0.05),
          selectInput(inputId = "metric2",
                      label = "Metric 2:",
                      choices = c('Accuracy', 'Robustness', 'Interpretability'),
                      selected = "Robustness"),
          sliderInput(inputId = "weight2",
                      label='Weight:',
                      min=0, max=1, value=0.3, step=0.05)
        ),
        mainPanel(
          align = 'center',
          plotOutput("scatterPlot"),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          "Weighted Score for Model Selection",
          tableOutput("modelSelection")
        )
      )
    )
    
    # tabPanel(
    #   title = "Model Selection",
    #   sidebarLayout(
    #     sidebarPanel(
    #       selectInput(inputId = "rank1",
    #                   label = "Highest weighted metric:",
    #                   choices = c('Accuracy', 'Robustness', 'Interpretability'),
    #                   selected = "Accuracy"),
    #        selectInput(inputId = "rank2",
    #                   label = "Second weighted metric:",
    #                   choices = c('Accuracy', 'Robustness', 'Interpretability'),
    #                   selected = "Robustness")
    #     ),
    #     mainPanel(
    #       plotOutput("scatterPlot")
    #     )
    #   )
    #)
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ## Accuracy
    output$lm_acc <- renderPlot({
      if(input$acc_metric == 'MSE'){
        boxplot(acc.lm$MSE , horizontal = TRUE, main='Distribution of MSE')
      } else if(input$acc_metric == 'QLIKE') {
        boxplot(acc.lm$QLIKE , horizontal = TRUE, main='Distribution of QLIKE')
      } else if(input$acc_metric == 'MAE') {
        boxplot(acc.lm$MAE , horizontal = TRUE, main='Distribution of MAE')
      } else {
        boxplot(acc.lm$RMSPE , horizontal = TRUE, main='Distribution of RMSPE')
      }
    })
    
    output$hav_acc <- renderPlot({
      if(input$acc_metric == 'MSE'){
        boxplot(acc.HAV$MSE , horizontal = TRUE, main='Distribution of MSE')
      } else if(input$acc_metric == 'QLIKE'){
        boxplot(acc.HAV$QLIKE , horizontal = TRUE, main='Distribution of QLIKE')
      } else if(input$acc_metric == 'MAE') {
        boxplot(acc.HAV$MAE , horizontal = TRUE, main='Distribution of MAE')
      } else {
        boxplot(acc.HAV$RMSPE, horizontal = TRUE, main='Distribution of RMSPE')
      }
    })
    
    output$rf_acc <- renderPlot({
      if(input$acc_metric == 'MSE'){
        boxplot(acc.rf$MSE , horizontal = TRUE, main='Distribution of MSE')
      } else if(input$acc_metric == 'QLIKE'){
        boxplot(acc.rf$QLIKE , horizontal = TRUE, main='Distribution of QLIKE')
      } else if(input$acc_metric == 'MAE') {
        boxplot(acc.rf$MAE , horizontal = TRUE, main='Distribution of MAE')
      } else {
        boxplot(acc.rf$RMSPE , horizontal = TRUE, main='Distribution of RMSPE')
      }
    })
    
    output$svm_acc <- renderPlot({
      if(input$acc_metric == 'MSE'){
        boxplot(acc.svm$MSE , horizontal = TRUE, main='Distribution of MSE')
      } else if(input$acc_metric == 'QLIKE'){
        boxplot(acc.svm$QLIKE , horizontal = TRUE, main='Distribution of QLIKE')
      } else if(input$acc_metric == 'MAE') {
        boxplot(acc.svm$MAE , horizontal = TRUE, main='Distribution of MAE')
      } else {
        boxplot(acc.svm$RMSPE , horizontal = TRUE, main='Distribution of RMSPE')
      }
    })
    
    output$acc_table <- renderTable({
      if(input$acc_table == 'Median Errors'){
        tab = rownames_to_column(median_acc, 'Metric')
        format(tab, scientific = TRUE, digits = 4)
      } else if(input$acc_table == 'Ranking'){
        acc_rank = apply(median_acc, 1, function(x) rank(x, ties.method = "min"))
        acc_rank = as.data.frame(t(acc_rank))
        rownames_to_column(acc_rank, 'Metric')
      } else {
        acc_score
      }
    })
    
    
    ## Robustness
    output$lm_robust <- renderPlot({
      p = ggplot(robust.df[[1]], aes(x=time)) + 
        geom_line(aes(y=pred, color='0%')) +
        geom_point(aes( y=pred, color='0%'), size = 1, alpha = 0.6) +
        theme_minimal() +
        theme(text=element_text(size=14))
      
      if('5%' %in% input$replace_level){
        p = p + 
          geom_line(aes(y=train_5, color='5%')) +
          geom_point(aes(y=train_5, color='5%'), size = 1, alpha = 0.6)
      }

      if('10%' %in% input$replace_level){
        p = p +
          geom_line(aes(y=train_10, color='10%')) +
          geom_point(aes(y=train_10, color='10%'), size = 1, alpha = 0.6)
      }

      if('15%' %in% input$replace_level){
        p = p +
          geom_line(aes(y=train_15, color='15%')) +
          geom_point(aes(y=train_15, color='15%'), size = 1, alpha = 0.6)
      }
      p + 
        labs(x='Time', y='Predicted Realised Volatility') +
        scale_color_manual(name = "Data Contamination Level", 
                           values = c("0%" = "#f9938c", "5%" = "#7dae02", "10%" = "#24babd", "15%" = "#cc86ec"),
                           breaks = c("0%", "5%", "10%", "15%")) +
        ylim(min(robust.df[[1]][,2:5]),max(robust.df[[1]][,2:5]))
    })
    
    output$hav_robust <- renderPlot({
      p = ggplot(robust.df[[2]], aes(x=time)) + 
        geom_line(aes(y=pred, color='0%')) +
        geom_point(aes( y=pred, color='0%'), size = 1, alpha = 0.6) +
        theme_minimal() +
        theme(text=element_text(size=14))
      
      if('5%' %in% input$replace_level){
        p = p + 
          geom_line(aes(y=train_5, color='5%')) +
          geom_point(aes(y=train_5, color='5%'), size = 1, alpha = 0.6)
      }

      if('10%' %in% input$replace_level){
        p = p +
          geom_line(aes(y=train_10, color='10%')) +
          geom_point(aes(y=train_10, color='10%'), size = 1, alpha = 0.6)
      }

      if('15%' %in% input$replace_level){
        p = p +
          geom_line(aes(y=train_15, color='15%')) +
          geom_point(aes(y=train_15, color='15%'), size = 1, alpha = 0.6)
      }
      p + 
        labs(x='Time', y='Predicted Realised Volatility') +
        scale_color_manual(name = "Data Contamination Level", 
                           values = c("0%" = "#f9938c", "5%" = "#7dae02", "10%" = "#24babd", "15%" = "#cc86ec"),
                           breaks = c("0%", "5%", "10%", "15%")) +
        ylim(min(robust.df[[2]][,2:5]), max(robust.df[[2]][,2:5]))
    })
    
    output$rf_robust <- renderPlot({
      p = ggplot(robust.df[[3]], aes(x=time)) + 
        geom_line(aes(y=pred, color='0%')) +
        geom_point(aes( y=pred, color='0%'), size = 1, alpha = 0.6) +
        theme_minimal() +
        theme(text=element_text(size=14))
      
      if('5%' %in% input$replace_level){
        p = p + 
          geom_line(aes(y=train_5, color='5%')) +
          geom_point(aes(y=train_5, color='5%'), size = 1, alpha = 0.6)
      }

      if('10%' %in% input$replace_level){
        p = p +
          geom_line(aes(y=train_10, color='10%')) +
          geom_point(aes(y=train_10, color='10%'), size = 1, alpha = 0.6)
      }

      if('15%' %in% input$replace_level){
        p = p +
          geom_line(aes(y=train_15, color='15%')) +
          geom_point(aes(y=train_15, color='15%'), size = 1, alpha = 0.6)
      }
      p + 
        labs(x='Time', y='Predicted Realised Volatility') +
        scale_color_manual(name = "Data Contamination Level", 
                           values = c("0%" = "#f9938c", "5%" = "#7dae02", "10%" = "#24babd", "15%" = "#cc86ec"),
                           breaks = c("0%", "5%", "10%", "15%")) +
        ylim(min(robust.df[[3]][,2:5]), max(robust.df[[3]][,2:5]))
    })
    
    output$svm_robust <- renderPlot({
      p = ggplot(robust.df[[4]], aes(x=time)) + 
        geom_line(aes(y=pred, color='0%')) +
        geom_point(aes(y=pred, color='0%'), size = 1, alpha = 0.6) +
        theme_minimal() +
        theme(text=element_text(size=14))
      
      if('5%' %in% input$replace_level){
        p = p + 
          geom_line(aes(y=train_5, color='5%')) +
          geom_point(aes(y=train_5, color='5%'), size = 1, alpha = 0.6)
      }

      if('10%' %in% input$replace_level){
        p = p +
          geom_line(aes(y=train_10, color='10%')) +
          geom_point(aes(y=train_10, color='10%'), size = 1, alpha = 0.6)
      }

      if('15%' %in% input$replace_level){
        p = p +
          geom_line(aes(y=train_15, color='15%')) +
          geom_point(aes(y=train_15, color='15%'), size = 1, alpha = 0.6)
      }
      p + 
        labs(x='Time', y='Predicted Realised Volatility') +
        scale_color_manual(name = "Data Contamination Level", 
                           values = c("0%" = "#f9938c", "5%" = "#7dae02", "10%" = "#24babd", "15%" = "#cc86ec"),
                           breaks = c("0%", "5%", "10%", "15%")) +
        ylim(min(robust.df[[4]][,2:5]), max(robust.df[[4]][,2:5]))
    })
    
    output$robust_table<- renderTable({
      if(input$robust_table == 'Area of Difference'){
        combined_area
      } else if(input$robust_table == 'Ranking'){
        area_ranking_table
      } else {
        robust_score
      }
    }, digits=6)
    
    ## Interpretability
    output$lm_inter <- renderPlot({
      if(input$understanding == 'Understanding Confidence'){
        p2 = survey |>
            group_by(Q3, Q2) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=Q3, y=count, fill=Q2)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='', y='Count', fill='Machine learning proficiency') +
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))

        p1 = survey |>
              group_by(Q3, Q1) |> 
              summarise(count = n()) |> 
              ungroup() |> 
              ggplot(aes(x=Q3, y=count, fill=Q1)) +
                geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
                labs(x='', y='Count', fill='Finance proficiency') +
                theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        gridExtra::grid.arrange(p1, p2, ncol=2)
      } else{
        p1 = survey |>
            group_by(LinearTest, Q1) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=LinearTest, y=count, fill=Q1)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='Number of correct answers', y='Count', fill='Finance proficiency') +
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        
        p2 = survey |>
            group_by(LinearTest, Q2) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=LinearTest, y=count, fill=Q2)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='Number of correct answers', y='Count', fill='Machine learning proficiency') + 
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        gridExtra::grid.arrange(p1, p2, ncol=2)
      }
    })
      
    output$hav_inter <- renderPlot({
      if(input$understanding == 'Understanding Confidence'){
        p2 = survey |>
            group_by(Q6, Q2) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=Q6, y=count, fill=Q2)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='', y='Count', fill='Machine learning proficiency') +
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))

        p1 = survey |>
              group_by(Q6, Q1) |> 
              summarise(count = n()) |> 
              ungroup() |> 
              ggplot(aes(x=Q6, y=count, fill=Q1)) +
                geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
                labs(x='', y='Count', fill='Finance proficiency') +
                theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        gridExtra::grid.arrange(p1, p2, ncol=2)
      } else{
        p1 = survey |>
            group_by(HAVTest, Q1) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=HAVTest, y=count, fill=Q1)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='Number of correct answers', y='Count', fill='Finance proficiency') +
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        
        p2 = survey |>
            group_by(HAVTest, Q2) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=HAVTest, y=count, fill=Q2)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='Number of correct answers', y='Count', fill='Machine learning proficiency') + 
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        gridExtra::grid.arrange(p1, p2, ncol=2)
      }
    })
    
    output$rf_inter <- renderPlot({
      if(input$understanding == 'Understanding Confidence'){
        p2 = survey |>
            group_by(Q14, Q2) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=Q14, y=count, fill=Q2)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='', y='Count', fill='Machine learning proficiency') +
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))

        p1 = survey |>
              group_by(Q14, Q1) |> 
              summarise(count = n()) |> 
              ungroup() |> 
              ggplot(aes(x=Q14, y=count, fill=Q1)) +
                geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
                labs(x='', y='Count', fill='Finance proficiency') +
                theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        gridExtra::grid.arrange(p1, p2, ncol=2)
      } else{
        p1 = survey |>
            group_by(RFTest, Q1) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=RFTest, y=count, fill=Q1)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='Number of correct answers', y='Count', fill='Finance proficiency') +
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        
        p2 = survey |>
            group_by(RFTest, Q2) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=RFTest, y=count, fill=Q2)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='Number of correct answers', y='Count', fill='Machine learning proficiency') + 
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        gridExtra::grid.arrange(p1, p2, ncol=2)
      }
    })
    
    output$svm_inter <- renderPlot({
      if(input$understanding == 'Understanding Confidence'){
        p2 = survey |>
            group_by(Q9, Q2) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=Q9, y=count, fill=Q2)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='', y='Count', fill='Machine learning proficiency') +
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))

        p1 = survey |>
              group_by(Q9, Q1) |> 
              summarise(count = n()) |> 
              ungroup() |> 
              ggplot(aes(x=Q9, y=count, fill=Q1)) +
                geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
                labs(x='', y='Count', fill='Finance proficiency') +
                theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        gridExtra::grid.arrange(p1, p2, ncol=2)
      } else{
        p1 = survey |>
            group_by(SVMTest, Q1) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=SVMTest, y=count, fill=Q1)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='Number of correct answers', y='Count', fill='Finance proficiency') +
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        
        p2 = survey |>
            group_by(SVMTest, Q2) |> 
            summarise(count = n()) |> 
            ungroup() |> 
            ggplot(aes(x=SVMTest, y=count, fill=Q2)) +
              geom_bar(stat="identity", position = 'stack', alpha = 0.75) + 
              labs(x='Number of correct answers', y='Count', fill='Machine learning proficiency') + 
              theme(text = element_text(size=12), legend.position = "bottom", legend.text = element_text(size=12))
        gridExtra::grid.arrange(p1, p2, ncol=2)
      }
    })
    
    output$inter_table <- renderTable({
      inter_score
    }, digits=4)
    
    ## Summary
    output$scatterPlot <- renderPlot({
      # Metric scores combined
      if(input$stakeholder == 'Technical'){
        eval = data.frame('Model' = c('Linear Regression', 'HAV-WLS', 'Random Forest', 'Support Vector Machine'),
                          'Accuracy' = norm_acc_score,
                          'Robustness' = norm_robust_score,
                          'Interpretability' = norm_tech_inter_score)
      } else {
        eval = data.frame('Model' = c('Linear Regression', 'HAV-WLS', 'Random Forest', 'Support Vector Machine'),
                          'Accuracy' = norm_acc_score,
                          'Robustness' = norm_robust_score,
                          'Interpretability' = norm_general_inter_score)
      }
      p = eval |> 
        ggplot(aes(x=.data[[input$metric1]], y=.data[[input$metric2]], color=Model)) +
        geom_point(size = 5)+
        theme_minimal()+
        theme(text=element_text(size=15))
      p
    }, width = 800, height = 500)
    
    output$modelSelection <- renderTable({
      if(input$stakeholder == 'Technical'){
        eval = data.frame('Model' = c('Linear Regression', 'HAV-WLS', 'Random Forest', 'Support Vector Machine'),
                          'Accuracy' = norm_acc_score,
                          'Robustness' = norm_robust_score,
                          'Interpretability' = norm_tech_inter_score)
      } else {
        eval = data.frame('Model' = c('Linear Regression', 'HAV-WLS', 'Random Forest', 'Support Vector Machine'),
                          'Accuracy' = norm_acc_score,
                          'Robustness' = norm_robust_score,
                          'Interpretability' = norm_general_inter_score)
      }
      metric3 = colnames(eval[,-1])[!colnames(eval[,-1]) %in% c(input$metric1, input$metric2)]
      weight3 = 1-(input$weight1 + input$weight2)
      final_score = input$weight1 * eval[[input$metric1]] + input$weight2 * eval[[input$metric2]] + weight3* eval[[metric3]]
      tab = data.frame(t(final_score))
      colnames(tab) = c('Linear Regression', 'HAV-WLS', 'Random Forest', 'Support Vector Machine')
      tab
    }, digits=4)
}

# Run the application 
shinyApp(ui = ui, server = server)
