function(input, output, session){
  # infoboxes
  output$total <- renderInfoBox(
    infoBox("Total Animes", nrow(anime1), icon("chart-bar"))
  )
  output$avg_score <- renderInfoBox(
    infoBox("Average Score", sprintf("%.2f", mean(anime1$score, na.rm = TRUE)), icon("chart-bar"))
  )
  output$avg_member <- renderInfoBox(
    infoBox("Average Number Watching", sprintf("%.2f", mean(anime1$members, na.rm = TRUE)), icon("chart-bar"))
  )
  # pie charts in the overview
  output$typePie <- renderPlot(
    anime1 %>% group_by(type) %>% summarise(count = n()) %>% 
      ggplot(aes(x="", y=count, fill=type)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + geom_text(aes(label = paste0(round((count/rows_anime1)*100), "%")), 
                                            position = position_stack(vjust = 0.5)) +
      ggtitle("Pie Chart of Type of Anime") + xlab(NULL) + ylab(NULL) + theme_bw()
  )
  
  output$genrePie <- renderPlot(
    anime_genre %>% group_by(genre) %>% summarise(count = n()) %>% arrange(desc(count)) %>% 
      head(10) %>% 
      ggplot(aes(x="", y=count, fill=genre)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + geom_text(aes(label = paste0(round((count/rows_genre)*100), "%")), 
                                            position = position_stack(vjust = 0.5)) +
      ggtitle("Pie Chart of Genre of Anime") + xlab(NULL) + ylab(NULL) + theme_bw()
  )
  
  output$sourcePie <- renderPlot(
    anime1 %>% group_by(source) %>% summarise(count = n()) %>% 
      ggplot(aes(x="", y=count, fill=source)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + geom_text(aes(label = paste0(round((count/rows_anime1)*100), "%")), 
                                            position = position_stack(vjust = 0.5)) +
      ggtitle("Pie Chart of Sources of Anime") + xlab(NULL) + ylab(NULL) + theme_bw()
  )
  
  output$ratingPie <- renderPlot(
    anime1 %>% group_by(rating) %>% summarise(count = n()) %>% 
      ggplot(aes(x="", y=count, fill=rating)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + geom_text(aes(label = paste0(round((count/rows_anime1)*100), "%")), 
                                            position = position_stack(vjust = 0.5)) +
      ggtitle("Pie Chart of Ratings of Anime") + xlab(NULL) + ylab(NULL) + theme_bw()
  )

  
  #type plot
  output$type <- renderPlot(
    if(input$selecttype == "score"){
      anime1 %>% filter(type %in% input$typecheckbox) %>% 
        ggplot() + geom_histogram(aes(x = score, y = ..density.., color = type, fill = type),  
                       alpha = 0.4, position = "identity") +
        geom_density(aes(x = score, color = type), size =1) + 
        ggtitle("Plot of Types of Anime VS. Scores") + theme_bw()
    }else{
      anime1 %>% filter(type %in% input$typecheckbox) %>% 
        mutate(outlier = is_outlier(.data[[input$selecttype]])) %>%
        filter(outlier == FALSE) %>%
        ggplot(aes_string(x = "type", y = input$selecttype)) + geom_boxplot() +
        ggtitle("Boxplots of Types of Anime") + theme_bw()
    }
  )
  
  #source plot
  output$source <- renderPlot(
    if(input$selectsource == "score"){
    anime1 %>% filter(source %in% input$sourcecheckbox) %>% 
      ggplot() +
        geom_histogram(aes(x = score, y = ..density.., color = source, fill = source),  
                     alpha = 0.4, position = "identity") +
        geom_density(aes(x = score, color = source), size =1) +
        ggtitle('Plot of Sources of Anime VS. Scores') + theme_bw()
    }else{
      anime1 %>% filter(source %in% input$sourcecheckbox) %>% 
        mutate(outlier = is_outlier(.data[[input$selectsource]])) %>%
        filter(outlier == FALSE) %>%
        ggplot(aes_string(x = "source", y = input$selectsource)) + geom_boxplot() +
        ggtitle('Boxplots of Sources of Anime') + theme_bw()
    }
  )
  
  #year plot
  output$year <- renderPlot(
    if(input$radioyear == "yes"){
      anime1 %>% filter(year > input$yearslider[1] & year < input$yearslider[2]) %>% 
        group_by(year) %>% summarise(count = n(), average_score = mean(score),
                                         average_raters = mean(scored_by),
                                         average_watching = mean(members),
                                         average_favorites = mean(favorites)) %>%
        mutate(outlier = is_outlier(.data[[input$selectyear]])) %>%
        filter(outlier == FALSE) %>%
        ggplot() + geom_point(aes_string(x = "year", y = input$selectyear)) +
        ggtitle("Scatterplots of anime statistics VS. years") + theme_bw()
    }else{
      anime1 %>% filter(year > input$yearslider[1] & year < input$yearslider[2]) %>% 
        group_by(year) %>% summarise(count = n(), average_score = mean(score),
                                     average_raters = mean(scored_by),
                                     average_watching = mean(members),
                                     average_favorites = mean(favorites)) %>%
        ggplot() + geom_point(aes_string(x = "year", y = input$selectyear)) +
        ggtitle("Scatterplots of anime statistics VS. years") + theme_bw()
    }
  )
  
  #rating plot
  output$rating <- renderPlot(
    if(input$selectrating == "score"){
    anime1 %>% filter(rating %in% input$ratingcheckbox) %>% 
      ggplot() +
        geom_histogram(aes(x = score, y = ..density.., color = rating, fill = rating),  
                     alpha = 0.4, position = "identity") +
        geom_density(aes(x = score, color = rating), size =1) + 
        ggtitle('Plot of Ratings of Anime VS. Scores') + theme_bw()
    }else{
      anime1 %>% filter(rating %in% input$ratingcheckbox) %>% 
        mutate(outlier = is_outlier(.data[[input$selectrating]])) %>%
        filter(outlier == FALSE) %>%
        ggplot(aes_string(x = "rating", y = input$selectrating)) + geom_boxplot() + 
        ggtitle('Boxplots of Ratings of Anime') + theme_bw()
    }
  )
  
  #duration plot
  output$duration <- renderPlot(
    if(input$radioduration == "yes"){
    anime1 %>% filter(duration > input$durationlider[1] & duration < input$durationlider[2]) %>% 
      group_by(duration) %>% summarise(count = n(), average_score = mean(score),
                                       average_raters = mean(scored_by),
                                       average_watching = mean(members),
                                       average_favorites = mean(favorites)) %>%
      mutate(outlier = is_outlier(.data[[input$selectduration]])) %>%
      filter(outlier == FALSE) %>%
      ggplot() + geom_point(aes_string(x = "duration", y = input$selectduration)) + 
        ggtitle('Plot of Characteristics of Anime VS. Durations') + theme_bw()
    }else{
      anime1 %>% filter(duration > input$durationlider[1] & duration < input$durationlider[2]) %>% 
        group_by(duration) %>% summarise(count = n(), average_score = mean(score),
                                         average_raters = mean(scored_by),
                                         average_watching = mean(members),
                                         average_favorites = mean(favorites)) %>%
        ggplot() + geom_point(aes_string(x = "duration", y = input$selectduration)) +
        ggtitle('Plot of Characteristics of Anime VS. Durations') + theme_bw()
    }
  )
  
  #studio plot
  anime_studio_list <- reactive({
    if(input$radiostudio == "top"){
      anime_studio %>% group_by(studio) %>% 
      summarise(count = n()) %>% arrange(desc(count)) %>% 
      head(input$studiotext) %>% pull(studio)
    }else{
      input$selectstudio1
    }
  })
  
  output$studio <- renderPlot(
    anime_studio %>% filter(studio %in% anime_studio_list()) %>% 
      group_by(studio) %>% summarise(count = n(), average_score = mean(score),
                                     average_raters = mean(scored_by),
                                     average_watching = mean(members),
                                     average_favorites = mean(favorites)) %>% 
      ggplot() + geom_bar(aes_string(x = "studio", y = input$selectstudio, fill = "studio"),
                          stat = 'identity') + 
      ggtitle('Plot of Characteristics of Anime VS. Studios') + theme_bw()
  )
  
  #genre plot
  anime_genre_list <- reactive({
    if(input$radiogenre == "top"){
    anime_genre_list <- anime_genre %>% group_by(genre) %>% 
      summarise(count = n()) %>% arrange(desc(count)) %>% 
      head(input$genretext) %>% pull(genre)
    }else{
      input$selectgenre1
    }
  })
  
  output$genre <- renderPlot(
    anime_genre %>% filter(genre %in% anime_genre_list()) %>% 
      group_by(genre) %>% summarise(count = n(), average_score = mean(score),
                                     average_raters = mean(scored_by),
                                     average_watching = mean(members),
                                     average_favorites = mean(favorites)) %>% 
      ggplot() + geom_bar(aes_string(x = "genre", y = input$selectgenre, fill = "genre"),
                          stat = 'identity') + 
      ggtitle('Plot of Characteristics of Anime VS. Genres') + theme_bw()
  )
  
  anime_artists <- reactive({
    if(input$radiosongs == "Opening_Theme"){
    anime_op %>% group_by(OP) %>% 
      summarise(count = n()) %>% arrange(desc(count)) %>% 
      top_n(input$songtext) %>% pull(OP)
    }else{
      anime_ed %>% group_by(ED) %>% 
        summarise(count = n()) %>% arrange(desc(count)) %>% 
        top_n(input$songtext) %>% pull(ED)
    }
  })
  
  output$songs1 <- renderPlot(
    if(input$radiosongs == "Opening_Theme"){
      anime_op$iftop = ifelse(anime_op$OP %in% anime_artists(), "Top Artist", "Not Top Artist")
      anime_op %>% ggplot(aes(x = iftop, y = score)) + geom_boxplot() +
        ggtitle('Boxplots on Scores of Top and Non-top Artists') + theme_bw()
    }else{
      anime_ed$iftop = ifelse(anime_ed$ED %in% anime_artists(), "Top Artist", "Not Top Artist")
      anime_ed %>% ggplot(aes(x = iftop, y = score)) + geom_boxplot() +
        ggtitle('Boxplots on Scores of Top and Non-top Artists') + theme_bw()
    }
  )
  
  output$songs2 <- renderPlot(
    if(input$radiosongs == "Opening_Theme"){
      anime_op$iftop = ifelse(anime_op$OP %in% anime_artists(), "Top Artist", "Not Top Artist")
      anime_op %>% mutate(outlier = is_outlier(.data[["members"]])) %>%
        filter(outlier == FALSE) %>%
        ggplot(aes(x = iftop, y = members)) + geom_boxplot() +
        ggtitle('Boxplots on Number Watching of Top and Non-top Artists') + theme_bw()
    }else{
      anime_ed$iftop = ifelse(anime_ed$ED %in% anime_artists(), "Top Artist", "Not Top Artist")
      anime_ed %>%  mutate(outlier = is_outlier(.data[["members"]])) %>%
        filter(outlier == FALSE) %>%
        ggplot(aes(x = iftop, y = members)) + geom_boxplot() + 
        ggtitle('Boxplots on Number Watching of Top and Non-top Artists') + theme_bw()
    }
  )
  
  output$corr <- renderPlot(
    ggcorr(anime1[input$selectcorr], label = TRUE, palette = "RdGy",
           label_size = 5, label_round = 2, label_color = "black") +
      ggtitle("Correlation Plots")
  )
  
  scatter_plot_stat <- reactive(
    anime1[is_outlier(anime1[,input$selecttwo[1]]) == F &
                     is_outlier(anime1[,input$selecttwo[2]]) == F,]
  )
  
  output$scatter <- renderPlot(
    ggplot(scatter_plot_stat(), (aes(x = scatter_plot_stat()[,input$selecttwo[1]],
                y = scatter_plot_stat()[,input$selecttwo[2]])), na.rm = T) + geom_point() +
      geom_smooth(method='lm') + xlab(input$selecttwo[1]) + ylab(input$selecttwo[2]) + 
      ggtitle("Scatter Plots of Two Variables") + theme_bw()
  )

  #data table
  output$table <- DT::renderDataTable({
    datatable(anime1, options = list(scrollX = TRUE,autoWidth = TRUE), rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
}
