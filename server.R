function(input, output, session){
  # infoboxes
  output$total <- renderInfoBox(
    infoBox("Total Animes", nrow(anime1), icon("chart-bar"))
  )
  output$avg_score <- renderInfoBox(
    infoBox("Average Score", sprintf("%.2f", mean(anime1$score, na.rm = TRUE)), icon("chart-bar"))
  )
  output$avg_member <- renderInfoBox(
    infoBox("Average Number Watching", sprintf("%.2f", mean(anime1$members, na.rm = TRUE)), 
            icon("chart-bar"))
  )
  # pie charts in the overview
  output$typeHist <- renderPlot(
    anime1 %>% group_by(type) %>% summarise(count = n()) %>% 
      ggplot(aes(x=reorder(.$type, -count) , y=count, fill=type)) + geom_bar(stat="identity") +
      ggtitle("Histogram of Type of Anime") + theme_bw() + xlab("types of the aniem") +
      theme(plot.title = element_text(size = 20, face = "bold")) 
  )
  
  output$genreHist <- renderPlot(
    anime_genre %>% group_by(genre) %>% summarise(count = n()) %>% arrange(desc(count)) %>% 
      top_n(10) %>% 
      ggplot(aes(x=reorder(.$genre, -count), y=count, fill=genre)) + geom_bar(stat="identity") +
      ggtitle("Histogram of top 10 Genre of Anime") + theme_bw() + xlab("genres of the anime") +
      theme(plot.title = element_text(size = 20, face = "bold"))
  )
  
  output$sourceHist <- renderPlot(
    anime1 %>% group_by(source) %>% summarise(count = n()) %>% 
      ggplot(aes(x=reorder(.$source, -count), y=count, fill=source)) + geom_bar(stat="identity") + 
      ggtitle("Histogram of Sources of Anime") + theme_bw() + xlab("sources of the anime") + 
      theme(plot.title = element_text(size = 20, face = "bold"))
  )
  
  output$ratingHist <- renderPlot(
    anime1 %>% group_by(rating) %>% summarise(count = n()) %>% 
      ggplot(aes(x=reorder(.$rating, -count), y=count, fill=rating)) + geom_bar(stat="identity") +
      ggtitle("Histogram of Ratings of Anime") +  theme_bw() + xlab("ratings of the anime") +
      theme(plot.title = element_text(size = 20, face = "bold"))
  )

  
  #type plot/table
  output$type <- renderPlot(
    if(input$selecttype == "score"){
      anime1 %>% filter(type %in% input$typecheckbox) %>% 
        ggplot() + geom_histogram(aes(x = score, y = ..density.., color = type, fill = type),  
                       alpha = 0.4, position = "identity") +
        geom_density(aes(x = score, color = type), size =1) + 
        ggtitle("Plot of Types of Anime VS. Scores") + theme_bw() + 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold")) +
        theme(axis.title = element_text(size = 12, face = "bold"))
    }else{
      anime1 %>% filter(type %in% input$typecheckbox) %>% 
        mutate(outlier = is_outlier(.data[[paste(strsplit(input$selecttype, " ")[[1]], 
                                                 collapse = "_")]])) %>% filter(outlier == FALSE) %>%
        ggplot(aes_string(x = "type", y = input$selecttype, color = "type")) + geom_boxplot() +
        ggtitle("Boxplots of Types of Anime") + theme_bw() + 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold")) +
        theme(axis.title = element_text(size = 12, face = "bold"))
    }
  )
  
  output$typeTable <- renderTable(
    if(input$radiotype == "Yes"){
      anime1 %>% filter(type %in% input$typecheckbox) %>% 
        mutate(outlier = is_outlier(.data[[paste(strsplit(input$selecttype, " ")[[1]], 
                                                 collapse = "_")]])) %>%
        filter(outlier == FALSE) %>% group_by(type) %>% 
        summarise(mean_score = mean(score), min_score = min(score), max_score = max(score),
                  mean_watchers = mean(members), min_watchers = min(members), max_watchers = max(members))
    }else{
      anime1 %>% filter(type %in% input$typecheckbox) %>% group_by(type) %>% 
        summarise(mean_score = mean(score), min_score = min(score), 
                  max_score = max(score), mean_watchers = mean(members), 
                  min_watchers = min(members), max_watchers = max(members)) 
    },caption = "Summary Statistics of Types of Anime"
  )
  
  # type statistical test
  type_test_table <- reactive({anime1 %>% filter(type %in% input$typecheckbox1)})

  output$typetext1 <- renderPrint({ summary(aov(score ~ type, data = type_test_table())) })
  output$typetext2 <- renderPrint({ summary(aov(members ~ type, data = type_test_table())) })
  
  #source plot/table
  output$source <- renderPlot(
    if(input$selectsource == "score"){
    anime1 %>% filter(source %in% input$sourcecheckbox) %>% 
      ggplot() +
        geom_histogram(aes(x = score, y = ..density.., color = source, fill = source),  
                     alpha = 0.4, position = "identity") +
        geom_density(aes(x = score, color = source), size =1) +
        ggtitle('Plot of Sources of Anime VS. Scores') + theme_bw() + 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }else{
      anime1 %>% filter(source %in% input$sourcecheckbox) %>% 
        mutate(outlier = is_outlier(.data[[paste(strsplit(input$selectsource, " ")[[1]],
                                                 collapse = "_")]])) %>% 
        filter(outlier == FALSE) %>%
        ggplot(aes_string(x = "source", y = input$selectsource, color = "source")) + geom_boxplot() +
        ggtitle('Boxplots of Sources of Anime') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }
  )
  
  output$sourceTable <- renderTable(
    if(input$radiosource == "Yes"){
      anime1 %>% filter(source %in% input$sourcecheckbox) %>% 
        mutate(outlier = is_outlier(.data[[paste(strsplit(input$selectsource, " ")[[1]],
                                                 collapse = "_")]])) %>%
        filter(outlier == FALSE) %>% group_by(source) %>% 
        summarise(mean_score = mean(score), min_score = min(score), max_score = max(score),
                  mean_watchers = mean(members), min_watchers = min(members), max_watchers = max(members))
    }else{
      anime1 %>% filter(source %in% input$sourcecheckbox) %>% group_by(source) %>% 
        summarise(mean_score = mean(score), min_score = min(score), max_score = max(score),
                  mean_watchers = mean(members), min_watchers = min(members), max_watchers = max(members))
    },caption = "Summary Statistics of Sources of Anime"
  )
  
  # source statistical test
  source_test_table <- reactive({anime1 %>% filter(source %in% input$sourcecheckbox1)})
  
  output$sourcetext1 <- renderPrint({ summary(aov(score ~ source, data = source_test_table())) })
  output$sourcetext2 <- renderPrint({ summary(aov(members ~ source, data = source_test_table())) })
  
  #year plot
  year_table <- reactive({
    anime1 %>% filter(year > input$yearslider[1] & year < input$yearslider[2]) %>% 
      group_by(year) %>% summarise(count = n(), average_score = mean(score),
                                   average_raters = mean(raters),
                                   average_watching = mean(members),
                                   average_favorites = mean(favorites))
  })
  
  output$year <- renderPlot(
    if(input$radioyear == "yes"){
       year_table() %>%
        mutate(outlier = is_outlier(.data[[paste(strsplit(input$selectyear, " ")[[1]],
                                                 collapse = "_")]])) %>%
        filter(outlier == FALSE) %>%
        ggplot() + geom_point(aes_string(x = "year", 
                                         y = paste(strsplit(input$selectyear, " ")[[1]], 
                                                   collapse = "_"))) +
        ggtitle("Scatterplots of anime statistics VS. years") + theme_bw() + 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }else{
        year_table() %>%
        ggplot() + geom_point(aes_string(x = "year", y = paste(strsplit(input$selectyear, " ")[[1]],
                                                               collapse = "_"))) +
        ggtitle("Scatterplots of anime statistics VS. years") + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }
  )
  
  
  output$year1 <- renderInfoBox({
    minyear <- year_table() %>% arrange(average_score) %>% head(1)
    infoBox("Lowest Average Year/Score", value = paste("Year:", minyear$year, ",",
                                                       "Score:", sprintf("%.2f",minyear$average_score)))
  })
  
  output$year2 <- renderInfoBox({
    maxyear <- year_table() %>% arrange(desc(average_score)) %>% head(1)
    infoBox("Highest Average Year/Score", value = paste("Year:", maxyear$year, ",",
                                                        "Score:", sprintf("%.2f", maxyear$average_score)))
  })
  
  output$year3 <- renderInfoBox({
    averageScore <- anime1 %>% filter(year > input$yearslider[1] & year < input$yearslider[2]) %>% 
      summarise(avg_score = mean(score)) %>% pull(avg_score)
    infoBox("Average Score", value = sprintf("%.2f", averageScore))
  })
  
  output$year4 <- renderInfoBox({
    averagewatch <- anime1 %>% filter(year > input$yearslider[1] & year < input$yearslider[2]) %>% 
      summarise(avg_watchers = mean(members)) %>% pull(avg_watchers)
    infoBox("Average Audience", value =  round(averagewatch))
  })
  
  #rating plot
  output$rating <- renderPlot(
    if(input$selectrating == "score"){
    anime1 %>% filter(rating %in% input$ratingcheckbox) %>% 
      ggplot() +
        geom_histogram(aes(x = score, y = ..density.., color = rating, fill = rating),  
                     alpha = 0.4, position = "identity") +
        geom_density(aes(x = score, color = rating), size =1) + 
        ggtitle('Plot of Ratings of Anime VS. Scores') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }else{
      anime1 %>% filter(rating %in% input$ratingcheckbox) %>% 
        mutate(outlier = is_outlier(.data[[paste(strsplit(input$selectrating, " ")[[1]],
                                                 collapse = "_")]])) %>%
        filter(outlier == FALSE) %>%
        ggplot(aes_string(x = "rating", y = paste(strsplit(input$selectrating, " ")[[1]],
                                                  collapse = "_"), color = "rating")) + geom_boxplot() + 
        ggtitle('Boxplots of Ratings of Anime') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }
  )
  
  output$ratingTable <- renderTable(
    if(input$radiorating == "Yes"){
      anime1 %>% filter(rating %in% input$ratingcheckbox) %>% 
        mutate(outlier = is_outlier(.data[[paste(strsplit(input$selectrating, " ")[[1]],
                                                 collapse = "_")]])) %>%
        filter(outlier == FALSE) %>% group_by(rating) %>% 
        summarise(mean_score = mean(score), min_score = min(score), max_score = max(score),
                  mean_watchers = mean(members), min_watchers = min(members), max_watchers = max(members))
    }else{
      anime1 %>% filter(rating %in% input$ratingcheckbox) %>% group_by(rating) %>% 
        summarise(mean_score = mean(score), min_score = min(score), max_score = max(score),
                  mean_watchers = mean(members), min_watchers = min(members), max_watchers = max(members))
    },caption = "Summary Statistics of Ratings of Anime"
  )
  
  # rating statistical test
  rating_test_table <- reactive({anime1 %>% filter(rating %in% input$ratingcheckbox1)})
  
  output$ratingtext1 <- renderPrint({ summary(aov(score ~ rating, data = rating_test_table())) })
  output$ratingtext2 <- renderPrint({ summary(aov(members ~ rating, data = rating_test_table())) })
  
  #duration plot
  duration_table <- reactive({
    anime1 %>% filter(duration > input$durationlider[1] & duration < input$durationlider[2]) %>% 
      group_by(duration) %>% summarise(count = n(), average_score = mean(score),
                                       average_raters = mean(raters),
                                       average_watching = mean(members),
                                       average_favorites = mean(favorites))
  })
  
  output$duration <- renderPlot(
    if(input$radioduration == "yes"){
     duration_table() %>%
        mutate(outlier = is_outlier(.data[[paste(strsplit(input$selectduration, " ")[[1]],
                                                 collapse = "_")]])) %>%
        filter(outlier == FALSE) %>%
        ggplot() + geom_point(aes_string(x = "duration", 
                                         y = paste(strsplit(input$selectduration, " ")[[1]], 
                                                   collapse = "_"))) + 
        ggtitle('Plot of Characteristics of Anime VS. Durations') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }else{
      duration_table()%>%
        ggplot() + geom_point(aes_string(x = "duration", y = paste(strsplit(input$selectduration, " ")[[1]],
                                                                   collapse = "_"))) +
        ggtitle('Plot of Characteristics of Anime VS. Durations') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }
  )
  
  output$duration1 <- renderInfoBox({
    minduration <- duration_table() %>% arrange(average_score) %>% head(1)
    infoBox("Lowest Average Score", value = paste(sprintf("%.2f",minduration$average_score),
                                                  "(occurs at",minduration$duration,"minutes)"))
  })
  
  output$duration2 <- renderInfoBox({
    maxduration <- duration_table() %>% arrange(desc(average_score)) %>% head(1)
    infoBox("Highest Average Score", value = paste(sprintf("%.2f",maxduration$average_score),
                                                   "(occurs at",maxduration$duration,"minutes)"))
  })
  
  output$duration3 <- renderInfoBox({
    averageScored <- mean(duration_table()$average_score)
    infoBox("Average Score", value = sprintf("%.2f", averageScored))
  })
  
  output$duration4 <- renderInfoBox({
    averagewatchd <- mean(duration_table()$average_watching)
    infoBox("Average Audience", value = round(averagewatchd))
  })
  
  #studio plot
  observe({
    if(input$radiostudio1 == "average productions per year"){
    updateSelectizeInput(session, 'selectstudio',
                          choices = displaychoice1,
                          selected = displaychoice1[1])
    }else{
      updateSelectizeInput(session, 'selectstudio',
                           choices = displaychoice,
                           selected = displaychoice[1])
      }
  })
  
  anime_studio_list <- reactive({
    if(input$radiostudio == "top"){
      if (input$radiostudio1 == "total productions"){
      anime_studio %>% group_by(studio) %>% 
      summarise(count = n()) %>% arrange(desc(count)) %>% 
      head(input$studiotext) %>% pull(studio)
      }else{
        anime_studio %>% group_by(studio) %>%
          summarise(avg_per_year = n()/length(unique(year))) %>% 
          arrange(desc(avg_per_year)) %>% head(input$studiotext) %>% pull(studio)
        }
    }else{
      input$selectstudio1
    }
  })
  
  output$studio <- renderPlot(
    anime_studio %>% filter(studio %in% anime_studio_list()) %>% 
      group_by(studio) %>% summarise(count = n(), average_per_year = n()/length(unique(year)),
                                     average_score = mean(score),
                                     average_raters = mean(raters),
                                     average_watching = mean(members),
                                     average_favorites = mean(favorites)) %>%
      ggplot() + scale_x_discrete(limits = anime_studio_list()) +
      geom_bar(aes_string(x = "studio", y = paste(strsplit(input$selectstudio, " ")[[1]], collapse = "_"),
                          fill = "studio"),stat = 'identity') +
      ggtitle('Plot of Characteristics of Anime VS. Studios') + theme_bw() + coord_flip() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
      theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
      theme(plot.title = element_text(size = 20, face = "bold"))+
      theme(axis.title = element_text(size = 12, face = "bold"))
  )
  
  output$studio1 <- renderInfoBox({
    averageScore <- anime_studio %>% filter(studio %in% anime_studio_list()) %>% 
      summarise(meanscore = mean(score)) %>% pull(meanscore)
    infoBox("Average Score", value = sprintf("%.2f", averageScore))
  })
  
  output$studio2 <- renderInfoBox({
    averagewatch <- anime_studio %>% filter(studio %in% anime_studio_list()) %>% 
      summarise(meanaudience = mean(members)) %>% pull(meanaudience)
    infoBox("Average Audience", value = round(averagewatch))
  })
  
  # studio statistical test
  studio_test_table <- reactive({anime1 %>% filter(studio %in% input$selectstudiotest)})
  
  output$studiotext1 <- renderPrint({ summary(aov(score ~ studio, data = studio_test_table())) })
  output$studiotext2 <- renderPrint({ summary(aov(members ~ studio, data = studio_test_table())) })
  
  #genre plot
  
  observe({
    if(input$radiogenre1 == "average productions per year"){
      updateSelectizeInput(session, 'selectgenre',
                           choices = displaychoice1,
                           selected = displaychoice1[1])
    }else{
      updateSelectizeInput(session, 'selectgenre',
                           choices = displaychoice,
                           selected = displaychoice[1])
    }
  })
  
  anime_genre_list <- reactive({
    if(input$radiogenre == "top"){
      if(input$radiogenre1 == "total productions"){
      anime_genre %>% group_by(genre) %>% 
      summarise(count = n()) %>% arrange(desc(count)) %>% 
      head(input$genretext) %>% pull(genre)
      }else{
        anime_genre %>% group_by(genre) %>%
          summarise(avg_per_year = n()/length(unique(year))) %>% 
          arrange(desc(avg_per_year)) %>% head(input$genretext) %>% pull(genre)
      }
    }else{
      input$selectgenre1
    }
  })
  
  
  output$genre <- renderPlot(
    anime_genre %>% filter(genre %in% anime_genre_list()) %>% 
      group_by(genre) %>% summarise(count = n(), average_per_year = n()/length(unique(year)),
                                     average_score = mean(score),
                                     average_raters = mean(raters),
                                     average_watching = mean(members),
                                     average_favorites = mean(favorites)) %>% 
      ggplot() + scale_x_discrete(limits = anime_genre_list()) + 
      geom_bar(aes_string(x = "genre", y = paste(strsplit(input$selectgenre, " ")[[1]],
                                                 collapse = "_"), fill = "genre"), stat = 'identity') +
      ggtitle('Plot of Characteristics of Anime VS. Genres') + theme_bw() + coord_flip() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
      theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
      theme(plot.title = element_text(size = 20, face = "bold"))+
      theme(axis.title = element_text(size = 12, face = "bold"))
  )
  
  output$genre1 <- renderInfoBox({
    averageScore <- anime_genre %>% filter(genre %in% anime_genre_list()) %>% 
      summarise(meanscore = mean(score)) %>% pull(meanscore)
    infoBox("Average Score", value = sprintf("%.2f", averageScore))
  })
  
  output$genre2 <- renderInfoBox({
    averagewatch <- anime_genre %>% filter(genre %in% anime_genre_list()) %>% 
      summarise(meanaudience = mean(members)) %>% pull(meanaudience)
    infoBox("Average Audience", value = round(averagewatch))
  })
  
  # genre statistical test
  genre_test_table <- reactive({anime1 %>% filter(genre %in% input$selectgenretest)})
  
  output$genretext1 <- renderPrint({ summary(aov(score ~ genre, data = genre_test_table())) })
  output$genretext2 <- renderPrint({ summary(aov(members ~ genre, data = genre_test_table())) })
  
  # Anime Songs
  
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
      anime_op$topArtists = ifelse(anime_op$OP %in% anime_artists(), "Top Artist", "Not Top Artist")
      anime_op %>% ggplot(aes(x = topArtists, y = score)) + geom_boxplot() +
        ggtitle('Boxplots on Scores of Top and Non-top Artists') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }else{
      anime_ed$topArtists = ifelse(anime_ed$ED %in% anime_artists(), "Top Artist", "Not Top Artist")
      anime_ed %>% ggplot(aes(x = topArtists, y = score)) + geom_boxplot() +
        ggtitle('Boxplots on Scores of Top and Non-top Artists') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }
  )
  
  output$songs2 <- renderPlot(
    if(input$radiosongs == "Opening_Theme"){
      anime_op$topArtists = ifelse(anime_op$OP %in% anime_artists(), "Top Artist", "Not Top Artist")
      anime_op %>% mutate(outlier = is_outlier(.data[["members"]])) %>%
        filter(outlier == FALSE) %>%
        ggplot(aes(x = topArtists, y = members)) + geom_boxplot() +
        ggtitle('Boxplots on Number Watching of Top and Non-top Artists') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }else{
      anime_ed$topArtists = ifelse(anime_ed$ED %in% anime_artists(), "Top Artist", "Not Top Artist")
      anime_ed %>%  mutate(outlier = is_outlier(.data[["members"]])) %>%
        filter(outlier == FALSE) %>%
        ggplot(aes(x = topArtists, y = members)) + geom_boxplot() + 
        ggtitle('Boxplots on Number Watching of Top and Non-top Artists') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    }
  )
  
  output$songTable <- renderTable({
    if(input$radiosongs == "Opening_Theme"){
      anime_op %>% group_by(OP) %>% summarise(count = n()) %>%
        arrange(desc(count)) %>% head(input$songtext)
    }else{
      anime_ed %>% group_by(ED) %>% summarise(count = n()) %>%
        arrange(desc(count)) %>% head(input$songtext)
    }
  })
  
  # song statistical test
  
  anime_artists1 <- reactive({
    if(input$radiosongs1 == "Opening_Theme"){
      anime_op %>% group_by(OP) %>% 
        summarise(count = n()) %>% arrange(desc(count)) %>% 
        top_n(input$songtext1) %>% pull(OP)
    }else{
      anime_ed %>% group_by(ED) %>% 
        summarise(count = n()) %>% arrange(desc(count)) %>% 
        top_n(input$songtext1) %>% pull(ED)
    }
  })
  
  anime_artists2 <- reactive({
    if(input$radiosongs1 == "Opening_Theme"){
      anime_op %>% filter(anime_op$OP %in% anime_artists1())
    }else{
      anime_ed %>% filter(anime_ed$ED %in% anime_artists1())
    }
  })
  
  anime_artists3 <- reactive({
    if(input$radiosongs1 == "Opening_Theme"){
      anime_op %>% filter(!anime_op$OP %in% anime_artists1())
    }else{
      anime_op %>% filter(!anime_op$OP %in% anime_artists1())
    }
  })
  
  output$songtesttext1 <- renderPrint({ 
    if (var.test(anime_artists2()$score, anime_artists3()$score,
                 alternative = "two.sided")$p.value > 0.05){
      t.test(anime_artists2()$score, anime_artists3()$score, var.equal = TRUE)
    }else{
      t.test(anime_artists2()$score, anime_artists3()$score, var.equal = FALSE)
    }})
  
  output$songtesttext2 <- renderPrint({ 
    if (var.test(anime_artists2()$members, anime_artists3()$members,
                 alternative = "two.sided")$p.value > 0.05){
      t.test(anime_artists2()$members, anime_artists3()$members, var.equal = TRUE)
    }else{
      t.test(anime_artists2()$members, anime_artists3()$members, var.equal = FALSE)
    }})
  
  # Interactions
  output$corr <- renderPlot(
    ggcorr(anime1[input$selectcorr], label = TRUE, palette = "RdGy",
           label_size = 5, label_round = 2, label_color = "black") +
      ggtitle("Correlation Plots")+ 
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
      theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
      theme(plot.title = element_text(size = 20, face = "bold"))+
      theme(axis.title = element_text(size = 12, face = "bold"))
  )
  
  scatter_plot_stat <- reactive(
    anime1[is_outlier(anime1[,input$selecttwo[1]]) == F &
                     is_outlier(anime1[,input$selecttwo[2]]) == F,]
  )
  
  output$scatter <- renderPlot(
    ggplot(scatter_plot_stat(), (aes(x = scatter_plot_stat()[,input$selecttwo[1]],
                y = scatter_plot_stat()[,input$selecttwo[2]])), na.rm = T) + geom_point() +
      geom_smooth(method='lm') + xlab(input$selecttwo[1]) + ylab(input$selecttwo[2]) + 
      ggtitle("Scatter Plots of Two Variables") + theme_bw()+ 
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
      theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
      theme(plot.title = element_text(size = 20, face = "bold"))+
      theme(axis.title = element_text(size = 12, face = "bold"))
  )
  
  # Rankings
  ranking_table <- eventReactive(input$show, {
    if(input$rankchoice1 == "Yes" & input$rankchoice2 == "Yes"){
    anime1 %>% filter(type %in% input$ranktype) %>% 
      filter(source %in% input$ranksource) %>% filter(rating %in% input$rankrating) %>% 
      filter(year > input$rankyear[1] & year < input$rankyear[2]) %>% 
      filter(duration > input$rankduration[1] & duration < input$rankduration[2])
    }else if(input$rankchoice1 == "No" & input$rankchoice2 == "Yes"){
      anime_genre %>% filter(genre %in% input$rankgenre) %>% filter(type %in% input$ranktype) %>% 
        filter(source %in% input$ranksource) %>% filter(rating %in% input$rankrating) %>% 
        filter(year > input$rankyear[1] & year < input$rankyear[2]) %>% 
        filter(duration > input$rankduration[1] & duration < input$rankduration[2])
    }else if(input$rankchoice1 == "Yes" & input$rankchoice2 == "No"){
      anime_studio %>% filter(studio %in% input$rankstudio) %>% filter(type %in% input$ranktype) %>% 
        filter(source %in% input$ranksource) %>% filter(rating %in% input$rankrating) %>% 
        filter(year > input$rankyear[1] & year < input$rankyear[2]) %>% 
        filter(duration > input$rankduration[1] & duration < input$rankduration[2])
    }else{
      anime_ranking %>% filter(genre %in% input$rankgenre) %>% filter(studio %in% input$rankstudio) %>% 
        group_by(title) %>% filter(type %in% input$ranktype) %>% 
        filter(source %in% input$ranksource) %>% filter(rating %in% input$rankrating) %>% 
        filter(year > input$rankyear[1] & year < input$rankyear[2]) %>% 
        filter(duration > input$rankduration[1] & duration < input$rankduration[2])
    }
  })
  
  output$top <- renderTable({
    if(input$rankchoice == "score"){
      ranking_table() %>% select(title, title_japanese, score, members) %>% 
        arrange(desc(score)) %>% head(input$ranktext)
    }else{
      ranking_table() %>% select(title, title_japanese, members, score) %>% 
        arrange(desc(members)) %>% head(input$ranktext)
    }
  })

  #data table
  output$table <- DT::renderDataTable({
    datatable(anime1, options = list(scrollX = TRUE,autoWidth = TRUE), rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
}
