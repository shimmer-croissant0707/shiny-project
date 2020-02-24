dashboardPage(
  #header 
  dashboardHeader(title = "Investigation of Anime"),
  
  dashboardSidebar(
    sidebarUserPanel(name = "Hanbo Shao"),
    sidebarMenu(id = 'sidebarmenu',
                menuItem('Welcome Page', tabName = 'welcome', icon = icon('tv')),
                menuItem('Anime Overview', tabName = 'anime', icon = icon('tv')),
                
                menuItem('Anime', tabName = 'anime-category', icon = icon('tv'),
                menuItem('Type', tabName = 'type', icon = icon('tv')),
                menuItem('Source', tabName = 'source', icon = icon('book')),
                menuItem('Rating', tabName = 'rating', icon = icon('bars')),
                menuItem('Year', tabName = 'year', icon = icon('calendar')),
                menuItem('Duration', tabName = 'duration', icon = icon('stream')),
                menuItem('Studio', tabName = 'studio', icon = icon('building')),
                menuItem('Genre', tabName = 'genre', icon = icon('folder')),
                menuItem('Anime Songs', tabName = 'songs', icon = icon('music'))),
                
                #menuItem('Users', tabName='user', icon = icon('user')),
                menuItem('Interactions', tabName='interactions', icon = icon('chart-area')),
                menuItem('Rankings', tabName='ranking', icon = icon('line-chart')),
                menuItem('Data', tabName='data', icon = icon('database')),
                menuItem('About', tabName='about', icon = icon('info'))
                
                )),
  
  
  dashboardBody(    
    shinyDashboardThemes(theme = "blue_gradient"),
  tabItems(
    tabItem(tabName = 'welcome',
            h1("Welcome to the World of Anime!", align = 'center'),
            img(src='anime.png', height="80%", width="80%", 
                style="display: block; margin-left: auto; margin-right: auto;"),
            img(src='akiba.jpg', height="80%", width="80%",
            style="display: block; margin-left: auto; margin-right: auto;")
            ),
    
    tabItem(tabName = 'anime',
            h3("General summary of Anime Dataset"),
            fluidRow(infoBoxOutput("total"),
                     infoBoxOutput("avg_score"),
                     infoBoxOutput("avg_member")
            ),
            fluidRow(
              h3("Summary Histograms of Anime Dataset", align = "center"),
              column(width = 6,
                    plotOutput("typeHist")),
              column(width = 6,
                    plotOutput("genreHist"))
            ),
            fluidRow(
              column(width = 6,
                     plotOutput("sourceHist")),
              column(width = 6,
                     plotOutput("ratingHist"))
            )
            ),
    
    tabItem(tabName = 'type',
            tabsetPanel(
              tabPanel('graphical analysis',
            fluidRow(
              column(width = 10,
                     plotOutput("type")
                     ),
              column(width = 2,
                     checkboxGroupInput("typecheckbox", "Show Type",
                                        choices = c("TV", "Movie", "OVA", "Music"),
                                        selected = c("TV", "Movie", "OVA", "Music")
                                        ),
                     selectizeInput("selecttype","Select Item to Display",
                                    choices = choice,
                                    selected = "score")
                     )),
              fluidRow(
                column(width = 10,
                       h3("Summary Table of Different Types of Anime"),
                       tableOutput("typeTable")
                       ),
                column(width = 2,
                       radioButtons("radiotype", "Remove Outliers for the table",
                                    choices = c("Yes", "No"), selected = "Yes")
                       )
              )),
            tabPanel('Statistical Tests',
                     box(width = 7,
                       title = "Test Result for Score", status = "primary", solidHeader = TRUE,
                       verbatimTextOutput("typetext1")
                     ),
                     
                     box(width = 5,
                       title = "Please Select Variables for ANOVA test", status = "warning",
                       solidHeader = TRUE,
                       checkboxGroupInput("typecheckbox1", "Select Variables",
                                          choices = c("TV", "Movie", "OVA", "Music"),
                                          selected = c("TV", "Movie", "OVA", "Music"),
                       )
                     ),
                     box(width = 7,
                         title = "Test Result for Audience", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("typetext2")
                     ))
            )),
    
    tabItem(tabName = 'source',
            tabsetPanel(
              tabPanel('Graphical Analysis',
            fluidRow(
              column(width = 10,
                     plotOutput("source")
                    ),
              column(width = 2,
                     checkboxGroupInput("sourcecheckbox", "Show Types of Sources",
                                        choices = unique_source,
                                        selected = unique_source
                     ),
                     selectizeInput("selectsource","Select Item to Display",
                                    choices = choice,
                                    selected = "score")
                    )),
            fluidRow(
              column(width = 10,
                     h3("Summary Table of Different Sources of Anime"),
                     tableOutput("sourceTable")
              ),
              column(width = 2,
                     radioButtons("radiosource", "Remove Outliers for the table",
                                  choices = c("Yes", "No"), selected = "Yes")
              )
              )),
            
            tabPanel('Statistical Test',
                     box(width = 7,
                         title = "Test Result for Score", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("sourcetext1")
                     ),
                     
                     box(width = 5,
                         title = "Please Select Variables for ANOVA test", status = "warning",
                         solidHeader = TRUE,
                         checkboxGroupInput("sourcecheckbox1", "Show Types of Sources",
                                            choices = unique_source,
                                            selected = unique_source
                         )
                         ),
                     box(width = 7,
                         title = "Test Result for Audience", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("sourcetext2")
                     )
                     )
            )),
    
    tabItem(tabName = 'year',
            fluidRow(
              column(width = 10,
                     h3("Summary Plots Related to Years"),
                     plotOutput("year")
              ),
              column(width = 2,
                     sliderInput("yearslider", "Choose Range of Years",
                                  min = min(anime1$year),
                                  max = max(anime1$year) - 1, 
                                  value = c(min(anime1$year), max(anime1$year) - 1)
                     ),
                     selectizeInput("selectyear","Select Item to Display",
                                    choices = displaychoice,
                                    selected = "score"),
                     radioButtons("radioyear","Removing Outliers",
                                  choices = list("yes", "no"),
                                  selected = "yes")
              )
              ),
            fluidRow(
              h3("Summary Info Boxes of Selected Years"),
              infoBoxOutput("year1"),
              infoBoxOutput("year2")
            ),
            fluidRow(
              infoBoxOutput("year3"),
              infoBoxOutput("year4")
            )
            ),
    
    
    tabItem(tabName = 'rating',
            tabsetPanel(
              tabPanel('Graphical Analysis',
            fluidRow(
              column(width = 10,
                     plotOutput("rating")
              ),
              column(width = 2,
                     checkboxGroupInput("ratingcheckbox", "Show Types of Ratings",
                                        choices = unique_rating,
                                        selected = unique_rating
                     ),
                     selectizeInput("selectrating","Select Item to Display",
                                    choices = choice,
                                    selected = "score")
              )),
            fluidRow(
              column(width = 10,
                     h3("Summary Table of Different Ratings of Anime"),
                     tableOutput("ratingTable")
              ),
              column(width = 2,
                     radioButtons("radiorating", "Remove Outliers for the table",
                                  choices = c("Yes", "No"), selected = "Yes")
              )
            )),
            
            tabPanel('Statistical Test',
                     box(width = 7,
                         title = "Test Result for Score", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("ratingtext1")
                     ),
                     
                     box(width = 5,
                         title = "Please Select Variables for ANOVA test", status = "warning",
                         solidHeader = TRUE,
                         checkboxGroupInput("ratingcheckbox1", "Show Types of Ratings",
                                            choices = unique_rating,
                                            selected = unique_rating
                         )
                     ),
                     box(width = 7,
                         title = "Test Result for Audience", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("ratingtext2")
                     )
            )
            )),
    
    tabItem(tabName = 'duration',
            fluidRow(
              column(width = 10,
                     h3("Summary Plots Related to Durations"),
                     plotOutput("duration")
              ),
              column(width = 2,
                     sliderInput("durationlider", "Choose Range of Durations",
                                 min = min(anime1$duration),
                                 max = max(anime1$duration), 
                                 value = c(min(anime1$duration), max(anime1$duration))
                     ),
                     selectizeInput("selectduration","Select Item to Display",
                                    choices = displaychoice,
                                    selected = "score"),
                     radioButtons("radioduration","Removing Outliers",
                                    choices = list("yes", "no"),
                                    selected = "yes")
              )
            ),
            fluidRow(
              h3("Summary Info Boxes of Selected Durations"),
              infoBoxOutput("duration1"),
              infoBoxOutput("duration2")
            ),
            fluidRow(
              infoBoxOutput("duration3"),
              infoBoxOutput("duration4")
            )
            ),
    
    tabItem(tabName = 'studio',
            tabsetPanel(
              tabPanel('Graphical Analysis',
            fluidRow(
              column(width = 10,
                     h3("Summary Plots Related to Various Anime Creating Studios"),
                     plotOutput("studio")
              ),
              column(width = 2,
                     textInput("studiotext", "Enter Number of Studios",
                                 value = 10),
                     selectizeInput("selectstudio1","Select Studios",
                                    choices = unique(anime_studio$studio),
                                    multiple = TRUE),
                     selectizeInput("selectstudio","Select Variables to Display",
                                    choices = displaychoice,
                                    selected = "count"),
                     radioButtons("radiostudio","Choose Options",
                                  choices = list("top", "select yourself"),
                                  selected = "top"),
                     radioButtons("radiostudio1","Choose Ranking Methods",
                                  choices = list("total productions", "average productions per year"),
                                  selected = "total productions")
              )
            ),
            fluidRow(
              h3("Summary Info Boxes of Selected Studios"),
              infoBoxOutput("studio1"),
              infoBoxOutput("studio2")
            )),
            tabPanel('Statistical Test',
                     box(width = 7,
                         title = "Test Result for Score", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("studiotext1")
                     ),
                     
                     box(width = 5,
                         title = "Please Select Variables for ANOVA test", status = "warning",
                         solidHeader = TRUE,
                         selectizeInput("selectstudiotest","Select Studios",
                                        choices = unique(anime_studio$studio),
                                        multiple = TRUE,
                                        selected = unique(anime_studio$studio)[1:2])
                     ),
                     box(width = 7,
                         title = "Test Result for Audience", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("studiotext2")
                     )
            )
            )),
    
    tabItem(tabName = 'genre',
            tabsetPanel(
              tabPanel('Graphical Analysis',
            fluidRow(
              column(width = 10,
                     h3("Summary Plots Related to Various Anime Genres"),
                     plotOutput("genre")
              ),
              column(width = 2,
                     textInput("genretext", "Enter Number of genres",
                               value = 10),
                     selectizeInput("selectgenre1","Select Genres",
                                    choices = unique(anime_genre$genre),
                                    multiple = TRUE),
                     selectizeInput("selectgenre","Select Variables to Display",
                                    choices = displaychoice,
                                    selected = "count"),
                     radioButtons("radiogenre","Choose Options",
                                  choices = list("top", "select yourself"),
                                  selected = "top"),
                     radioButtons("radiogenre1","Choose Ranking Methods",
                                  choices = list("total productions", "average productions per year"),
                                  selected = "total productions")
              )
            ),
            fluidRow(
              h3("Summary Info Boxes of Selected Genres"),
              infoBoxOutput("genre1"),
              infoBoxOutput("genre2")
            )),
            tabPanel('Statistical Test',
                     box(width = 7,
                         title = "Test Result for Score", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("genretext1")
                     ),
                     
                     box(width = 5,
                         title = "Please Select Variables for ANOVA test", status = "warning",
                         solidHeader = TRUE,
                         selectizeInput("selectgenretest","Select Genres",
                                        choices = unique(anime_genre$genre),
                                        multiple = TRUE,
                                        selected = unique(anime_genre$genre)[1:2])
                     ),
                     box(width = 7,
                         title = "Test Result for Audience", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("genretext2")
                     )
            )
            )),
    
    
    tabItem(tabName = 'songs',
            tabsetPanel(
              tabPanel('Graphical Analysis',
            fluidRow(
              column(width = 9,
                     h3("Summary Plots of Anime Opening/Ending Songs"),
                     plotOutput("songs1"),
                     plotOutput("songs2")
              ),
              column(width = 3,
                     textInput("songtext", "Enter Top X Artists",
                               value = 10),
                     radioButtons("radiosongs","Please choose",
                                  choices = list("Opening_Theme", "Ending_Theme"),
                                  selected = "Opening_Theme"),
                     h4("Top Artists"),
                     tableOutput("songTable"))
            )),
            
            tabPanel('Statistical Test',
                     box(width = 7,
                         title = "Test Result for Score", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("songtesttext1")
                     ),
                     box(width = 5,
                         title = "Please Select Variables for t-test", status = "warning",
                         solidHeader = TRUE,
                         textInput("songtext1", "Enter Top X Artists",
                                   value = 10),
                         radioButtons("radiosongs1","Please choose",
                                      choices = list("Opening_Theme", "Ending_Theme"),
                                      selected = "Opening_Theme")
                     ),
                     box(width = 7,
                         title = "Test Result for Audience", status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("songtesttext2")
                     )
            )
    )),
    
    tabItem(tabName = "interactions",
            fluidRow(
              column(width = 9,
                     plotOutput("corr")
                     ),
              column(width = 3,
                     selectizeInput("selectcorr","Select Variables to Compute Correlations",
                                    choices = corr_choice,
                                    selected = corr_choice,
                                    multiple = TRUE)
                     )
            ),
            fluidRow(
              column(width = 9,
                     plotOutput("scatter")
              ),
              column(width = 3,
                     selectizeInput("selecttwo","Select Two Variables for Scatter Plots",
                                    choices = corr_choice,
                                    multiple = TRUE, options = list(maxItems = 2))
              )
            )),
    
    tabItem(tabName = "ranking",
            fluidRow(
              column(width = 9,
                     h3("Top Animes of Your Choice"),
                     title = "Top Animes",
                     tableOutput("top")),
              column(width = 3,
                     actionButton("show" ,"Show Rankings", icon("gem"),
                                  class = "btn btn-primary"),
                     radioButtons("rankchoice","Choose Options",
                                  choices = list("Score" = "score", "Polularity"),
                                  selected = "score"),
                     textInput("ranktext", "Enter Number of Items to Display",
                               value = 10),
                     selectizeInput("ranktype", "Select Type",
                                        choices = c("TV", "Movie", "OVA", "Music"),
                                        selected = c("TV", "Movie", "OVA", "Music"),
                                        multiple = TRUE
                     ),
                     selectizeInput("ranksource", "Select Types of Sources",
                                        choices = unique_source,
                                        selected = unique_source,
                                        multiple = TRUE
                     ),
                     selectizeInput("rankrating", "Select Types of Ratings",
                                        choices = unique_rating,
                                        selected = unique_rating,
                                        multiple = TRUE
                     ),
                     sliderInput("rankyear", "Choose Range of Years",
                                 min = min(anime1$year),
                                 max = max(anime1$year) - 1, 
                                 value = c(min(anime1$year), max(anime1$year) - 1)
                     ),
                     sliderInput("rankduration", "Choose Range of Durations",
                                 min = min(anime1$duration),
                                 max = max(anime1$duration), 
                                 value = c(min(anime1$duration), max(anime1$duration))
                     ),
                     radioButtons("rankchoice1","Select all genres",
                                  choices = list("Yes", "No"),
                                  selected = "Yes"),
                     selectizeInput("rankgenre","Select Genres",
                                    choices = unique(anime_genre$genre),
                                    multiple = TRUE),
                     radioButtons("rankchoice2","Select all studios",
                                  choices = list("Yes", "No"),
                                  selected = "Yes"),
                     selectizeInput("rankstudio","Select Studios",
                                    choices = unique(anime_studio$studio),
                                    multiple = TRUE)
                     )
            )),
      

    tabItem(tabName = "data", 
            fluidRow(box(DT::dataTableOutput("table"), width = 200))
    ),
    
    tabItem(tabName = "about", 
            fluidRow(
              box(h2("About the Project"),
                  br(),
                  h4("In this project, I studied different aspects of Anime.", 
                  "I used the", 
                  shiny::a(href = 'https://www.kaggle.com/azathoth42/myanimelist', 'dataset'), 
                  "that contains information on MyAnimeLists from Kaggle open dataset collections.",
                  "MyAnimeLists can be seen as the imdb for animes and a lot of interesting discoveries",
                  "are found in this project. I also included the statistical tests section so that",
                  "users can perform statistical tests. Is you have any suggestion or improvement",
                  "on this shiny app. Please contact me via",
                  shiny::a(href = 'shimmer_croissant@hotmail.com', 
                           'my email.')),
                  br(),
                  h3("Future Work"),
                  br(),
                  h4("As for future work, I would like to investigate the dataset about users and see",
                     "if any interesting insights could be drawn from there. Also, I would like to ",
                     "look into Japanese anime rating website and merge two datasets in some way",
                     "and see there is any interesting observations could be made. Further, if ",
                     "possible, I would be interested to look at the budget and profits and perform some",
                     "data analysis around those metrics as well.")
                  ),
              box(h2("About Myself"),
                  br(),
                  h4("My name is Hanbo Shao, currently a data science fellow at NYC Data Science Academy.",
                     "I love watching animes (I watch it nearly everyday) and working on this project",
                     "is pretty fun."),
                     br(),
                     h4("Before this, I have a quantitative background in both pure math and operations",
                     "research. I obtained my undergraduate math degree from Colorado College and my master's",
                     "degree in Operations Research from the Georgia Institute of Technology.",
                     "I did some research on certain pure math topics during my undergraduate.",
                     "If you are interested in Vertex Operator Algebra,",
                     "here is a great",
                     shiny::a(href = 'https://arxiv.org/pdf/0809.1380.pdf', 'paper.')),
                     br(),
                     h4("For more information about myself, please check out the",
                     shiny::a(href = 'https://www.linkedin.com/in/hanbo-shao-899a8aaa/', 'LinkedIn'),
                     "page and also the ",
                     shiny::a(href = 'https://github.com/shimmer-croissant0707/', 'GitHub'),
                     "page.")
                  )
            )
    )
    
  ))
)


