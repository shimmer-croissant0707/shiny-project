dashboardPage(
  #header 
  dashboardHeader(title = "Investigation of Anime"),
  
  dashboardSidebar(
    sidebarUserPanel(name = "Hanbo Shao"),
    sidebarMenu(id = 'sidebarmenu',
                menuItem('Anime Overview', tabName = 'anime', icon = icon('tv')),
                
                menuItem('Type', tabName = 'type', icon = icon('tv')),
                menuItem('Source', tabName = 'source', icon = icon('book')),
                menuItem('Year', tabName = 'year', icon = icon('calendar')),
                menuItem('Rating', tabName = 'rating', icon = icon('bars')),
                menuItem('Duration', tabName = 'duration', icon = icon('stream')),
                menuItem('Studio', tabName = 'studio', icon = icon('building')),
                menuItem('Genre', tabName = 'genre', icon = icon('folder')),
                menuItem('Anime Songs', tabName = 'songs', icon = icon('music')),
                
                #menuItem('Rankings', tabName='ranking', icon = icon('line-chart')),
                menuItem('Interactions', tabName='interactions', icon = icon('chart-area')),
                
                #menuItem('Users', tabName='user', icon = icon('user')),
                
                menuItem('Data', tabName='data', icon = icon('database'))
                
                )),
  
  
  dashboardBody(    
    shinyDashboardThemes(theme = "blue_gradient"),
  tabItems(
    tabItem(tabName = 'anime',
            fluidRow(infoBoxOutput("total"),
                     infoBoxOutput("avg_score"),
                     infoBoxOutput("avg_member")
            ),
            fluidRow(
              column(width = 6,
                    plotOutput("typePie")),
              column(width = 6,
                    plotOutput("genrePie"))
            ),
            fluidRow(
              column(width = 6,
                     plotOutput("sourcePie")),
              column(width = 6,
                     plotOutput("ratingPie"))
            )
            ),
    
    tabItem(tabName = 'type',
            fluidRow(
              column(width = 9,
                     plotOutput("type")
                     ),
              column(width = 3,
                     checkboxGroupInput("typecheckbox", "Show Type",
                                        choices = c("TV", "Movie", "OVA", "Music"),
                                        selected = c("TV", "Movie", "OVA", "Music")
                                        ),
                     selectizeInput("selecttype","Select Item to Display",
                                    choices = choice,
                                    selected = "score")
                     )
            )),
    
    tabItem(tabName = 'source',
            fluidRow(
              column(width = 9,
                     plotOutput("source")
                    ),
              column(width = 3,
                     checkboxGroupInput("sourcecheckbox", "Show Types of Sources",
                                        choices = unique_source,
                                        selected = unique_source
                     ),
                     selectizeInput("selectsource","Select Item to Display",
                                    choices = choice,
                                    selected = "score")
                    )
              )),
    
    tabItem(tabName = 'year',
            fluidRow(
              column(width = 9,
                     plotOutput("year")
              ),
              column(width = 3,
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
            )),
    
    tabItem(tabName = 'rating',
            fluidRow(
              column(width = 9,
                     plotOutput("rating")
              ),
              column(width = 3,
                     checkboxGroupInput("ratingcheckbox", "Show Types of Ratings",
                                        choices = unique_rating,
                                        selected = unique_rating
                     ),
                     selectizeInput("selectrating","Select Item to Display",
                                    choices = choice,
                                    selected = "score")
              )
            )),
    
    tabItem(tabName = 'duration',
            fluidRow(
              column(width = 9,
                     plotOutput("duration")
              ),
              column(width = 3,
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
            )),
    
    tabItem(tabName = 'studio',
            fluidRow(
              column(width = 9,
                     plotOutput("studio")
              ),
              column(width = 3,
                     textInput("studiotext", "Enter Number of Studios",
                                 value = 10),
                     selectizeInput("selectstudio1","Select Studios",
                                    choices = unique(anime_studio$studio),
                                    multiple = TRUE),
                     selectizeInput("selectstudio","Select Number of Studios to Display",
                                    choices = displaychoice,
                                    selected = "count"),
                     radioButtons("radiostudio","Choose Options",
                                  choices = list("top", "select yourself"),
                                  selected = "top")
              )
            )),
    
    tabItem(tabName = 'genre',
            fluidRow(
              column(width = 9,
                     plotOutput("genre")
              ),
              column(width = 3,
                     textInput("genretext", "Enter Number of genres",
                               value = 10),
                     selectizeInput("selectgenre1","Select Genres",
                                    choices = unique(anime_genre$genre),
                                    multiple = TRUE),
                     selectizeInput("selectgenre","Select Number of Genres to Display",
                                    choices = displaychoice,
                                    selected = "count"),
                     radioButtons("radiogenre","Choose Options",
                                  choices = list("top", "select yourself"),
                                  selected = "top")
              )
            )),
    
    tabItem(tabName = 'songs',
            fluidRow(
              column(width = 9,
                     plotOutput("songs1"),
                     plotOutput("songs2")
              ),
              column(width = 3,
                     textInput("songtext", "Enter Top X Artists",
                               value = 10),
                     radioButtons("radiosongs","Please choose",
                                  choices = list("Opening_Theme", "Ending_Theme"),
                                  selected = "Opening_Theme"))
            )),
    
    tabItem(tabName = "interactions",
            fluidRow(
              column(width = 9,
                     plotOutput("corr")
                     ),
              column(width = 3,
                     selectizeInput("selectcorr","Select Variables to Compute Correlations",
                                    choices = corr_choice,
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
            )
            
            ),
    
    tabItem(tabName = "data", 
            fluidRow(box(DT::dataTableOutput("table"), width = 200))
    )
    
  ))
)


