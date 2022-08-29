ui <- bootstrapPage(
  navbarPage(
    title = "FlightR",
    id = "navbar",
    theme = bs_theme(version = "4", bootswatch = "flatly"),
    collapsible = TRUE,
    windowTitle = "FlightR",
    
    tags$head(
      tags$style(HTML("
      .shiny-notification {
        height: 100px;
        width: 800px;
        position: fixed;
        top: calc(50% - 50px);
        left: calc(50% - 400px);
        font-size: 250%;
        text-align: center;}
        
      .shiny-notification-close {
        visibility: hidden;}
        
       #controls {
       background-color: white;
       padding: 0 10px 10px 10px;
       cursor: move;
       opacity: 0.55;
       zoom: 0.9;
       transition: opacity 500ms 1s;}
          
       #controls:hover {
          opacity: 0.75;
          transition-delay: 0;}")
      )
    ),
    
    # Queries SQL ----
    navbarMenu(
      title = "Queries SQL",
      
      # Queries SQL - Statistic features ----
      tabPanel(
        title = "Statistic features",
        
        tabsetPanel(
          tabPanel(
            title = "Attribute distribution",
            br(),
            uiOutput("panelstatisticattrdistr")),
          tabPanel(
            title = "N° Arrivals/Departure",
            br(),
            uiOutput("panelstatisticarrdep")
          )
        )
      ),
      
      # Queries SQL - General Informations ----
      tabPanel(
        title = "General Informations",
        
        tabsetPanel(
          tabPanel(
            title = "N° of routes per carrier",
            withSpinner(
              dataTableOutput("numberRoutesCarrier"),
              type = 4,
              color = "#d33724",
              size = 0.7
            )
          )
        )
      ),
      
      # Queries SQL - Delays or cancellations ----
      tabPanel(
        title = "Delays or cancellations",
        
        sidebarLayout(
          sidebarPanel(includeHTML("input_data/infocause.html")),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = "Cause of cancellation % per carrier",
                br(),
                withSpinner(
                  plotlyOutput("causeCancellationCarrier", height = 550),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              ),
              tabPanel(
                title = "Cause of delay % on carrier",
                br(),
                uiOutput("panelcausedelaycarrier")
              ),
              tabPanel(
                title = "Arrival's Delay % per carrier",
                br(),
                withSpinner(
                  plotlyOutput("arrDelayCarrier", height = 550),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              )
            )
          )
        )
      )
    ),
    
    # Maps ----
    tabPanel(
      title = "Maps",
      
      tabsetPanel(
        tabPanel(
          title = "Top flight paths",
          withSpinner(
            leafletOutput("topPaths", height = 820),
            type = 4,
            color = "#d33724",
            size = 0.7
          ),
          
          fixedPanel(
            id = "controls",
            class = "panel panel-default",
            top = "50%",
            left = 40,
            width = 250,
            draggable = TRUE,
            height = "auto",
            
            span(("This map shows the best routes from the entire dataset. A route is identified by the Origin-Dest pair."),align = "left", style = "font-size:80%"),
            br(),
            sliderInput(inputId = "topPaths_paths", label = "Number of routes", min = 10, max = 40, value = 25, step = 5)
          )
        ),
        
        tabPanel(
          title = "Flight distribution - Airports",
          withSpinner(
            leafletOutput("airportsMapDistr", height = 820),
            type = 4,
            color = "#d33724",
            size = 0.7
          ),
          
          fixedPanel(
            id = "controls",
            class = "panel panel-default",
            top = "50%",
            left = 40,
            width = 250,
            draggable = TRUE,
            height = "auto",
            
            span(("This map shows the best airports from the entire dataset. The size of each bubble is the sum of the departures and arrivals."),align = "left", style = "font-size:80%"),
            br(),
            sliderInput(inputId = "airportsMapDistr_airports", label = "Number of airports", min = 10, max = 40, value = 25, step = 5)
          )
        )
      )
    ),
    
    # Machine Learning ----
    navbarMenu(
      title = "Machine Learning",
      
      # Machine Learning - Matrix Correlation ----
      tabPanel(
        title = "Matrix Correlation",
        sidebarLayout(
          sidebarPanel(includeHTML("input_data/infomatrixcorr.html")),
          mainPanel(
            withSpinner(
              plotlyOutput("matrixCorr", height = 600),
              type = 4,
              color = "#d33724",
              size = 0.7
            )
          )
        )
      ),
      
      # Machine Learning - Classification ----
      tabPanel(
        title = "Classification",
        
        tabsetPanel(
          tabPanel(
            title = "Performance comparision",
            br(),
            uiOutput("panelclassifcomp")
          ),
          tabPanel(
            title = "Roc Curve",
            br(),
            sidebarLayout(
              sidebarPanel(includeHTML("input_data/infoaucroc.html")),
              mainPanel(
                withSpinner(
                  plotlyOutput("MLClassROC"),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              )
            )
           ),
          tabPanel(
            title = "Feature importance",
            br(),
            sidebarLayout(
              sidebarPanel(includeHTML("input_data/infofeatureimportance.html")),
              mainPanel(
                withSpinner(
                  plotlyOutput("MLClassFeaImpo"),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              )
            )
          )
        )
      ),
      
      # Machine Learning - Clustering ----
      tabPanel(
        title = "Clustering (K-Means)",
        #uiOutput("panelclusteringkmeans"),
        tabsetPanel(
          tabPanel(
            title = "Principal Component Analysis",
            br(),
            uiOutput("panelclusteringpca")
          ),
          tabPanel(
            title = "Silhouette Comparision",
            br(),
            sidebarLayout(
              sidebarPanel(includeHTML("input_data/infosilhouettemetric.html")),
              mainPanel(
                withSpinner(
                  plotlyOutput("silhouettecomp"),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              )
            )
          ),
          tabPanel(
            title = "Clusters Plot",
            br(),
            sidebarLayout(
              sidebarPanel(includeHTML("input_data/infoclustersplot.html")),
              mainPanel(
                dropdown(
                  tags$h4("List of Inputs"),
                  selectInput(inputId = "clustersplot_xcol", label = "X Variable", choices = c("PC1", "PC2", "PC3")),
                  selectInput(inputId = "clustersplot_ycol", label = "Y Variable", choices = c("PC1", "PC2", "PC3")),
                  sliderInput(inputId = "clustersplot_k", label = "Number of clusters", min = 2, max = 8, value = 5, step = 1),
                  style = "unite",
                  status = "danger",
                  icon = icon("gear"), 
                  width = "300px",
                  tooltip = tooltipOptions(title = "Click to see inputs !")
                ),
                withSpinner(
                  plotlyOutput("clustersplot", height = 550),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              )
            )
          )
        )
      )
    ),
    
    # About this site ----
    tabPanel(title = "About this site", includeHTML("input_data/readme.html"))
  )
)