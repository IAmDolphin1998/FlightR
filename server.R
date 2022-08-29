server <- function(input, output, session) {
  valuesR <- reactiveValues(semaforClass = TRUE, semaforClust = TRUE)
  
  # DEFINE SETS ----
  
  withProgress(message = "Loading data...", value = 0, {
    tbl <- spark_read_csv(sc, name = "flights", path = here("input_data", "flights.csv"))
    incProgress(0.33)
    
    tbl.carriers <- spark_read_csv(sc, name = "carriers", path = here("input_data", "airlines.csv"), memory = FALSE)
    incProgress(0.33)
    
    tbl.airports <- spark_read_csv(sc, name = "airports", path = here("input_data", "airports.csv"), memory = FALSE)
    incProgress(0.34)
  })
  
  observeEvent(input$navbar, {
    if (input$navbar == "Classification") {
      if (isolate(valuesR$semaforClass)) {
        withProgress(message = "Preparing data for classification...", value = 0, {
          tbl.classification <- tbl %>%
            mutate(OnTime = ifelse(ArrDelay > 15, 0, 1)) %>%
            select(OnTime, DepDelay, Distance, TaxiIn, TaxiOut) %>%
            na.omit()
          
          partitioned.classification.flights <- sdf_random_split(tbl.classification, training = 0.7, testing = 0.3, seed = 42)
          ml.formula <- formula(OnTime ~ DepDelay + Distance + TaxiIn + TaxiOut)
          
          ml.log <- ml_logistic_regression(partitioned.classification.flights$training, formula = ml.formula)
          ml.log.pred <- ml_predict(ml.log, partitioned.classification.flights$testing)
          incProgress(0.25, message = "Precessed: Logistic Regression")
          
          ml.dt <- ml_decision_tree(partitioned.classification.flights$training, formula = ml.formula)
          ml.dt.pred <- ml_predict(ml.dt, partitioned.classification.flights$testing)
          incProgress(0.25, message = "Precessed: Decision Tree")
          
          ml.rf <- ml_random_forest(partitioned.classification.flights$training, formula = ml.formula)
          ml.rf.pred <- ml_predict(ml.rf, partitioned.classification.flights$testing)
          incProgress(0.25, message = "Precessed: Random Forest")
          
          ml.gbt <- ml_gradient_boosted_trees(partitioned.classification.flights$training, formula = ml.formula)
          ml.gbt.pred <- ml_predict(ml.gbt, partitioned.classification.flights$testing)
          incProgress(0.25, message = "Precessed: Gradient Boosted Trees")
          
          valuesR$ml.models.pred <- list(
            "Logistic" = ml.log.pred,
            "Decision Tree" = ml.dt.pred,
            "Random Forest" = ml.rf.pred,
            "Gradient Boosted Trees" = ml.gbt.pred
          )
          
          valuesR$ml.tree.models <- list(
            "Decision Tree" = ml.dt,
            "Random Forest" = ml.rf,
            "Gradient Boosted Trees" = ml.gbt
          )
          
          valuesR$ml.log <- ml.log
          valuesR$partitioned.classification.flights.test <- partitioned.classification.flights$testing
        })
        
        valuesR$semaforClass <- FALSE
      }
    }
    
    if (input$navbar == "Clustering (K-Means)") {
      if (isolate(valuesR$semaforClust)) {
        withProgress(message = "Preparing data for clustering...", value = 0, {
          tbl.filtered <- tbl %>%
            select(c(ends_with("Time"), ends_with("Delay"), starts_with("Taxi"), starts_with("Wheels"), Distance)) %>%
            filter(CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay > 15)
          
          tbl.tmp <- tbl.filtered %>%
            select(CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>%
            spark_apply(
              function(data){
                max.index <- max.col(data, "first")
                max.colname <- names(data)[max.index]
                data.frame(max.colname, data) 
              }, 
              names = c("DelayCode", colnames(data))
            ) %>%
            mutate(DelayCode = regexp_extract(DelayCode, "([A-Za-z]+)Delay", 1))
          
          tbl.clustering <- sdf_bind_cols(tbl.filtered, tbl.tmp) %>%
            select(!ends_with("Delay_x") & !ends_with("Delay_y"))
          list.butone <- colnames(tbl.clustering)[colnames(tbl.clustering) != "DelayCode"]
          
          valuesR$tbl.clustering <- tbl.clustering %>%
            ft_vector_assembler(
              input_cols = list.butone,
              output_col = "features_vectorized"
            ) %>%
            ft_min_max_scaler(
              input_col = "features_vectorized",
              output_col = "features_temp_scaled"
            ) %>%
            select(DelayCode, features_temp_scaled) %>%
            sdf_separate_column("features_temp_scaled", into = list.butone)
          
          incProgress(1)
        })
        
        valuesR$semaforClust <- FALSE
      }
    }
  }, ignoreInit = TRUE)
  
  # Queries SQL - Statistic features ----
  
  output$panelstatisticattrdistr <- renderUI({
    tbl.onlynum <- tbl %>%
      select_if(is.numeric)
    
    sidebarLayout(
      sidebarPanel(includeHTML("input_data/infoattrdistr.html")),
      mainPanel(
        dropdown(
          tags$h4("List of Inputs"),
          pickerInput(inputId = "attrDistr_attributes", label = "Variable", choices = colnames(tbl)),
          style = "unite",
          status = "danger",
          icon = icon("gear"), 
          width = "300px",
          tooltip = tooltipOptions(title = "Click to see inputs !")
        ),
        withSpinner(
          plotlyOutput("attrDistr"),
          type = 4,
          color = "#d33724",
          size = 0.7
        )
      )
    )
  })
  
  plot_attrDistr <- reactive({
    colname <- sym(input$attrDistr_attributes)
    
    tbl.filtered <- tbl %>%
      count(colname)
    
    ggplot(tbl.filtered) +
      geom_bar(aes_string(input$attrDistr_attributes, "n"), stat = "identity") +
      labs(
        x = input$attrDistr_attributes,
        y = "Count",
        title = "Feature distributions"
      )
  })
  
  output$attrDistr <- renderPlotly({
    plot_attrDistr()
  })
  
  output$panelstatisticarrdep <- renderUI({
    withProgress(message = "Preparing data...", value = 0, {
      tbl.choices <- tbl %>%
        distinct(Origin) %>% 
        full_join(distinct(tbl, Dest), c("Origin" = "Dest")) %>%
        collect()
      
      incProgress(1)
    })
    
    sidebarLayout(
      sidebarPanel(includeHTML("input_data/infoarrdepwindow.html")),
      mainPanel(
        dropdown(
          tags$h4("List of Inputs"),
          pickerInput(inputId = "ArrDepAirport_airport", label = "Airport (IATA Code)",choices = tbl.choices$Origin),
          sliderInput(inputId = "ArrDepAirport_range", label = "Temporal range", min = 3, max = 20, value = 12),
          style = "unite",
          status = "danger",
          icon = icon("gear"), 
          width = "300px",
          tooltip = tooltipOptions(title = "Click to see inputs !")
        ),
        withSpinner(
          plotlyOutput("ArrDepAirport"),
          type = 4,
          color = "#d33724",
          size = 0.7
        )
      )
    )
  })

  plot_ArrDepAirport <- reactive({
    airport <- input$ArrDepAirport_airport
    range <- paste(input$ArrDepAirport_range, "Days")
    
    tbl.filtered <- tbl %>%
      filter(Origin == airport || Dest == airport) %>%
      select(Origin, Dest, Year, Month, DayofMonth, WheelsOff, AirTime) %>%
      left_join(tbl.airports, by = c("Origin" = "iata_code")) %>%
      mutate(
        ActualDepDate = to_utc_timestamp(make_timestamp(Year, Month, DayofMonth, floor(WheelsOff / 100), (WheelsOff / 100 - floor(WheelsOff / 100)) * 100, 00.000), timezone),
        ActualArrDate = try_add(ActualDepDate, make_interval(0, 0, 0, 0, floor(AirTime / 60), (AirTime / 60 - floor(AirTime / 60)) * 60))
      )
    
    tbl.originwin <- tbl.filtered %>%
      filter(Origin == airport) %>%
      count(ActualDateWin = window(ActualDepDate, range)) %>%
      mutate(Type = "Departures")
    
    tbl.destwin <- tbl.filtered %>%
      filter(Dest == airport) %>%
      count(ActualDateWin = window(ActualArrDate, range)) %>%
      mutate(Type = "Arrivals")
    
    tbl.final <- sdf_bind_rows(tbl.originwin, tbl.destwin) %>%
      spark_apply(function(data) {
        ActualDate <- reshape2::melt(data$ActualDateWin)
        data.frame(ActualDate, data$n, data$Type)
      }) %>%
      filter(L1 %% 2 != 0)
    
    ggplot(tbl.final) + 
      geom_line(aes(value, data.n, colour = factor(data.Type))) +
      labs(
        color = "Type",
        x = "Date",
        y = "Count",
        title = "Time series of take-off and landings"
      )
  })
  
  output$ArrDepAirport <- renderPlotly({
    plot_ArrDepAirport()
  })
  
  # Queries SQL - General Informations ----
  
  plot_numberRoutesCarrier <- reactive({
    tbl.final <- tbl %>% 
      group_by(Reporting_Airline) %>%
      distinct(Origin, Dest) %>%
      count() %>%
      left_join(tbl.carriers, by = c("Reporting_Airline" = "carrier")) %>%
      collect()
    
    datatable(
      tbl.final,
      rownames = FALSE,
      options = list(
        dom = 'frtp',
        style = "bootstrap",
        pageLength = 10
      )
    )
  })
  
  output$numberRoutesCarrier <- renderDataTable({
    plot_numberRoutesCarrier()
  })
  
  # Queries SQL - Delays or cancellations ----
  
  plot_causeCancellationCarrier <- reactive({
    tbl.total <- tbl %>% 
      filter(Cancelled == "1") %>%
      group_by(Reporting_Airline)
    
    tbl.filtered1 <- tbl.total %>% count()
    
    tbl.filtered2 <- tbl.total %>% count(CancellationCode)
    
    tbl.final <- full_join(tbl.filtered1, tbl.filtered2, by = "Reporting_Airline") %>%
      mutate(
        Percent = (n_y / n_x) * 100,
        TypeCause = try_element_at(map("A", "Carrier", "B", "Weather", "C", "NAS", "D", "Security"), CancellationCode)) %>%
      left_join(tbl.carriers, by = c("Reporting_Airline" = "carrier")) %>%
      select(name, Percent, TypeCause)
    
    ggplot(tbl.final) +
      geom_bar(aes(x = name, y = Percent, fill = TypeCause), position = "stack", stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      labs(
        x = "Airline",
        y = "%",
        title = "Frequency of CancellationCode per Airline"
      )
  })
  
  output$causeCancellationCarrier <- renderPlotly({
    plot_causeCancellationCarrier()
  })
  
  calculate_sum <- function(attr, tbl, n.total) {
    n.filtered <- NULL
    
    if(attr == "Carrier"){
      n.filtered <- tbl %>%
        filter(CarrierDelay > 0) %>%
        summarise(TotalSum = sum(CarrierDelay, na.rm = TRUE)) %>% 
        collect()
    }
    
    if(attr == "Weather"){
      n.filtered <- tbl %>%
        filter(WeatherDelay > 0) %>%
        summarise(TotalSum = sum(WeatherDelay, na.rm = TRUE), na.rm = TRUE) %>% 
        collect()
    }
    
    if(attr == "NAS"){
      n.filtered <- tbl %>%
        filter(NASDelay > 0) %>%
        summarise(TotalSum = sum(NASDelay, na.rm = TRUE)) %>% 
        collect()
    }
    
    if(attr == "Security"){
      n.filtered <- tbl %>%
        filter(SecurityDelay > 0) %>%
        summarise(TotalSum = sum(SecurityDelay, na.rm = TRUE)) %>% 
        collect()
    }
    
    if(attr == "LateAircraft"){
      n.filtered <- tbl %>%
        filter(LateAircraftDelay > 0) %>%
        summarise(TotalSum = sum(LateAircraftDelay, na.rm = TRUE)) %>% 
        collect()
    }
    
    (n.filtered$TotalSum / n.total) * 100
  }
  
  output$panelcausedelaycarrier <- renderUI({
    tbl.choices <- tbl %>%
      distinct(Reporting_Airline) %>%
      collect()
    
    div(
      dropdown(
        tags$h4("List of Inputs"),
        pickerInput(inputId = "causeDelayCarrier_airport", label = "Carrier", choices = tbl.choices$Reporting_Airline),
        style = "unite",
        status = "danger",
        icon = icon("gear"), 
        width = "300px",
        tooltip = tooltipOptions(title = "Click to see inputs !")
      ),
      withSpinner(
        plotlyOutput("causeDelayCarrier"),
        type = 4,
        color = "#d33724",
        size = 0.7
      )
    )
  })
  
  plot_causeDelayCarrier <- reactive({
    tbl.filtered <- tbl %>% 
      filter(Reporting_Airline == !!input$causeDelayCarrier_airport & ArrDelay >= 15) 
    
    group <- c("Carrier", "Weather", "NAS", "Security", "LateAircraft")
    n.total <- tbl.filtered %>% 
      summarise(TotalSum = sum(ArrDelay, na.rm = TRUE)) %>%
      collect()
    
    value <- sapply(group, calculate_sum, tbl = tbl.filtered, n.total = n.total$TotalSum)
    df <- data.frame(group, value)
    
    plot_ly(df, labels = ~ group, values = ~ value, type = "pie") %>%
      layout(
        title = paste("Cause of delay % for", input$causeDelayCarrier_airport),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        legend = list(title = list(text = "TypeCause"))
      )
  })
  
  output$causeDelayCarrier <- renderPlotly({
    plot_causeDelayCarrier()
  })
  
  plot_arrDelayCarrier <- function(){
    tbl.total <- tbl %>% 
      count(Reporting_Airline)
    
    tbl.bucketed <- tbl %>%
      ft_bucketizer(
        input_col = 'ArrDelay',
        output_col = 'ArrDelayBucket',
        splits = list(-Inf, 0, 5, 15, 30, 60, Inf)
      ) %>%
      group_by(Reporting_Airline) %>%
      count(ArrDelayBucket) %>%
      mutate(ArrDelayBucketMapped = try_element_at(map(as.double(0), "Advance", as.double(1), "0 to 5 minutes", as.double(2), "5 to 15 minutes", as.double(3), "15 to 30 minutes", as.double(4), "30 to 60 minutes", as.double(5), "Above an hour"), ArrDelayBucket))
    
    tbl.final <- tbl.total %>%
      full_join(tbl.bucketed, by = "Reporting_Airline") %>%
      mutate(Percent = (n_y / n_x) * 100) %>%
      left_join(tbl.carriers, by = c("Reporting_Airline" = "carrier")) %>%
      select(name, Percent, ArrDelayBucketMapped)
    
    ggplot(tbl.final) +
      geom_bar(aes(x = name, y = Percent, fill = ArrDelayBucketMapped), position = "stack", stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      labs(
        color = "ArrDelay Range",
        x = "Airline",
        y = "%",
        title = "Frequency of ArrDelayBucketMapped (Interval of delay in minutes) per Airline"
      )
  }
  
  output$arrDelayCarrier <- renderPlotly({
    plot_arrDelayCarrier()
  })
  
  # Maps - Top flight paths  ----
  
  plot_topPaths <- reactive({
    tbl.filtered.origin <- tbl %>%
      count(Origin, Dest) %>%
      ungroup() %>%
      slice_max(order_by = n, n = as.integer(input$topPaths_paths)) %>%
      mutate(
        RouteID = paste0(Origin, Dest),
        AirportID = Origin)
    
    tbl.filtered.dest <- tbl.filtered.origin %>%
      mutate(AirportID = Dest)
    
    tbl.final <- sdf_bind_rows(tbl.filtered.origin, tbl.filtered.dest) %>%
      left_join(tbl.airports, c("AirportID" = "iata_code")) %>%
      select(RouteID, longitude_deg, latitude_deg, name) %>%
      collect()
    
    leaflet() %>%
      addTiles() %>%
      addPolylines(
        data = tbl.final,
        lng = ~ longitude_deg,
        lat = ~ latitude_deg,
        group = ~ RouteID,
        weight = 1,
        fillOpacity = 0.1,
        color = "#ff4500"
      )
    
  })
  
  output$topPaths <- renderLeaflet({
    plot_topPaths()
  })
  
  # Maps - Statistic features  ----
  
  plot_airportsMapDistr <- reactive({
    tbl.origin <- tbl %>% count(Origin)
    
    tbl.dest <- tbl %>% count(Dest)
    
    tbl.filtered <- full_join(tbl.origin, tbl.dest, c("Origin" = "Dest")) %>%
      mutate(nTotal = (n_x + n_y) / 10000) %>%
      slice_max(
        order_by = nTotal,
        n = as.integer(input$airportsMapDistr_airports)) %>%
      left_join(tbl.airports, by = c("Origin" = "iata_code")) %>%
      collect()
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = tbl.filtered,
        lat = ~ latitude_deg,
        lng = ~ longitude_deg,
        weight = 1,
        radius = ~ nTotal,
        fillOpacity = 0.1,
        color = "#ff4500"
      )
  })
  
  output$airportsMapDistr <- renderLeaflet({
    plot_airportsMapDistr()
  })

  # Machine Learning - Matrix Correlation ----
  
  plot_matrixCorr <- reactive({
    tbl.cleaned <- select_if(tbl, negate(is.character)) %>%
      na.omit()
    
    p <- correlate(tbl.cleaned, quiet = TRUE) %>%
      shave() %>%
      rplot()
    
    p + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      labs(title = "Correlation Matrix")
  })
  
  output$matrixCorr <- renderPlotly({
    plot_matrixCorr()
  })
  
  # Machine Learning - Classification ----
  
  output$panelclassifcomp <- renderUI({
    sidebarLayout(
      sidebarPanel(includeHTML("input_data/infometrics.html")),
      mainPanel(
        withSpinner(
          plotlyOutput("MLClassComp"),
          type = 4,
          color = "#d33724",
          size = 0.7
        )
      )
    )
  })
    
  plot_MLClassComp <- function(){
    ml.models.pred <- valuesR$ml.models.pred
    
    ml.models.results <- data.frame()
    for (i in 1:length(ml.models.pred)) {
      for (metric in list("f1", "weightedRecall", "weightedPrecision", "accuracy")) {
        ml.model.result <- ml_multiclass_classification_evaluator(ml.models.pred[[i]], metric_name = metric) 
        ml.models.results <- rbind(ml.models.results, data.frame(names(ml.models.pred[i]), metric, ml.model.result))
      }
    }
    names(ml.models.results) <- c("Classifier", "Metric", "Performance")
    
    ggplot(ml.models.results) +
      geom_bar(aes(Classifier, Performance, fill = Metric), stat = "identity", position = "dodge")
  }
  
  output$MLClassComp <- renderPlotly({
    plot_MLClassComp()
  })
  
  plot_MLClassROC <- function(){
    eval.summary <- ml_evaluate(valuesR$ml.log, valuesR$partitioned.classification.flights.test)
    
    roc <- eval.summary$roc() %>%
      collect()
    
    ggplot(roc) +
      geom_line(aes(FPR, TPR)) + 
      geom_abline(lty = "dashed")
  }
  
  output$MLClassROC <- renderPlotly({
    plot_MLClassROC()
  })
  
  plot_MLClassFeaImpo <- function(){
    feature.importance <- data.frame()
    
    for(i in c("Decision Tree", "Random Forest", "Gradient Boosted Trees")){
      feature.importance <- ml_tree_feature_importance(valuesR$ml.tree.models[[i]]) %>%
        mutate(
          Model = i,
          feature = as.character(feature)) %>%
        rbind(feature.importance, .)
    }
    
    ggplot(feature.importance) + 
      facet_wrap(~ Model) +
      geom_bar(aes(reorder(feature, importance), importance, fill = Model), stat = "identity") + 
      coord_flip() +
      labs(
        x = "Importance",
        y = "Feature",
        title = "Feature Importance"
      )
  }
  
  output$MLClassFeaImpo <- renderPlotly({
    plot_MLClassFeaImpo()
  })
  
  # Machine Learning - Clustering (K-Means) ----
  
  output$panelclusteringpca <- renderUI({
    sidebarLayout(
      sidebarPanel(includeHTML("input_data/infopca.html")),
      mainPanel(
        withSpinner(
          plotlyOutput("pcavariance", height = 450),
          type = 4,
          color = "#d33724",
          size = 0.7
        )
      )
    )
  })
  
  plot_pcavariance <- function(){
    tbl.clusteringpca <- valuesR$tbl.clustering %>%
      select(-c(DelayCode, features_temp_scaled))
    
    n.pc <- 1:length(colnames(tbl.clusteringpca))
    ml.pca <- ml_pca(tbl.clusteringpca)
    ml.pca.expvar <- ml.pca[["explained_variance"]]
    
    pca.explain.variance <- do.call(rbind, Map(data.frame, NPC = n.pc, PCValue = ml.pca.expvar * 100)) %>%
      mutate(PCCum = cumsum(PCValue))
    
    ggplot(pca.explain.variance) +
      geom_line(aes(NPC, PCCum)) +
      labs(
        x = "Principal Component Rank", 
        y = "% Variance Explained",
        title = "PCA - Cumulative variance explained"
      )
  }
  
  output$pcavariance <- renderPlotly({
    plot_pcavariance()
  })
  
  plot_silhouettecomp <- function(){
    withProgress(message = "Calculating silhouette values...", value = 0, {
      k.values <- 2:8
      
      tbl.clusteringsilh <- valuesR$tbl.clustering %>%
        ft_pca(
          input_col = "features_temp_scaled",
          output_col = "features",
          k = 3
        ) %>%
        select(DelayCode, features) %>%
        sdf_separate_column("features", into = c("PC1", "PC2", "PC3")) %>%
        select(-features)
      
      partitioned.clustering.flights <- sdf_random_split(tbl.clusteringsilh, training = 0.7, testing = 0.3, seed = 42)
      
      silhouette.values <- lapply(k.values, function(k){
        ml_kmeans <- ml_kmeans(partitioned.clustering.flights$training, formula = DelayCode ~ ., k = k)
        eval_summary <- ml_evaluate(ml_kmeans, partitioned.clustering.flights$testing)
        incProgress(1/length(k.values), message = paste("Calculated:", k, "clusters"))
        eval_summary
      })
      
      tbl.clustering.silhouette <- do.call(rbind, Map(data.frame, NumberCluster = k.values, Silhouette = silhouette.values))
    })
    
    ggplot(tbl.clustering.silhouette) +
      geom_line(aes(NumberCluster, Silhouette)) +
      labs(
        x = "Number of clusters K",
        y = "Silhouette coefficent",
        title = "Silhouette coefficent comparision"
      )
  }
  
  output$silhouettecomp <- renderPlotly({
    plot_silhouettecomp()
  })
  
  plot_clustersplot <- function(){
    k <- input$clustersplot_k
    sample.n <- floor(5000/k)
    
    tbl.clusteringfinal <- valuesR$tbl.clustering %>%
      ft_pca(
        input_col = "features_temp_scaled",
        output_col = "features",
        k = 3
      ) %>%
      select(DelayCode, features) %>%
      sdf_separate_column("features", into = c("PC1", "PC2", "PC3"))
    
    partitioned.clustering.flights <- sdf_random_split(tbl.clusteringfinal, training = 0.7, testing = 0.3, seed = 42)
    
    ml_kmeans <- ml_kmeans(partitioned.clustering.flights$training, k = k)
    predicted <- ml_predict(ml_kmeans, partitioned.clustering.flights$testing) %>%
      group_by(prediction) %>%
      sample_n(sample.n)
    
    xcolname <- input$clustersplot_xcol
    ycolname <- input$clustersplot_ycol
    
    ggplot(predicted, aes(col = factor(prediction + 1))) +
      geom_point(aes_string(xcolname, ycolname), size = 2, alpha = 0.5) +
      scale_color_discrete(
        name = "Predicted Cluster",
        labels = paste("Cluster", 1:k)
      ) +
      labs(
        x = input$clustersplot_xcol,
        y = input$clustersplot_ycol,
        title = "K-Means Clustering"
      )
  }
  
  output$clustersplot <- renderPlotly({
    plot_clustersplot()
  })
}