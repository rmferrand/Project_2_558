library(shiny)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(ggridges)
library(shinycssloaders)
library(shinyalert)

mobile_behavior<- read_csv("user_behavior_dataset.csv")
mobile_behavior$`User Behavior Class` <- as.factor(mobile_behavior$`User Behavior Class`)
mobile_behavior$AgeCat <- as.factor(ifelse(mobile_behavior$Age <= 28, 1,
                                           ifelse(mobile_behavior$Age <= 38, 2,
                                                  ifelse(mobile_behavior$Age <= 49, 3, 4))))

#create cat and num vars to allow for easy choices in the buttons
cat_vars <- mobile_behavior |>
  select(`AgeCat`, `Operating System`, `Gender`, `User Behavior Class`) |>
  mutate(
    AgeCat = as.factor(AgeCat),
    `Operating System` = as.factor(`Operating System`),
    Gender = as.factor(Gender),
    `User Behavior Class` = as.factor(`User Behavior Class`)
  )

num_vars <- mobile_behavior |>
  select(`App Usage Time (min/day)`, `Screen On Time (hours/day)`, `Battery Drain (mAh/day)`, `Data Usage (MB/day)`) 


#create the UI
ui <- fluidPage(
  titlePanel("Project 2 558 - Mobile Behavior Data Exploration App"),
  sidebarLayout(
    sidebarPanel(
      h1("Subset The Data!"),
      h2("Categorical Variables"),
      #subset by age category
      radioButtons(
        "AgeCat",
        "Age Category",
        selected = "All",
        choiceNames = c("All","28 or Younger", "29-38", "39-49", "50 or Older"),
        choiceValues = c("All","1", "2", "3", "4")
      ),
      #subset by gender
      radioButtons(
        "Gender",
        "Gender",
        selected = "All",
        choiceNames = c("All","Male", "Female"),
        choiceValues = c("All","Male", "Female")
      ),
      #subset by operating system
      radioButtons(
        "OS",
        "Operating System",
        selected = "All",
        choiceNames = c("All","Android", "iOS"),
        choiceValues = c("All","Android", "iOS")
      ),
      h2("Numerical Variables"),
      #allow user to subset by numeric variables
      selectizeInput(
        "num_var1",
        "Numerical Variable 1",
        selected = "`App Usage Time (min/day)`",
        choices = as.factor(colnames(num_vars))
      ),
      #subset by numeric variable 2
      selectizeInput(
        "num_var2",
        "Numerical Variable 2",
        selected = "`Screen On Time (hours/day)`",
        choices = as.factor(colnames(num_vars))
      ),
      #numerical sliders for users to subset
      uiOutput("num_slider1"),
      uiOutput("num_slider2"),
      #allows users to subset on numerical or categorical variables
      actionButton("subset_num", "Apply Subset")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 img(src = "mobile_phones.jpg", alt = "Cartoon Image of Phone Usage", height = "400px", width = "400px"),
                 h3("Purpose"),
                  p("This Shiny app was created to fulfill the requirements for Project 2 in ST558, a Master of Statistics
                  Course at North Carolina State University. The project focuses on the integration of functions, data analysis, 
                  and the development of Shiny applications. The Shiny application allows users to interactively explore a datset,
                  by means of subsetting, downloading, and variable analysis."),
                 h3("About the Data"),
                 p("The data",
                   tags$a(href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset", 
                          "(found at  this Kaggle link)", 
                          target = "_blank"),
                  "is a dataset containing 700 observations, with the topic of interest being mobile user behavior.
                  The dataset categories users into 1 of 5 usage categories, from least extreme to most extreme based on various
                  numerical variables (such as app usage, data usage, etc.). Users are also categorized by variables such as
                  age, gender, etc."
                 ),
                 h3("Data Subsetting and Exploration"),
                 p("The sidebar layout allows the user to subset the data at any point. Users may select from any specific
                   categorical variable and the various possible levels. The users may also select from a few numeric
                   variables, and use the sliders to change the range of the data as they best see fit. The about tab,
                   which you are reading now, is intended to outline the purpose and function of the Shiny app. the Data Download
                   tab allows users to download the data for personal use, including subsetted and unsubsetted data. Finally,
                   the data explore tab allows the user to run through numerical, categorical, and graphical analysis of the
                   data."),
                 h3("I hope you enjoy this app! Have fun!")
        ),
        tabPanel("Data Download",
                 h3("Filtered Data"),
                 #display filtered data table with complete data
                 DT::dataTableOutput("filtered_data"),
                 #show download button for the data
                 downloadButton("download_data", "Download Data")
        ),
        tabPanel("Data Explore",
                 tabsetPanel(
                   tabPanel("Categorical Variable Analysis",
                            tabsetPanel(
                              tabPanel("One-Way Contingency Tables",
                                       h3("One-Way Contingency Tables"),
                                       selectizeInput("oneway_cat", "Select Categorical Variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "AgeCat"),
                                       tableOutput("oneway_table"),
                              ),
                              tabPanel("Two-Way Contingency Tables",
                                       h3("Two-Way Contingency Tables"),
                                       selectizeInput("twoway_cat1", "Select Categorical Variable 1", 
                                                      choices = colnames(cat_vars), 
                                                      selected = "AgeCat"),
                                       selectizeInput("twoway_cat2", "Select Categorical Variable 2", 
                                                      choices = colnames(cat_vars), 
                                                      selected = "User Behavior Class"),
                                       tableOutput("twoway_table"),
                              )
                            )
                   ),
                   tabPanel("Numerical Variable Analysis",
                            h3("Summary Statistics"),
                            selectInput("one_cat", "Group by:", choices = c("None", colnames(cat_vars))),
                            selectInput("one_num", "Select Numerical Variable:", choices = c("All", colnames(num_vars))),
                            tableOutput("numerical_summaries")
                   ),
                   tabPanel("Graphical Analysis",
                            tabsetPanel(
                              tabPanel("Histogram",
                                       h3("Histogram"),
                                       selectInput("hist_cat", "Select Faceting variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "User Behavior Class"),
                                       withSpinner(plotOutput("multihistogram", width = "100%", height = "700px")),
                                       h3(uiOutput("hist_info"))
                              ),
                              tabPanel("Boxplots",
                                       h3("Boxplots"),
                                       selectInput("box_cat", "Select Faceting variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "Gender"),
                                       withSpinner(plotOutput("multiboxplot", width = "100%", height = "700px")),
                                       h3(uiOutput("box_info"))
                              ),
                              tabPanel("Correlation Plot",
                                       h3("Correlation Plot"),
                                       withSpinner(plotOutput("corrplot", width = "100%", height = "700px")),
                                       h3(uiOutput("corr_info"))
                              ),
                              tabPanel("Violin Plot",
                                       h3("Violin Plot"),
                                       selectInput("violin_cat", "Select Faceting variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "AgeCat"),
                                       withSpinner(plotOutput("violin", width = "100%", height = "700px")),
                                       h3(uiOutput("violin_info"))
                              ),
                              tabPanel("Pie Chart",
                                       h3("Pie Chart"),
                                       selectInput("pie_cat", "Select categorical Variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "Operating System"),
                                       withSpinner(plotOutput("pie", width = "100%", height = "700px")),
                                       h3(uiOutput("pie_info"))
                              ),
                              tabPanel("Ridge Plot",
                                       h3("Ridge Plot"),
                                       selectInput("ridge_cat", "Select categorical Variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "AgeCat"),
                                       selectInput("ridge_num", "Select Numerical Variable", 
                                                   choices = colnames(num_vars), 
                                                   selected = "App Usage Time (min/day)"),
                                       withSpinner(plotOutput("ridge", width = "100%", height = "700px")),
                                       h3(uiOutput("ridge_info"))
                              ),
                              tabPanel("Scatterplot",
                                       h3("Scatterplot"),
                                       selectInput("scatternum_var1", "Select X-axis Variable", 
                                                   choices = colnames(num_vars), 
                                                   selected = "App Usage Time (min/day)"),
                                       selectInput("scatternum_var2", "Select Y-axis Variable", 
                                                   choices = colnames(num_vars), 
                                                   selected = "Screen On Time (hours/day)"),
                                       withSpinner(plotOutput("scatter", width = "100%", height = "700px")),
                                       h3(uiOutput("scatter_info"))
                              ),
                              tabPanel("Bar Graph",
                                       h3("Bar Graph"),
                                       selectInput("barcat_var1", "Select First Categorical Variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "Gender"),
                                       selectInput("barcat_var2", "Select Second Categorical Variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "User Behavior Class"),
                                       withSpinner(plotOutput("bargraph", width = "100%", height = "700px")),
                                       h3(uiOutput("bar_info"))
                              ),
                            )
                   )
                 ),
        )
      )
    )
  )
)


#start server side
server <- function(input, output, session) {
  
  #make sure users can't pick duplicates for num var. similar to code from shiny app hw7
  observeEvent(c(input$num_var1, input$num_var2), {
    num_var1 <- input$num_var1
    num_var2 <- input$num_var2
    choices <- as.factor(colnames(num_vars))
    if (num_var1 == num_var2){
      choices <- choices[-which(choices == num_var1)]
      updateSelectizeInput(session,
                           "num_var2",
                           choices = choices)
    }
  })
  
  #no duplicate two way contigency table
  observeEvent(c(input$twoway_cat1, input$twoway_cat2), {
    twoway_cat1 <- input$twoway_cat1
    twoway_cat2 <- input$twoway_cat2
    choices <- as.factor(colnames(cat_vars))
    if (twoway_cat1 == twoway_cat2){
      choices <- choices[-which(choices == twoway_cat1)]
      updateSelectizeInput(session,
                           "twoway_cat2",
                           choices = choices)
    }
  })
  
  #no duplicate scatterplot
  observeEvent(c(input$scatternum_var1, input$scatternum_var2), {
    scatternum_var1 <- input$scatternum_var1
    scatternum_var2 <- input$scatternum_var2
    choices <- as.factor(colnames(num_vars))
    if (scatternum_var1 == scatternum_var2){
      choices <- choices[-which(choices == scatternum_var1)]
      updateSelectizeInput(session,
                           "scatternum_var2",
                           choices = choices)
    }
  })
  
  #no duplicate bar graph
  observeEvent(c(input$barcat_var1, input$barcat_var2), {
    barcat_var1 <- input$barcat_var1
    barcat_var2<- input$barcat_var2
    choices <- as.factor(colnames(cat_vars))
    if (barcat_var1 == barcat_var2){
      choices <- choices[-which(choices == barcat_var1)]
      updateSelectizeInput(session,
                           "barcat_var2",
                           choices = choices)
    }
  })
  
  #numerical slider 1
  output$num_slider1 <- renderUI({
    req(input$num_var1)
    num_range1 <- range(num_vars[[input$num_var1]])
    sliderInput(
      "range1",
      label = paste("Range Of", input$num_var1),
      min = num_range1[1],
      max = num_range1[2],
      value = num_range1
    )
  })
  
  #numerical slider 2
  output$num_slider2 <- renderUI({
    req(input$num_var2)
    num_range2 <- range(num_vars[[input$num_var2]])
    sliderInput(
      "range2",
      label = paste("Range Of", input$num_var2),
      min = num_range2[1],
      max = num_range2[2],
      value = num_range2
    )
  })
  
  #let data react to subsets.
  filtered_data <- reactiveVal(mobile_behavior)
  observeEvent(input$subset_num, {
    #filter the data, check number of obs. display warning 
    if (nrow(mobile_behavior[
      mobile_behavior[[input$num_var1]] >= input$range1[1] & mobile_behavior[[input$num_var1]] <= input$range1[2] &
      mobile_behavior[[input$num_var2]] >= input$range2[1] & mobile_behavior[[input$num_var2]] <= input$range2[2] &
      (input$AgeCat == "All" | mobile_behavior$AgeCat == input$AgeCat) &
      (input$Gender == "All" | mobile_behavior$Gender == input$Gender) &
      (input$OS == "All" | mobile_behavior$`Operating System` == input$OS),
    ]) < 2) {
      shinyalert(
        title = "Whoops!",
        text = "The data has fewer than two observations after subsetting. This may cause issues in graphing. Please adjust your filters.",
        type = "warning",
        showConfirmButton = TRUE,
        closeOnClickOutside = TRUE
      )
    } else {
      #update fitered_data() only if the numbers are sufficient
      filtered_data(mobile_behavior[
        mobile_behavior[[input$num_var1]] >= input$range1[1] & mobile_behavior[[input$num_var1]] <= input$range1[2] &
          mobile_behavior[[input$num_var2]] >= input$range2[1] & mobile_behavior[[input$num_var2]] <= input$range2[2] &
          (input$AgeCat == "All" | mobile_behavior$AgeCat == input$AgeCat) &
          (input$Gender == "All" | mobile_behavior$Gender == input$Gender) &
          (input$OS == "All" | mobile_behavior$`Operating System` == input$OS),
      ])
    }
  })
  
  #show filtered data on data download tab
  output$filtered_data <- DT::renderDataTable({
    filtered_data()})
  
  #allow user to download the data table in the download tab
  output$download_data <- downloadHandler(
    filename = function() {
      paste('filtered_data_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE) 
    }
  )
  
  #one way table allowing users to select categorical variable
  output$oneway_table <- renderTable({
    filtered_data() |>
      select(!!sym(input$oneway_cat)) |>
      table() |>
      as.data.frame() |>
      setNames(c(input$oneway_cat, "Frequency"))
  }, rownames = FALSE)
  
  #two way table allowing users to select two categorical variables
  output$twoway_table <- renderTable({
    filtered_data() |>
      select(!!sym(input$twoway_cat1), !!sym(input$twoway_cat2)) |>
      table() |>
      as.data.frame() |>
      setNames(c(input$twoway_cat1, input$twoway_cat2, "Frequency"))
  }, rownames = FALSE)

  #output numerical summaries. small error right now
  output$numerical_summaries <- renderTable({
    if (input$one_num != "All" && input$one_cat != "None"){
      filtered_data() |>
        group_by(!!sym(input$one_cat)) |>
        summarise(across(all_of(input$one_num), list(
          Min = ~min(. , na.rm = TRUE),
          Max = ~max(. , na.rm = TRUE),
          Mean = ~mean(. , na.rm = TRUE),
          Median = ~median(. , na.rm = TRUE),
          SD = ~sd(. , na.rm = TRUE),
          IQR = ~IQR(. , na.rm = TRUE)), 
          .names = "{.col}_{.fn}")) |>
        pivot_longer(cols = -!!sym(input$one_cat), names_to = c("Variable", ".value"), 
                     names_sep = "_") |>
        as.data.frame()
    }
    else if(input$one_cat != "None") {
      filtered_data() |>
        group_by(!!sym(input$one_cat)) |>
        summarise(across(where(is.numeric), list(
          Min = ~min(. , na.rm = TRUE),
          Max = ~max(. , na.rm = TRUE),
          Mean = ~mean(. , na.rm = TRUE),
          Median = ~median(. , na.rm = TRUE),
          SD = ~sd(. , na.rm = TRUE),
          IQR = ~IQR(. , na.rm = TRUE)), 
          .names = "{.col}_{.fn}")) |>
        pivot_longer(cols = -!!sym(input$one_cat), names_to = c("Variable", ".value"), 
                     names_sep = "_") |>
        slice(-c(1, 8)) |> 
        as.data.frame()
    } 
    else if (input$one_num != "All") {
      filtered_data() |>
        summarise(across(all_of(input$one_num), 
                         list(
                           Min = ~min(. , na.rm = TRUE),
                           Max = ~max(. , na.rm = TRUE),
                           Mean = ~mean(. , na.rm = TRUE),
                           Median = ~median(. , na.rm = TRUE),
                           SD = ~sd(. , na.rm = TRUE),
                           IQR = ~IQR(. , na.rm = TRUE)), 
                         .names = "{.col}_{.fn}")) |>
        pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_")
    }
    else {
      filtered_data() |>
        summarise(across(where(is.numeric), 
                         list(
                           Min = ~min(. , na.rm = TRUE),
                           Max = ~max(. , na.rm = TRUE),
                           Mean = ~mean(. , na.rm = TRUE),
                           Median = ~median(. , na.rm = TRUE),
                           SD = ~sd(. , na.rm = TRUE),
                           IQR = ~IQR(. , na.rm = TRUE)), 
                         .names = "{.col}_{.fn}")) |>
        pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_") |>
        slice(-c(1, 8))
    }
  })
  
  #find total number of observations of filtered data
  total_obs <- reactive({
    nrow(filtered_data())
  })
  
  #observations per group histogram
  cat_obs_countshist <- reactive({
    curr_cat_var <- filtered_data()[[input$hist_cat]]
    cat_counts <- table(curr_cat_var)
    cat_counts
  })
  
  #observations per group boxplot
  cat_obs_countsbox <- reactive({
    curr_cat_var <- filtered_data()[[input$box_cat]]
    cat_counts <- table(curr_cat_var)
    cat_counts
  })
  
  #observations per group violin plot
  cat_obs_countsviolin <- reactive({
    curr_cat_var <- filtered_data()[[input$violin_cat]]
    cat_counts <- table(curr_cat_var)
    cat_counts
  })
  
  #observations per group pie chart
  cat_obs_countspie <- reactive({
    curr_cat_var <- filtered_data()[[input$pie_cat]]
    cat_counts <- table(curr_cat_var)
    cat_counts
  })
  
  #observations per group ridge plot
  cat_obs_countsridge <- reactive({
    curr_cat_var <- filtered_data()[[input$ridge_cat]]
    cat_counts <- table(curr_cat_var)
    cat_counts
  })
  
  #observations per group bar graph 1
  cat_obs_countsbar1 <- reactive({
    curr_cat_var1 <- filtered_data()[[input$barcat_var1]]
    cat_counts1 <- table(curr_cat_var1)
  })
  
  #observations per group bar graph 2
  cat_obs_countsbar2 <- reactive({
    curr_cat_var2 <- filtered_data()[[input$barcat_var2]]
    cat_counts2 <- table(curr_cat_var2)
  })
  
  #create separate dataset with user selection
  output$multihistogram <- renderPlot({
    num_var_graphdat <- filtered_data() |>
      select(!!sym(input$hist_cat), 
             `App Usage Time (min/day)`, 
             `Screen On Time (hours/day)`, 
             `Battery Drain (mAh/day)`, 
             `Data Usage (MB/day)`) |>
      pivot_longer(cols = -(!!sym(input$hist_cat)), 
                   names_to = "numvar", 
                   values_to = "value")
    
    #output histogram with faceting, increasing size of axes for user readability
    ggplot(num_var_graphdat, aes(x = value, fill = as.factor(!!sym(input$hist_cat)))) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.7, color = "black") +
      facet_wrap(~ numvar, scales = "free") +
      labs(x = "Numeric Variables", y = "Frequency By User Behavior", fill = sym(input$hist_cat)) +
      theme_minimal(base_size = 17) +
      theme(legend.position = "top",
            axis.title.x = element_text(size = 17), 
            axis.title.y = element_text(size = 17),
            strip.text = element_text(size = 14))
  })
  
  output$hist_info <- renderText({
    HTML(paste0(
      "After subsetting, the total number of observations is ", total_obs(), ".<br/>",
      "For each ", input$hist_cat, " group, the number of observations is as follows: <br/>",
      paste(names(cat_obs_countshist()), cat_obs_countshist(), sep = ": ", collapse = "<br/>")
    ))
  })
  
  #create separate dataset with user selection
  output$multiboxplot <- renderPlot({
    boxplot_var_graphdat <- filtered_data() |>
      select(!!sym(input$box_cat), 
             `App Usage Time (min/day)`, 
             `Screen On Time (hours/day)`, 
             `Battery Drain (mAh/day)`, 
             `Data Usage (MB/day)`) |>
      pivot_longer(cols = -(!!sym(input$box_cat)), 
                   names_to = "numvar", 
                   values_to = "value")
    
    #output boxplot with faceting, increasing size of axes for user readability
    ggplot(boxplot_var_graphdat, aes(x = `value`, y = `numvar`, fill = as.factor(!!sym(input$box_cat)))) +
      geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.7, color = "black") +
      facet_wrap(~ numvar, scales = "free") +
      labs(x = "Scale For Numeric Variables", y = "Frequency of User", fill = (input$box_cat)) +
      theme_minimal(base_size = 17) +
      theme(legend.position = "top",
            axis.title.x = element_text(size = 17), 
            axis.title.y = element_text(size = 17),
            strip.text = element_text(size = 14)) +
      coord_flip()
    
  })
  
  output$box_info <- renderText({
    HTML(paste0(
      "After subsetting, the total number of observations is ", total_obs(), ".<br/>",
      "For each ", input$box_cat, " group, the number of observations is as follows: <br/>",
      paste(names(cat_obs_countsbox()), cat_obs_countsbox(), sep = ": ", collapse = "<br/>")
    ))
  })

  #create corrplot for numerical variable analysis
  output$corrplot <- renderPlot({
    cor_matrix <- filtered_data() |>
      select(`App Usage Time (min/day)`, 
             `Screen On Time (hours/day)`, 
             `Battery Drain (mAh/day)`, 
             `Data Usage (MB/day)`,
             `Age`,
             `Number of Apps Installed`) |>
      cor(use = "complete.obs") |>
      corrplot(method = "circle", type = "upper", 
               tl.col = "red", tl.srt = 45, addCoef.col = "black", col = colorRampPalette(c("cyan", "white", "salmon"))(200))
  })
  
  output$corr_info <- renderText({
    HTML(paste0(
      "After subsetting, the total number of observations is ", total_obs(), ".<br/>"))
  })
  
  
  #create separate dataset with user selection
  output$violin <- renderPlot({
    violin_var_graphdat <- filtered_data() |>
      select(!!sym(input$violin_cat), 
             `App Usage Time (min/day)`, 
             `Screen On Time (hours/day)`, 
             `Battery Drain (mAh/day)`, 
             `Data Usage (MB/day)`) |>
      pivot_longer(cols = -(!!sym(input$violin_cat)), 
                   names_to = "numvar", 
                   values_to = "value")
    
    #output violin plot with faceting, increasing size of axes for user readability
    ggplot(violin_var_graphdat, aes(x = `value`, y = `numvar`, fill = as.factor((!!sym(input$violin_cat))))) +
      geom_violin(position = position_dodge(width = 0.75), alpha = 0.7, color = "black") +
      facet_wrap(~ numvar, scales = "free") +
      labs(x = "Numeric Variables", y = "Frequency of User", fill = ((input$violin_cat))) +
      theme_minimal(base_size = 17) +
      theme(legend.position = "top",
            axis.title.x = element_text(size = 17), 
            axis.title.y = element_text(size = 17),
            strip.text = element_text(size = 14)) +
      coord_flip()
  })
  
  output$violin_info <- renderText({
    HTML(paste0(
      "After subsetting, the total number of observations is ", total_obs(), ".<br/>",
      "For each ", input$violin_cat, " group, the number of observations is as follows: <br/>",
      paste(names(cat_obs_countsviolin()), cat_obs_countsviolin(), sep = ": ", collapse = "<br/>")
    ))
  })
  
  #create counts for the pie chart
  output$pie <- renderPlot({
    pie_counts <- filtered_data() |>
      group_by(!!sym(input$pie_cat)) |>
      summarise(count = n(), .groups = 'drop')
    
    #pie chart for categorical variables
    ggplot(pie_counts, aes(x = "", y = count, fill = factor(!!sym(input$pie_cat)))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +  
      labs(title = paste("Pie Chart Of", input$pie_cat), fill = input$pie_cat) +
      theme_minimal(base_size = 17) +
      theme(legend.position = "top",
            axis.title.x = element_text(size = 17), 
            axis.title.y = element_text(size = 17),
            strip.text = element_text(size = 14))
  })
  
  output$pie_info <- renderText({
    HTML(paste0(
      "After subsetting, the total number of observations is ", total_obs(), ".<br/>",
      "For each ", input$pie_cat, " group, the number of observations is as follows: <br/>",
      paste(names(cat_obs_countspie()), cat_obs_countspie(), sep = ": ", collapse = "<br/>")
    ))
  })
  
  #ridge plot for one numeric one categorical variable with user selection
  output$ridge <- renderPlot({
    ggplot(filtered_data(), aes(x = !!sym(input$ridge_num), y = !!sym(input$ridge_cat), fill = !!sym(input$ridge_cat))) +
      geom_density_ridges(alpha = 0.7) +
      labs(title = "Ridge Plot", x = input$ridge_num,y = input$ridge_cat) +
      theme_minimal(base_size = 17) +
      theme(
        axis.title.x = element_text(size = 17), 
        axis.title.y = element_text(size = 17),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$ridge_info <- renderText({
    HTML(paste0(
      "After subsetting, the total number of observations is ", total_obs(), ".<br/>",
      "For each ", input$ridge_cat, " group, the number of observations is as follows: <br/>",
      paste(names(cat_obs_countsridge()), cat_obs_countsridge(), sep = ": ", collapse = "<br/>")
    ))
  })
  
  #scatterplot for two numeric variables with user selection
  output$scatter <- renderPlot({
    ggplot(filtered_data(), aes(x = !!sym(input$scatternum_var1), y = !!sym(input$scatternum_var2))) +
      geom_point(alpha = 0.7, color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(
        x = input$scatternum_var1, 
        y = input$scatternum_var2, 
        title = paste("Scatterplot of", input$scatternum_var1, "vs.", input$scatternum_var2)
      ) +
      theme_minimal(base_size = 17) +
      theme(
        axis.title.x = element_text(size = 17), 
        axis.title.y = element_text(size = 17),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$scatter_info <- renderText({
    HTML(paste0(
      "After subsetting, the total number of observations is ", total_obs(), ".<br/>"
    ))
  })
  
  #bar graph between two categorical variables with user selection
  output$bargraph <- renderPlot({
    ggplot(filtered_data(), aes(x = !!sym(input$barcat_var1), fill = as.factor(!!sym(input$barcat_var2)))) +
      geom_bar(position = "stack") +
      labs(x = input$barcat_var1, y = "Frequency", fill = (input$barcat_var2)) +
      theme_minimal(base_size = 17) +
      theme(axis.title.x = element_text(size = 17), 
            axis.title.y = element_text(size = 17),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "top")
  })
  
  output$bar_info <- renderText({
    HTML(paste0(
      "After subsetting, the total number of observations is ", total_obs(), ".<br/><br/>",
      "For each ", input$barcat_var1, " group, the number of observations is as follows: <br/>",
      paste(names(cat_obs_countsbar1()), cat_obs_countsbar1(), sep = ": ", collapse = "<br/>"), "<br/><br/>",
      "For each ", input$barcat_var2, " group, the number of observations is as follows: <br/>",
      paste(names(cat_obs_countsbar2()), cat_obs_countsbar2(), sep = ": ", collapse = "<br/>")
    ))
  })
  
}
shinyApp(ui, server)