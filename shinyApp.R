library(shiny)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(ggridges)

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
                 h3("Purpose"),
                 "This Shiny app was constructed to complete the requirements listed for Project 2 in ST558.
                 In general, this project combines functions, data analysis, and Shiny app creation.", 
                 h3("About the Data"),
                 "The Data (found at https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset)
                 is a 700 observation ",
                 h3("Data Subsetting and Exploration"),
                 "In particular, we allow the user to subset the data at any point for their convenience.
                 We allow the user to download the data (filtered or unfiltered) for their personal analysis.
                 We allow the user to print numerical and categorical data analysis under the explore tab.
                 Lastly, we allow the user to to view multiple graphs of the data for further numerical and
                 categorical analysis."
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
                              ),
                              tabPanel("Boxplots",
                                       h3("Boxplots"),
                                       selectInput("box_cat", "Select Faceting variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "Gender"),
                                       withSpinner(plotOutput("multiboxplot", width = "100%", height = "700px")),
                              ),
                              tabPanel("Correlation Plot",
                                       h3("Correlation Plot"),
                                       withSpinner(plotOutput("corrplot", width = "100%", height = "700px")),
                              ),
                              tabPanel("Violin Plot",
                                       h3("Violin Plot"),
                                       selectInput("violin_cat", "Select Faceting variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "AgeCat"),
                                       withSpinner(plotOutput("violin", width = "100%", height = "700px")),
                              ),
                              tabPanel("Pie Chart",
                                       h3("Pie Chart"),
                                       selectInput("pie_cat", "Select categorical Variable", 
                                                   choices = colnames(cat_vars), 
                                                   selected = "Operating System"),
                                       withSpinner(plotOutput("pie", width = "100%", height = "700px")),
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
    filtered_data(mobile_behavior[
      #subset data based on users slider input for numerical variables
      mobile_behavior[[input$num_var1]] >= input$range1[1] & mobile_behavior[[input$num_var1]] <= input$range1[2] &
        mobile_behavior[[input$num_var2]] >= input$range2[1] & mobile_behavior[[input$num_var2]] <= input$range2[2] &
        #let data be unsubsetted (all) or let it react to the users input for agecat, gender, and OS
        (input$AgeCat == "All" | mobile_behavior$AgeCat == input$AgeCat) &
        (input$Gender == "All" | mobile_behavior$Gender == input$Gender) &
        (input$OS == "All" | mobile_behavior$`Operating System` == input$OS),
    ])
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
  
    filtered_data() |> 
      summarise(across(where(is.numeric), list(
        Min = ~min(.),
        Max = ~max(.),
        Mean = ~mean(.),
        Median = ~median(.),
        SD = ~sd(.),
        IQR = ~IQR(.)), .names = "{.col}_{.fn}")) |> 
      pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_") |>
      slice(-c(1, 8, 9)) |> as.data.frame()
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
  
  
}
shinyApp(ui, server)