library(shiny)
library(tidyverse)
library(corrplot)

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
  titlePanel("Project 2 558"),
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
                 downloadButton("download_data", "Download Data")
        ),
        tabPanel("Data Explore",)
                            
                   )
                 ),
        )
)
server <- function(input, output, session) {
  
  
  #allow user to download the data table in the download tab
  output$download_data <- downloadHandler(
    filename = function() {
      paste('filtered_data_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE) 
    }
  )
  
  
}
shinyApp(ui, server)