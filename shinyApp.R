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



ui <- fluidPage(
  titlePanel("Project 2 558"),
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
        ),
        tabPanel("Data Explore",)
                            
                   )
                 ),
        )
server <- function(input, output, session) {
}
shinyApp(ui, server)