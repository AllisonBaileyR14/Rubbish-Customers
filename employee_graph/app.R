#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tidyverse)
library(shiny)
library(shinydashboard)
library(dplyr)

# Generate rubbish palette

rubbish_palette <- c("#bb5731", "#21322a", "#829719", "#7a8f76", "#eee9eb")

# Use employee_count data from the working directory.

# call in every csv for each of the diffferent employee counts
count_5 <- read_csv("count_5.csv") %>%
    clean_names()
count_6 <- read_csv("count_6.csv") %>%
    clean_names()
count_7 <- read_csv("count_7.csv") %>%
    clean_names()
count_8 <- read_csv("count_8.csv") %>%
    clean_names()
count_9 <- read_csv("count_9.csv") %>%
    clean_names()
# Merge all csv's and name employee_waste

employee_bind <- rbind(count_5,count_6, count_7, count_8, count_9) %>%
    select("material_category", "material_tons_generated_sum_of_all_streams",
           "employee_count")

employee_waste <- write_csv(employee_bind, 'employee_bind.csv') %>%
    as.data.frame() %>%
    drop_na() %>%
    filter(material_category == 'Paper'| material_category == 'Plastic' |
           material_category == 'Glass'| material_category == 'Other Organic' |
               material_category == 'Metal')
   #sum(material_tons_generated_sum_of_all_streams)






# Creating the user interface
ui <- fluidPage(
    titlePanel("Annual Restaurant Waste"), # This is the title!
    sidebarLayout( # Adding a sidebar & main panel
        sidebarPanel("Restaurant Size",
                     sliderInput("count", h3("# of Employees"),
                                 min = 5, max = 9, value = 7)),
        mainPanel("Annual Restaurant Waste", # Adding things to the main panel
                  plotOutput(outputId = "employee_plot", height = 500)

        )
    )
)

# Building the server:
server <- function(input, output) {

    employee_select <- reactive({

        employee_waste %>%
            dplyr::filter(employee_count == input$count)

    })



    # Create a reactive plot, which depends on 'species' widget selection:
    output$employee_plot <- renderPlot({


        ggplot(data = employee_select(), aes(y = material_tons_generated_sum_of_all_streams,
                                             fill = material_category,
                                             x = material_category)) +
            geom_bar(stat = "identity", show.legend = TRUE) +
            theme_minimal() +
            scale_fill_manual("legend", values = c("Other Organic" = "#bb5731",
                                                   "Plastic" = "#21322a",
                                                   "Glass" = "#829719",
                                                   "Paper" = "#7a8f76",
                                                   "Metal" = "#bab0ac")) +
            labs(x = "Waste Type",
                 y = "Waste Generated (Tons)")




    })



}

shinyApp(ui = ui, server = server)
