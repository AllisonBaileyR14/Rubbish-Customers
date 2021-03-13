#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# attach packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(leaflet)
library(spData)
library(janitor)
library(sf)
library(shinythemes)
library(leaflet.extras)
library(RColorBrewer)
library(mapproj)
library(DT)
library(tmap)
library(mapview)
library(tmap)
library(maps)
library(mapdata)
library(rworldmap)
library(ggmap)
library(stringr)
library(readr)
library(fresh)


# Create a rubbish theme

# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#bb5731"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#21322a",
    dark_hover_bg = "#21322a",
    dark_color = "#ffffff"
  ),
  adminlte_global(
    content_bg = "#eee9eb",
    box_bg = "#CCCCCC",
    info_box_bg = "#eee9eb"
  )
)

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
count_10 <- read_csv("count_10.csv") %>%
  clean_names()
count_11 <- read_csv("count_11.csv") %>%
  clean_names()
count_12 <- read_csv("count_12.csv") %>%
  clean_names()
count_13 <- read_csv("count_13.csv") %>%
  clean_names()
count_14 <- read_csv("count_14.csv") %>%
  clean_names()
count_15 <- read_csv("count_15.csv") %>%
  clean_names()
count_16 <- read_csv("count_16.csv") %>%
  clean_names()
count_17 <- read_csv("count_17.csv") %>%
  clean_names()
count_18 <- read_csv("count_18.csv") %>%
  clean_names()
count_19 <- read_csv("count_19.csv") %>%
  clean_names()
count_20 <- read_csv("count_20.csv") %>%
  clean_names()
count_21 <- read_csv("count_21.csv") %>%
  clean_names()
count_22 <- read_csv("count_22.csv") %>%
  clean_names()
count_23 <- read_csv("count_23.csv") %>%
  clean_names()
count_24 <- read_csv("count_24.csv") %>%
  clean_names()
count_25 <- read_csv("count_25.csv") %>%
  clean_names()
# Merge all csv's and name employee_waste

employee_bind <- rbind(count_5,count_6, count_7, count_8, count_9, count_10, count_11,
                       count_12, count_13, count_14, count_15, count_16, count_17, count_18,
                        count_19, count_20, count_21, count_22, count_23, count_24, count_25) %>%
    select("material_category", "material_tons_generated_sum_of_all_streams",
           "employee_count")

employee_waste <- write_csv(employee_bind, 'employee_bind.csv') %>%
    as.data.frame() %>%
    drop_na() %>%
    filter(material_category == 'Paper'| material_category == 'Plastic' |
               material_category == 'Glass'| material_category == 'Other Organic' |
               material_category == 'Metal')
#sum(material_tons_generated_sum_of_all_streams)

#########################3


# Define UI for application that draws a boxplot.

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "rubbish"),
                    dashboardSidebar(
                        sidebarMenu(id = "menu",
                                    menuItem("Home Page",
                                             tabName = "rubbish",
                                             icon = icon("forumbee")),
                                    menuItem("About Us",
                                             tabName = "about_us",
                                             icon = icon("drupal")),
                                    menuItem("Customers",
                                             tabName = "customers",
                                             icon = icon("jedi"))
                                   # menuItem("Global Waste Index",
                                            # tabName = "waste_index",
                                             #icon = icon("pastafarianism")),
                                   # menuItem("U.S. Landfills Status",
                                             #tabName = "landfill_stat",
                                            # icon = icon("earlybirds")))
                    )),
                    dashboardBody(
                      use_theme(mytheme),
                        fluidPage(theme = "lux.css"),
                        tabItems(
                            tabItem(
                                tabName = "rubbish",
                                h1("Welcome to Rubbish"),
                                br(),
                                img(src='rubbish_values.png', width="1200", height="650", align = "right"),
                                hr(),

                                  #a("Global Waste Count",
                                    #href = "https://www.theworldcounts.com/challenges/planet-earth/state-of-the-planet/world-waste-facts")),
                                hr(),
                                br(),
                                br(),
                                br(),
                                h2("Our Mission:"),
                                p("Our mission is to create small businesses' opportunities to bridge the gap between their environmental
                                  values and their actions. We do this by using our economically-friendly organic
                                  waste diversion processes at the neighborhood level. This process reduces landfill
                                  greenhouse gas emissions, provides a renewable replacement for fossil fuel-derived
                                  natural gas, and creates high-quality compost for agricultural applications.", style = "font-size:16px"),
                                br(),
                                br(),
                                br(),
                                br(),
                                hr(),
                                #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/1CSm4GG2VrU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),

                                br(),
                                hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br()),


                            tabItem(
                                tabName = "about_us",
                                h1("How We Work"),
                                br(),
                                hr(),
                                br(),
                                HTML('<p><img width="560 height="315" src="rubbish_model.png"/></p>'),
                                br(),
                                hr(),
                                h2("The Waste Crisis"),
                                p("Our current food systems are wasteful and disconnected. Nowhere is that better illustrated than
                                in organic waste disposal practices. The United States is at risk of reaching
                                landfill capacity by 2045; states like California could reach capacity as
                                much as a decade sooner (SWEEP, 2021). Food waste is a significant
                                contributor to this problem. Although federal and state governments
                                have made efforts to divert organic waste from landfills, 33% of
                                organic waste generated by the food retail sector still goes to the
                                landfill (EPA and Office of Resource Conservation and Recovery 2020).
                                Organic waste diversion can be specifically challenging for small businesses.
                                  Small businesses face more challenges than larger businesses regarding organic
                                  waste reduction due to slim margins and lack of infrastructure. This is an important
                                  segment to address since approximately 70% of restaurants have less than 20 employees
                                  (US Census Bureau . n.d.). These small businesses must begin to manage their organic
                                  waste or risk regulatory non-compliance (AB1826). However, to do this, small businesses
                                  want cost-effective and operationally convenient options, so environmental action is less
                                  of a burden. ", style = "font-size:16px"),
                                p("In general, lower-income countries rely on open dumping. Three geographical
                          regions are known to openly dump more than half of their waste: the Middle East
                          and North Africa, Sub-Saharan Africa, and South Asia. This is exacerbated by the
                          volume of solid waste entering these areas. Research has shown that 93 percent of
                          global waste is dumped in low-income countries, while only 2 percent is dumped in
                          high-income countries. Upper-middle-income countries have the highest percentage of
                          waste in landfills, at 54 percent. This rate decreases to 39 percent in high-income
                          countries, with 36 percent of this waste diverted to recycling and composting.", style = "font-size:15px"),
                                p("As you explore this app, please take note that regardless of the waste-management
                          practices implemented within these countries, solid waste will likely remain at these
                          sites for thousands of years. Consider the sheer volume of waste that has been produced
                          globally, and that this waste will not simply “go away”. Then ask yourself, is there
                          one single-use item that I use daily that I can replace with a long-term use alternative?", style = "font-size:15px")),

                            tabItem(
                                tabName = "customers",
                                h1("Restaurant Waste Index"),
                                br(),
                                hr(),
                                br(),
                                br(),
                                fluidRow(
                                    infoBox("Global Waste", "2.01 billion tons", "Annual Global Waste", icon = icon("globe-asia"), color = "black"),
                                    infoBox("Individual Waste", "10lbs", "Individual Waste per Day", icon = icon("trash"), color = "blue"),
                                    infoBox("Waste MGMT.", "33%", "Environmentally Safe Management", icon = icon("fire"), color = "red")),
                                br(),
                                hr(),
                                br(),
                                p("A check box for different waste products (compost, landfill, glass recycling, paper recycling, metal recycling, etc.),
                        and a select box for the user's country and/or state of choice. The outputs will show an interactive graph of the total
                        volume of selected waste products by that state/country in comparison with other countries/states that the user could
                        specify (or none) compared to to the top global producers of those waste products.", style = "font-size:15px"),
                                br(),
                                fluidRow(
                                    shinydashboard::box(title = "Restaurant Waste Graph",
                                                        sliderInput("count", h3("# of Employees"),
                                                                    min = 5, max = 25, value = 15)),
                                    shinydashboard::box(plotOutput(outputId = "employee_plot"))
                                    #column(12, div(dataTableOutput("country_table"))))),


                        )))))






server <- function(input, output) {

    # Allison's Graph and Table
    ###################################

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
