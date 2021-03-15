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
                                img(src='rubbish_values.png', width="1200", height="650", align = "center"),
                                hr(),

                                  #a("Global Waste Count",
                                    #href = "https://www.theworldcounts.com/challenges/planet-earth/state-of-the-planet/world-waste-facts")),

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

                                #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/1CSm4GG2VrU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),

                                br(),

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
                                img(src='rubbish_model.png', width="1000", height="450", align = "center"),
                                br(),
                                hr(),
                                h2("The Waste Crisis"),
                                p("Rubbish  takes food waste
                                from small food retailers and dog waste--to produce renewable energy and high-quality compost.
                                Our approach's by-products help lower our operation cost, which we can then pass on to our small
                                business customers. This process also provides small businesses with a holistic narrative connecting
                                their food business back to the land that their food comes from.   Anaerobic digestion is the technology
                                that drives our innovative waste management approach. During anaerobic digestion, microbes need macronutrients
                                for energy and micronutrients for essential minerals. Food waste can provide macronutrients but is limited by
                                micronutrients resulting in suboptimal energy yield. Animal waste typically acts as a micronutrient source in
                                co-digestion, and based on our small business customer's desires for community engagement; we decided that
                                dog waste could act as a potential micronutrient source. Incorporating dog waste into our key resources can
                                maximize biogas potential while solving a public and environmental health issue caused by mismanaged dog waste
                                streams.", style = "font-size:16px")
                                ),

                            tabItem(
                                tabName = "customers",
                                h1("Restaurant Waste Index"),
                                br(),
                                hr(),
                                br(),
                                br(),
                                fluidRow(
                                    infoBox("Food Retailers", "33%", "Organic Waste to Landfills", icon = icon("globe-asia"), color = "black"),
                                    infoBox("Landfill Capacity", "100%", "# of Landfills full by 2045", icon = icon("trash"), color = "blue"),
                                    infoBox("Rubbish", "3400 Tons", "Annual diversion per neighborhood", icon = icon("fire"), color = "red")),
                                br(),
                                hr(),
                                br(),
                                p("Use the slider scale to select the number of employees of your restaurant. The outputs will show an interactive graph of the total
                        tons of waste generated by type annually.(Data provided by Calrecycle Business Calculator, 2021)", style = "font-size:15px"),
                                br(),
                                fluidRow(
                                    shinydashboard::box(title = "Restaurant Waste Graph",
                                                        sliderInput("count", h3("# of Employees"),
                                                                    min = 5, max = 25, value = 15)),
                                    shinydashboard::box(plotOutput(outputId = "employee_plot")),
                                    br(),
                                    hr(),
                                    br(),
                                    hr(),
                                    br(),
                                    p("Rubbish's value proposition is generated due to our approach to customer relationships and our key activities.
                                The value propositions for our small business customers include opportunities for community engagement,
                                a story for their food waste management,cost savings as a result of organic waste diversion, and
                                compliance with existing food waste diversion mandates.  We also offer community engagement, high-quality compost, and affordability for
                                  our landowner customers due to cost savings generated by our energy production.", style = "font-size:15px")





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
