# This Shiny Dashboard is designed to visually keep track of my climbing progress
# It is connected to a private GoogleSheets doc, where the data is stored

# Load Libraries
library(dplyr)
library(DT)
library(ggplot2)
library(googlesheets)
library(lubridate)
library(plotly)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyr)


## prepare the OAuth token and set up the target sheet:
##  - do this EXACTLY ONCE
# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "gs_app_token.rds")
# dirty30_app <- gs_title("The Dirty30 List")
# dirty30_app$sheet_key # XXXXXXXXXXXXXXXXXXXXX

## Load & Clean Data
googlesheets::gs_auth(token = "gs_app_token.rds")
sheet_key <- "XXXXXXXXXXXXXXXXXXXXX"
dirty30_app <- googlesheets::gs_key(sheet_key)

# Creat a function to load data already submitted
loadData <- function() {
    # Grab the Google Sheet
    sheet <- gs_key(sheet_key)
    # Read the data
    gs_read_csv(sheet)
}

## Begin App Build
sidebar <- dashboardSidebar(
    width = 350,
    sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("about")),
        menuItem("The List", tabName = "the_list", icon = icon("the_list")),
        menuItem("Climb Demographics", tabName = "climb_demos", icon = icon("climb_demos")),
        menuItem("Send Demographics", tabName = "send_demos", icon = icon("send_demos"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "about",
                h2("About this dashboard..."),
                h4("The intent of this dashboard is to track and visualize my climbing progress"),
                br(),
                h4("I turned 30 in December of 2018. I've been climbing for over 8 years and I've yet to send 5.12a+"), 
                br(),
                h4("This year, it's going to happen..."),
                h4("In preparation for sending my first 5.12a, I intend to send 30 of the most classic 5.11s at the New River Gorge"),
                h4("Thirty 5.11s in my 30th year"),
                br(),
                h4("I'm using a private GoogleSheets doc to populate the data behind this Shiny Dashboard"),
                h4("Feel free to follow along with my progress here, and follow some of my photo evidence on Instagram @ninthwheelstatus")
            ),
        tabItem(tabName = "the_list",
                h2("Listing of Climbs & their Attributes"),
                h4("ordered by most recent send date"),
                DT::dataTableOutput("data_table")
            ),
        tabItem(tabName = "climb_demos",
                h2("Demographics of List of Climbs"),
                fluidRow(
                    box(title = "Count of Climbs per 5.11x Grade", status = "primary", solidHeader = TRUE,
                        plotOutput("plot1", height = 400)),
                    box(title = "Count of Climbs by Area", status = "primary", solidHeader = TRUE,
                        plotOutput("plot2", height = 400))
                    )
                # fluidRow(
                #     box(title = "Count of Climbs by Area by Wall", status = "primary", solidHeader = TRUE,
                #         plotOutput("plot2", height = 400))
                # )
            ),
        tabItem(tabName = "send_demos",
                h2("Send Progress"),
                fluidRow(
                    box(title = "Grades by Send Status", status = "primary", solidHeader = TRUE,
                        plotOutput("plot4", height = 400)),
                    box(title = "Sends by Month", status = "primary", solidHeader = TRUE,
                        plotOutput("plot3", height = 400))
                ),
                # fluidRow(
                #     box(title = "Grades by Send Status", status = "primary", solidHeader = TRUE,
                #         plotOutput("plot4", height = 400))
                # ),
                fluidRow(
                    box(title = "Avg. Number of Attempts per Grade", status = "primary", solidHeader = TRUE,
                        plotOutput("plot5", height = 400))
                )
            )
        )
    )

ui <- dashboardPage(
    dashboardHeader(title = "The Dirty30 List of 11s at The New", titleWidth = 350),
    sidebar, body
)


server <- function(input, output) {
    
    # Load Data upon URL Load
    dat <- loadData() %>%
        mutate(SendDate = as.Date(`Send Date`, '%m/%d/%Y')) %>% select(-`Send Date`) %>%
        mutate(Month  = as.Date(paste(month(SendDate), 1, year(SendDate), sep = '-'), '%m-%d-%Y'),
               SendDay = weekdays(SendDate),
               Send_binary = ifelse(Send == 'Y', 1, 0))
    
    # Get data table for "The List" tab
    output$data_table <- DT::renderDataTable({
        DT::datatable(
            dat %>%
                select(Name, Location, Wall, Grade, Send, SendDate) %>%
                mutate(`Send Date` = as.Date(SendDate)) %>%
                rename(`Sent?` = Send) %>% select(-SendDate) %>%
                arrange(desc(`Send Date`)),
            options = list(pageLength = nrow(dat), scrollX = TRUE)
        )
    })
    
    ## Demographics Plots
    # Bar Plot of Grade Make-up
    output$plot1 <- renderPlot({
        dat %>%
            count(Grade) %>%
            ggplot(aes(x = factor(Grade), y = n, label = n))+
                geom_bar(stat = 'identity', fill = 'steelblue')+
                geom_text(vjust = 2, col = 'white', fontface = 'bold', size = 4)+
                scale_y_continuous(limits = c(0, 15), expand = c(0,0))+
                labs(x = 'Grade', y = '')+
                theme_bw()
    })
    
    # Bar Plot: Climbs by Area
    output$plot2 <- renderPlot({
        dat %>%
            count(Location) %>%
            group_by(Location) %>%
            ggplot(aes(x = reorder(factor(Location), n), y = n))+
                geom_bar(stat= 'identity', fill = 'steelblue')+
                geom_text(aes(label = n), hjust = 2, col = 'white', 
                          fontface = 'bold', size = 4)+
                scale_y_continuous(breaks = seq(0,10,1),
                                   labels = seq(0,10,1),
                                   limits= c(0,10),
                                   expand = c(0,0))+
                labs(x = '', y = '')+
                theme_bw()+ coord_flip()
    })
    
    ## Sends Progress Plots
    # Bar Plot: Count of sends by month
    output$plot3 <- renderPlot({
        dat %>%
            filter(!is.na(SendDate)) %>%
            count(Month, Send_binary) %>%
            ggplot(aes(x = Month, y = n, label = n))+
                geom_bar(stat= 'identity', fill = 'steelblue')+
                geom_text(vjust = 2, col = 'white', fontface = 'bold', size = 4)+
                scale_x_date(labels = date_format('%Y-%m'))+
                labs(x = 'Month', y = '')+
                theme_bw()
    })
    
    # Bar Plot: Count of Grades grouped by Send Status
    output$plot4 <- renderPlot({
        dat %>%
            count(Grade, Send) %>%
            group_by(Grade, Send) %>%
            ggplot(aes(x = as.factor(Grade), y = n, fill = Send, label = n))+
                geom_bar(stat = 'identity', position = position_dodge2(width=1, preserve = 'single', padding = 0))+
                geom_text(position = position_dodge2(width=0.9, preserve = 'single', padding = 0), 
                          vjust = -1, fontface = 'bold', size = 4)+
                scale_y_continuous('',
                                   breaks = seq(0,10,1),
                                   labels = seq(0,10,1),
                                   limits = c(0,10),
                                   expand = c(0,0))+
                labs(caption = "Note: to 'send' is to ascend the climb sans falling",
                     x = 'Grade', y = '')+
                theme_bw()
    })
    
    # Bar Plot: Count of attempts by Grade
    output$plot5 <- renderPlot({
        dat %>%
            filter(Attempts > 0) %>%
            group_by(Grade) %>%
            summarise(n = round(mean(Attempts), 2)) %>% ungroup() %>%
            ggplot(aes(x = factor(Grade), y = n, label = n))+
                geom_bar(stat = 'identity', fill = 'steelblue')+
                geom_text(vjust = 2, col = 'white', fontface = 'bold', size = 4)+
                labs(x = 'Grade', y = '')+
                theme_bw()
    })
    
}

shinyApp(ui, server)

