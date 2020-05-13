################################################
####
#### Minority Depository Institutions
#### Shiny App
#### PubH 7462
#### Cody R Tuttle
####
###############################################


### load packages

#install.packages("writexl")
#install.packages("shinythemes")
#install.packages("plotly")

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(tigris)
library(tidycensus)
library(htmlwidgets)
library(readxl)
library(writexl)
library(tidyr)
library(scales)
library(DT)
library(RColorBrewer)
library(plotly)

#### list of table columns for datatable output

table_cols <- list("Year" = "year", 
                   "Bank ID" = "cert_number", 
                   "Bank Name" = "name", 
                   "City" = "city", 
                   "State" = "state", 
                   "Date Established" = "est_date", 
                   "Community Bank?" = "com_bank", 
                   "Class" = "class", 
                   "Regulator" = "regulator", 
                   "MDI Type" = "min_status", 
                   "FDIC Region" = "fdic_reg", 
                   "Total Assets ($1000s)" = "tot_assets_thous")

### UI

ui <- navbarPage(theme = shinytheme("united"), strong("Minority-Owned Depository Institutions"),
    
    #### About page              
                 
    tabPanel("About", 
             titlePanel(strong("Minority-Owned Depository Institutions")),
             h3("About this Shiny App"),
             p("Every year the ", 
                strong("Federal Deposit Insurance Corporation (FDIC) "), 
                "produces a report about the minority-owned depository institutions (MDIs) they regulate. Along with the report, they keep extensive data on these banks and other institutions, which they make available to the public, starting from the year 2001. More information can be found on the ", 
               a("FDIC MDI website.", 
                 href = "https://www.fdic.gov/regulations/resources/minority/mdi.html"),
             "I use these data to create this app that explores the prominence and strength of these minority-owned banks over time and across the country. The vitality of these institutions has numerous benefits for minority communities, like stable banking and financial services, access to credit and loans, and avenues for economic and community development."), 
             p("This app has", 
               strong("three main components. "), 
               "The", 
               strong("first "),
               "shows trends over time of these banks from 2001 to 2019. Users can view trends of the number of institutions or the total assets of institutions of a particular minority type either for the country as a whole or for any state(s). Please note that if a state does not have any banks of the MDI type chosen, no line will appear on the graph. The ",
               strong("second "), 
               "component is a state map that shows the number of banks and the mean total assets for those banks of a particular type of MDI. Users can choose which year to view, and which measure to color the states by. For this view, only the states that have banks of the chosen type will be in color - all others are in grey. The",
               strong("third "),
               "component is an exportable table of the data at the bank level. Users can filter each of the measures (state, bank type, regulator, etc.) and sort on any column, as well as select which columns they'd like to appear."), 
             p("The MDI types, corresponding either to the race/ethnicity of the ownership of the bank or the community they service, in the data are as follows: "), 
             p("   - Asian or Pacific Islander American"), 
             p("   - Native American or Alaksan Native American"),
             p("   - Black or African American"),
             p("   - Hispanic American"),
             p("   - Minority Board and Serving African American Community"),
             p("   - Minority Board and Serving Hispanic American Community"),
             p("   - Multi-Racial American"),
             p("   - Minority Board and Serving Asian or Pacific Islander Americanb Community"),
             p("   - Minority Board and Serving Multi-Racial American Community"),
             
             br(), 
             
             p("Please note that the institutions in this application do not represent all minority-owned banks or credit unions in the country, only those certified and regulated by the FDIC. Other minority banks could be regulated by the Federal Reserve System."), 
             
             p("For any inquiries specific to the app, please contact Cody Tuttle, tuttl084@umn.edu."), 
             p("For any inquiries specific to the data or MDIs more generally, please consult the FDIC website in the link provided above.")
    
    ),
    
    ###### trends line graph page
    
    tabPanel("Trends Over Time",
             
             ### top panel for inputs
             
             fluidRow(
                 
                 column(2,
                     radioButtons("trends_measure", "Measure:",
                                  choices = list("Number of Institutions" = "n",
                                                 "Total Assets (in $1000s)" = "total_assets"), 
                                  selected = "n")
                 ),
                 
                 column(3,
                     sliderInput("year", "Select Years:",
                                 min = 2001,
                                 max = 2019,
                                 value = c(2001, 2019), 
                                 sep = "")
                 ), 
                 
                 column(3,
                     selectizeInput("state_select",
                                    "Select State(s):",
                                    choices = c("", unique(states@data$NAME)),
                                    multiple = TRUE,
                                    selected = "")
                 ), 
                 
                 column(4,
                     selectInput("trends_mdi_type", "Select MDI Type:", 
                                 choices = unique(mdi$min_status)), 
                     helpText("Warning: If chosen state(s) do(es) not appear in graph, 
                              it has no institutions for selected MDI type.")
                 )
                 
             ), 
             
             hr(), 
             
             #### output
             
             plotOutput("trends")
             
    ),
    
    ##### map output page
    
    tabPanel("Map",

             ### top panel for inputs
             
             fluidRow(
                 
                 column(4,
                    radioButtons("mdi_measure", "Measure:",
                                choices = list("Number of Institutions" = "n",
                                                "Mean Assets ($1000s)" = "mean_assets"), 
                                selected = "n")
                 ), 
                 
                 column(4,
                     selectInput("map_year", "Select year:", choices = unique(mdi$year), 
                                 selected = 2019)
                 ), 
                 
                 column(4,
                     selectInput("mdi_status", "Select MDI Type: ", choices = unique(mdi$min_status)), 
                     helpText("Note: Click on any filled state to display more information.")
                 )
                 
             ), 
             
             hr(), 
             
             ### map output
             
             leafletOutput("mdi_state_map")
             
    ), 
    
    #### data table output page
    
    tabPanel("Table",
             
             ### page with side column for input to select columns in table
             
             fluidRow(
                 
                 column(1, 
                     
                     checkboxGroupInput("table_vars", "Select Columns:", 
                                        choices = c("Year", "Bank ID", "Bank Name", "City", "State", 
                                                    "Date Established", "Community Bank?", "Class", 
                                                    "Regulator", "MDI Type", "FDIC Region", "Total Assets ($1000s)"), 
                                        selected = c("Year", "Bank ID", "Bank Name", "City", "State", 
                                                     "Date Established", "Community Bank?", "Class", 
                                                     "Regulator", "MDI Type", "FDIC Region", "Total Assets ($1000s)")), 
                     
                     downloadButton("download_table", "Export to CSV")
                     
                 ), 
                 
                 ### rest of row for table output
                 
                 column(10, offset = 1,
                     
                     DT::dataTableOutput("data_table")
                     
                 )
             )
    )
)






# Server
server <- function(input, output) {
    

    #### read in data 
    
    mdi <- read_excel("mdi_clean.xlsx")
    
    #### read in states shapefile from tigris package
    
    states <- states(cb = TRUE)
    
    ### base trends plot ouput
    
    output$trends <- renderPlot({
        
        ### state names and abbreviations table from base R for joining
        
        state_names <- tibble(
            state = state.abb, 
            state_name = state.name
        )
        
        ### join mdi data with state abbreviations for later geo-joining
        
        mdi_new <- left_join(mdi, state_names, by = "state")
        
        ### conditional statements for building output with customized features based on input measure
            # (wanted to avoid the conditionals, but just putting the input in the y aes arg doesn't allow for custom                    axis labels and formatting)
        
        if(input$trends_measure == "n") {
            
            mdi_new %>%
                filter(year >= input$year[1], year <= input$year[2], min_status == input$trends_mdi_type) %>%
                group_by(year) %>%
                summarise(total_assets = sum(tot_assets_thous), 
                          n = n()) %>%
                ggplot(aes(x = year, y = n, group = 1)) +
                geom_line(color = "blue") +
                theme_minimal() +
                scale_x_continuous(breaks = seq(1999, 2019, 2)) +
                labs(x = "", y = "Number of Institutions") + 
                theme(axis.text.x = element_text(face = "bold", size = 12), 
                      axis.text.y = element_text(face = "bold", size = 12), 
                      axis.title.y = element_text(face = "bold", size = 16)) +
                ggtitle(paste0("Number of Institutions for ", input$trends_mdi_type, " MDIs")) +
                theme(plot.title = element_text(face = "bold", size = 20))
            
            ### building new plot for assets measure input
            
        } else {
            
            mdi_new %>%
                filter(year >= input$year[1], year <= input$year[2], min_status == input$trends_mdi_type) %>%
                group_by(year) %>%
                summarise(total_assets = sum(tot_assets_thous), 
                          n = n()) %>%
                ggplot(aes(x = year, y = total_assets, group = 1)) +
                geom_line(color = "blue") +
                theme_minimal() +
                scale_y_continuous(labels = dollar) +
                scale_x_continuous(breaks = seq(1999, 2019, 2)) +
                labs(x = "", y ="Total Assets (in $1000s)") + 
                theme(axis.text.x = element_text(face = "bold", size = 12), 
                      axis.text.y = element_text(face = "bold", size = 12), 
                      axis.title.y = element_text(face = "bold", size = 16)) +
                ggtitle(paste0("Total Assets for ", input$trends_mdi_type, " MDIs")) +
                theme(plot.title = element_text(face = "bold", size = 20))
            
        }
        
        
    })
    
    #### build new output upon the selection of state input(s)
    
    observe({
        
        #### nested conditional logic to account for measure input (see reasoning above)
        
        if(!is.null(input$state_select)) {
            
            state_names <- tibble(
                state = state.abb, 
                state_name = state.name
            )
            
            mdi_new <- left_join(mdi, state_names, by = "state")
            
            trends <- mdi_new %>%
                filter(year >= input$year[1], year <= input$year[2], min_status == input$trends_mdi_type) %>%
                filter(state_name %in% input$state_select) %>%
                group_by(year, state) %>%
                summarise(total_assets = sum(tot_assets_thous), 
                          n = n()) 
            
            output$trends <- renderPlot({
                
                ### number of banks input
                
                if(input$trends_measure == "n") {
                    
                   trends %>%
                        ggplot(aes(x = year, y = n, color = state)) +
                        geom_line(size = 1) +
                        theme_minimal() +
                        scale_x_continuous(breaks = seq(1999, 2019, 2)) +
                        labs(x = "", y = "Number of Institutions") + 
                        theme(axis.text.x = element_text(face = "bold", size = 12), 
                              axis.text.y = element_text(face = "bold", size = 12), 
                              axis.title.y = element_text(face = "bold", size = 16)) +
                        ggtitle(paste0("Number of Institutions for ", input$trends_mdi_type, " MDIs")) +
                        theme(plot.title = element_text(face = "bold", size = 20))
                    
                    ### assets input
                    
                } else {
                    
                    trends %>%
                        ggplot(aes(x = year, y = total_assets, color = state)) +
                        geom_line(size = 1) +
                        theme_minimal() +
                        scale_y_continuous(labels = dollar) +
                        scale_x_continuous(breaks = seq(1999, 2019, 2)) +
                        labs(x = "", y = "Total Assets (in $1000s)") + 
                        theme(axis.text.x = element_text(face = "bold", size = 12), 
                              axis.text.y = element_text(face = "bold", size = 12), 
                              axis.title.y = element_text(face = "bold", size = 16)) +
                        ggtitle(paste0("Total Assets for ", input$trends_mdi_type, " MDIs")) +
                        theme(plot.title = element_text(face = "bold", size = 20))

                }
            })
        }
        
        #### have to be explicit about going back to original national level view when no states are selected in input
        
        else {
            
            output$trends <- renderPlot({
                
                state_names <- tibble(
                    state = state.abb, 
                    state_name = state.name
                )
                
                mdi_new <- left_join(mdi, state_names, by = "state")
                
                ### again, conditional logic to account for measure input
                
                if(input$trends_measure == "n") {
                    
                    mdi_new %>%
                        filter(year >= input$year[1], year <= input$year[2], min_status == input$trends_mdi_type) %>%
                        group_by(year) %>%
                        summarise(total_assets = sum(tot_assets_thous), 
                                  n = n()) %>%
                        ggplot(aes(x = year, y = n, group = 1)) +
                        geom_line(color = "blue") +
                        theme_minimal() +
                        scale_x_continuous(breaks = seq(1999, 2019, 2)) +
                        labs(x = "", y = "Number of Institutions") + 
                        theme(axis.text.x = element_text(face = "bold", size = 12), 
                              axis.text.y = element_text(face = "bold", size = 12), 
                              axis.title.y = element_text(face = "bold", size = 16)) +
                        ggtitle(paste0("Number of Institutions for ", input$trends_mdi_type, " MDIs")) +
                        theme(plot.title = element_text(face = "bold", size = 20))

                } else {
                    
                  mdi_new %>%
                        filter(year >= input$year[1], year <= input$year[2], min_status == input$trends_mdi_type) %>%
                        group_by(year) %>%
                        summarise(total_assets = sum(tot_assets_thous), 
                                  n = n()) %>%
                        ggplot(aes(x = year, y = total_assets, group = 1)) +
                        geom_line(color = "blue") +
                        theme_minimal() +
                        scale_y_continuous(labels = dollar) +
                        scale_x_continuous(breaks = seq(1999, 2019, 2)) +
                        labs(x = "", y ="Total Assets (in $1000s)") + 
                        theme(axis.text.x = element_text(face = "bold", size = 12), 
                              axis.text.y = element_text(face = "bold", size = 12), 
                              axis.title.y = element_text(face = "bold", size = 16)) +
                        ggtitle(paste0("Total Assets for ", input$trends_mdi_type, " MDIs")) +
                        theme(plot.title = element_text(face = "bold", size = 20))
                   
                }
            })
        }
    })
    
    ### start map output
    
    ## reactive data to feed into map
        
    map_feeder <- reactive({
        
        mdi_states <- mdi %>%
            filter(year == input$map_year, 
                   min_status == input$mdi_status) %>%
            group_by(state) %>%
            summarise(n = n(), 
                      mean_assets = mean(tot_assets_thous, na.rm = TRUE)) 
        
        geo_join(states, mdi_states, "STUSPS", "state")
        
    })
    
    ### default leaflet output
    
    output$mdi_state_map <- renderLeaflet({
            
            data <- map_feeder()   
            
            data <- subset(data, !is.na(n))
        
            pal <- colorNumeric("viridis", domain = data@data$n, reverse = T)
 
            label <- paste0("<strong>", data@data$NAME, 
                             "<strong><br />Number of Banks: ", data$n,
                             "<strong><br />Mean Total Assets ($1000s): ", dollar(data@data$mean_assets))
            
            data %>%
                leaflet() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                setView(-98.483330, 38.712046, zoom = 4) %>% 
                addPolygons(fillColor = ~pal(data@data$n),
                            weight = 1,
                            fillOpacity = 1,
                            popup = ~label) %>% 
                addLegend(pal = pal, 
                          values = data@data$n,
                          position = "bottomright", 
                          title = "Number of Banks",
                          group = "legend_1")
                
        })
    
    #### leaflet proxy based on measure input change
    
    observe({
        
        if(input$mdi_measure == "mean_assets") {
           
            data <- map_feeder()
            
            data <- subset(data, !is.na(mean_assets))
            
            pal2 <- colorNumeric("viridis", 
                                domain = data@data$mean_assets, reverse = T) 
            
            label2 <- paste0("<strong>", data@data$NAME, 
                            "<strong><br />Number of Banks: ", data@data$n,
                            "<strong><br />Mean Total Assets ($1000s): ", dollar(data@data$mean_assets))
            
            leafletProxy("mdi_state_map", data = data) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                setView(-98.483330, 38.712046, zoom = 4) %>% 
                clearShapes() %>%
                clearControls() %>%
                addPolygons(fillColor = ~pal2(mean_assets),
                            weight = 1,
                            fillOpacity = 1,
                            popup = ~label2) %>% 
                addLegend(pal = pal2, 
                          values = data@data$mean_assets,
                          position = "bottomright", 
                          title = "Mean Total Assets ($1000s)")
            
        }
        
    })
        
    ### leaflet proxy based on measure input change (have to be explicit to get it to change back)
    
    observe({
        
        if(input$mdi_measure == "n") {
            
            data <- map_feeder()
            
            data <- subset(data, !is.na(n))
            
            pal <- colorNumeric("viridis",
                                domain = data@data$n, reverse = T)
            
            label <- paste0("<strong>", data@data$NAME, 
                            "<strong><br />Number of Banks: ", data@data$n,
                            "<strong><br />Mean Total Assets ($1000s): ", dollar(data@data$mean_assets))
            
            leafletProxy("mdi_state_map", data = data) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                setView(-98.483330, 38.712046, zoom = 4) %>% 
                clearShapes() %>%
                clearControls() %>%
                addPolygons(fillColor = ~pal(n),
                            weight = 1,
                            fillOpacity = 1,
                            popup = ~label) %>% 
                addLegend(pal = pal, 
                          values = data@data$n,
                          position = "bottomright", 
                          title = "Number of Banks")
            
        }
        
    })
    
    ### reactive data table for data table output, rename columns to get column names to look good in output
    
    table <- reactive({
        
        mdi <- mdi %>%
            select(-min_status_alpha, -min_status_num) %>%
            rename("Year" = "year", 
                   "Bank ID" = "cert_number", 
                   "Bank Name" = "name", 
                   "City" = "city", 
                   "State" = "state", 
                   "Date Established" = "est_date", 
                   "Community Bank?" = "com_bank", 
                   "Class" = "class", 
                   "Regulator" = "regulator", 
                   "MDI Type" = "min_status", 
                   "FDIC Region" = "fdic_reg", 
                   "Total Assets ($1000s)" = "tot_assets_thous")

    })
    
    #### build data table output using reactive table from previous step
    
    output$data_table <- DT::renderDataTable(
        
        table()[ , input$table_vars], 
        filter = "top", 
        options = list(
            autoWidth = FALSE, 
            scrollX = TRUE)

        )
    
    ### download button
    
    output$download_table <- downloadHandler(
        
            filename = function() {
                paste(mdi, ".csv", sep = "")
            },
            content = function(file) {
                write.csv(table(), file)
            }
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
