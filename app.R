#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(highcharter) 
library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(wesanderson)

# load data
vis1 <- read_csv("vis1.csv")
line_phy <- read_csv("line_phy.csv")
line_online <- read_csv("line_online.csv")
vis1_merge <- read_csv("vis1_merge.csv")
vis3 <- read_csv("vis3.csv")
# name the column same for easy switch dataset
names(line_online)[names(line_online)=='LearnCodeOnline'] <- 'LearnCode'



# Define UI for application that draws a histogram
ui <- fluidPage(
  # dark theme
  theme = bslib::bs_theme(bootswatch = "darkly"),
  # ---------header & css---------------------------------------------
  headerPanel(h1("Another Aspect of Stack Overflow Annual Survey 2022"),
              tags$head(tags$style("h1{color:#F2F4F0; font-size:60px; font-weight:bold; fontFamily:Noto Sans Warang Citi;}"),
              tags$head(tags$style("h3{color:#F2F4F0; font-size:45px; font-weight:bold; fontFamily:Noto Sans Warang Citi;}")),
              tags$head(tags$style("h4{color:#B5B8C3; font-size:20px; font-weight:bold; fontFamily:Noto Sans Warang Citi;}")),
              tags$head(tags$style("h6{color:black; font-size:20px; font-weight:bold; fontFamily:Noto Sans Warang Citi;}"))
              )),
  # ---------intro---------------------------------------------
  fluidRow(
           column(9,h4(
  "Everyone who codes must have heard about Stack Overflow. It is an online platform built by a bunch of coding questions and answers, a good friend to all coders when it comes to debugging. Stack overflow has been conducting this developer survey for years, aiming to improve its services to the users. Even though the official has provided its analysis online, this project is trying to look at the data from another aspect. Have you ever wondered who those contributors are? How much they make? How do they learn to code? The project will focus on Stack Overflow Developers Survey 2022. Let's see what we found.
"))),
  # ---------section1--------------------------------------------
  # narrative
  fluidRow(column(1),
           column(7,
           h3("Who Are These Respondents?"),
           h4("Have a look at where they are!"),
           h4("And what are the differences of education, gender, age, experience distribution between each country?")),
           column(4,
           h3("How Do Background Affect Salary in General?"),
           p("**average salary worldwide"))
           ),
 # map, button and bar chart
  fluidRow(
            column(8, 
                   align="center",
                   leafletOutput("map",
                                 height = "50vh", width = "85%"),
                   h5("click circle in the map to see background distribution in each country")),
            column(4, radioButtons("var1", 
                                   h5("See how background affects salary:"),
                                   c("Education" = "edu",
                                     "Age" = "age",
                                     "Gender" = "sex",
                                     "Years of Experience" = "pro")),
                      plotOutput("bar", height = "35vh", width = "90%"),
                      uiOutput("tab"))
  ), 
 # another css
  fluidRow(
           column(2),
           column(10, h5(""),
                  tags$head(tags$style("p{color:#F2F4F0; font-size:15px; font-weight:bold;fontFamily:Noto Sans Warang Citi;}"),
                  tags$head(tags$style("h5{color:#FAE16A; font-size:18px; font-weight:bold; fontFamily:Noto Sans Warang Citi;}")))
  ),
  # donut title
  fluidRow(
            column(1),
            column(10, align="center",
                   verbatimTextOutput("click_text"),
                   tags$head(tags$style("#click_text{color:white; font-size:30px; font-weight:bold;overflow-y:scroll; max-height: 100px; background: transparent;font-family:Noto Sans;}")))
  ),
  # donuts
  fluidRow(
            column(3, highchartOutput("don1", height = "40vh", width = "90%")),
            column(3, highchartOutput("don2", height = "37vh", width = "90%")),
            column(3, highchartOutput("don3", height = "37vh", width = "90%")),
            column(3, highchartOutput("don4", height = "40vh", width = "90%")),
           ), # end of section 1
  # ---------section2--------------------------------------------
  fluidRow( br(),
            br(),
            br()
           ),
  # line chart and button
  fluidRow(
            column(1),
            column(8, highchartOutput("lines", height = "60vh")),
            column(3,
                   br(),
                   br(),
                   br(),
                   radioButtons("var2", h5("Check how developers learn to code"),
                                   c("In general" = "phy",
                                     "Focus on Online material" = "online")))
    
           ), # row 2 end
  # ---------section3--------------------------------------------
  fluidRow(br(),
           br(),
           br()),
  # treemap
  fluidRow(
    column(1),
    column(10, align="center",
            highchartOutput("tree", height = "80vh",width = "100%"),
            sliderInput(
              "range",
              h5("Select annual salary range:"),
              min = 0,
              max = 500,
              value = c(0,100),
              width = "30%",
              pre = "$", 
              post = "k",
              step = 100,
              sep = ",",
              animate =animationOptions(interval = 2000, loop = FALSE))),
    column(1)
  ), # row 3 end
  fluidRow(br(),
           br(),
           br(),
           br())

)) #fluidPage end

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # -----link---------------------------------------------------------
  url <- a("Kaggle", href="https://www.kaggle.com/datasets/dheemanthbhat/stack-overflow-annual-developer-survey-2022")
  output$tab <- renderUI({tagList("Data source:", url)})
  #-------map---------------------------------------------------------
  output$map <- renderLeaflet({
    # get data for map
    map_data <- vis1 %>% group_by(Country.y,x,y) %>% count
    leaflet(map_data) %>% 
      addTiles()  %>% 
      setView( lat=25, lng=60 , zoom=2) %>%
      setMaxBounds( lng1 = 90
                    , lat1 = 180
                    , lng2 = -90
                    , lat2 = -180 )%>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      addCircleMarkers(
        lng = map_data$x,
        lat = map_data$y,
        # circle size
        radius = sqrt(map_data$n)+3,
        color = "#C22F41",
        popup = paste("<h6>Country: ",
                      map_data$Country.y,
                      "<h6>",
                      "Number of respondants: ",
                      map_data$n,
                      "<br>")
      )
  })

  
#------- observe 1 click map---------------------------------------------------
  observe({
    # get info of user click
    click <- input$map_marker_click
    if(is.null(click))
      return()
    # get country lat and lon
    clng <- click$lng
    clat <- click$lat
    # filter data based on click
    address <- vis1 %>% filter(y == clat) %>% filter(x == clng) %>% select(Country.y) %>% head(1)
    cname <- paste(address[1])
    # -----cuntry name box--------------------------------------------------------------
    output$click_text <- renderText({
      paste("Background distribution in ",cname)
    }) # rendertext end
    #-------click react donut 1---------------------------------------------------
    output$don1 <- renderHighchart({
      # prepare data for edu level
      donut_edu <- vis1 %>% filter(y == click$lat) %>% filter(x == click$lng) %>% 
        select(EdLevel,ConvertedCompYearly) %>% 
        group_by(EdLevel) %>% count() %>% ungroup()
      # calculate %
      donut_edu <- donut_edu %>% mutate(fraction = n / sum(n))
      # plot
      donut_edu %>% 
        hchart("pie", 
               hcaes(x = EdLevel, y = fraction),
               innerSize = 120, dataLabels = list(enabled=FALSE), showInLegend= TRUE) %>%
        hc_add_theme(hc_theme_flat()) %>%
        hc_chart(backgroundColor = "transparent") %>%
        hc_title(text = "Education level",
                 style = list(fontSize = "20px", color = "white", fontWeight = "bold", fontFamily = "Noto Sans"),
                 align = "center")%>%
        hc_legend(title = list(text = "Click to add or remove category", 
                  style = list(fontSize = "15px", color = "#FAE16A")),
                  itemHiddenStyle = list(color = "black"),
                  itemHoverStyle = list(color = "white",fontSize = "17px"),
                  alignColumns = FALSE) %>%
        hc_tooltip(
          useHTML = TRUE,
          style = list(fontSize = "15px"),
          formatter = JS(
            "
      function(){
        outHTML = '<h6>' + this.point.EdLevel + '</h6> <br>' +Math.round((this.point.fraction + Number.EPSILON)*100) +'%'
        return(outHTML)
      }

      "
          ),
          shape = "callout",
          borderWidth = 5) 
      
    })
    #-------click react donut 2---------------------------------------------------
    output$don2 <- renderHighchart({
      # prepare data for gender level
      donut_Gender <- vis1 %>% filter(y == click$lat) %>% filter(x == click$lng) %>% 
        select(Gender,ConvertedCompYearly) %>% 
        group_by(Gender) %>% count() %>% ungroup()
      
      donut_Gender <- donut_Gender %>% mutate(fraction = n / sum(n))
      # plot
      donut_Gender %>% 
        hchart("pie", 
               hcaes(x = Gender, y = fraction), 
               innerSize = 120, dataLabels = list(enabled=FALSE), showInLegend= TRUE) %>%
        hc_add_theme(hc_theme_ft())%>%
        hc_chart(backgroundColor = "transparent")%>% 
        hc_title(text = "Gender",
                 style = list(fontSize = "25px", color = "white", fontWeight = "bold", fontFamily = "Noto Sans"),
                 align = "center")%>%
        hc_legend(
                  itemHiddenStyle = list(color = "black"),
                  itemHoverStyle = list(color = "white",fontSize = "17px"),
                  alignColumns = FALSE) %>%
        hc_tooltip(
          useHTML = TRUE,
          style = list(fontSize = "15px"),
          formatter = JS(
            "
      function(){
        outHTML = '<h6>' + this.point.Gender + '</h6> <br>' +Math.round((this.point.fraction + Number.EPSILON)*100) +'%'
        return(outHTML)
      }

      "
          ),
          shape = "callout",
          borderWidth = 5) 
    })
    #-------click react donut 3---------------------------------------------------
    output$don3 <- renderHighchart({
      # prepare data for pro years
      donut_pro_range <- vis1 %>% filter(y == click$lat) %>% filter(x == click$lng) %>% 
        select(pro_range,ConvertedCompYearly) %>% 
        group_by(pro_range) %>% count() %>% ungroup()
      
      donut_pro_range <- donut_pro_range %>% mutate(fraction = n / sum(n))
      # plot
      donut_pro_range %>% 
        hchart("pie", 
               hcaes(x = pro_range, y = fraction), 
               innerSize = 120, dataLabels = list(enabled=FALSE), showInLegend= TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_chart(backgroundColor = "transparent") %>%
        hc_title(text = "Years Of Pro Experience",
                 style = list(fontSize = "20px", color = "white", fontWeight = "bold", fontFamily = "Noto Sans"),
                 align = "center")%>%
        hc_legend(
          itemHiddenStyle = list(color = "black"),
          itemHoverStyle = list(color = "white",fontSize = "17px"),
          alignColumns = FALSE) %>%
        hc_tooltip(
          useHTML = TRUE,
          style = list(fontSize = "15px"),
          formatter = JS(
            "
      function(){
        outHTML = '<h6>' + this.point.pro_range + '</h6> <br>' +Math.round((this.point.fraction + Number.EPSILON)*100) +'%'
        return(outHTML)
      }

      "
          ),
          shape = "callout",
          borderWidth = 5) 
    })
    #-------click react donut 4---------------------------------------------------
    output$don4 <- renderHighchart({
      # prepare data for age
      donut_Age <- vis1 %>% filter(y == click$lat) %>% filter(x == click$lng) %>% 
        select(Age,ConvertedCompYearly) %>% 
        group_by(Age) %>% count() %>% ungroup()
      
      donut_Age <- donut_Age %>% mutate(fraction = n / sum(n))
      # plot 
      donut_Age %>% 
        hchart("pie", 
               hcaes(x = Age, y = fraction), 
               innerSize = 120, dataLabels = list(enabled=FALSE), showInLegend= TRUE) %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_chart(backgroundColor = "transparent") %>%
        hc_title(text = "Age",
                 style = list(fontSize = "25px", color = "white", fontWeight = "bold", fontFamily = "Noto Sans"),
                 align ="center")%>%
        hc_legend(
          itemHiddenStyle = list(color = "black"),
          itemHoverStyle = list(color = "white",fontSize = "17px"),
          alignColumns = FALSE) %>%
        hc_tooltip(
          useHTML = TRUE,
          style = list(fontSize = "15px"),
          formatter = JS(
            "
      function(){
        outHTML = '<h6>' + this.point.Age + '</h6> <br>' +Math.round((this.point.fraction + Number.EPSILON)*100) +'%'
        return(outHTML)
      }

      "
          ),
          shape = "callout",
          borderWidth = 5)
       
    })
    
  }) # observe 1 end
  #-------bar---------------------------------------------------------
observe({
    # get input
    bkg <- input$var1
    # plot 
  output$bar <- renderPlot({
    bar <- vis1_merge %>% filter(type==bkg)

        ggplot(bar,
             aes(x = reorder(background,salary), y= salary,
                 fill= background)) +
        geom_bar(stat="identity") +
        # add bar label salary
        geom_text(aes(label = paste0("$",salary,"k")),
                  stat = "identity",
                  color = "white",
                  vjust = -0.5,
                  fontface = "bold",
                  size = 4) +
          # bar label category
          geom_text(aes(label = background),
                    stat = "identity",
                    color = "black",
                    position = position_stack(vjust = 0.5),
                    fontface = "bold",
                    size = 6,
                    angle = 270) +
        theme_void()+
        scale_fill_manual(values = wes_palette("Darjeeling1", 9, type = "continuous"))+
        theme(legend.position="none",
              panel.background = element_blank(),
              plot.background = element_blank())
    
  }, bg = "transparent")
})

  #-------line---------------------------------------------------------
  observe({
    learn <- input$var2  
  
  output$lines <- renderHighchart({
    # get input to choose dataset
    if (learn == "phy"){
      df_learn <- line_phy
    } else {
      df_learn <- line_online
    }
    # add level 
    df_learn$Age <- factor(df_learn$Age, levels=c("65 years or older", "55-64 years old",
                                                  "45-54 years old", "35-44 years old",
                                                  "25-34 years old", "18-24 years old",
                                                  "Under 18 years old", "Prefer not to say"))
    # plot
    df_learn  %>% filter(Age !="Prefer not to say") %>% filter(Age !="Under 18 years old")%>% 
      hchart('line', hcaes(x = Age, y = prop, group = LearnCode)) %>% 
      hc_xAxis(title = list(text="Age Group", 
                            style = list(fontSize = "18px",color = "white")),
                            labels = list(style = list(fontSize = "13px",color = "white")))%>%
      hc_yAxis(title = list(text="Percentage %", 
                            style = list(fontSize = "18px",color = "white")),
               labels = list(format = "{value}%", 
                             style = list(fontSize = "13px",color = "white"))) %>% 
      hc_title(text = "How Developers Learn to Code",
              align = "left",
              style = list(fontSize = "40px", color = "white", fontWeight = "bold"))%>%
      hc_subtitle(text = "See the trend by comparing Age Group",
               align = "left",
               style = list(fontSize = "30px", color = "grey", fontWeight = "bold")) %>%
      hc_legend(title = list(text = "Click to add or remove category", 
                             style = list(fontSize = "17px", color = "#FAE16A")),
                itemHiddenStyle = list(color = "black"),
                itemHoverStyle = list(color = "white", fontSize = "17px")) %>%
      hc_tooltip(
        useHTML = TRUE,
        style = list(fontSize = "13px"),
        formatter = JS(
          "
      function(){
        outHTML = 'by <h6>' + this.point.LearnCode + '</h6> <br>' +Math.round(this.point.prop) +'%'+' in ' + this.point.Age + ' group'
        return(outHTML)
      }

      "
        ),
        shape = "callout",
        borderWidth = 0  
        
      )
  })
  })

  #-------tree map-----------------------------------------------------
  observe({
    # get slider input
    start <- input$range[1]
    end <- input$range[2]
    
    output$tree <- renderHighchart({
    # prepare data
    tree <- vis3 %>% filter(ConvertedCompYearly < 5000000 & ConvertedCompYearly > 50000) %>%
      filter(ConvertedCompYearly >= start*1000 & ConvertedCompYearly < end*1000) %>%
      group_by(type, tech) %>% summarise(salary=mean(ConvertedCompYearly),
                                                        total= n())
    # plot
    tree %>%
      hchart("treemap", hcaes(x = tech, value = total, color = salary))  %>% 
      hc_title(text = "Most Used Technology vs Its Average Salary",
               align = "left",
               style = list(fontSize = "40px", color = "white", fontWeight = "bold")) %>%
      hc_legend(title = list(text = "Annual Salary (USD)", style = list(fontSize = "14px", color = "white")),
                labels = list(style = list(fontSize = "13px", color = "white")),
                itemWidth = "200px",
                verticalAlign = "top",
                align = "left") %>%
      hc_colorAxis(stops = color_stops(colors = wes_palette("Zissou1", 10, type = "continuous"))) %>%
      hc_tooltip(
        useHTML = TRUE,
        style = list(fontSize = "13px"),
        formatter = JS(
          "
      function(){
        outHTML = '<h6>' + this.point.tech + '</h6> <br> Number of developers: ' + this.total + '<br> Average salary: $' + Math.round(this.point.salary) 
        return(outHTML)
      }

      "
        ),
        shape = "callout"
      )
  })
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
