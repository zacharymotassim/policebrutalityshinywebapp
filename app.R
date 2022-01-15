# Zachary Motassim
# https://www.propublica.org/datastore/dataset/civilian-complaints-against-new-york-city-police-officers
library(shiny)
library(tidyverse) 
library(ggplot2)
library(RColorBrewer)
data <- read_csv('allegationscleaned.csv')
data
data1 <- data %>% count(allegation) %>% arrange(desc(n)) %>% head(5)
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel( 
            numericInput('num1', 'Customize Font Size!' , value = 12),
            selectInput('size', 'Change Plot Height!', choices = c("big"="600px", "small"="300px")),
            selectInput('width', 'Change Plot Width!', choices = c("big"="100%", "small"="70%")),
            selectInput('Color' , 'Change color palette!', choices = c('PiYG','PRGn', 'Spectral', 'Dark2', 'Pastel2', 'OrRd')),
            downloadButton('downloadPlot','Download Plot!')
        ),
        mainPanel(
            tabsetPanel(
                type = 'pills' ,
                tabPanel('Visualization 1' , uiOutput('Visualization1')),
                tabPanel('Visualization 2' , uiOutput('Visualization2')),
                tabPanel('Visualization 3' , uiOutput('Visualization3')),
                tabPanel('Visualization 4' , uiOutput('Visualization4')),
                tabPanel('Visualization 5' , uiOutput('Visualization5'))
            )
        )
    )
)
server <- function(input, output){
    output$Visualization1 <- renderUI({
        plotOutput('Plot1', height = input$size, width = input$width)
    }
    )
    output$Plot1 <-renderPlot({
        #hist( data$rank_incident)
        data %>% 
            ggplot(aes(x=rank_incident, fill = ..count..)) + 
        geom_bar() +
            scale_fill_distiller( palette = input$Color ) +
            theme(axis.text.x = element_text(size = input$num1)) +
            labs(x="Rank", y="Count",title="Ranks With Highest Complaint Rates") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    output$Visualization2 <- renderUI({
        plotOutput('Plot2', height = input$size, width = input$width)
    }
    )
    output$Plot2 <-renderPlot({
        #hist( data$rank_incident)
        data %>% 
            count( mos_ethnicity , complainant_ethnicity ) %>%
            ggplot( aes(x=mos_ethnicity, fill = n , y = n ) ) + 
            geom_bar( stat = 'identity' ) +
            facet_wrap( complainant_ethnicity ~. ) +
            scale_fill_distiller( palette = input$Color ) +
            theme(axis.text.x = element_text(size = input$num1)) +
            labs(x="Race", y="Count",title="Police Complaints by Complaintant and officer Race", fill = "Race count") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    output$Visualization3 <- renderUI({
        plotOutput('Plot3', height = input$size , width = input$width)
    }
    )
    output$Plot3 <-renderPlot({
        #hist( data$rank_incident)
        data %>% 
            count( mos_gender , complainant_gender ) %>%
            ggplot( aes(x=mos_gender, fill = n , y = n ) ) + 
            geom_bar( stat = 'identity' ) +
            facet_wrap( complainant_gender ~. ) +
            scale_fill_distiller( palette = input$Color ) + # 2 dimensional 
            theme(axis.text.x = element_text(size = input$num1))+
            labs(x="Gender", y="Count",title="Police Complaints by Complaintant and officer Gender", fill = "gender count") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    output$Visualization4 <- renderUI({
        plotOutput('Plot4', height = input$size, width = input$width)
    }
    )
    output$Plot4 <- renderPlot({
        data %>% 
            count(year_received) %>%
            ggplot(aes(x = year_received , y = n , color = n) )+
            geom_point()+
            geom_line()+
            scale_color_distiller( palette = input$Color ) + # one dimensional
            theme(axis.text.x = element_text(size = input$num1)) +
        labs(x="Year", y="Count",title="Police Complaints by Year", fill = "count") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    output$Visualization5 <- renderUI({
        plotOutput("Plot5", height = input$size, width = input$width)
        
    })
    output$Plot5 <- renderPlot({
        #data %>% count(allegation) %>% arrange(desc(n)) %>%
        data1 %>%
            arrange(desc(allegation)) %>% 
            mutate(prop = n / sum(data1$n) *100) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop ) %>% 
        ggplot( aes(x="", y=prop, fill=allegation))+
            geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0) +
            theme_void() +
            theme(legend.position="none") +
            geom_text(aes(y = ypos, label = allegation), color = "black", size=6) +
            scale_fill_brewer(palette=input$Color)+
            labs(title="Top 5 Accusations") 
        
    })
    output$downloadPlot <- downloadHandler(
        filename = function(){paste("plot",'.png',sep='')},
        content = function(file){
            ggsave(file,plot=last_plot())
        })
}
shinyApp(ui, server)
