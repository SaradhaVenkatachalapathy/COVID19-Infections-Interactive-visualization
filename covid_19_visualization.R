# Visualising global trends of CoVid-19
# Author: Saradha Venkatachalapathy
# Source:Johns Hopkins Whiting School of Engineering:https://github.com/CSSEGISandData/COVID-19

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
# Obtain and cleanup the data
{
  # Obtain country wise data for the number of confirmed coronavirus cases
  {
    confirmed_cases<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",check.names = FALSE, stringsAsFactors = F)
    confirmed_cases_reformatted = aggregate(confirmed_cases[,5:ncol(confirmed_cases)],
                                            by = list(confirmed_cases$`Country/Region`),
                                            FUN = sum)
    temp<-confirmed_cases_reformatted$Group.1
    confirmed_cases_reformatted<-as.data.frame(t(as.matrix(confirmed_cases_reformatted[,5:ncol(confirmed_cases_reformatted)])))
    colnames(confirmed_cases_reformatted)<-temp
    confirmed_cases_reformatted$World<-apply(confirmed_cases_reformatted,1,sum)
    confirmed_cases_reformatted$Date<-as.Date(paste(lapply(rownames(confirmed_cases_reformatted), function(x) substr(x, 1, nchar(x)-2)),2020,sep=""), "%m/%d/%Y")
    rownames(confirmed_cases_reformatted)<-as.numeric(1:nrow(confirmed_cases_reformatted))
    rm(temp,confirmed_cases)
  }
  
  # Obtain country wise data for the number of deaths from coronavirus
  {
    dead_cases<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",check.names = FALSE, stringsAsFactors = F)
    dead_cases_reformatted = aggregate(dead_cases[,5:ncol(dead_cases)],
                                       by = list(dead_cases$`Country/Region`),
                                       FUN = sum)
    temp<-dead_cases_reformatted$Group.1
    dead_cases_reformatted<-as.data.frame(t(as.matrix(dead_cases_reformatted[,5:ncol(dead_cases_reformatted)])))
    colnames(dead_cases_reformatted)<-temp
    dead_cases_reformatted$World<-apply(dead_cases_reformatted,1,sum)
    dead_cases_reformatted$Date<-as.Date(paste(lapply(rownames(dead_cases_reformatted), function(x) substr(x, 1, nchar(x)-2)),2020,sep=""), "%m/%d/%Y")
    rm(temp,dead_cases)
  }
  
  # Obtain country wise data for the number of recoved cases from coronavirus
  {
    recovered_cases<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",check.names = FALSE, stringsAsFactors = F)
    recovered_cases_reformatted = aggregate(recovered_cases[,5:ncol(recovered_cases)],
                                            by = list(recovered_cases$`Country/Region`),
                                            FUN = sum)
    temp<-recovered_cases_reformatted$Group.1
    recovered_cases_reformatted<-as.data.frame(t(as.matrix(recovered_cases_reformatted[,5:ncol(recovered_cases_reformatted)])))
    colnames(recovered_cases_reformatted)<-temp
    recovered_cases_reformatted$World<-apply(recovered_cases_reformatted,1,sum)
    recovered_cases_reformatted$Date<-as.Date(paste(lapply(rownames(recovered_cases_reformatted), function(x) substr(x, 1, nchar(x)-2)),2020,sep=""), "%m/%d/%Y")
    rm(temp,recovered_cases)
  }
  
  #Data_rates
  {
    least_numb_cases<-apply(confirmed_cases_reformatted[,1:ncol(confirmed_cases_reformatted)-1],2,min)
    combination_vector<-as.data.frame(matrix())
    for( i in 1:(ncol(confirmed_cases_reformatted)-1)){
      if(least_numb_cases[i]==0){
        Last_disease_free_date<-max(confirmed_cases_reformatted$Date[which(confirmed_cases_reformatted[,i]==least_numb_cases[i])])
      } else{
        Last_disease_free_date<-min(confirmed_cases_reformatted$Date)
      }
      
      total<-confirmed_cases_reformatted[nrow(confirmed_cases_reformatted),i]
      dead<-dead_cases_reformatted[nrow(dead_cases_reformatted),i]
      recovered<-recovered_cases_reformatted[nrow(recovered_cases_reformatted),i]
      cases_time<-confirmed_cases_reformatted[,i]
      duration<-as.numeric(max(confirmed_cases_reformatted$Date)-(Last_disease_free_date+1),units="days")
      duration<-ifelse(duration>0,duration,1)
      combination_vector[i,1]<-colnames(confirmed_cases_reformatted)[i]
      combination_vector[i,2]<-total
      combination_vector[i,3]<-recovered
      combination_vector[i,4]<-total-recovered
      combination_vector[i,5]<-dead
      combination_vector[i,6]<-total-confirmed_cases_reformatted[nrow(confirmed_cases_reformatted)-1,i]
      
      combination_vector[i,7]<-Last_disease_free_date+1
      combination_vector[i,8]<-duration
      
      combination_vector[i,9]<-round((total/duration),2)
      combination_vector[i,10]<-confirmed_cases_reformatted$Date[nrow(confirmed_cases_reformatted)]-
        max(confirmed_cases_reformatted$Date[which(abs(cases_time-total/2)==min(abs(cases_time-total/2)))])
      
      combination_vector[i,11]<-round((dead/total)*100,2)
      combination_vector[i,12]<-round((dead/duration),2)
      combination_vector[i,13]<-round((recovered/total)*100,2)
      combination_vector[i,14]<-round((recovered/duration),2)
      
    } 
    colnames(combination_vector)<-c('Country','Total','Recovered', 'Active', 'Dead','Recent Infections',
                                    'First Case','Duration of Infection',
                                    'Infections per day', 'Infection Doubling time(days)', 
                                    'Death Percent (%)','Deaths per day','Recovery Percent (%)','Recovery per day')
    rm(cases_time,dead,total,i,Last_disease_free_date,least_numb_cases,recovered)
  }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("COVID-19 Infection Numbers"),
  
  h6('Source:Johns Hopkins'),
  
  varSelectInput("variable1", "Select an indicator:", combination_vector[2:ncol(combination_vector)]),
  
  plotlyOutput("Worldmap"),
  
  varSelectInput("variable", "Select a Country:", 
                 confirmed_cases_reformatted[,c((ncol(confirmed_cases_reformatted)-1),1:(ncol(confirmed_cases_reformatted)-2))]
                ),
  
  verticalLayout(br(),
                 plotlyOutput("Covid19_raw_numbers"),
                 br(),br(),
                 plotlyOutput("Covid19_pie"),
                 br(),br(),
                 dataTableOutput("Covid19_raw_table"))
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Covid19_raw_table <- DT::renderDataTable({
    datatable(combination_vector, selection = 'single')
  })
  
  output$Covid19_raw_numbers <- renderPlotly({
    g<- ggplot()+
      geom_line(data= confirmed_cases_reformatted, aes(x=Date, y=!!input$variable,color="Total")) + 
      geom_line(data= dead_cases_reformatted, aes(x=Date, y=!!input$variable,color="Dead")) + 
      geom_line(data= recovered_cases_reformatted, aes(x=Date, y=!!input$variable,color="Recovered")) +
      scale_color_manual(values = c(Total= "blue",Dead="red", Recovered="green4" )) +
      labs(color="Indices") +
      ylab("Number of people")+ xlab("Time")+ theme_bw()+
      theme(text = element_text(size=10)) 
    ggplotly(g)
  })
  
  
  output$Covid19_pie<- renderPlotly({
    df<-data.frame(value=c(combination_vector[which(combination_vector$Country==input$variable),]$Active,
                           combination_vector[which(combination_vector$Country==input$variable),]$Recovered,
                           combination_vector[which(combination_vector$Country==input$variable),]$Dead),
                   group=c("Active","Recovered","Dead"))
    df$color <- c('blue', 'green','red')
    
    plot_ly(df, labels =~group, values =~value, 
            marker = list(colors =df$color, showlegend = TRUE),
            type = 'pie') %>% layout(title = "Current numbers",
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$Worldmap<-renderPlotly({
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    fig <- plot_geo(data.frame(Country=combination_vector$Country,quant=combination_vector[,which(colnames(combination_vector)==input$variable1)]))
    fig <- fig %>% add_trace(
      z = ~quant, color = ~quant, colors = 'Reds',
      text = ~Country, locations = combination_vector$Country, locationmode='country names' 
    )
    fig <- fig %>% colorbar(title = '')
    fig <- fig %>% layout(
      title = ' ',
      geo = g
    )
    
    fig
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

