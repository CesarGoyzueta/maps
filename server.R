#library("httr")
library("readxl")
library("leaflet")
library("dplyr")
library("ggplot2")
library("htmltools")
library("lubridate")
library("scales")
library("forcats")
library("shinydashboard")
library("shinythemes")
library("shinyWidgets")
library("fresh")

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Metricas    
    output$total_power<-renderValueBox({
        
        if(input$PV%>%is.null()| input$Months%>%is.null()){
            total_power<-0
            
        }else {
            total_power<-df%>%filter(PV %in% input$PV,month_name %in% input$Months)%>%summarise(suma=sum(Power))%>%pull(suma)
        }
        
        
        valueBox(
            value =format(round(total_power,0),big.mark=","),
            subtitle="Total anual power",
            color="navy") 
        
    }) 
    
    output$avg_power<-renderValueBox({
        
        if(input$PV%>%is.null() | input$Months%>%is.null()){
            avg_power<-0
            
        }else {
            
            avg_power<-df%>%filter(PV %in% input$PV,month_name %in% input$Months)%>%summarise(mean1=mean(Power))%>%pull(mean1)
        }
        
        
        
        valueBox(
            value = format(round(avg_power,0),big.mark=","),
            subtitle="Daily average power",
            color="navy") 
        
        
    }) 
    
    
    
    
    
    
    
    output$max_power<-renderValueBox({
        
        if(input$PV%>%is.null() | input$Months%>%is.null()){
            max_power<-0
            
        }else {
            
            max_power<-df%>%filter(PV %in% input$PV,month_name %in% input$Months)%>%summarise(max=max(Power))%>%pull(max)
        }
        
        
        
        valueBox(
            value = format(round(max_power,0),big.mark=","),
            subtitle="Daily maximum power",
            color="navy") 
        
        
    }) 
    
    
    
    
    
    
    # Cantidad de Capacidades
    
    output$ggplot_bar<-renderPlot({
        
        df_bar<-df%>%filter(PV %in% input$PV,month_name %in% input$Months)%>%group_by(Capacity)%>%summarise(n=n())
        
        ggplot(df_bar,aes(Capacity,n,label=n))+geom_col(fill="#92140C")+coord_flip()+
            labs(y="Quantity")+
            theme_classic()+
            theme(axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.title=element_text(size=13.4),
                  legend.text=element_text(size=12),
                  legend.title=element_text(size=14),
                  axis.text.y = element_text(size=14, hjust = 1),
                  axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size=14))+
            geom_text(aes(label=format(n, big.mark = ",", scientific = FALSE),y=n+130),size = 4)
        
    })
    
    
    
    
    
    
    
    
    #MAPA
    
    output$mymap <- renderLeaflet({
        
        data_map<-df%>%filter(PV %in% input$PV,month_name %in% input$Months)%>%group_by(Latitude,Longitude,Capacity,PV,site_name)%>%summarise(Power=sum(Power))
        
        
        palette<-colorFactor(palette=c("#111D4A","#92140C"),levels=c("Distribution_PV","Utility_Scale PV"))  #esto debe estar igual al dato
        
        
        
        leaflet(options=leafletOptions(minZoom=7,maxZoom=18))%>%
            addTiles(group="OSM")%>%
            addProviderTiles("CartoDB",group="Carto")%>%
            addProviderTiles("Esri",group="Esri")%>%
            addCircleMarkers(data=data_map,
                             color=~palette(PV),
                             lng=~Longitude,
                             lat=~Latitude,
                             radius=~Power/700,
                             popup =~paste0(
                                 "<b>", "Capacity: ","</b>",Capacity, "</br>","Longitude: ",
                                 Longitude,"</br>","Latitude: ",Latitude),
                             clusterOptions=markerClusterOptions())%>%
            addLayersControl(baseGroups=c("OSM","Carto","Esri"))%>%
            addLegend(pal=palette,values = c("Distribution_PV", "Utility_Scale PV"),
                      opacity = 0.5, title = "PV", position = "topright")
        
        
        
        
    })
    
    
    
    
    
    
    
    output$violin<-renderPlot({
        #violin
        ggplot(df%>%filter(Power<350,month_name %in% input$Months),aes(PV,Power))+geom_violin(fill="#92140C")+coord_flip()+
            labs(y="Power")+
            theme_classic()+
            theme(plot.title=element_text(size=13.4),
                  axis.text.x = element_text(size=14, hjust = 1),
                  axis.text.y = element_text(size=14, hjust = 1),
                  axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size=14),
                  legend.text=element_text(size=12),
                  legend.title=element_text(size=14),
                  strip.text.x = element_text(size = 7))+
            scale_y_continuous(labels = comma)
        
        
    })
    
    
    
    
    
    
    output$geom_line<-renderPlot({
        
        #geom_line
        df_geom_line<-df%>%group_by(PV,month_name)%>%summarise(power=sum(Power))  #colour="#111D4A",size=0.8
        ggplot(df_geom_line,aes(month_name,power,group = 1))+geom_line(colour="#111D4A",size=0.8)+
            theme(plot.title=element_text(size=14),
                  strip.text.x = element_text(size = 10))+
            labs(y="Power",x="Month")+
            theme_classic()+
            scale_y_continuous(labels =comma)+facet_grid(PV~.,scales="free_y")+
            theme(strip.text.y = element_text(size = 14, colour = "#111D4A", angle = 0),
                  axis.text.y = element_text(size = 14),
                  axis.text.x = element_text(size = 14),
                  axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size=14),)
    })
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)