library("httr")
library("readxl")
library("leaflet")
library("dplyr")
library("ggplot2")
library("htmltools")
library("lubridate")
library("scales")
library("forcats")

GET("https://query.data.world/s/rjngr5uzhv4ihxvhak3bvny66uoj47", write_disk(tf1 <- tempfile(fileext = ".xlsx")))

#Sampling
df1 <- read_excel(tf1)
data<-df1
data_actuals<-data%>%slice_sample(prop=0.025)


#111D4A azul  #639FAB  celeste   #92140C rojo   #FBF5F3 medio blanco
df<-data_actuals%>%rename(Power='Power(MW)',site_name='Location Site Name')%>%mutate(month=month(Date),year=year(Date))%>%filter(year==2020)%>%
                   select(-year,-Reading)


df$month_name<- ifelse(df$month==1,"Jan",
                    ifelse(df$month==2,"Feb",
                         ifelse(df$month==3,"Mar",
                                ifelse(df$month==4,"Apr",
                                       ifelse(df$month==5,"May",
                                              ifelse(df$month==6,"Jun",
                                                     ifelse(df$month==7,"Jul",
                                                            ifelse(df$month==8,"Aug",
                                                                   ifelse(df$month==9,"Sept",
                                                                          ifelse(df$month==10,"Oct",
                                                                                 ifelse(df$month==11,"Nov",
                                                                   ifelse(df$month==12,"Dic","check"
                                                                          ))))))))))))

df$month_name<- factor(df$month_name,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dic"))



# Characters to factors
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],  as.factor)
  #reorder a factor 5MW
df<-df%>%mutate(Capacity=fct_relevel(Capacity,"5MW","12MW","13MW"))




#MAPA
data_distribution<-df%>%filter(PV=="Distribution PV")%>%group_by(Latitude,Longitude,
                                                                 Capacity,PV,site_name)%>%summarise(Power=sum(Power))
data_utility<-df%>%filter(PV=="Utility Scale PV")%>%group_by(Latitude,Longitude,
                                                                 Capacity,PV,site_name)%>%summarise(Power=sum(Power))

palette<-colorFactor(palette=c("#111D4A","#92140C"),levels=c("Distribution PV","Utility Scale PV"))  #esto debe estar igual al dato



leaflet(options=leafletOptions(minZoom=7,maxZoom=18))%>%
  addTiles(group="OSM")%>%
  addProviderTiles("CartoDB",group="Carto")%>%
  addProviderTiles("Esri",group="Esri")%>%
  addCircleMarkers(data=data_distribution,
                   color=~palette(PV),
                   lng=~Longitude,
                   lat=~Latitude,
                   radius=~Power/700,
                   popup =~paste0(
                     "<b>", "Capacity: ","</b>",Capacity, "</br>","Longitude: ",
                     Longitude,"</br>","Latitude: ",Latitude),
                   group="Distribution PV",clusterOptions=markerClusterOptions())%>%
  addCircleMarkers(data=data_utility,
                   color=~palette(PV),
                   lng=~Longitude,
                   lat=~Latitude,
                   radius=~Power/700,
                   popup =~paste0(
                     "<b>", "Capacity: ","</b>",Capacity, "</br>","Longitude: ",
                     Longitude,"</br>","Latitude: ",Latitude),
                   group="Utility Scale PV",clusterOptions=markerClusterOptions())%>%
  addLayersControl(baseGroups=c("OSM","Carto","Esri"),overlayGroups = c("Distribution PV","Utility Scale PV"))%>%
  addLegend(pal=palette,values = c("Distribution PV", "Utility Scale PV"),
            opacity = 0.5, title = "PV", position = "topright")


#geom_line
df_geom_line<-df%>%group_by(PV,month_name)%>%summarise(power=sum(Power))  #colour="#111D4A",size=0.8
ggplot(df_geom_line,aes(month_name,power,group = 1))+geom_line(colour="#111D4A",size=0.8)+
  theme(plot.title=element_text(size=14),
        strip.text.x = element_text(size = 10))+
        labs(title="Power evolution in 2020", y="Power",x="Month")+
        theme_classic()+scale_y_continuous(labels =comma)+facet_grid(PV~.,scales="free_y")+ 
  theme(strip.text.y = element_text(size = 14, colour = "#111D4A", angle = 0),
        axis.text.y = element_text(size = 12))



#violin

ggplot(df%>%filter(Power<350),aes(PV,Power))+geom_violin(fill="#92140C")+coord_flip()+
  labs(title="Power daily distribution in 2020",y="Power")+
  theme_classic()+
 theme(plot.title=element_text(size=13.4),
        axis.text.x = element_text(size=12, hjust = 1),
         axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=10, hjust = 1),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        strip.text.x = element_text(size = 7))+
        scale_y_continuous(labels = comma)


        
        
# Cantidad de Capacidades
        
  df_bar<-df%>%group_by(Capacity)%>%summarise(n=n())
  
  ggplot(df_bar,aes(Capacity,n,label=n))+geom_col(fill="#92140C")+coord_flip()+
          labs(title="Quantity of centers order by their capacity",y="Quantity")+
          theme_classic()+
          theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title=element_text(size=13.4),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14))+
         geom_text(aes(label=format(n, big.mark = ",", scientific = FALSE),y=n+100),size = 4)
  
  
        

        
        
        

