#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library("httr")
library("readxl")
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
GET("https://query.data.world/s/rjngr5uzhv4ihxvhak3bvny66uoj47", write_disk(tf1 <- tempfile(fileext = ".xlsx")))

#Sampling
df1 <- read_excel(tf1)
data<-df1
data_actuals<-data%>%slice_sample(prop=0.04)


#111D4A azul  #639FAB  celeste   #92140C rojo   #FBF5F3 medio blanco
df<-data_actuals%>%rename(Power='Power(MW)',site_name='Location Site Name')%>%mutate(month=month(Date),year=year(Date))%>%filter(year==2020)%>%
    select(-year,-Reading)

df$site_name <- sub(" ", "_", df$site_name)
df$PV <- sub(" ", "_", df$PV)

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

#reorder a factor 5MW
df<-df%>%mutate(Capacity=fct_relevel(Capacity,"5MW","12MW","13MW"))


#Choices in filters

pv_choices<-df%>%pull(PV)%>%unique()
months_choices<-c("Jan", "Feb","Mar", "Apr", "May" ,"Jun","Jul","Aug","Sept", "Oct","Nov","Dic")





df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],  as.factor)

#theme
mytheme <- create_theme(
    adminlte_color(
        light_blue = "#111D4A"
    ),
    adminlte_sidebar(
        dark_hover_bg  = "#111D4A",
        dark_color= "#111D4A"
    )
)


#Body
body<-dashboardBody(use_theme(mytheme),
                    tabItems(
                        tabItem(
                            tabName="Overview",
                            fluidRow(
                                valueBoxOutput("total_power"),
                                valueBoxOutput("avg_power"),
                                valueBoxOutput("max_power"),
                                column(width=6,
                                       box(solidHeader=TRUE,title="Quantity of centers order by their capacity",
                                           plotOutput("ggplot_bar",height="50vh"),width=12))
                            ),
                            fluidRow(
                                column(width=12,
                                       box(solidHeader=TRUE,title="Power evolution in 2020",
                                           plotOutput("geom_line",height="30vh"),width=12))
                                
                                
                            )
                        ),
                        
                        
                        tabItem(
                            tabName="Monthly",
                            fluidRow(
                                column(width=6,
                                       box(solidHeader=TRUE,title="Power daily distribution in 2020",
                                           plotOutput("violin",height="55vh"),width=12))  
                            )
                            
                            
                            
                            
                            
                        )
                        
                    )
)








#Sidebar
sidebar<-dashboardSidebar(
    sidebarMenu(id = 'sidebar',
                menuItem("Overview", tabName = "Overview"),
                conditionalPanel(condition = "input.sidebar == 'Overview'",
                                 pickerInput(
                                     inputId = "PV",
                                     label = "PV", 
                                     choices = pv_choices,selected = pv_choices, options = list(`actions-box` = TRUE),multiple = T
                                 ),
                                 pickerInput(
                                     inputId = "Months",
                                     label = "Months", 
                                     choices = months_choices,selected=months_choices,options = list(`actions-box` = TRUE),multiple = T
                                 )
                                 
                )
    )
    
)







#Header

header<-dashboardHeader(title = "Solar Power Report 2020 ",titleWidth  = 350,
                        dropdownMenu(
                            type="messages",
                            messageItem(
                                from="Cesar Goyzueta",
                                message="Check Out my Linkedin",
                                href="http://www.linkedin.com/in/cesargoyzueta/",
                                icon=icon("user-tie")
                            )))



ui <- dashboardPage(skin="blue",
                    header=header,
                    sidebar=sidebar,
                    body=body)







