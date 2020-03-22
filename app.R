## app.R ##
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggrepel)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(dashboardthemes)
library(RCzechia)
library(leaflet)
library(sf)
library(RJSONIO)
library(deSolve)


my_colors <- c("#DC143C","#F9A828", "#36648B", "#8B1C62", "#00868B", "#698B69", "#CDC673",
               "#8B5A00", "#EE9572", "#483D8B", "#7A378B", "#CD69C9", "#FFB6C1", "#00C78C")

df = rjson::fromJSON(file="https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")

tdf2 <- data.frame(matrix(unlist(df$infectedDaily), nrow=length(df$infectedDaily), byrow=T))
tdf2$X1 <- as.numeric(as.character(tdf2$X1))
tdf2$X2 <- as.Date(as.character(tdf2$X2))
tdf2 <- tdf2[32:nrow(tdf2),]
names(tdf2) <- c("Pocet", "Den")



cz <- RCzechia::kraje("low")
tdf1 <- data.frame(matrix(unlist(df$infectedByRegion), nrow=length(df$infectedByRegion), byrow=T))
names(tdf1) <- c("NAZ_CZNUTS3", "Pocet")
tdf1$NAZ_CZNUTS3 <- as.character(tdf1$NAZ_CZNUTS3)
tdf1$Pocet <- as.numeric(as.character(tdf1$Pocet))
dc <- cz %>% inner_join(tdf1, by = "NAZ_CZNUTS3")

poc <- c(df[[2]],df[[3]])
year <- c("CZ", "CZ")
cond <- c("nakazeny","uzdraveny")
nak <- data_frame(cond,poc,year)

#Infected <- c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515, 5974, 7711, 9692, 11791, 14380, 17205, 20440)

inf <- tdf2[tdf2$Pocet != 0, ]
Infected <- inf$Pocet
Day <- 1:(length(Infected))
N <- 10000000 # population of mainland china

old <- par(mfrow = c(1, 2))

SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
        dS <- -beta/N * I * S
        dI <- beta/N * I * S - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
    })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions

Opt_par <- setNames(Opt$par, c("beta", "gamma"))


t <- 1:70 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour


R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")

fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic





ui <- dashboardPagePlus(collapse_sidebar = FALSE,

    header = dashboardHeaderPlus(title = tagList(
        span(class = "logo-lg", "COVID19 Czechia"), 
        img(src = "https://image.flaticon.com/icons/svg/204/204074.svg")), enable_rightsidebar = FALSE, rightSidebarIcon = "gears"),
    
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Total", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Daily", tabName = "summary" , icon = icon("bar-chart-o"), badgeLabel = "info", badgeColor = "red"),
            menuItem("Prediction", tabName = "prediction" , icon = icon("bar-chart-o"), badgeLabel = "new", badgeColor = "green"),
            menuItem("Map", tabName = "map" , icon = icon("map-marker-alt"), badgeLabel = "new", badgeColor = "blue")
            
            
            
        ),
        collapsed = FALSE
    ),
    
    
    body = dashboardBody(
        
        
        
        
        
        ### changing theme
        shinyDashboardThemes(
            theme = "grey_dark"
        ),
        
        tabItems(
            
            
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(boxPlus(plotOutput("chart2", height = 680) %>% withSpinner(type="5"), width = 10, background = "black"),
                             
                             #boxPlus(plotlyOutput("chart1", height =300) %>% withSpinner(type = "5"), width = 4),
                             
                             boxPlus(background = "blue" , descriptionBlock(
                                 header = h3(df$totalTested), 
                                 text =  h4("Testovaných"), 
                                 right_border = FALSE,
                                 margin_bottom = FALSE
                             ), width = 2),
                             
                             boxPlus(background = "red",
                                 descriptionBlock(
                                 header = h3(df[[2]]), 
                                 text = h4("Nakažených") , 
                                 right_border = FALSE,
                                 margin_bottom = FALSE
                             ), width = 2),
                             
                             boxPlus(background = "green",
                                 descriptionBlock(
                                 header = h3(df[[3]]), 
                                 text = h4("Uzdraveno"), 
                                 right_border = FALSE,
                                 margin_bottom = FALSE
                             ), width = 2)
                    ),
                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                    p("Source", "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")

        ),
                    
            
        tabItem(tabName = "summary",
                
                fluidRow(boxPlus(plotOutput("chart3", height = 680) %>% withSpinner(type="5"), width = 12, background = "yellow"),
                         #boxPlus(plotOutput("chart4", height =600) %>% withSpinner(type = "5"), width = 4)
                         ),
                h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                p("Source", "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
                
                    
                    ),
        
        
        
        tabItem(tabName = "prediction",
                
                fluidRow(boxPlus(plotOutput("chart5", height = 680) %>% withSpinner(type="5"), width = 12, background = "black")),
                h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                p("Source", "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
                
                
        ),
        
        tabItem(tabName = "map",
                
                fluidRow(boxPlus(plotOutput("chart1", height = 680) %>% withSpinner(type="5"), width = 12, background = "black")),
                h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                p("Source", "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
                
                
        )
        
        
        
        
            ),
        
        title = "Covid19-CZ"
        
        )
)


server <- function(input, output, session) {
    
    # output$chart1 <- renderPlotly(ggplot(data =dc)+
    #                                   geom_sf(aes(fill=Pocet )) + scale_fill_continuous(high = "#641E16", low = "#D98880")+
    #                                   theme_void()+theme(panel.background = element_rect(fill ="#17202A", color = "#17202A"),
    #                                                      plot.background = element_rect(fill = "#17202A", color="#17202A"))
    # )
    # 
    
    
    output$chart1 <- renderPlot(ggplot(data = dc) +
                                    geom_sf(aes(fill=Pocet)) +
                                    scale_fill_continuous(high = "#DC143C", low = "goldenrod")+
                                    geom_sf_text(aes(label=Pocet), size=8)+
                                    theme_solid() +
                                    theme(legend.text.align = 1,
                                          legend.title.align = 0.5,
                                          plot.background = element_rect(fill = "#116979", color="black"),
                                          legend.position = "none", 
                                          
                                    ), bg="#116979"
        
        
        
        
        
    )
    
    
    
    
    
    
    output$chart2 <- renderPlot(ggplot(dc, aes(x=reorder(NAZ_CZNUTS3, Pocet), y=Pocet, fill=NAZ_CZNUTS3))+
                                      geom_bar(stat = "identity")+ coord_flip()+ggtitle("Nakažených Celkem / Infected Total")+
                                      geom_text(aes(label=Pocet), hjust = 0, color="white", size=8)+
                                      theme_bw()+xlab("")+
                                      theme(
                                          panel.background = element_rect(fill ="#2b374b", color = "#17202A"),
                                          plot.background = element_rect(fill = "#17202A", color="#17202A"),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                          legend.position = "none",axis.title.x=element_blank(), axis.text=element_text(size=14, colour = "white"),
                                          plot.title = element_text(color="white", size=16)
                                          )+
                                    scale_fill_manual(values = c(my_colors))
        
    )
    
    output$chart3 <- renderPlot(ggplot(tdf2, aes(x=Den, y=Pocet))+geom_line(lwd=1, color="#f9bd2e")+
                                      ggtitle("Nakažených za den / Infected per day")+theme_wsj()+
                                      theme(panel.background = element_rect(fill ="#2b374b", color = "#17202A"),
                                            plot.background = element_rect(fill = "#DC143C", color="#17202A"),
                                            panel.grid.minor = element_blank(),
                                            legend.position = "none",plot.title = element_text(color="#17202A"),
                                            axis.title.x=element_blank())+
                                    geom_point(aes(col=Pocet,size=2))+
                                    geom_text(aes(label=Pocet),hjust=0, vjust=0, color="white", size=7)
    
    
        
    )
    
    output$chart4 <- renderPlot(ggplot(nak, aes(x=year, y=poc, fill=cond))+
                                    ggtitle("Nakažených / Uzdravených  Infected / Recovered  ")+
                                    geom_col()+scale_fill_manual(values = c("#DC143C","#66CDAA"))+
                                    geom_text(aes(label = paste0(poc)), position = position_stack(vjust = 0.7), size=14)+
                                    #scale_fill_brewer(palette = "Set2") +
                                    theme_minimal(base_size = 16) +
                                    ylab("") +
                                    xlab(NULL)+
                                    theme(panel.background = element_rect(fill ="#17202A", color = "#17202A"),
                                          plot.background = element_rect(fill = "#17202A", color="#17202A"),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                          legend.position = "none",plot.title = element_text(color="white", size=14),
                                          axis.title.x=element_blank())
                                
        
        
        
    )
    
    output$chart5 <- renderPlot(ggplot(fit[0:35,], aes(time, I))+
                                      geom_line(lwd=3, color="#00868B")+
                                      geom_point(aes(), color="#DC143C", size=6)+
                                      ggtitle("SIR model prediction COV19CZ")+xlab("Days")+ ylab("Infected")+ theme_minimal()+
                                      geom_text(aes(label=ceiling(I)), hjust=1, vjust=0, color="black", size=7)+
                                      theme(panel.background = element_rect(fill ="goldenrod", color = "black"),
                                            plot.background = element_rect(fill = "#DC143C", color="black"),
                                            legend.position = "none",plot.title = element_text(color="#17202A"),
                                            panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
                                            panel.grid.major = element_line(
                                                colour = "black",
                                                size = 1,
                                                linetype = "dotted",
                                                lineend = NULL,
                                                color = NULL,
                                                arrow = NULL,
                                                inherit.blank = FALSE
                                            ), axis.text = element_text(size=15, colour = "black"),
                                            axis.title = element_text(size = 15),
                                            title = element_text(size=20)
                                            
                                            
                                            
                                      )
                                  
                                  
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    )
        
}

shinyApp(ui, server)
