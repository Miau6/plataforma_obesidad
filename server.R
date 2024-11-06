
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sf)
library(sp)
library(ggplot2)
library(plotly)
library(tidyr)
library(DT)
library(shiny)
library(patchwork)
library(stringr)

# colrs = c("#CF9400", "#72B000", "#00BF7D", "#00BBDB", "#7997FF", "khaki2", "#F8766D")

colrs = c("North East"="#CF9400" ,
          "South"="#72B000",
          "South North"="#00BF7D", 
          "Center East"="#00BBDB", 
          "Center West"="#E78AC3", 
          "North West"="#7997FF",
          # "North West"="khaki2", 
          "Mexico City"="#F8766D")

sort_colr <- c( "Center East"="#00BBDB", 
                "Center West"="#E78AC3", 
                "Mexico City"="#F8766D",
                "North East"="#CF9400",
                "North West"="#7997FF",
                "South"="#72B000",
                "South North"="#00BF7D"
                )

sex_colors <- c("Female"="#BF2F24",
                "Male"="#436685")

#metemos la info
# Upload the shape file
shpEnt      <- st_read("data/spatial/mexico.shp")
shpEnt      <- shpEnt %>% select(state,geometry)

# Loading regional info
regions_ordered <- read_excel("data/regions_ordered.xlsx") %>% select(-Subregion)

# Upload malnutrition data
# load("data/spatial/Data.RData")
Data <- read_excel("data/data_Feb24.xlsx")
Data$Pctg <- round(Data$Pctg,2)

function(input, output, session) {

  ## Page1. Interactive Map ###########################################
  
  baloo <- reactive({
    Data %>% filter(indicator==input$indicator, year==input$year, 
                    gender == input$sex, age ==input$age)
  })

  output$map <- renderLeaflet({
    
    # filter the data to be mapped
    # state <- Data %>% filter(indicator==input$indicator, year==input$year, 
    #                          gender == input$sex, age ==input$age) %>% 
    #   select(CVE_Reg,Pctg,Region)
    
    state <- baloo() %>% 
      select(CVE_Reg,Pctg,Region) %>% 
      filter(if(input$region=="National") Region!="" else Region ==input$region)
    
    # merge state-level shape with regional info
    map  <- merge(shpEnt, regions_ordered, by.x = "state", by.y = "State") 
    
    # create shape at regional-level
    shpReg <- map %>%
      group_by(CVE_Reg) %>%
      summarize(geometry = st_union(geometry))
    
    # add the data to be mapped to the shape
    # map  <- merge(shpReg, state, by.x = "CVE_Reg", by.y = "CVE_Reg") %>%
    #   as_Spatial()
    
    map  <- merge(shpReg, state, by.x = "CVE_Reg", by.y = "CVE_Reg") %>%
      st_as_sf()
    
    # change the CRS to WGS84
    # map <- spTransform(map, CRS("+proj=longlat +datum=WGS84")) 
    # map <- sf::st_crs(map, crs=4326)
    
    # pal <- colorFactor(
    #   palette = colrs,
    #   domain = sort(map@data$Region)
    # )
    
    pal <- colorFactor(
      palette = sort_colr,
      domain = names(sort_colr)
    )
    
    # mytext <- paste(
    #   "\nRegion: ", map@data$Region,"<br/>", 
    #   "Rate: ", scales::percent(round(map@data$Pctg,1)/100), 
    #   sep="") %>%
    #   lapply(htmltools::HTML)
    
    mytext <- paste(
      "\nRegion: ", map$Region,"<br/>", 
      "Rate: ", scales::percent(round(map$Pctg,1)/100), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    m1 <- 
        if (input$region == "National") {
          leaflet(map) %>% 
            addTiles() %>%  addPolygons( 
            fillColor = ~colorQuantile("Reds", Pctg)(Pctg),  # Ajuste a fillColor
            stroke = FALSE, 
            fillOpacity = 0.75, 
            weight = 10,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>%
            addLegend(
              position = "bottomleft", 
              pal = colorQuantile("Reds", map$Pctg),
              values = map$Pctg, 
              title = "Rate Percent"
            ) %>%
            setView(lng = -86.35, lat = 22.45, zoom = 4.95)
        } else {
          leaflet(map) %>% 
            addTiles() %>%  addPolygons( 
            fillColor = ~pal(Region),  # Ajuste a fillColor
              # fillColor = colrs,
            stroke = FALSE, 
            fillOpacity = 0.55, 
            weight = 10,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>%
            setView(lng = -86.35, lat = 22.45, zoom = 4.95)
        }
      
    
      # addPolygons( 
      #   # color = ~colorQuantile("YlGnBu", Pctg)(Pctg), 
      #   color = ~colorQuantile("Reds", Pctg)(Pctg),
      #   
      # #  color = ~pal(Region), 
      #   stroke=FALSE, 
      #   fillOpacity = 0.75, 
      #   weight=10,
      #   label = mytext,
      #   labelOptions = labelOptions( 
      #     style = list("font-weight" = "normal", padding = "3px 8px"), 
      #     textsize = "13px", 
      #     direction = "auto", 
      #     # if(input$region=="National") color=~colorQuantile("Reds", Pctg)(Pctg) else 
      #     #   color=~colorFactor("YlGnBu", Pctg)(Pctg)
      #   )
      # ) %>%
      # addLegend(position = "bottomleft", 
      #           pal = colorQuantile("Reds", map$Pctg),
      #           values = map$Pctg, 
      #           title = "Rate Percent") %>% 
      # setView(lng = -86.35, lat = 22.45, zoom = 4.95)
    
    
    
    m1
    
  })

  output$Plot1 <- renderPlotly({

    # Data1 <- Data %>% filter(indicator==input$indicator, year==input$year, age ==input$age)
    
    Data1 <- baloo()
    mytext <- paste(
      "\nSex: ",     Data1$gender, 
      "\nRegion: ",  Data1$Region, 
      "\nRate(%): ", Data1$Pctg, 
      sep="") 
    
    f1 <- ggplot(Data1, aes(x=reorder(Region, Region), y=Pctg/100, fill=Region, text = mytext)) +
      geom_bar(stat="identity", position = position_dodge(), alpha=0.75) +
      labs(
#        title = "Obesity rates per sex and region in 2018",
#        subtitle = "Children of age 4",
        x = "Regions ",
        y = "Rate"
      ) +
     scale_fill_manual(values = colrs) +
      # scale_fill_brewer(palette="Set3") +
      facet_grid(. ~ gender) + # Separate panels for each 'gender' +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      theme(text = element_text(size = 13)) 
    
    ggplotly(f1,tooltip = "mytext")
    
     })

  output$Plot2 <- renderPlotly({
    
    req(input$region != "Nacional")

    
      Data2 <- Data %>%
        filter(Region ==input$region,
               indicator==input$indicator,
               age==input$age) %>%
        select(year,gender,Pctg)

      #aqui no sirve balooo porque nos interesa ambos sexos
      # Data2 <- baloo() %>% #filter(Region ==input$region, indicator==input$indicator, age==input$age) %>%
      #   select(year,gender,Pctg)

      D <- spread(Data2, key = gender, value = Pctg)
      D$diff <- D$Male - D$Female
      D$aver <- 0.5*D$Female + 0.5*D$Male
      D <- gather(D, gender, Pctg, Female:Male , factor_key=TRUE)
      D <- D %>%
        mutate(Difference=comma(abs(diff), .01))

      Males <- D %>%
        filter(gender == "Male")
      Females <- D %>%
        filter(gender == "Female")
      head(Females)

      p <- ggplot(D)+
        geom_segment(data = Males,
                     aes(x = Pctg, y = year,
                         yend = Females$year, xend = Females$Pctg), #use the $ operator to fetch data from our "Females" tibble
                     color = "azure3",
                     size = 7, #Note that I sized the segment to fit the points
                     alpha = .5) +
        geom_point(aes(x = Pctg, y = year, color = gender,
                       text=paste0("Year: ", year,
                                   "<br>Percent: ", Pctg,
                                   "<br>Sex: ", gender)), #shape =15,
                   show.legend = F, size=5) +
        # geom_text(aes(x = aver, y = year, label= Difference), color = "gray3", hjust=0, size=3) +
        # scale_y_continuous(breaks = seq(min(D$year), max(D$year), by = 2)) +
        scale_y_continuous(breaks = unique(D$year)) +
        scale_x_continuous(limits = c(min(D$Pctg), max(D$Pctg)+1)) +
        labs(
          #       title = "Evolution of obesity rates per sex",
          #        subtitle = "Children of age 4 in the South",
          x = " Rate(%)",
          y = " "
        ) +
        theme_minimal( ) +
        theme(text = element_text(size = 13),
              legend.position = "bottom") +
        theme(legend.title = element_blank(), 
              # axis.title.x = element_blank()
              ) +
        labs(color="Gender") +
        scale_color_manual(values=sex_colors)

      Males <- Males %>%
        mutate(Difference=comma(abs(diff), .01),
               sex_max=ifelse(diff>0, 1, 0),
               sex_max=factor(sex_max),
               label=ifelse(sex_max==1, "Male", "Female"))
      p2 <-
        if (length(unique(Males$label))==2) {
        Males %>%
        ggplot(aes(x=diff,y=year)) +
        # geom_text(aes(x=0, label=Difference, color=label,
        #               text=paste0("Year: ", year,
        #                           "<br>Difference: ", Difference,
        #                           "<br>Max sex: ", label)),
        #           fontface="bold", show.legend = F,
        #           # color=sex_colors[2],
        #           size=5) +
        
        geom_text(data=. %>%
                    filter(label=="Male"),
                    aes(x=0, label=Difference, #color=label,
                      text=paste0("Year: ", year,
                                  "<br>Difference: ", Difference,
                                  "<br>Max sex: ", label)),
                  fontface="bold", show.legend = F,
                  color=sex_colors[2],
                  size=5) +
        geom_text(data=. %>%
                    filter(label=="Female"),
                  aes(x=0, label=Difference, #color=label,
                      text=paste0("Year: ", year,
                                  "<br>Difference: ", Difference,
                                  "<br>Max sex: ", label)),
                  fontface="bold", show.legend = F,
                  color=sex_colors[1],
                  size=5) +
        scale_y_continuous(breaks = unique(D$year)) +

        # geom_text(aes(x=0, y=3), # 7 because that's the # of y-axis values
        #           # label="Diff",
        #           nudge_y =.5, # match the nudge value of the main plot legend
        #           fontface="bold",
        #           size=3.25) +

        theme_void() +
        # coord_cartesian(xlim = c(-.05, 0.05),
        #                 ylim=c(1,7.5) # needs to match main plot
        # )+
        theme(
          plot.margin = margin(l=0, r=0, b=0, t=0), #otherwise it adds too much space
          panel.background = element_rect(fill="#EFEFE3", color="#EFEFE3"),
          legend.position = "none"
        )
          } else if (unique(Males$label)=="Male"){
        Males %>%
          ggplot(aes(x=diff,y=year)) +
          # geom_text(aes(x=0, label=Difference, color=label,
          #               text=paste0("Year: ", year,
          #                           "<br>Difference: ", Difference,
          #                           "<br>Max sex: ", label)),
          #           fontface="bold", show.legend = F,
          #           # color=sex_colors[2],
          #           size=5) +
          
          geom_text(data=. %>%
                      filter(label=="Male"),
                    aes(x=0, label=Difference, #color=label,
                        text=paste0("Year: ", year,
                                    "<br>Difference: ", Difference,
                                    "<br>Max sex: ", label)),
                    fontface="bold", show.legend = F,
                    color=sex_colors[2],
                    size=5) +
          scale_y_continuous(breaks = unique(D$year)) +
          
          # geom_text(aes(x=0, y=3), # 7 because that's the # of y-axis values
          #           # label="Diff",
          #           nudge_y =.5, # match the nudge value of the main plot legend
          #           fontface="bold",
          #           size=3.25) +
          
          theme_void() +
          # coord_cartesian(xlim = c(-.05, 0.05),
          #                 ylim=c(1,7.5) # needs to match main plot
          # )+
          theme(
            plot.margin = margin(l=0, r=0, b=0, t=0), #otherwise it adds too much space
            panel.background = element_rect(fill="#EFEFE3", color="#EFEFE3"),
            legend.position = "none"
          )#+
          # scale_color_manual(values=sex_colors)
      } else if(unique(Males$label)=="Female") {
        Males %>%
          ggplot(aes(x=diff,y=year)) +
          # geom_text(aes(x=0, label=Difference, color=label,
          #               text=paste0("Year: ", year,
          #                           "<br>Difference: ", Difference,
          #                           "<br>Max sex: ", label)),
          #           fontface="bold", show.legend = F,
          #           # color=sex_colors[2],
          #           size=5) +
          
          geom_text(data=. %>%
                      filter(label=="Female"),
                    aes(x=0, label=Difference, #color=label,
                        text=paste0("Year: ", year,
                                    "<br>Difference: ", Difference,
                                    "<br>Max sex: ", label)),
                    fontface="bold", show.legend = F,
                    color=sex_colors[1],
                    size=5) +
          scale_y_continuous(breaks = unique(D$year)) +
          
          # geom_text(aes(x=0, y=3), # 7 because that's the # of y-axis values
          #           # label="Diff",
          #           nudge_y =.5, # match the nudge value of the main plot legend
          #           fontface="bold",
          #           size=3.25) +
          
          theme_void() +
          # coord_cartesian(xlim = c(-.05, 0.05),
          #                 ylim=c(1,7.5) # needs to match main plot
          # )+
          theme(
            plot.margin = margin(l=0, r=0, b=0, t=0), #otherwise it adds too much space
            panel.background = element_rect(fill="#EFEFE3", color="#EFEFE3"),
            legend.position = "none"
          )#+
        # scale_color_manual(values=sex_colors)
      }


      # p_whole <-
      #   # syntax from `patchwork`
      #   p + p2 + plot_layout(design=
      #                                  c(
      #                                    area(l=0,  r=45, t=0, b=1), # defines the main figure area
      #                                    area(l=46, r=52, t=0, b=1)  # defines the gap figure area
      #                                  ))
      p_ly <- ggplotly(p, tooltip = "text") #%>% 
        # layout(legend = list(
        #   orientation = "h",      # Orientación horizontal
        #   x = 0.5,                # Centrar la leyenda en el eje x
        #   y = -0.2,               # Colocar la leyenda debajo del gráfico
        #   xanchor = "center",     # Anclar la leyenda al centro
        #   yanchor = "top"         # Anclar en la parte superior de la leyenda
        # )) #%>% layout(showlegend = FALSE)
      p2_ly <- ggplotly(p2, tooltip = "text") #%>% layout(showlegend = FALSE)
      subplot(p_ly,
              p2_ly, nrows = 1, shareX = TRUE, #shareY = TRUE,
              titleX = T, titleY = T, margin = c(0.005, 0.005, .8, 0.005),
              widths = c(.9,.1)) %>%
        layout(legend = list(
          orientation = "h",      # Orientación horizontal
          x = 0.5,                # Centrar la leyenda en el eje x
          y = -0.5,               # Colocar la leyenda debajo del gráfico
          xanchor = "center",     # Anclar la leyenda al centro
          yanchor = "top",
          font = list(size = 14) # Anclar en la parte superior de la leyenda
        ), 
        showlegend=T,showlegend2=F#, 
        # annotations = list(
        #   list(
        #     x = 0.5,            # Posición horizontal centrada
        #     y = -0.15,           # Colocar arriba del gráfico
        #     text = "Rate (%)",  # Texto del título
        #     xref = "paper",
        #     yref = "paper",
        #     showarrow = FALSE,
        #     font = list(size = 16)
        #   )
        # )
        
        )
      
    
   

    
    # p_whole
  })
 
  
  
  output$region_text <- renderText({
    paste0("Evolution by sex, Region: ", input$region, " Indicator: ", input$indicator)
  })


  ## Page 2. Data Explorer ###########################################


  output$Table <- DT::renderDataTable({

# Data
    table <- baloo() %>% 
      select(-c(CVE_Reg, Subregion))
    names(table) <- str_to_upper(names(table))
    
    datatable(table)
    
  })
  

}
