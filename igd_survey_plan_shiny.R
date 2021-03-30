# Clear workspace
rm(list=ls())
options(scipen = 999)

# Load essential packages
library(shiny)
library(leaflet)
library(gapminder)
library(ggplot2)
library(shiny)
library(gganimate)

# Load data
dat <- 
  readRDS(paste0(here::here(), "/shiny_test_data.rds")) %>% 
  dplyr::rename(unique_mg = group_id) %>% 
  dplyr::mutate(
    unique_mg = factor(unique_mg), 
    rollout_wave = factor(rollout_wave),
    measurement_group = factor(measurement_group)
  )

# Specify nice colors for measurement groups
col <- 
  c(
    "1_1" = "#E69F00",
    "1_0" = "#000000",
    "2_1" = "#56B4E9",
    "2_0" = "#000000",
    "3_1" = "#009E73",
    "3_0" = "#000000",
    "4_1" = "#0072B2",
    "4_0" = "#000000",
    "5_1" = "#D55E00",
    "5_0" = "#000000",
    "6_1" = "#CC79A7",
    "6_0" = "#000000"
  )

# Specify custom names for legend text
my_breaks <- c("1_1", "2_1", "3_1", "4_1", "5_1", "6_1")
my_labels <- c("1", "2", "3", "4", "5", "6")

# Prep data
dat2 <- 
  dat %>% 
  dplyr::distinct(
    district, 
    tehsil_id, 
    gp_id, 
    x_cent, 
    y_cent, 
    rollout_wave, 
    measurement_group, 
    month, 
    mg_surv, 
    unique_mg,
    measurement_sequence
    ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(
    gp_id, 
    x_cent, 
    y_cent, 
    rollout_wave, 
    measurement_group, 
    month, 
    mg_surv, 
    unique_mg,
    measurement_sequence
    ) %>% 
  as.data.frame(.)

# Begin app UI
ui <- 
  fluidPage(
    
    # App title
    titlePanel("Overview of Survey Procedure"),
    
    # Sidebar layout 
    sidebarLayout(
      
      # Sidebar panel 
      sidebarPanel(
        
        # Input: Selector for variable to plot
        selectInput(
          "variable", "Variable:",
          c("Measurement groups" = "mg_surv"),
          selected = "mg_surv"
        ),
        
        # Month slider
        sliderInput(
          inputId = "month", "Month", 
          min = 5,
          max = 22, 
          step = 1, 
          value = range(5),
          animate = animationOptions(
            interval = 5000, 
            loop = FALSE,
            playButton = "Animate",
            pauseButton = "Animate"
          )
        ),
        
        # Location of points
        fluidRow(
          column(
            width = 10,
            h4("Points near click"),
            verbatimTextOutput("click_info")
          ),
          column(
            width = 10,
            h4("Brushed points"),
            verbatimTextOutput("brush_info")
          )
        )
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        plotOutput(
          "beep", 
          width = "100%", 
          height = "400px"
        ), 
        plotOutput(
          "boop",
          click = "plot1_click", 
          brush = brushOpts(
            id = "plot1_brush"
          ), 
          width = "100%", 
          height = "800px"
        )
      )
    )
  )

# Server
server <- 
  function(input, output) {
    
    # Prep data 
    dat3 <- 
      reactive({
        dat2 <- dat[dat$month %in% seq(from=min(input$month), to=max(input$month), by = 1),]
        print(dat2)
        dat2
      })
    
    # Nearest points
    output$click_info <- 
      renderPrint({
        nearPoints(
          as.data.frame(
            dat3() %>% 
              dplyr::ungroup() %>% 
              dplyr::select(
                district, 
                tehsil_id, 
                gp_id, 
                rollout_wave, 
                measurement_group, 
                month,
                measurement_sequence,
                x_cent, 
                y_cent
              )
          ), 
          input$plot1_click, 
          addDist = FALSE
        )
      })
    
    # Aggregate points
    output$brush_info <- 
      renderPrint({
        brushedPoints(
          as.data.frame(
            dat3() %>% 
              dplyr::ungroup() %>% 
              dplyr::select(
                district, 
                tehsil_id, 
                gp_id, 
                rollout_wave, 
                measurement_group, 
                month,
                measurement_sequence,
                x_cent, 
                y_cent
              )
          ), 
          input$plot1_brush
        )
      })
    
    # Final output
    output$boop <- 
      renderPlot({
        ggplot(
          dat3(), 
          mapping = aes(
            x = x_cent, 
            y = y_cent
          )
        ) + 
          geom_point(
            aes_string(
              color = input$variable
            ),  
            size = 4
          ) +
          ggConvexHull::geom_convexhull(
            alpha = .5, 
            mapping = aes(
              fill = factor(unique_mg)
            ), 
            color = NA
          ) +
          scale_fill_manual(
            values = rep("lightblue", 54), 
            guide = FALSE
          ) + 
          scale_color_manual(
            values = col, 
            breaks = my_breaks, 
            labels = my_labels
          ) +
          labs(
            title = "Study Area",
            subtitle = "Sundargarh and Kendujhar", 
            y = "", 
            x = "",
            color = "Measurement group",
            caption = "Black points indicate GPs that are not surveyed in that given measurement group, cluster, and month."
          ) + 
          theme(
            title = element_text(size = 20),
            axis.text = element_text(size = 16), 
            legend.text = element_text(size = 16)
          )
      })
    
  }

app <- shinyApp(ui, server)

app
