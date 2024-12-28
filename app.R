library(shiny)
library(tidyverse)
library(tidymodels)
library(bslib)
library(xgboost)
library(shinyWidgets)
library(ggplot2)
library(leaflet)
library(sf)
library(plotly)

# Load all data
mn_df_final <- readRDS("data/mn_df_final.rds")
mn_df_final <- as_tibble(mn_df_final)
importance_plot <- readRDS("data/importance_plot.rds")
final_model <- readRDS("model/final_model.rds")
data_prep <- readRDS("model/data_prep.rds")
eth_adm_merged2 <- st_read("data/eth_adm_merged2/eth_adm_merged2.shp")  # Spatial data

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C3E50",
  "navbar-bg" = "#2C3E50"
)

# UI
ui <- page_navbar(
  title = div(
    tags$span("Childhood Micronutrient Deficiency Prediction", style = "font-size: 24px;"),
    tags$span(" | ", style = "color: #666;"),
    tags$span("by Leykun Getaneh", style = "font-style: italic;")
  ),
  theme = my_theme,
  
  nav_panel(
    "Prediction Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Input Parameters",
        
        accordion(
          open = TRUE,
          accordion_panel(
            "Demographics",
            icon = icon("users"),
            selectInput("region", "Region", choices = levels(mn_df_final$region)),
            selectInput("residence", "Residence", choices = levels(mn_df_final$residence)),
            selectInput("mother_age", "Mother's Age", choices = levels(mn_df_final$mother_age)),
            selectInput("child_age_months", "Child's Age (Months)", 
                        choices = levels(mn_df_final$child_age.months))
          ),
          
          accordion_panel(
            "Socioeconomic Factors",
            icon = icon("coins"),
            selectInput("wealth_index", "Wealth Index", choices = levels(mn_df_final$wealth_index)),
            selectInput("education_level", "Education Level", 
                        choices = levels(mn_df_final$education_level)),
            selectInput("media_exposure", "Media Exposure", 
                        choices = levels(mn_df_final$media_exposure))
          ),
          
          accordion_panel(
            "Reproductive and Maternal Health",
            icon = icon("baby"),
            selectInput("current_pregnant", "Current Pregnant", 
                        choices = levels(mn_df_final$current_pregnant)),
            selectInput("currently_breastfeeding", "Currently Breastfeeding",
                        choices = levels(mn_df_final$currently_breastfeeding)),
            selectInput("no_children", "Number of Children", 
                        choices = levels(mn_df_final$no_children))
          ),
          
          accordion_panel(
            "Healthcare Access",
            icon = icon("hospital"),
            selectInput("ANC_visit", "ANC Visit", choices = levels(mn_df_final$ANC_visit)),
            selectInput("place_delivery", "Place of Delivery", 
                        choices = levels(mn_df_final$place.delivery)),
            selectInput("health_check_after_delivery", "Health Check After Delivery",
                        choices = levels(mn_df_final$health_check_after_delivery)),
            selectInput("PNC_check", "PNC Check", choices = levels(mn_df_final$PNC.check))
          )
        )
      ),
      # Prediction boxes 
      layout_columns(
        col_widths = c(6, 6),
        heights_equal = "row",
        value_box(
          title = "MN Deficiency Probability",
          value = textOutput("mn_prob"),
          showcase = icon("percentage"),
          theme = "warning"
          # max_height = "100px"
        ),
        uiOutput("prediction_box")
      ),
      
      # Spatial visualization and feature importance
      layout_column_wrap(
        width = 1/2,
        height = 650,
        card(
          full_screen = TRUE,
          card_header("Spatial MN Deficiency Prevalence by Regions"),
          card_body(
            class = "p-0",
            leafletOutput("prevalence_map")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Feature Importance"),
          plotlyOutput("feature_importance_plot")
        )
      )
    )
  ),
  
  nav_panel(
    "About",
    card(
      card_header("About This Tool"),
      "This application predicts childhood micronutrient deficiency using machine learning.",
      "It uses an XGBoost model trained on various demographic and health-related features.",
      "The model was developed using the tidymodels framework and achieves good predictive performance.",
      tags$br(),
      tags$br(),
      "For more information, contact: ",
      tags$a(href = "mailto:leyk.get@gmail.com", "leyk.get@gmail.com")
    )
  )
)

# Server
server <- function(input, output) {
  # Prediction logic
  new_data <- reactive({
    tibble(
      region = input$region,
      residence = input$residence,
      media_exposure = input$media_exposure,
      no_children = input$no_children,
      wealth_index = input$wealth_index,
      current_pregnant = input$current_pregnant,
      currently_breastfeeding = input$currently_breastfeeding,
      mother_age = input$mother_age,
      education_level = input$education_level,
      ANC_visit = input$ANC_visit,
      place.delivery = input$place_delivery,
      health_check_after_delivery = input$health_check_after_delivery,
      PNC.check = input$PNC_check,
      child_age.months = input$child_age_months
    ) %>% 
      mutate(id = "1")
  })
  
  pred_prob <- reactive({
    pred <- predict(final_model, new_data(), type = "prob")
    return(pred$.pred_1)
  })
  
  # Outputs
  output$mn_prob <- renderText({
    sprintf("%.1f%%", pred_prob() * 100)
  })
  
  output$prediction_box <- renderUI({
    prob <- pred_prob()
    theme_color <- if(prob >= 0.5) "danger" else "success"
    prediction_text <- if(prob >= 0.5) "MN Deficient" else "MN Sufficient"
    
    value_box(
      title = "Predicted Outcome",
      value = prediction_text,
      showcase = icon("user-check"),
      theme = theme_color
    )
  })
  
  # Interactive map
  output$prevalence_map <- renderLeaflet({
    leaflet(eth_adm_merged2) %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      
      addPolygons(
        fillColor = ~colorNumeric(palette = "Reds", 
                                  domain = eth_adm_merged2$prevalence)(prevalence),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<strong>Region: </strong>", region,
          "<br><strong>Prevalence: </strong>", 
          round(prevalence, 2)
          # round(prevalence, 2)*100, "%"
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal"),
          textsize = "13px",
          direction = "auto",
          offset = c(0, -5),
          sticky = TRUE
        )
      ) %>%
      addLegend(
        pal = colorNumeric(palette = "Reds", 
                           domain = eth_adm_merged2$prevalence),
        values = eth_adm_merged2$prevalence,
        title = "<strong>MN Deficiency<br>Prevalence</strong>",
        # title = "Prevalence of MN Deficiency",
        position = "topright"
      ) %>%
      # Add layer control
      addLayersControl(
        baseGroups = c("Satellite", "Dark", "Light"),
        position = "topright",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      setView(lng = 40, lat = 9, zoom = 5) %>%  # Center on Ethiopia
      setMaxBounds(lng1 = 32, lat1 = 3,         # SW corner
                   lng2 = 48, lat2 = 15)         # NE corner
  })

  # Interactive feature importance plot
  output$feature_importance_plot <- renderPlotly({
    top_importance_plot <- importance_plot %>%
      top_n(7, Value) %>%
      arrange(desc(Value))
    
    p <- ggplot(top_importance_plot, 
                aes(x = reorder(Variable, Value), y = Value, text = Variable)) +
      geom_col(fill = "#2C3E50") +
      coord_flip() +
      labs(x = "Features", y = "Importance Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)
