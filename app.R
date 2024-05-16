library(shiny)
library(echarts4r)
library(dplyr)


ui <- fluidPage(
  titlePanel("Updating Trend Chart"),
  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start Updates"),
      actionButton("stop", "Stop Updates")
    ),
    mainPanel(
      echarts4rOutput("trendChart", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(data.frame(
    time = Sys.time() - 10:1,
    value = rnorm(10)
  ))
  
  updating <- reactiveVal(FALSE)
  
  output$trendChart <- renderEcharts4r({
    data() %>%
      e_charts(time, dispose = FALSE) %>% #dispose set to FALSE to prevent the refreshing of whole plot
      e_line(value) %>%
      e_tooltip(trigger = "axis") %>%
      e_datazoom(startValue = -14)
  })
  
  observe({
    if (updating()) {
      invalidateLater(1000, session)
      isolate({
        new_data <- data.frame(
          time = Sys.time(),
          value = rnorm(1)
        )
        data(rbind(data(), new_data))
      })
      echarts4rProxy("trendChart") %>%
        e_line(data(), time, value, serie_index = 0)
    }
  })
  
  observeEvent(input$start, {
    updating(TRUE)
  })
  
  observeEvent(input$stop, {
    updating(FALSE)
  })
}

shinyApp(ui = ui, server = server)
