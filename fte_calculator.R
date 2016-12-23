library(shiny)
library(knitr)

ui <- fluidPage(
        pageWithSidebar(
                headerPanel("FTE Calculator"),
                sidebarPanel(
                numericInput(inputId = "vol", label = "Volume of Work", value = 300, step = 100),
                numericInput(inputId = "aht", label = "Average Handle Time Per Unit of Work", value = 300, step = 10),
                numericInput(inputId = "vol_step", label = "Step Volume By", value = 10, step = 10),
                numericInput(inputId = "aht_step", label = "Step Average Handle Time By", value = 10, step = 10),
                numericInput(inputId = "mo_hrs", label = "FTE Hours per Unit of Time", value = 168, step = 1),
                numericInput(inputId = "occ", label = "Occupancy (1 for Processing)", value = 0.75),
                sliderInput(inputId = "util", label = "Utilization of Staff", value = 0.65, min = 0.30, max = 1),
                sliderInput(inputId = "nc", label = "Non-Conformance (Non-Compliance)", value = .04, min = 0, max = 0.20)
                ),
                mainPanel(tableOutput("table1"))
        )
)

server <- function(input, output) {
              fte_table <- reactive({
                       vol_x <- seq(from = input$vol - (5 * input$vol_step),
                                    to = input$vol + (5 * input$vol_step), by = input$vol_step)
                        aht_y <- seq(from = input$aht - (10 * input$aht_step),
                                     to = input$aht + (10 * input$aht_step), by = input$aht_step)
                        rsf <- 1/(input$occ * (input$util - input$nc))
                        mat_xy <- aht_y %o% vol_x
                        df <- data.frame(mat_xy)
                        names(df) <- as.character(vol_x)
                        rownames(df) <- aht_y
                        return((round(((df/3600)*rsf)/input$mo_hrs)))
        })
        output$table1 <- renderTable(fte_table(), rownames = TRUE)
}
shinyApp(ui = ui, server = server)
