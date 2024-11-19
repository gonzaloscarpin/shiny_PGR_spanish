#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(rsconnect)

rsconnect::setAccountInfo(name='gonzaloscarpin', 
                          token='B16652A3B5A654F1159005D76A2520A4', 
                          secret='0pYK9Q6ZgdP1twj/RG8SkRWUtam4SRrzNHltNnlT')


# Define UI for the app
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Modelo para el calculo de la necesidad de Regulador de crecimiento en algodón"),
    sidebarLayout(
        sidebarPanel(
            numericInput("final_height", "Altura final deseada (cm):", value = 100, min = 0),
            numericInput("actual_height", "Altura actual (cm):", value = 93.3, min = 0),
            numericInput("num_nodes", "Número de nudos actual:", value = 14, min = 1),
            numericInput("row_distance", "Distancia entre surcos (cm):", value = 91, min = 0),
            numericInput("n_plants", "N de plantas por m:", value = 6, min = 0),
            numericInput("branch_node", "Nudo de la 1era rama reproductiva:", value = 6, min = 0),
            numericInput("product_concentration", "Concentración del regulador (%):", value = 5, min = 0),
            numericInput("product_applied", "Dosis de regulador aplicada (ml/ha):", value = 0, min = 0),
            actionButton("calculate", "Calculate", class = "btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Results", verbatimTextOutput("calculations"))
            )
        )
    )
)

# Define server logic for the app
server <- function(input, output) {
    observeEvent(input$calculate, {
        area_factor <- 0.7
        light_interception <- ifelse(1.0756 * input$actual_height / input$row_distance > 1, 1, 1.0756 * (input$actual_height / input$row_distance))
        rowspace_factor <- 1 + ifelse(area_factor - light_interception > 0, area_factor - light_interception, 0)
        density_factor <- ifelse(input$n_plants <= 10, (1 + (10 - input$n_plants) * 0.07), 1 - input$n_plants * 0.007)
        
        if (input$num_nodes < 6) {
            plant_weight <- -0.10055 - 0.018657 * input$num_nodes + 0.148958 * input$actual_height + 0.0026535 * input$actual_height^2
        } else {
            plant_weight <- -6.5 + 0.534446 * input$actual_height + 0.009 * input$num_nodes^2
        }
        
        fruting_branch_position <- ifelse(input$branch_node <= 6, 0, (input$branch_node - 6) * 1)
        adjusted_plant_weight <- plant_weight * rowspace_factor * density_factor
        gms_MC_per_liter <- input$product_concentration * 10
        height_node_ratio <- round(input$actual_height/input$num_nodes,1)
        days_nodes <- c(0.8, 1.7, 2.5, 4.2, 6.7, 10, 14.2, 19.2, 24.2, 30, 35.8, 41.7, 47.5, 54.2, 60, 65.8, 71.7, 77.5, 82.5, 87.5, 91.7, 95, 97.5, 99.2, 100)
        days_nodes_value <- days_nodes[input$num_nodes]
        x_value <- input$final_height * (days_nodes_value / 120)
        dif_porc <- ((input$actual_height - days_nodes_value) / days_nodes_value) * 100
        pre_PPM_value <- (dif_porc / 10)
        PPM <- ifelse(dif_porc <= 10, 10, 10 + pre_PPM_value + fruting_branch_position)
        plants_ha <- input$n_plants * ((100 / input$row_distance) * 10000)
        
        recommended_PGR_rate <- ifelse(dif_porc > 10, (((adjusted_plant_weight * plants_ha) / 1000) * PPM) / gms_MC_per_liter, 0)
        final_recommended_PGR_rate <- recommended_PGR_rate - input$product_applied
        final_recommended_PGR_rate_show <- ifelse(final_recommended_PGR_rate < 0, 0, round(final_recommended_PGR_rate, 1))
        
        output$calculations <- renderPrint({
            cat("Relación altura - número de nudos:", height_node_ratio, "\n")
            cat("Dosis ajustada de regulador recomendada (ml/ha):", final_recommended_PGR_rate_show, "\n")
        })
        
        output$data_table <- renderDT({
            data.frame(
                "Final Height (cm)" = input$final_height,
                "Actual Height (cm)" = input$actual_height,
                "Number of Nodes" = input$num_nodes,
                "Row Distance (cm)" = input$row_distance,
                "N Plants/m" = input$n_plants,
                "Planted Date" = input$planted_date,
                "1st Branch Node" = input$branch_node,
                "Product Concentration (%)" = input$product_concentration,
                "Product Applied (ml/ha)" = input$product_applied,
                "Area Factor" = area_factor,
                "Light Interception" = light_interception,
                "Rowspace Factor" = rowspace_factor,
                "Density Factor" = density_factor,
                "Plant Weight" = plant_weight,
                "Adjusted Plant Weight" = adjusted_plant_weight,
                "Gms MC per Liter" = gms_MC_per_liter,
                "Days Nodes Value" = days_nodes_value,
                "X Value" = x_value,
                "Difference Percentage" = dif_porc,
                "Pre PPM Value" = pre_PPM_value,
                "PPM" = PPM,
                "Plants per Hectare" = plants_ha,
                "Recommended PGR Rate (ml/ha)" = recommended_PGR_rate,
                "Final Recommended PGR Rate (ml/ha)" = final_recommended_PGR_rate_show
            )
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

