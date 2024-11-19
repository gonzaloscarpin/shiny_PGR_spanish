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
library(tidyverse)
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
            numericInput("final_height", "Altura final deseada (cm):", value = 70, min = 0),
            numericInput("actual_height", "Altura actual (cm):", value = 0, min = 0),
            numericInput("num_nodes", "Número de nudos actual:", value = 10, min = 1),
            numericInput("row_distance", "Distancia entre surcos (cm):", value = 52, min = 0),
            numericInput("n_plants", "N de plantas por m:", value = 12, min = 0),
            numericInput("branch_node", "Nudo de la 1era rama reproductiva:", value = 6, min = 0),
            numericInput("product_concentration", "Concentración del regulador (%):", value = 5, min = 0),
            numericInput("product_applied", "Dosis de regulador aplicada (ml/ha):", value = 0, min = 0),
            actionButton("calculate", "CALCULAR", class = "btn-primary")
        ),
        mainPanel(
            
                tabPanel("Resultado", verbatimTextOutput("calculations")),
                tabPanel("Gráfico:", plotOutput("heightPlot"))
            
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
        days_when_nn_equal <- c(1,2,3,5,8,12,17,23,29,36,43,50,57,65,72,79,86,93,99,105,110,114,117,119,120)
        relation_nn_height <- input$final_height * (days_when_nn_equal / 120)
        days_nodes_value <- days_when_nn_equal[input$num_nodes]
        x_value <- input$final_height * (relation_nn_height[input$num_nodes] / 120)
        dif_porc <- ((input$actual_height - relation_nn_height[input$num_nodes]) / relation_nn_height[input$num_nodes]) * 100
        pre_PPM_value <- (dif_porc / 10)
        PPM <- ifelse(dif_porc <= 10, 10, 10 + pre_PPM_value + fruting_branch_position)
        plants_ha <- input$n_plants * ((100 / input$row_distance) * 10000)
        
        recommended_PGR_rate <- ifelse(dif_porc > 10, (((adjusted_plant_weight * plants_ha) / 1000) * PPM) / gms_MC_per_liter, 0)
        final_recommended_PGR_rate <- recommended_PGR_rate - input$product_applied
        final_recommended_PGR_rate_show <- ifelse(final_recommended_PGR_rate < 0, 0, round(final_recommended_PGR_rate, 1))
        
        output$calculations <- renderPrint({
            cat("Relación altura - número de nudos:", height_node_ratio, "\n")
            cat("Número de plantas por ha:", round(plants_ha,0), "\n")
            cat("Dosis ajustada de regulador recomendada (ml/ha):", final_recommended_PGR_rate_show, "\n")
        })
        
        output$heightPlot <- renderPlot({
            nodes <- 0:26
            heights <- seq(0, input$final_height, length.out = length(nodes))
            relation_nn_height <- input$final_height * (days_when_nn_equal / 120)
            
            df <- data.frame(
                Nodes = nodes,
                Heights = heights,
                Relation_NN_Height = c(0, relation_nn_height[1:26])
            )
            
            ggplot(df, aes(x = Nodes)) +
                geom_line(aes(y = Relation_NN_Height, 
                              color = "Altura teórica")) +
                geom_point(aes(x = input$num_nodes, 
                               y = input$actual_height, 
                               color = "Altura actual"), 
                           size = 3) +
                scale_x_continuous(breaks = 0:26) +
                scale_y_continuous(breaks = seq(0, input$final_height, by = 10)) +
                labs(x = "Número de nudos",
                     y = "Altura de planta (cm)",
                     title = "Modelo teórico de crecimiento de planta",
                     color = "Legend"
                ) +
                scale_color_manual(values = c("Altura teórica" = "blue", 
                                              "Altura actual" = "red")) +
                theme_minimal() +
                theme(legend.position = c(0.8, 0.25))
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
