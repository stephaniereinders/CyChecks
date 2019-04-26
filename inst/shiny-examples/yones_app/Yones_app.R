
library(shiny)
require(ggplot2)
require(dplyr)

#Cysalary <- read.csv("C:/Users/ykh/Desktop/Cysalary.csv")
Cysalary <- data("all_sals")

ui <- fluidPage(
  # App Title
  titlePanel("Cysalary"),

  # Sidebar # - Based on gender
  sidebarPanel(
    selectInput("gender", label = ("Gender"), # - Based on gender
                choices = sort(unique(Cysalary$gender)),
                selected = F)
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Total Sales", plotOutput(outputId = "boxplot")),
      tabPanel("Travel Subsistence", plotOutput(outputId = "travel_subsistence"))
    )
  ),
  verbatimTextOutput("summary")
)

# server
server <- function(input, output){
  liq_subset <- reactive({
    Cysalary %>%
      filter(gender == input$gender) %>%
      select("total_salary_paid","travel_subsistence")
  })
  output$boxplot <- renderPlot({
    boxplot(liq_subset(),
            xlab = " ",
            main = "Boxplot of Salary")
  })
  output$summary <- renderPrint({
    dataset <- liq_subset()
    summary(dataset)
  })
  liq_subset1 <- reactive({
    Cysalary %>%
      filter(gender == input$gender) %>%
      select("total_salary_paid","travel_subsistence","place_of_residence")
  })
  output$travel_subsistence <- renderPlot({
    ggplot(data = liq_subset1(), aes(x = place_of_residence, y = total_salary_paid))+
      geom_col()+
      theme_bw()+
      ggtitle(paste("Total Salary per County", input$Year))
  })
}


shinyApp(ui, server)

