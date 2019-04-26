library(shiny)
require(ggplot2)
require(dplyr)
library(DT)

# read in data
load(file = "sals_dept.rda")
sals_dept <- sals_dept %>% filter(!is.na(gender))


ui <- fluidPage(
  # App Title
  titlePanel("CyChecks"),

  # Sidebar # - Based on gender
  sidebarPanel(
    selectInput("department", label = ("Department"), # - Based on gender
                choices = sort(unique(sals_dept$department)),
                selected = "AGRONOMY")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("All", plotOutput(outputId = "allDat"), DT::dataTableOutput("allDatTab")),
      tabPanel("Professors", plotOutput(outputId = "prof"), DT::dataTableOutput("profTab")),
      tabPanel("Post Docs", plotOutput(outputId = "postdoc"), DT::dataTableOutput("postdocTab"))
    )
  )
)

# server
server <- function(input, output){
  liq_all <- reactive({
    sals_dept %>%
      filter(department == input$department) %>%
      select("total_salary_paid", "gender", "position")
  })
  output$allDat <- renderPlot({
    ggplot(data = liq_all(), aes(x = gender, y= total_salary_paid, color = position, group = position)) +
      geom_jitter(size = 2, width = 0.2, alpha = 0.5) +
      stat_summary(fun.y = mean, geom = "line") +
      stat_summary(fun.y = mean, geom = "point", size = 3) +
      theme_bw()
  })
  output$allDatTab <- renderDataTable({
    dataset <- liq_all()
    summary(dataset)
  })
  liq_prof <- reactive({
    sals_dept %>%
      filter(department == input$department) %>%
      filter(grepl('PROF', position)) %>%
      select("total_salary_paid","travel_subsistence","gender", "position", "fiscal_year")
  })
  output$prof <- renderPlot({
    ggplot(data = liq_prof(),
           aes(x = gender,
               y = total_salary_paid/1000,
               color = position,
               group = position)) +
      geom_jitter(size = 2, width = 0.2, alpha = 0.2) +
      stat_summary(fun.y = mean, geom = "line", size = 2) +
      stat_summary(fun.y = mean, geom = "point", size = 3) +
      theme_bw() +
      labs(x = NULL, y = "Total Salary Paid\nThousands of $")
  })
  output$profTab <- renderDataTable({
    dataset <- liq_prof()
    dataset %>%
      group_by(fiscal_year, gender)%>%
      summarize(n = n(), avg_pay = signif(mean(total_salary_paid), 6))
  })
  liq_postdoc<- reactive ({
    sals_dept %>%
      filter(department == input$department)%>%
      filter(grepl('POSTDOC', position)) %>%
      select("total_salary_paid","travel_subsistence","gender", "fiscal_year")
  })
  output$postdoc <- renderPlot({
    ggplot(data = liq_postdoc(),
           aes(x = gender,
               y = total_salary_paid/1000)) +
      geom_jitter(size = 3, width = 0.2, alpha = 0.5, aes(color = gender)) +
      stat_summary(fun.y = mean, geom = "line") +
      stat_summary(fun.y = mean, geom = "point", size = 5) +
      theme_bw()+
      scale_color_manual(values = c("tomato", "dodgerblue2")) +
      labs(x = NULL, y = "Total Salary Paid\nThousands of $") +
      guides(color = F)
  })
  output$postdocTab <- renderDataTable({
    dataset <- liq_postdoc()
    dataset %>%
      group_by(fiscal_year, gender)%>%
      summarize(n = n(), avg_pay = signif(mean(total_salary_paid), 6))
  })
}

shinyApp(ui, server)

