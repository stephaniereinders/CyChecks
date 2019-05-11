library(shiny)
require(ggplot2)
require(dplyr)
require(tidyr)
require(stringr)
require(DT)

# read in data
#load(file = "sals_dept.rda")
#load(file = "sals_dept_profs.rda")
sals_dept <- sals_dept %>% filter(!is.na(gender), gender != "*")
sals_dept_profs <- sals_dept_profs %>% filter(!is.na(gender), gender != "*")
department <- c("All departments", sort(unique(as.character(sals_dept$department))))
fiscal_year <- c("All years", sort(unique(as.character(sals_dept$fiscal_year))))


ui <- fluidPage(
  # App Title
  titlePanel("CyChecks"),

  # Sidebar # - Based on gender
  sidebarPanel(
    selectInput("department", label = ("Department"), # - Based on gender
                choices = department,
                selected = "AGRONOMY"),
    selectInput("fiscal_year", label = ("Year"), # - Based on gender
                choices = fiscal_year,
                selected = "2018")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("All",
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("allDat1"),
                             plotOutput("allDat2"))
               ),
               #plotOutput(outputId = "allDat"),
               #plotOutput(outputId = "allDat2"),
               DT::dataTableOutput("allDatTab")),
      tabPanel("Professors",
               #plotOutput(outputId = "prof"),
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("prof1"),
                             plotOutput("prof2"))
               ),
               DT::dataTableOutput("profTab")),
      tabPanel("Post Docs",
               #plotOutput(outputId = "postdoc"),
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("postdoc1"),
                             plotOutput("postdoc2"))
               ),
               DT::dataTableOutput("postdocTab"))
    )
  )
)

# server
server <- function(input, output){

  # All tab----
  liq_all <- reactive({
    # Show all departments and all years
    if (input$department == "All departments" & input$fiscal_year == 'All years'){
      sals_dept %>%
        filter(!is.na(total_salary_paid)) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Show all departments but filter on years
    else if (input$department == "All departments"){
      sals_dept %>%
        filter(!is.na(total_salary_paid), fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Show all years but filter on department
    else if (input$fiscal_year == "All years"){
      sals_dept %>%
        filter(!is.na(total_salary_paid), department == input$department) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Filter on department and year
    else {
      sals_dept %>%
        filter(!is.na(total_salary_paid),
               department == input$department,
               fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }

  })


  liq_all_ns <- reactive({
    # Show all departments
    if (input$department == "All departments") {
      sals_dept %>%
        filter(!is.na(total_salary_paid), !is.na(gender), gender != "*") %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    } else {
      sals_dept %>%
        filter(!is.na(total_salary_paid),
               !is.na(gender),
               gender != "*",
               department == input$department) %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    }
  })


  # All scatter -------------------------------------------------------------

  output$allDat1 <- renderPlot({
    # Plot for all departments, all years
    if (input$department == "All departments"){
      ggplot(data = liq_all(), aes(x = gender,
                                   y= total_salary_paid/1000,
                                   color = gender)) +
        geom_jitter(size = 2, width = 0.2, alpha = 0.5) +
        stat_summary(fun.y = mean, geom = "line", color = "gray") +
        stat_summary(fun.y = mean, geom = "point", size = 3,  pch = 17) +
        labs(x = NULL, y = "Total Salary Paid\nThousands of $", color = "Gender") +
        theme_bw() +
        scale_color_manual(values = c("darkblue", "goldenrod")) +
        theme(legend.position = c(0.01, 0.99),
              legend.justification = c(0, 1),
              legend.background = element_rect(linetype = "solid",
                                               color = "black"))

    }
    # Plot for single department, all years
    else {
      ggplot(data = liq_all() %>% filter(gender != "*"),
             aes(x = gender, y = total_salary_paid/1000, color = gender, group = position)) +
        geom_jitter(size = 2, width = 0.2, alpha = 0.5) +
        stat_summary(fun.y = mean, geom = "line", color = "gray") +
        stat_summary(fun.y = mean, geom = "point", size = 3,  pch = 17) +
        theme_bw() +
        labs(x = NULL, y = "Total Salary Paid\nThousands of $", color = "Gender") +
        scale_color_manual(values = c("darkblue", "goldenrod")) +
        theme(legend.position = c(0.01, 0.99),
              legend.justification = c(0, 1),
              legend.background = element_rect(linetype = "solid",
                                               color = "black"))

    }

  })


  # All bar chart -----------------------------------------------------------

  output$allDat2 <- renderPlot({
    # Plot for all departments, all years
    ggplot(data = liq_all_ns(),
           aes(x = fiscal_year, y = n, fill =gender)) +
      geom_col() +
      theme_bw() +
      scale_fill_manual(values = c("darkblue", "goldenrod")) +
      labs(x = NULL, y = "Number of Employees", fill = "Gender") +
      theme(legend.position = c(0.01,0.99),
            legend.justification = c(0,1),
            legend.background = element_rect(linetype = "solid", color = "black"))
  })

  output$allDatTab <- renderDataTable({

    dataset <- liq_all()
    if (input$fiscal_year == 'All years') {

      dataset %>%
        group_by(gender) %>%
        summarize(
          n = n(),
          avg_pay = round(mean(total_salary_paid), 0)) %>%
        dplyr::mutate(fiscal_year = "all years") %>%
        dplyr::select(fiscal_year, gender, n, avg_pay) %>%
        dplyr::rename(
          "Gender" = gender,
          "Fiscal Year" = fiscal_year,
          "Mean Total Salary" = avg_pay
        ) %>%
        DT::datatable() %>%
        DT::formatCurrency("Mean Total Salary", interval = 3,
                           mark = ",",  digits = 0)
    } else {
      dataset %>%
        group_by(fiscal_year, gender)%>%
        summarize(n = n(),
                  avg_pay = round(mean(total_salary_paid), 0)) %>%
        dplyr::rename("Gender" = gender,
                      "Fiscal Year" = fiscal_year,
                      "Mean Total Salary" = avg_pay) %>%
        DT::datatable() %>%
        DT::formatCurrency("Mean Total Salary", interval = 3, mark = ",", digits = 0)
    }
  })


  # liq_prof ----------------------------------------------------------------
  liq_prof <- reactive({

    # Show all departments and all years
    if (input$department == "All departments" & input$fiscal_year == 'All years'){
      sals_dept_profs %>%
        filter(!is.na(total_salary_paid)) %>%
        select("total_salary_paid","travel_subsistence",
               "gender", "position_simplified","fiscal_year")
    }
    # Show all departments but filter on years
    else if (input$department == "All departments"){
      sals_dept_profs %>%
        filter(!is.na(total_salary_paid),fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid","travel_subsistence",
               "gender", "position_simplified","fiscal_year")
    }
    # Show all years but filter on department
    else if (input$fiscal_year == "All years"){
      sals_dept_profs %>%
        filter(!is.na(total_salary_paid),department == input$department) %>%
        select("total_salary_paid","travel_subsistence",
               "gender", "position_simplified","fiscal_year")
    }
    # Filter on department and year
    else {
      sals_dept_profs %>%
        filter(!is.na(total_salary_paid),
               department == input$department,
               fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid","travel_subsistence",
               "gender", "position_simplified","fiscal_year")
    }

  })


  # liq_prof_ns -------------------------------------------------------------
  liq_prof_ns <- reactive({
    # Show all departments
    if (input$department == "All departments") {
      sals_dept_profs %>%
        filter(!is.na(total_salary_paid), !is.na(gender), gender != "*") %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    } else {
      sals_dept_profs %>%
        filter(!is.na(total_salary_paid),
               !is.na(gender),
               gender != "*",
               department == input$department) %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    }
  })

  # Prof scatter ------------------------------------------------------------
  output$prof1 <- renderPlot({
    ggplot(data = liq_prof(),
           aes(x = gender,
               y = total_salary_paid/1000,
               color = position_simplified,
               group = position_simplified)) +
      geom_jitter(size = 2, width = 0.2, alpha = 0.5, pch = 19) +
      stat_summary(fun.y = mean, geom = "line", size = 2) +
      stat_summary(fun.y = mean, geom = "point", size = 3,  pch = 17) +
      labs(x = NULL, y = "Total Salary Paid\nThousands of $", color = NULL) +
      theme_bw() +
      scale_color_brewer(palette = "Spectral") +
      theme(legend.position = "top",
            legend.background = element_rect(linetype = "solid", color = "black"))
  })


  # Prof bar graph ------------------------------------------------------------
  output$prof2 <- renderPlot({
    ggplot(data = liq_prof_ns(),
           aes(x = fiscal_year,
               y = n,
               fill = gender)) +
      geom_col() +
      theme_bw() +
      labs(x = NULL,
           y = "Number of Employees",
           fill = "Gender") +
      scale_fill_manual(values = c("darkblue", "goldenrod")) +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"
        ))


  })
  # Data table
  output$profTab <- renderDataTable({
    dataset <- liq_prof()
    if (input$fiscal_year == 'All years') {
      dataset %>%
        mutate(fiscal_year = as.character(fiscal_year),
               fiscal_year2 = as.numeric(fiscal_year)) %>%
        group_by(gender)%>%
        summarize(n = n(),
                  avg_pay = round(mean(total_salary_paid), 0),
                  min_year = min(fiscal_year2),
                  max_year = max(fiscal_year2)) %>%
        mutate(fiscal_year = paste0(min_year, "-", max_year)) %>%
        dplyr::select(fiscal_year, gender, n, avg_pay) %>%
        dplyr::rename("Gender" = gender,
                      "Fiscal Year" = fiscal_year,
                      "Mean Total Salary" = avg_pay) %>%
        DT::datatable() %>%
        DT::formatCurrency("Mean Total Salary", interval = 3, mark = ",", digits = 0)



    } else {
      dataset %>%
        group_by(fiscal_year, gender)%>%
        summarize(n = n(),
                  avg_pay = round(mean(total_salary_paid), 0)) %>%
        dplyr::rename("Gender" = gender,
                      "Fiscal Year" = fiscal_year,
                      "Mean Total Salary" = avg_pay) %>%
        DT::datatable() %>%
        DT::formatCurrency("Mean Total Salary", interval = 3, mark = ",", digits = 0)

    }


  })

  # Post-doc tab----
  liq_postdoc <- reactive ({

    # Show all departments and all years
    if (input$department == "All departments" & input$fiscal_year == 'All years'){
      sals_dept %>%
        filter(grepl('POSTDOC', position)) %>%
        filter(!is.na(total_salary_paid)) %>%
        select("total_salary_paid","travel_subsistence","gender", "fiscal_year")
    }
    # Show all departments but filter on years
    else if (input$department == "All departments"){
      sals_dept %>%
        filter(grepl('POSTDOC', position)) %>%
        filter(!is.na(total_salary_paid),
               fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid","travel_subsistence","gender", "fiscal_year")
    }
    # Show all years but filter on department
    else if (input$fiscal_year == "All years"){
      sals_dept %>%
        filter(grepl('POSTDOC', position)) %>%
        filter(!is.na(total_salary_paid),
               department == input$department) %>%
        select("total_salary_paid","travel_subsistence","gender", "fiscal_year")
    }
    # Filter on department and year
    else {
      sals_dept %>%
        filter(grepl('POSTDOC', position)) %>%
        filter(!is.na(total_salary_paid),
               department == input$department,
               fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid","travel_subsistence","gender", "fiscal_year")
    }


  })

  # liq_postdocs_ns -------------------------------------------------------------
  liq_postdoc_ns <- reactive({
    # Show all departments
    if (input$department == "All departments") {
      sals_dept %>%
        filter(grepl('POSTDOC', position)) %>%
        filter(!is.na(total_salary_paid), !is.na(gender), gender != "*") %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    } else {
      sals_dept %>%
        filter(grepl('POSTDOC', position)) %>%
        filter(!is.na(total_salary_paid),
               !is.na(gender),
               gender != "*",
               department == input$department) %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    }
  })

  # Post-doc scatter ------------------------------------------------------------
  output$postdoc1 <- renderPlot({
    ggplot(data = liq_postdoc(),
           aes(x = gender,
               y = total_salary_paid/1000,
               color = gender)) +
      geom_jitter(size = 2, width = 0.2, alpha = 0.5, pch = 19) +
      stat_summary(fun.y = mean, geom = "line", size = 2, color = "gray") +
      stat_summary(fun.y = mean, geom = "point", size = 5, pch = 17) +
      labs(x = NULL, y = "Total Salary Paid\nThousands of $", color = "Gender") +
      theme_bw() +
      scale_color_manual(values = c("darkblue", "goldenrod")) +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"))

  })


  # Post-doc bar graph ------------------------------------------------------------
  output$postdoc2 <- renderPlot({
    ggplot(data = liq_postdoc_ns(),
           aes(x = fiscal_year,
               y = n,
               fill = gender)) +
      geom_col() +
      theme_bw() +
      labs(x = NULL,
           y = "Number of Employees",
           fill = "Gender") +
      scale_fill_manual(values = c("darkblue", "goldenrod")) +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"
        ))


  })
  # Data table
  output$postdocTab <- renderDataTable({
    dataset <- liq_postdoc()

    if (input$fiscal_year == 'All years') {

      dataset %>%
        group_by(gender) %>%
        summarize(
          n = n(),
          avg_pay = round(mean(total_salary_paid), 0)) %>%
        dplyr::mutate(fiscal_year = "all years") %>%
        dplyr::select(fiscal_year, gender, n, avg_pay) %>%
        dplyr::rename(
          "Gender" = gender,
          "Fiscal Year" = fiscal_year,
          "Mean Total Salary" = avg_pay
        ) %>%
        DT::datatable() %>%
        DT::formatCurrency(
          "Mean Total Salary",
          interval = 3,
          mark = ",",
          digits = 0
        )

    } else {
      dataset %>%
        group_by(fiscal_year, gender)%>%
        summarize(n = n(),
                  avg_pay = round(mean(total_salary_paid), 0)) %>%
        dplyr::rename("Gender" = gender,
                      "Fiscal Year" = fiscal_year,
                      "Mean Total Salary" = avg_pay) %>%
        DT::datatable() %>%
        DT::formatCurrency("Mean Total Salary", interval = 3, mark = ",", digits = 0)
    }


  })
}

shinyApp(ui, server)

