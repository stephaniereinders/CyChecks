library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(CyChecks)

# create data -------------------------------------------------------------

# all depts, all positions
sals_dept <-
  sals_dept %>%
  filter(!is.na(gender), gender != "*", !is.na(total_salary_paid))


# profs, simplified
myprofs <- c("ASST PROF", "ASSOC PROF", "PROF")
profs <-
  sals_dept %>%
  filter(grepl("PROF", position)) %>%
  mutate(prof_simp = ifelse(position %in% myprofs, position, "OTHER"),
         prof_simp = factor(prof_simp, levels = c(myprofs, "OTHER")))

# drop-down menus
department <- c("All departments", sort(unique(as.character(sals_dept$department))))
fiscal_year <- c("All years", sort(unique(as.character(sals_dept$fiscal_year))))



# user interface ----------------------------------------------------------

ui <- fluidPage(
  # App Title
  titlePanel("CyChecks"),

  # Sidebar drop-downs (department, fiscal_year)
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
      tabPanel("All Employees",
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
               DT::dataTableOutput("profTab"))
      )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output){

  # All tab----
  liq_all <- reactive({
    # Show all departments and all years
    if (input$department == "All departments" & input$fiscal_year == 'All years'){
      sals_dept %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Show all departments but filter on years
    else if (input$department == "All departments"){
      sals_dept %>%
        filter(fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Show all years but filter on department
    else if (input$fiscal_year == "All years"){
      sals_dept %>%
        filter(department == input$department) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Filter on department and year
    else {
      sals_dept %>%
        filter(department == input$department,
               fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }

  })


  liq_all_ns <- reactive({
    # Show all departments
    if (input$department == "All departments") {
      sals_dept %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    } else {
      sals_dept %>%
        filter(department == input$department) %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    }
  })


  # All scatter -------------------------------------------------------------

  output$allDat1 <- renderPlot({
      ggplot(data = filter(liq_all(), total_salary_paid < 500000),
             aes(x = total_salary_paid/1000,
             fill = gender)) +
        geom_density(alpha = 0.5, color = "black") +
        labs(x = NULL, y = "Density", fill = "Gender") +
        theme_bw() +
        scale_fill_manual(values = c(M = "darkblue",
                                      `F` = "goldenrod")) +
        theme(legend.position = c(0.99, 0.99),
              legend.justification = c(1, 1),
              legend.background = element_rect(linetype = "solid",
                                               color = "black"))

  })


  # All line graph chart -----------------------------------------------------------

  output$allDat2 <- renderPlot({
    # Plot for all departments, all years
    ggplot(data = liq_all_ns(),
           aes(x = fiscal_year, y = n, color = gender)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      scale_color_manual(values = c(M = "darkblue",
                                    `F` = "goldenrod")) +
      labs(x = NULL, y = "Number of Employees", color = "Gender",
           title = "If an employee left before Jan 1 2019\n they are not included in our dataset.\nPlease ask ISU to make yearly departmental\naffiliations publicly available.") +
      theme(legend.position = c(0.01,0.99),
            legend.justification = c(0,1),
            legend.background = element_rect(linetype = "solid", color = "black"))
  })


# All datatable -----------------------------------------------------------


  output$allDatTab <- renderDataTable({

    dataset <- liq_all()
    if (input$fiscal_year == 'All years') {

      dataset %>%
        group_by(gender, position) %>%
        summarize(
          n = n(),
          avg_pay = round(mean(total_salary_paid), 0)) %>%
        dplyr::mutate(fiscal_year = "all years") %>%
        dplyr::select(fiscal_year, gender, n, avg_pay) %>%
        arrange(fiscal_year, position, gender) %>%
        dplyr::rename(
          "Gender" = gender,
          "Position Title" = position,
          "Fiscal Year" = fiscal_year,
          "Mean Total Salary" = avg_pay
        ) %>%
        DT::datatable() %>%
        DT::formatCurrency("Mean Total Salary", interval = 3,
                           mark = ",",  digits = 0)
    } else {
      dataset %>%
        group_by(fiscal_year, gender, position) %>%
        summarize(n = n(),
                  avg_pay = round(mean(total_salary_paid), 0)) %>%
        arrange(fiscal_year, position, gender) %>%
        dplyr::rename("Gender" = gender,
                      "Position Title" = position,
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
      profs
    }
    # Show all departments but filter on years
    else if (input$department == "All departments"){
      profs %>%
        filter(fiscal_year == input$fiscal_year)
    }
    # Show all years but filter on department
    else if (input$fiscal_year == "All years"){
      profs %>%
        filter(department == input$department)
    }
    # Filter on department and year
    else {
      profs %>%
        filter(department == input$department,
               fiscal_year == input$fiscal_year)
    }

  })


  # liq_prof_ns -------------------------------------------------------------
  liq_prof_ns <- reactive({
    # Show all departments
    if (input$department == "All departments") {
      profs %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    } else {
      profs %>%
        filter(department == input$department) %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    }
  })

  # Prof scatter + bar ------------------------------------------------------------
  output$prof1 <- renderPlot({

    ggplot(data = filter(liq_prof(), prof_simp != "OTHER"),
           aes(x = gender,
               y = total_salary_paid/1000)) +
      geom_col(data = liq_prof() %>%
                 filter(prof_simp != "OTHER") %>%
                 group_by(prof_simp, gender) %>%
                 summarise(total_salary_paid = mean(total_salary_paid)),
                           aes(x = gender,
                               y = total_salary_paid/1000,
                               fill = gender)) +
      geom_point(color = "white", size = 2, pch = 21, fill = "black") +
      scale_fill_manual(values = c(M = "darkblue",
                                    `F` = "goldenrod")) +
      labs(x = NULL,
           y = "Total Salary Paid\nThousands of $",
           color = NULL,
           title = "Professors with modified titles\nare not included in this figure") +
      theme_bw() +
      guides(color = F, fill = F) +
      facet_wrap(~prof_simp) #+
      #theme(legend.position = "top",
      #      legend.background = element_rect(linetype = "solid", color = "black"))
  })


  # Prof line graph ------------------------------------------------------------
  output$prof2 <- renderPlot({
    ggplot(data = liq_prof_ns(),
           aes(x = fiscal_year,
               y = n,
               color = gender,
               group = gender)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      labs(x = NULL,
           y = "Number of Employees\nWIth Professor Titles",
           color = "Gender",
           title = "If an employee left before Jan 1 2019\n they are not included in our dataset.\nPlease ask ISU to make yearly departmental\naffiliations publicly available.") +
      scale_color_manual(values = c(M = "darkblue",
                                   `F` = "goldenrod")) +
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
        group_by(gender) %>%
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
        group_by(fiscal_year, gender, prof_simp) %>%
        summarize(n = n(),
                  avg_pay = round(mean(total_salary_paid), 0)) %>%
        arrange(fiscal_year, prof_simp, gender) %>%
        dplyr::rename("Gender" = gender,
                      "Professor Title" = prof_simp,
                      "Fiscal Year" = fiscal_year,
                      "Mean Total Salary" = avg_pay) %>%

        DT::datatable() %>%
        DT::formatCurrency("Mean Total Salary", interval = 3, mark = ",", digits = 0)

    }


  })

}

shinyApp(ui, server)

