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
fiscal_year <- c(sort(unique(as.character(sals_dept$fiscal_year))))



# user interface ----------------------------------------------------------


ui <- fluidPage(
  # App Title
  titlePanel("CyChecks"),

  # Sidebar drop-downs (department, fiscal_year)
  sidebarPanel(
    selectInput(
      "department",
      label = ("Department"),
      # - Based on gender
      choices = department,
      selected = "AGRONOMY"
    ),
    selectInput(
      "fiscal_year",
      label = ("Year"),
      # - Based on gender
      choices = fiscal_year,
      selected = "2018"
    )
  ),
  mainPanel(tabsetPanel(
    tabPanel(
      "All Employees",
      fluidRow(splitLayout(
        cellWidths = c("50%", "50%"),
        plotOutput("allfig1"),
        plotOutput("allfig2")
      )),
      #plotOutput(outputId = "allDat"),
      #plotOutput(outputId = "allDat2"),
      DT::dataTableOutput("alltbl")
    ),

    tabPanel(
      "Top 10",
      plotOutput(outputId = "top10fig"),
      DT::dataTableOutput("top10tbl")
    ),

    tabPanel("Professors",
             #plotOutput(outputId = "prof"),
             fluidRow(
               splitLayout(
                 cellWidths = c("50%", "50%"),
                 plotOutput("proffig1"),
                 plotOutput("proffig2")
               )
             ),
             DT::dataTableOutput("top10tbl"))

  ))
)


### SERVER####

server <- function(input, output){


# All data ----------------------------------------------------------------


  liq_all <- reactive({
    if (input$department == "All departments") {
      sals_dept %>%
        filter(fiscal_year == input$fiscal_year)
    }
    else {
      sals_dept %>%
        filter(department == input$department,
               fiscal_year == input$fiscal_year)
    }

  })


  liq_all_ns <- reactive({
    if (input$department == "All departments") {
      sals_dept %>%
        filter(fiscal_year == input$fiscal_year) %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    } else {
      sals_dept %>%
        filter(department == input$department,
               fiscal_year == input$fiscal_year) %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    }
  })


# All fig1 (density) ------------------------------------------------------


  output$allfig1 <- renderPlot({

    ggplot(data = (liq_all() %>% filter(total_salary_paid < 500000)),
           aes(x = total_salary_paid/1000,
               fill = gender)) +
        geom_density(alpha = 0.5, color = "black") +
        labs(x = "Total Salary Paid\nThousands of $",
             #y = "Proportion of Employees",
             fill = "Gender",
             title = "This figure is filtered to include only\nemployees earning less than $500,000/yr") +
      guides(color = F) +
      theme_bw() +
      scale_fill_manual(values = c(M = "darkblue", `F` = "goldenrod")) +
      coord_cartesian(xlim = c(0, 500)) +
        theme(legend.position = c(0.99, 0.99),
              legend.justification = c(1, 1),
              legend.background = element_rect(linetype = "solid",
                                               color = "black"))

    })


  # All fig 2 (line graph) -----------------------------------------------------------

  output$allfig2 <- renderPlot({

    ggplot(data = liq_all_ns(),
           aes(x = fiscal_year,
               y = n,
               color = gender)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      scale_color_manual(values = c(M = "darkblue", `F` = "goldenrod")) +
      labs(x = "Fiscal Year\n",
           y = "Number of Employees",
           color = "Gender",
           title = "If an employee left before Jan 1 2019\n they are not included in our dataset.\nPlease ask ISU to make yearly departmental\naffiliations publicly available.") +
      theme(legend.position = c(0.01,0.99),
            legend.justification = c(0,1),
            legend.background = element_rect(linetype = "solid", color = "black"))
  })


# All datatable -----------------------------------------------------------


  output$alltbl <- renderDataTable({

    liq_all() %>%
        group_by(fiscal_year, gender, position, department) %>%
        summarize(n = n(),
                  avg_pay = round(mean(total_salary_paid), 0)) %>%
        arrange(fiscal_year, position, gender) %>%
        dplyr::rename("Gender" = gender,
                      "Position Title" = position,
                      "Fiscal Year" = fiscal_year,
                      "Mean Total Salary" = avg_pay,
                      "Department" = department) %>%
        DT::datatable() %>%
        DT::formatCurrency("Mean Total Salary", interval = 3, mark = ",", digits = 0)
    })


  # prof data ----------------------------------------------------------------

  liq_prof <- reactive({

    if (input$department == "All departments"){
      profs %>%
        filter(fiscal_year == input$fiscal_year,
               prof_simp != "OTHER")
    }
    else {
      profs %>%
        filter(department == input$department,
               fiscal_year == input$fiscal_year,
               prof_simp != "OTHER")
    }

  })

  liq_prof_ns <- reactive({
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


  # Prof fig 1 (scatter + bar) ------------------------------------------------------------
  output$proffig1 <- renderPlot({
    ggplot(data = liq_prof(),
           aes(x = gender,
               y = total_salary_paid/1000)) +
      geom_col(data = liq_prof() %>%
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
           title = "Professors With Modified Titles\nAre Not Included In This Figure") +
      theme_bw() +
      guides(color = F, fill = F) +
      facet_wrap(~prof_simp)
    })


  # Prof fig2 (line graph) ------------------------------------------------------------
  output$proffig2 <- renderPlot({
    ggplot(data = liq_prof_ns(),
           aes(x = fiscal_year,
               y = n,
               color = gender,
               group = gender)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      labs(x = NULL,
           y = "Number of Employees With Professor Titles",
           color = "Gender") +
      scale_color_manual(values = c(M = "darkblue",
                                   `F` = "goldenrod")) +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"
        ))


  })

  # Prof data table ---------------------------------------------------------

  output$proftbl <- renderDataTable({
    liq_prof() %>%
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

    })


# top10 fig (bar) ---------------------------------------------------------------

  output$top10fig <- renderPlot({

    top10dat <- liq_all() %>%
                   top_n(10, total_salary_paid) %>%
                   mutate(rank = rank(total_salary_paid, ties.method="first")) %>%
                   select(fiscal_year, position, total_salary_paid, rank, gender) %>%
                   arrange(-rank) %>%
                   mutate(rank2 = as.character(rank))

    ggplot(data = top10dat,
           aes(x = reorder(rank2, -rank),
               y = total_salary_paid/1000,
               fill = gender)) +
      geom_col() +
      theme_bw() +
      labs(x = NULL,
           y = "Sal",
           color = "Gender") +
      scale_color_manual(values = c(M = "darkblue",
                                    `F` = "goldenrod")) +
      scale_x_discrete(labels = top10dat$position) +

      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"
        ))


  })
  # Data table
  output$top10tbl <- renderDataTable({
    dataset <- liq_all() %>%
      top_n(10, total_salary_paid) %>%
      mutate(rank = rank(total_salary_paid, ties.method="first")) %>%
      select(fiscal_year, position, total_salary_paid, rank, gender) %>%
      arrange(-rank) %>%
        dplyr::rename("Position Title" = position,
                      "Fiscal Year" = fiscal_year,
                      "Total Salary" = total_salary_paid) %>%
      select(-rank) %>%

        DT::datatable() %>%
        DT::formatCurrency("Total Salary", interval = 3, mark = ",", digits = 0)

    })

}

shinyApp(ui, server)

