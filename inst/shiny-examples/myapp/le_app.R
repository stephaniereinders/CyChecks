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
organization <- c("All organizations", sort(unique(as.character(sals_dept$organization))))
fiscal_year <- c("All years", sort(unique(as.character(sals_dept$fiscal_year))))



# user interface ----------------------------------------------------------

ui <- fluidPage(
  # App Title
  titlePanel("CyChecks"),
  # Navigation panes for dept, organzation, etc...

  navbarPage("Iowa State Salary Data",
      tabPanel("Pay by department",
                 mainPanel(
                   fluidRow(
                    column(width = 6,
                          selectInput("department", label = ("Department"), # - Based on gender
                                      choices = department,
                                      selected = "AGRONOMY")),
                    column(width = 6, selectInput("fiscal_year", label = ("Year"), # - Based on gender
                                         choices = fiscal_year,
                                         selected = "2018"))),

                    fluidRow(
                      column(width = 12, align = "center", h3(em("All Employees")),
                        fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                  plotOutput("allDat1"),
                                  plotOutput("allDat2"))
                        )
                    )),

                    br(),

                    fluidRow(
                      column(width = 12, align = "center", h3(em("Professors")),
                             fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                  plotOutput("prof1"),
                                  plotOutput("prof2"))
                        )
                      )
            ),
            br(),
            br()
        )),
      tabPanel("Pay by college",
               mainPanel(
                 fluidRow(
                  column(width = 6,
                        selectInput("organization", label = ("Organization"), # - Based on gender
                                    choices = organization,
                                    selected = "COLLEGE of AGRICULTURE & LIFE SCIENCES")),
                  column(width = 3, selectInput("fiscal_year", label = ("Year"), # - Based on gender
                                    choices = fiscal_year,
                                    selected = "2018"))
               ),
               fluidRow(
                 column(width = 12, align = "center", h3(em("All Employees")),
                        fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                             plotOutput("allDat3"),
                                             plotOutput("allDat4"))
                        )
                 )),

               br(),

               fluidRow(
                 column(width = 12, align = "center", h3(em("Professors")),
                        fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                             plotOutput("prof3"),
                                             plotOutput("prof4"))
                                )
                 )
        ))
        ),
      tabPanel("Problem Areas"),
      tabPanel("About")
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

  org_all <- reactive({
    # Show all colleges and all years
    if (input$organization == "All organizations" & input$fiscal_year == 'All years'){
      sals_dept %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Show all college but filter on years
    else if (input$organization == "All organizations"){
      sals_dept %>%
        filter(fiscal_year == input$fiscal_year) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Show all years but filter on college
    else if (input$fiscal_year == "All years"){
      sals_dept %>%
        filter(organization == input$organization) %>%
        select("total_salary_paid", "gender", "position", "fiscal_year")
    }
    # Filter on college and year
    else {
      sals_dept %>%
        filter(organization == input$organization,
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

  org_all_ns <- reactive({
    # Show all colleges
    if (input$organization == "All organizations") {
      sals_dept %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    } else {
      sals_dept %>%
        filter(organization == input$organization) %>%
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
      labs(x = "Salary (in thousands of $)", y = "Density", fill = "Gender",
           title = "Density plot of employee salaries, by gender") +
      theme_bw() +
      scale_fill_manual(values = c(M = "darkblue",
                                   `F` = "goldenrod")) +
      theme(legend.position = c(0.99, 0.99),
            legend.justification = c(1, 1),
            legend.background = element_rect(linetype = "solid",
                                             color = "black"),
            plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

  })

  # All scatter for colleges
  output$allDat3 <- renderPlot({
    ggplot(data = filter(org_all(), total_salary_paid < 500000),
           aes(x = total_salary_paid/1000,
               fill = gender)) +
      geom_density(alpha = 0.5, color = "black") +
      labs(x = "Salary (in thousands of $)", y = "Density", fill = "Gender",
           title = "Density plot of employee salaries, by gender") +
      theme_bw() +
      scale_fill_manual(values = c(M = "darkblue",
                                   `F` = "goldenrod")) +
      theme(legend.position = c(0.99, 0.99),
            legend.justification = c(1, 1),
            legend.background = element_rect(linetype = "solid",
                                             color = "black"),
            plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

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
      labs(x = "Fiscal Year", y = "Number of Employees", color = "Gender",
           title = "The number of employees over time") +
      theme(legend.position = c(0.01,0.99),
            legend.justification = c(0,1),
            legend.background = element_rect(linetype = "solid", color = "black"),
            plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  })
  # separate graph for colleges
  output$allDat4 <- renderPlot({
    # Plot for all departments, all years
    ggplot(data = org_all_ns(),
           aes(x = fiscal_year, y = n, color = gender)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      scale_color_manual(values = c(M = "darkblue",
                                    `F` = "goldenrod")) +
      labs(x = "Fiscal Year", y = "Number of Employees", color = "Gender",
           title = "The number of employees over time") +
      theme(legend.position = c(0.01,0.99),
            legend.justification = c(0,1),
            legend.background = element_rect(linetype = "solid", color = "black"),
            plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
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
  # profs for colleges
  org_prof <- reactive({

    # Show all college and all years
    if (input$organization == "All organizations" & input$fiscal_year == 'All years'){
      profs
    }
    # Show all college but filter on years
    else if (input$organization == "All organizations"){
      profs %>%
        filter(fiscal_year == input$fiscal_year)
    }
    # Show all years but filter on college
    else if (input$fiscal_year == "All years"){
      profs %>%
        filter(organization == input$organization)
    }
    # Filter on college and year
    else {
      profs %>%
        filter(organization == input$organization,
               fiscal_year == input$fiscal_year)
    }

  })

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

  org_prof_ns <- reactive({
    # Show all departments
    if (input$organization == "All organizations") {
      profs %>%
        group_by(fiscal_year, gender) %>%
        summarise(n = n())
    } else {
      profs %>%
        filter(organization == input$organization) %>%
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
           y = "Total Salary Paid\nthousands of $",
           color = NULL,
           title = "Salaries of three professor positions, \nin Thousands of $") +
      theme_bw() +
      guides(color = F, fill = F) +
      facet_wrap(~prof_simp)+
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))#+
    #theme(legend.position = "top",
    #      legend.background = element_rect(linetype = "solid", color = "black"))
  })

  # same graph for colleges
  output$prof3 <- renderPlot({

    ggplot(data = filter(org_prof(), prof_simp != "OTHER"),
           aes(x = gender,
               y = total_salary_paid/1000)) +
      geom_col(data = org_prof() %>%
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
           y = "Total Salary Paid\nthousands of $",
           color = NULL,
           title = "Salaries of three professor positions, \nin Thousands of $") +
      theme_bw() +
      guides(color = F, fill = F) +
      facet_wrap(~prof_simp)+
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))#+
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
      labs(x = "Fiscal Year",
           y = "Number of Employees",
           color = "Gender",
           title = "The number of employees with \n'professor' titles over time") +
      scale_color_manual(values = c(M = "darkblue",
                                    `F` = "goldenrod")) +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5))


  })
  output$prof4 <- renderPlot({
    ggplot(data = org_prof_ns(),
           aes(x = fiscal_year,
               y = n,
               color = gender,
               group = gender)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      labs(x = "Fiscal Year",
           y = "Number of Employees",
           color = "Gender",
           title = "The number of employees with \n'professor' titles over time") +
      scale_color_manual(values = c(M = "darkblue",
                                    `F` = "goldenrod")) +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5))


  })
}

shinyApp(ui, server)

