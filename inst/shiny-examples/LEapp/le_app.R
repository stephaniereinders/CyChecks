library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(CyChecks)

# create data -------------------------------------------------------------

# all depts, all positions, consolidating levels
probs <- c("-AGLS|-LAS|-HSCI") # get rid of where department gets funding from
sals_dept <-
  sals_dept %>%
  filter(!is.na(gender), gender != "*", !is.na(total_salary_paid)) %>%
  mutate(department = str_replace(department,  probs, ""))%>%
  # consolidate some departments
  mutate(department = str_replace(department, "BBMB", "BIOCH/BIOPH"))


# profs, simplified
myprofs <- c("ASST PROF", "ASSOC PROF", "PROF")
profs <-
  sals_dept %>%
  filter(grepl("PROF", position)) %>%
  mutate(prof_simp = ifelse(position %in% myprofs, position, "OTHER"),
         prof_simp = factor(prof_simp, levels = c(myprofs, "OTHER")))

# drop-down menus

department <- sals_dept %>%
  select(department)%>%
  distinct(.)%>%
  arrange(department)%>%
  unlist()%>%
  unname() %>%
  append(., "All departments", 0)
fiscal_year <- c("All years", sort(unique(as.character(sals_dept$fiscal_year))))


# user interface ----------------------------------------------------------

ui <- fluidPage(
  # App Title
  titlePanel("CyChecks"),
  # Navigation panes for dept, organzation, etc...

  navbarPage("Iowa State Salary Data",
      tabPanel("Summary",
                      includeMarkdown("summary.md")),
      tabPanel("Pay by department",
                 mainPanel(
                   fluidRow(
                    column(width = 6,
                          selectizeInput("department", label = ("Department"), # - Based on gender
                                      choices = department,
                                      selected = "AGRONOMY")),
                    column(width = 6,
                           selectizeInput("fiscal_year", label = ("Year"), # - Based on gender
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
      tabPanel("About",
               includeMarkdown("About.md"))
        )
)




# server ------------------------------------------------------------------

server <- function(input, output){
  # All tab----
  dept_all <- reactive({
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

  dept_all_ns <- reactive({
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
    ggplot(data = filter(dept_all(), total_salary_paid < 500000),
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
    ggplot(data = dept_all_ns(),
           aes(x = fiscal_year, y = n, color = gender)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      scale_color_manual(values = c(M = "darkblue",
                                    `F` = "goldenrod")) +
      labs(x = "Fiscal Year", y = "Number of Employees", color = "Gender",
           title = "The number of employees over time") +
      expand_limits(y = 0)+
      theme(legend.position = c(0.01,0.99),
            legend.justification = c(0,1),
            legend.background = element_rect(linetype = "solid", color = "black"),
            plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  })

  # dept_prof ----------------------------------------------------------------
  dept_prof <- reactive({

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

  dept_prof_ns <- reactive({
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

    ggplot(data = filter(dept_prof(), prof_simp != "OTHER"),
           aes(x = gender,
               y = total_salary_paid/1000)) +
      geom_col(data = dept_prof() %>%
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


  # Prof line graph ------------------------------------------------------------
  output$prof2 <- renderPlot({
    ggplot(data = dept_prof_ns(),
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
      expand_limits(y = 0)+
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5))


  })
}

shinyApp(ui, server)

