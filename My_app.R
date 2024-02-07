pacman::p_load(
  shiny, 
  bslib,
  ggpubr,
  ggplot2
  
)
# Load the here package
library(here)
library(readr)
library(rsconnect)
library(dplyr)
library(janitor)
library(scales)


# there is an another file named Rscript.R attached in Github repository which contains cleaning, mutating , filtering of raw data.

# "df_final.csv" file is located within the "Desktop/new project/Interactive_webpage" directory
df_final <- read_csv(here("df_final.csv"))


Code_of_issues <- c( "01","02",
                     "03","04","05",
                     "06","07",
                     "08","9","10",
                     "11","12",
                     "13","14",
                     "15","16","17")




# Define UI for the application that draws a barplot
library(shiny)

ui <- fluidPage(
  titlePanel("Patient Review System"),
  style = "background-color: #F5F5DC;",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "select_hospital",
        label = "Select hospital",
        choices = c(
          "All",
          "Sir.T hospital",
          "HCG hospital"
        )
      ),
      selectInput(
        inputId = "select_department",
        label = "Select department",
        choices = c(
          "Cardiology",
          "Neurology",
          "Orthopedics",
          "Pediatrics",
          "Dermatology"
        )
      ),
      textOutput("selected_var"),
      tags$br(), 
      tags$b("Code of Issues:"),
      tags$ul(
        tags$li(
          tags$b("Code No.1"), " - Poor Sanitation ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.2"), " - Bureaucracy and administration malfunction ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.3"), " - Staff shortage ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.4"), " - Low quality care ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.5"), " - Overcrowding ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.6"), " - Overuse of health facilities ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.7"), " - Increased out pocket payment ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.8"), " - Rude and unprofessional behaviour ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.9"), " - Medical negligence ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.10"), " - Inadequate facilities for treatment ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.11"), " - Availability of drugs ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.12"), " - Lack of essential medicine ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.13"), " - Inequal access of healthcare or racism ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.14"), " - Poor food quality ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.15"), " - Poor emergency services ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.16"), " - Lack of transparency regarding treatment ",
          style = "list-style-type: disc; margin-left: 20px;"
        ),
        tags$li(
          tags$b("Code No.17"), " - Language barrier ",
          style = "list-style-type: disc; margin-left: 20px;"
        )
      )
    ),
    
    mainPanel(
      plotOutput("Barplot"),
      tags$br(), 
      tags$br(), 
     plotOutput("PieChart"),
      
      tags$br(), 
      tags$br(), 
      tags$b("Public Hospital System in India:"),
      tags$br(), 
      tags$br(),
      tags$ul(
        tags$li(tags$b("Overview:")),
        tags$br(),
        tags$ul(
          tags$li("The public hospital system is essentially free for all Indian residents except for small, often symbolic co-payments in some services. Since the country's independence, the public hospital system has been entirely funded through general taxation."),
          tags$li("Indian citizens have access to free outpatient and inpatient care in public, government facilities. It also collaborates with international public health organizations to gather more knowledge and direct discussions around needs and improvements to the current system.")
        ),
        tags$br(), 
        tags$br(), 
        tags$li(tags$b("Model Description")),
        tags$br(),
        tags$ul(
          tags$li("Step 1: Patients can access the Online Review form by scanning a Barcode which is placed everywhere in the hospital."), 
          tags$li("Step 2: Data obtained from the form will be collected into an online database."), 
          tags$li("Step 3: Every month, this collected data will be evaluated by the Federal Government of Health and Hospital Administration can both access the data."), 
          tags$li("Step 4: Data Visualization will be performed by R programming and an interactive webpage will be available on the Center for Drug Standard Control Organization and Health Ministry and welfare website.")
        ),
        tags$br(),
        tags$br(),
        tags$li(tags$b("Benefits of this Review System:")),
        tags$br(),
        tags$ul(
          tags$li("Comparative study: One public hospital can learn from other hospitals regarding the issue which it is facing."),
          tags$li("Techniques of improvisation can be learned by a particular hospital, which has poorer performance, from a hospital which has better performance. This co-learning helps the hospitals to apply practical solutions which are not possible if they tried to solve theoretically."),
          tags$li("Hospital administration and State Government can both be benefited by the aggregated data, as hospital staff can review the issues occurring in the system on a monthly basis and state government can evaluate and analyze the problems regarding service delivery, finances, and administration. Sources of data are patients themselves, so data collected is authentic. Moreover, there is no external influence which can modify the data."),
          tags$li("Central Government /Health Ministry of India can compare the service delivery of hospital to the budget provided. So when a hospital doesn't deliver up to the mark then it can be guided accordingly.")
        ),
        tags$br(), 
        tags$br(), 
        tags$li(tags$b("Agenda of this Review System:")),
        tags$br(),
        tags$ul(
          tags$li("Improvisation of Healthcare service and administration by reviewing feedbacks."),
          tags$li("Patient satisfaction."),
          tags$li("Comparative data can provide an overview of all public hospitals, which helps to decide the amount of National Health Budget"),
          tags$li("Public Health experts can have real-life data to evaluate the issues and identify trends regarding healthcare delivery.")
        )
      ),
      tags$br(), 
      tags$br(), 
      tags$li(tags$b("References and other:")),
      tags$br(), 
      tags$br(),
      tags$a(
        href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6166510/",
        "References for content :
        Kasthuri A. Challenges to Healthcare in India - The Five A's.
        Indian J Community Med. 2018 Jul-Sep;43(3):141-143. 
        doi: 10.4103/ijcm.IJCM_194_18.
        PMID: 30294075; PMCID: PMC6166510.
        https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6166510/"
      ),
      tags$br(),
      tags$br(),
      
      tags$a(
        href = "https://airtable.com/appPHhrdHoqZGJRHJ/shrczpWVevf29Urpk" ,
        "Link to online form:
        https://airtable.com/appPHhrdHoqZGJRHJ/shrczpWVevf29Urpk "
      ),
      tags$br(),
      tags$br(),
      tags$br()
    )
  )
)


server <- function(input, output, session) {
 
   # Code for the selected variable---------------------------------
  output$selected_var <- renderText({
    paste("You have selected", input$select_department, "Department of", input$select_hospital)
  })

    # code for plot output--------------------------------------------
  filtered_data <- reactive({
    data <- df_final %>%
      dplyr::filter(hospital == input$select_hospital)%>%
      filter(department == input$select_department)
  })
  
  # renderplot for barplot----------------------------------------------
  output$Barplot <- renderPlot({
    if (!is.null(filtered_data())) {
      ggplot(filtered_data(), aes(x = Code_of_Issues, y = Avg_Rating, fill = Code_of_Issues)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Average Patient Rating for Healthcare Challenges per Month",
             x = "Code of Issues",
             y = "Avg Rating") +
        theme_minimal()+
        theme(plot.background = element_rect(fill = "#F0F0F0"))+
        scale_x_continuous(breaks = seq(1, 17, 1))
    } else {
      # Handle case when filtered data is NULL or empty
      ggplot() + 
        geom_blank() +
        labs(title = "No Data Available",
             x = "Code of Issues",
             y = "Avg Rating") +
        theme_minimal()+
        theme(plot.background = element_rect(fill = "#FFDAB9"))
    }
  })
  
# renderplot for piechart---------------------------------
  output$PieChart <- renderPlot({
    if (!is.null(filtered_data())) {
      # Define custom fill colors for each code of issue
      custom_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "orange", "5" = "purple",
                         "6" = "cyan", "7" = "magenta", "8" = "yellow", "9" = "darkgreen", "10" = "darkred",
                         "11" = "darkblue", "12" = "darkorange", "13" = "#8B008B", "14" = "darkcyan",
                         "15" = "darkmagenta", "16" = "brown", "17" = "pink")
      
      # Calculate total rating for scaling to percentage
      total_rating <- sum(filtered_data()$Avg_Rating)
      
      ggplot(filtered_data(), aes(x = "", y = Avg_Rating, fill = factor(Code_of_Issues))) +
        geom_bar(stat = "identity") +
        coord_polar(theta = "y") +
        labs(title = "Distribution of Healthcare Challenges",
             fill = "Code of Issues") +
        scale_fill_manual(values = custom_colors) +  
        theme_minimal() +
        theme(plot.background = element_rect(fill = "#F0F0F0")) +
        geom_text(aes(label = paste0(round((Avg_Rating / total_rating) * 100, 2), "%")), 
                  position = position_stack(vjust = 0.5), size = 3)  # Display percentage labels with 2 digits
    } else {
      ggplot() + 
        geom_blank() +
        labs(title = "No Data Available",
             fill = "Code of Issues") +
        theme_minimal()
    }
  })
  
}
shinyApp(ui = ui, server = server)
