library(plotly)
source("GetChurningData.R")
shinyUI(fluidPage(br(),
  # Title
  titlePanel("Estimate Your Chance of Credit Approval"),
  
  sidebarPanel(
    h4("Enter your info to calculate your chance:"),
    # ------ inputs -------------------------------
    fluidRow(
      htmlOutput("bank_selector"),
      htmlOutput("card_selector"),
      numericInput("score", "Your Credit Score: ", value = 700, min = 300, max = 850),
      numericInput("Age", "Your Average Age of Accounts (in months)", value = 24, min = 0),
      selectInput("Income", "Your Stated Income: ", choices = sort(unique(credit$Stated.Income)), selected = "$50,000 - $60,000"),
      numericInput("New3", "Number of new accounts last 3 months: ", value = 0, min = 0),
      numericInput("New6", "Number of new accounts last 6 months: ", value = 0, min = 0),
      numericInput("New12", "Number of new accounts last 12 months: ", value = 0, min = 0),
      actionButton("gobutton", "Calculate"),
      helpText("Will take a few seconds. You can check how calculations are made on tab Methodology Details")
    )),
  # ------ outputs ---------------------------
  mainPanel(
    p("We should carefully consider the decision to apply for a credit card. If we get denied, our credits get hurt by an additional hard inquiry while no benefit is gained."),
    p("This app uses data collected on Reddit to estimate your chance of credit card approval. Beware that not many denied applications were submitted, so the results may look more optimistic than reality."),
    p("Data is collected ", a("here", 
                              href = "https://www.reddit.com/r/churning/comments/3igsfx/credit_card_approvaldenial_reporting_mega_thread/")),
    
    br(),
    tabsetPanel(type = "tabs",
                tabPanel("Outputs",
                         h2("Approval Chance Estimates"),
                         p(textOutput("Instant")),
                         p(textOutput("Final")),
                         p(textOutput("helptext1")),
                         tags$head(tags$style("#Warning524{color: red}")),
                         p(htmlOutput("Warning524")),
                         
                         p("------------------------------------------------------------------------------"),
                         br(),
                         h2("Visualize your position among applicants"),
                         helpText("This 3D Graph can be rotated and zoomed. Hold and drag to rotate the graph. Use the scroll on your mouse/touchpad to zoom."),
                         wellPanel(plotlyOutput("plot"))
                ),
                tabPanel("Methodology Details",
                         includeHTML("description.html")
                ),
                tabPanel("Result details",
                         h3("Instant approval:"),
                         p(textOutput("accu_LR_I")),
                         p(textOutput("accu_KNN_I")),
                         p(textOutput("accu_RF_I")),
                         p(textOutput("accu_NN_I")),
                         br(),
                         h3("Final approval:"),
                         p(textOutput("accu_LR_F")),
                         p(textOutput("accu_KNN_F")),
                         p(textOutput("accu_RF_F")),
                         p(textOutput("accu_NN_F"))
                ),
                tabPanel("About",
                         br(),
                         p("Author: ", a("My Website", href = "https://minhp.weebly.com/")),
                         p("Source code: ", a("https://github.com/minh2182000/credit-cards", href = "https://github.com/minh2182000/credit-cards")),
                         p("Want to leave feedback? You can join the discussion ", a("here", href = "https://www.reddit.com/r/churning/comments/6oiq2e/credit_card_approvaldenial_app/"))
                )
    )
  )
)
)
