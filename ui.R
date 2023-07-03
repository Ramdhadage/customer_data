library(shiny)
library(shinydashboard)
source("customer_updated.R")
ui <- dashboardPage(
  dashboardHeader(title = "CA One"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Menu 1", tabName = "menu1",
        menuSubItem("Tab a", tabName = "tab1"),
        menuSubItem("Tab b", tabName = "tab2"),
        menuSubItem("Tab c", tabName = "tab3"),
        menuSubItem("Tab d", tabName = "tab4")
      ),
      menuItem(
        "Menu 2", tabName = "menu2",
        menuSubItem("Tab a", tabName = "tab5"),
        menuSubItem("Tab b", tabName = "tab6"),
        menuSubItem("Tab c", tabName = "tab7")
      ),
      menuItem(
        "Menu 3", tabName = "menu3",
        menuSubItem("Tab a", tabName = "tab9"),
        menuSubItem("Tab b", tabName = "tab11"),
        menuSubItem("Tab c", tabName = "tab12")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab1",
        h3("Basic density plot in ggplot2"),
        plotOutput("Menu1Tab1Plot3"),
        h3("Barplot on the variable Type"),
        plotOutput("Menu1Tab1Plot1"),
        h3("Boxplot on the variable type and ratings of the earphone"),
        plotOutput("Menu1Tab1Plot2"),

      ),
      tabItem(
        tabName = "tab2",
        fluidPage(
          h3("Summary of Income"),
          verbatimTextOutput("summaryOutput1"),
          h3("variance of Income"),
          textOutput("varianceText1"),
          h3("SD of Income"),
          textOutput("SDText1")

        )
      ),
      tabItem(
        tabName = "tab3",
        fluidPage(
          h1("Chebyshev's rule on Rating"),
          h3("Mean of Rating"),
          textOutput("MeanText1"),
          h3("SQRT of Rating"),
          textOutput("SQRTText1"),
          plotOutput("Menu1_Tab3_Plot1_1"),
        )
      ),
      tabItem(
        tabName = "tab4",
        fluidPage(
          h3("BoxPlot of Rating"),
          plotOutput("Menu1_Tab4_Plot1_1"),
          plotOutput("Menu1_Tab4_Plot2_1"),
        )
      ),
      tabItem(
        tabName = "tab5",
        h2("Fitting multiple Regression model"),
        h4("The interpretation of the results from fitting the multiple regression probability model on different response variables provides valuable insights into the relationship between the selected variables and the level of uncertainty associated with each variable's impact on the response. By examining the coefficients or parameter estimates, we can determine the direction and magnitude of the relationship between the independent variables and the response variables. Additionally, assessing the significance of these coefficients using statistical tests helps identify variables that have a statistically significant impact on the response. By quantifying the uncertainty for each variable, we can gauge their relative importance in explaining the variability in the response. Variables with lower uncertainty are considered more reliable in their contribution to the model, while variables with higher uncertainty may have a less certain influence on the response. Overall, this interpretation provides a deeper understanding of the significance and uncertainty of the selected variables and their role in predicting the response variables within the dataset.")
      ),
      tabItem(
        tabName = "tab6",
        h3("Estimate parameters"),
        verbatimTextOutput("MCText1_1")
        # h3("Model 2 coefficients"),
        # verbatimTextOutput("MCText2_1"),
        # h3("Model 3 coefficients"),
        # verbatimTextOutput("MCText3_1"),
        # h3("Model 4 coefficients"),
        # verbatimTextOutput("MCText4_1"),
      ),
      tabItem(
        tabName = "tab7",
        fluidPage(

          h3("Predictive Analytics"),
          verbatimTextOutput("myTable1_1"),
          # DT::dataTableOutput("myTable1_1"),

          # h3("Model 2 - Predicting the target variable"),
          # DT::dataTableOutput("myTable2_1"),

          # h3("Model 3 - Predicting the target variable"),
          # DT::dataTableOutput("myTable3_1"),

          # h3("Model 4 - Predicting the target variable"),
          # DT::dataTableOutput("myTable4_1"),
        )
      ),
      tabItem(
        tabName = "tab9",
        fluidPage(
          h2("Hypothesis"),
          h4("H0: attributes are independent"),
          h4("H1: attributes are not independent"),
          verbatimTextOutput("Menu4_Tab1_1"),
          h4("p value is less than l.o.s.  therefore we can say that attributes may not be independent.")
        )
      ),
      tabItem(
        tabName = "tab11",
        fluidPage(
          h2("Goodness of fit test"),
          verbatimTextOutput("Menu4_Tab2_1")
        )
      ),
      tabItem(
        tabName = "tab12",
        fluidPage(
          h2("Test of mean"),
          textInput(inputId = "mu_value_1", label = "Enter mu value:", value = "60000"),
          verbatimTextOutput("Menu4_Tab3_1")
        )
      )
    )
  )
)
