
library(shiny)
library(readxl)
library(ggplot2)
library(recipes)
library(dplyr)
library(rsconnect)
rsconnect::setAccountInfo(name='9qgamm-jacob-goodwin',
                          token='EE0C6B6899B663691EFB671579527E40',
                          secret='duaJTxKiqqho0mdlgS1bxhqjdoxNc9Av+Ksdw3qB')
Beer_Data <- read_excel("DS501_CaseStudy3 Beer Data.xlsx")
colname<-c("ABV","AverageRating", "MinIBU", "MaxIBU", "Astringency", "Body",
            "Alcohol","Bitter","Sweet","Sour","Salty","Fruits","Hoppy","Spices",
            "Malty")
stylechoice<-c("Altbier","Barleywine_American","Barleywine_English","Braggot","English_Bitter","Light_Lager","American_Lager","Kvass","NewEngland_IPA","Imperial_IPA","English_IPA","Brut_IPA","Belgian_IPA","American_IPA","Happoshu","Dubbel","Cream_Ale","English_BrownAle","American_BrownAle","Weizenbock","Traditional_Bock","Maibock","Eisbock","Doppelbock","Belgian_BlondeAle","American_BlondeAle")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Craft Beer Ratings & Characteristics"),
    
    print("The purpose of this app is to allow those with less knowledge of craft beer to 
          gain an understanding both of how craft beer is described and how these factors 
          correlate to one another. For example, many beers have a strong correlation between their body 
          and maltiness, clearly seen by the plot and regression statistics whereas other characteristics may not correlate as well. This app allows you to plot and determine regression stats for these relationships. Play around with
          the plotting parameters to see how various beer characteristics correlate to one another. Information about the source data for the app can be found at the bottom of the page."),
    br(),
    br(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "xchar",label = "Select a beer characteristic (x-axis):",
                        choices = colname, selected = "Malty"),
        
            selectInput(inputId = "ychar",label = "Select a beer characteristic (y-axis):",
                        choices = colname, selected = "Body"),
            br(color='white'),
            selectInput(inputId = "styles", label = "Choose one of the select beer styles to produce it's stats on ABV & rating below.", choices = stylechoice),
            h5("Beer Style Stats"),
            tableOutput("avgs")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(type="tabs",
                       tabPanel("ScatterPlot", plotOutput("scatPlot")),
                       tabPanel("Regression Stats",verbatimTextOutput("RegOut")),
                       tabPanel("Beer Descriptors", textOutput("defs"))
                       
                                )
                       )
           
           
        ),
    strong("Motivation:"),
    print("As a chemical engineer and current part-time employee of a brewery, I've really enjoyed learning about how different
           aspects of beer flavors tend to be associated with characteristics such as prodution method, alcohol content, or even type of hops used in the brew process. As a result, the ultimate goal for this app
           is to provide people with less understanding of these craft beer characteristics with the ability to see how they relate to one another (based on expert ratings) and hopefully help them find more styles of craft beer that interest them."),
    br(),
    br(),
    strong("Data Set Description:"),
    print("A dataset of up to 50 top-rated beers across 112 styles, 5558 beers in total. Source: BeerAdvocate.com. 
The goal of the project was to create a tasting profile on beer based on word counts for a classification and recommendation system.  The descriptive words represent the tasting profile features of the beer, and are defined by word counts found in up to 25 reviews of each beer. The assumption is that people writing reviews are more than likely describing what they do experience rather than what they do not.
Note that one or two styles did not have 50 beers to collect information on, and that few beers may have had limited or no reviews available to define features with. Link:https://www.kaggle.com/stephenpolozoff/top-beer-information?select=beer_data_set.csv")
  
)


# Define server logic required to draw a plot
server <- function(input, output) {
       
    xdata = reactive({input$xchar}) 
    ydata = reactive({input$ychar})
   
    
    output$scatPlot <- renderPlot({
      ggplot(data = Beer_Data,aes_string(x = xdata(), y = ydata()))+geom_point(color='darkblue')+
       ggtitle("Craft Beer Characteristics")+geom_smooth(method='lm',color = 'green')
         })
    
    recipe_formula <- reactive(Beer_Data %>%
                                   recipe() %>%
                                   update_role(!!!input$ychar,new_role = "outcome") %>%
                                   update_role(!!!input$xchar,new_role = "predictor") %>% 
                                   prep()%>%
                                   formula())
    
    lm_reg <- reactive(
        lm(recipe_formula(),data = Beer_Data)
    )

    
    output$RegOut = renderPrint({
        if (input$ychar==input$xchar){
          "Please select characteristics that aren't the same for the regression analysis. This analysis would simply give y=x."}
        else{
         summary(lm_reg())
            }
        })
    output$defs=renderText({
    defin<-c("ABV: The amount of alcohol present by volume.",
            "IBU: AKA, the International Bitterness Unit, measures the 
            bitterness levels of a beer.", "Astringency: a tactile taste felt 
            as a dry, rough feeling in the mouth and contraction of the 
            tongue tissue.", "Body: the fullness of the flavor and mouthfeel",
            "Bitter: sharpness of taste or lack of sweetness.", "Hoppy: having 
             the taste or aroma of hops —used especially of ale or beer.", "Malty: 
            possessing varying levels of sweetness and deep notes of nuts, caramel, 
            toffee, bread, and dark, dried fruits")
    print(defin)      
    })
   ##style of beer averages 
     ## reactive({
      ##  for (i in 1:length(stylechoice)){
       ##   if(input$styles==stylechoice[i]){styledata()=stylek[i]}
       ## }
        
     ## })
    
      styledata = reactive({input$styles}) 
      output$avgs=renderTable({
        grpdata<-subset(Beer_Data, Style==styledata())
        abvavg=mean(grpdata$ABV)
        minabv=min(grpdata$ABV)
        maxabv=max(grpdata$ABV)
        avgrate=mean(grpdata$AverageRating)
        minrate=min(grpdata$AverageRating)
        maxrate=max(grpdata$AverageRating)
        
        abvstats<-c(minabv,abvavg,maxabv)
        ratstats<-c(minrate,avgrate,maxrate)
        stylestats<-data.frame(abvstats,ratstats)
       ## avgdata<-c(abvavg,avgrat,avgminib,avgmaxib)
       ## avgdata
        coln<-c("Style ABV","Style Ratings")
        rown<-c("Minimum", "Average", "Maximum")
        colnames(stylestats)<-coln
        rownames(stylestats)<-rown
        stylestats
        
      },rownames=TRUE)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
