
# app examples
# https://github.com/mkearney/shinyapps_links

library(shiny)
library(shinydashboard)
library(ggplot2)

# Data
moods <- read.csv("C:/Users/jmcgirr/Documents/GitHub/mood_tracker/data.csv", header = TRUE, stringsAsFactors = FALSE)
moods$Mood <- factor(moods$Mood, levels = c("unhappy","okay","good","happy","great"))
moods$Date <- as.Date(moods$Date)

moodicon <- ifelse(moods$Mood[nrow(moods)] == "great" | 
                       moods$Mood[nrow(moods)] == "good" , "smile-o", 
                   ifelse(moods$Mood[nrow(moods)] == "okay" , "meh-o",
                          ifelse(moods$Mood[nrow(moods)] == "unhappy" ,       
                                 "frown-o")))
moodstatus <- ifelse(moods$Mood[nrow(moods)] == "great" | 
                         moods$Mood[nrow(moods)] == "good" , "green", 
                     ifelse(moods$Mood[nrow(moods)] == "okay" , "yellow",
                            ifelse(moods$Mood[nrow(moods)] == "red" ,       
                                   "frown-o")))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Miko Mood"),
    dashboardSidebar(      menuItem("Github repo", href = "http://zfleeman.com", icon = icon("home"))
    ),
    
    dashboardBody(
        
        fluidRow(
            valueBoxOutput("day", width = 3),
            valueBoxOutput("mood", width = 3),
            valueBoxOutput("feels", width = 3),
            valueBoxOutput("activities", width = 3)
        ),
        
        fluidRow(
            box(plotOutput("plot1_all_general_moods", height = 250), width = 10),
        ),
        fluidRow(
            box(plotOutput("plot2_timeline", height = 250), width = 10),
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$day <- renderValueBox(valueBox(moods$Date[nrow(moods)], "Last Update", icon = icon("calendar")))
    output$mood <- renderValueBox(valueBox(moods$Mood[nrow(moods)], "General Mood", icon = icon(moodicon), color = moodstatus))
    output$feels <- renderValueBox(valueBox(strsplit(moods$Description[nrow(moods)], split = ",")[[1]][1],
                                            "Other Feels", icon = icon("bong")))
    output$activities <- renderValueBox(valueBox(strsplit(moods$Activities[nrow(moods)], split = ",")[[1]][1],
                                                 "Activity", icon = icon("bicycle"), color = ))
    
    
    output$plot1_all_general_moods <- renderPlot({
        barplot(table(moods$Mood), main = "All Tracked Days\n General Mood")
    })
    output$plot2_timeline <- renderPlot({
        ggplot(data=moods, aes(x=Date, y=Mood, group=1)) +
            geom_line(linetype = "dashed")+
            theme_bw()+
            xlab("Date")+
            geom_point()+
            scale_x_date(limits = as.Date(c(as.Date(moods$Date[1]), Sys.Date())))+
            ggtitle("Timeline")
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
