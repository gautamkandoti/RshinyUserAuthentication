library(shiny)
library(shinydashboard)
library(DT)
#suppressWarnings(expr)
Logged = FALSE
#my_username <- "test"
#my_password <- "test"

username<-c("abc","def","ghi")
password<-c("ghi","abc","def")
permissions<-as.data.frame(cbind(username,password))
# saveRDS(permissions,"permissions.rds")
# permissions<-readRDS("permissions.rds")

kl<-c("ram","male","42","abc")
fg<-c("raheem","male","45","def")
jk<-c("robert","male","46","ghi")
df<-as.data.frame(rbind(kl,fg,jk))
colnames(df)<-c("name","gender","age","username")
rownames(df)<-NULL

ui <- dashboardPage(header =dashboardHeader(title = " Home Dashboard"),
                    sidebar = dashboardSidebar(sidebarMenu(menuItem("Table",tabName = "table",icon = icon("Tables")),
                                                           menuItem("Histogram",tabName = "Histogram",icon = icon("Histogram")))),
                    body = dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "table",
                          fluidRow(
                            box(title = " Home Table",width = 12,collapsible = T,DTOutput("user_table"))
                          )),
                        tabItem(
                          tabName = "Histogram",
                          fluidRow(
                            box(title = " Home Histogram",width=12,collapsible = T,plotlyOutput("Den_freq")))))))
                    

server = function(input, output,session) {
  
  values <- reactiveValues(authenticated = F)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
         modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    # Id.username <- which(username %in% df[,4] == Username)
    # Id.password <- which(my_password == Password)
    if (length(Username) > 0 & length(Password) > 0) {
      if (Username %in% df[,4]) {
        if(permissions[permissions$username==Username,][2]==Password){
          Logged <<- TRUE
          values$authenticated <- TRUE
          obs1$suspend()
          removeModal()
        }
      } else {
        values$authenticated <- FALSE
      }     
    }
  })
  
  
  
  output$user_table<- renderDT({
    if (values$authenticated){
      df1<-df %>% filter(username==input$username)
      datatable(df1) %>% formatStyle('age',backgroundColor = 'yellow',fontWeight = 'bold')
    
    }
  })

output$Den_freq<-renderPlotly({
  if(values$authenticated){
    iris_2=iris %>% select(Sepal.Length,Species) %>%head(10) %>%  ggplot(aes(x=Sepal.Length))+geom_histogram()
    ggplotly(iris_2)
  }
})


}

shinyApp(ui,server)




#################################

# obs2 <- observe({
#   req(input$ok)
#   isolate({
#     Username <- input$username
#     Password <- input$password
#   })
#   Id.username <- which(my_username == Username)
#   Id.password <- which(my_password == Password)
#   if (length(Id.username) > 0 & length(Id.password) > 0) {
#     if (Id.username == Id.password) {
#       Logged <<- TRUE
#       values$authenticated <- TRUE
#       obs1$suspend()
#       removeModal()
#       
#     } else {
#       values$authenticated <- FALSE
#     }     
#   }
# })





 