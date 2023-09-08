userPermissionTemplate <- function(rcon = NULL){
  if (is.null(rcon)){
    rcon <- list(url = "", 
                 token = "")
  }
  
  shinyApp(
    ui = .userPermissionTemplate_UI, 
    server = .userPermissionTemplate_Server
  )
}


.userPermissionTemplate_Choices <- 
  c("Project Design and Setup" = "design", 
    "Alerts & Notifications" = "alerts", 
    "User Rights" = "user_rights", 
    "Data Access Groups" = "data_access_groups", 
    "Reports & Report Building" = "reports", 
    "Stats & Charts" = "stats_and_charts", 
    "Manage Survey Participants" = "manage_survey_participants", 
    "Calendar & Scheduling" = "calendar", 
    "Data Import Tool" = "data_import_tool", 
    "Data Comparison Tool" = "data_comparison_tool", 
    "Logging" = "logging", 
    "FileRepository" = "file_repository", 
    "Data Quality (create/edit rules)" = "data_quality_create", 
    "Data Quality (execute rules)" = "data_quality_execute", 
    "API export" = "api_export", 
    "API import" = "api_import", 
    "REDCap Mobile App" = "mobile_app", 
    "REDCap Mobile Data Download" = "mobile_app_download_data", 
    "Create Records" = "record_create", 
    "Rename Records" = "record_rename", 
    "Delete Records" = "record_delete", 
    "Lock/Unlock Entire Records" = "lock_records_all_forms", 
    "Lock/Unlock Records (instrument level)" = "lock_records", 
    "Record Locking Customization" = "lock_records_customization", 
    "Randomization Setup" = "random_setup", 
    "Randomization Dashboard" = "random_dashboard", 
    "Randomization (Perform)" = "random_perform")

.userPermissionTemplate_UI <- 
  fluidPage(
    fluidRow(
      # Left Column -------------------------------------------------
      column(
        width = 6,
        textInput(inputId = "api_url", 
                  label = "URL", 
                  value = rcon$url), 
        passwordInput(inputId = "api_token", 
                      label = "Token", 
                      value = rcon$token),
        selectInput(inputId = "project_user", 
                    label = "Username", 
                    choices = character(0), 
                    selected = character(0)),
        h3("Code for Data Import"), 
        verbatimTextOutput("data_row"), 
        actionButton(inputId = "btn_importUser", 
                     label = "Import This User")
      ), 
      
      # Right Column ------------------------------------------------
      column(
        width = 6, 
        dateInput(inputId = "expiration", 
                  label = "Expiration"), 
        checkboxGroupInput(inputId = "user_permission", 
                           label = "Permissions",
                           choices = .userPermissionTemplate_Choices)
      )
    )
  )

.userPermissionTemplate_Server <- 
  shinyServer(function(input, output, session){
    # Reactive Values -----------------------------------------------
    App <- reactiveValues(
      rcon = NULL, 
      Users = NULL, 
      ThisUser = NULL, 
      ImportData = NULL
    )
    
    # Passive Observers ---------------------------------------------
    observe({
      req(input$api_url, 
          input$api_token)
      
      App$rcon <- redcapConnection(url = input$api_url, 
                                   token = input$api_token)
      App$rcon$refresh_users()
      
      App$Users <- exportUsers(rcon, labels = FALSE)
    })
    
    # Event Observers -----------------------------------------------
    observeEvent(
      App$rcon, 
      {
        req(App$rcon)
        updateSelectInput(inputId = "project_user", 
                          choices = App$Users$username)
      }
    )
    
    observeEvent(
      input$project_user, 
      {
        req(input$project_user, 
            App$rcon, 
            App$Users)
        
        App$ThisUser <- App$Users[App$Users$username == input$project_user, ]
        
        updateDateInput(inputId = "expiration",
                        value = format(App$ThisUser$expiration,
                                       format = "%Y-%m-%d"))
        
        permission <- vapply(App$ThisUser[unname(.userPermissionTemplate_Choices)],
                             function(x) x %in% 1,
                             logical(1))
        names(permission) <- unname(.userPermissionTemplate_Choices)
        
        updateCheckboxGroupInput(inputId = "user_permission",
                                 selected = names(permission)[permission])
      }
    )
    
    observeEvent(
      input$btn_importUser, 
      {
        importUsers(App$rcon, 
                    data = App$ImportData)
        showNotification("User Import Complete", 
                         duration = 3, 
                         type = "message")
      }
    )
    
    # Output Elements -----------------------------------------------
    
    output$data_row <- 
      renderPrint({
        req(App$rcon)
        
        Permission <- as.data.frame(lapply(.userPermissionTemplate_Choices %in% input$user_permission, as.numeric))
        names(Permission) <- unname(.userPermissionTemplate_Choices)
        
        Permission$username <- input$project_user
        Permission$expiration <- if (length(input$expiration) > 0) input$expiration else NA_character_
        
        Permission <- Permission[c("username", "expiration", 
                                   names(Permission)[!names(Permission) %in% c("username", "expiration")])]
        
        App$ImportData <- Permission
        
        dput(Permission)
      })
    
  })
