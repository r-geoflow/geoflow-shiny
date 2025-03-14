#metadata_editor_server
metadata_editor_server<- function(id, auth_info, i18n, geoflow_configs, parent.session){
  
  moduleServer(id, function(input, output, session){
    
    active_contact_form_tab <- reactiveVal("contact_identifiers")
    
    ns <- session$ns
    
    #reactives
    pageLoaded <- reactiveVal(FALSE)
    md_model <- reactiveVal(list())
    md_model_type <- reactiveVal(NULL)
    md_model_draft <- reactiveVal(NULL)
    md_model_draft_idx <- reactiveVal(0L)
    md_model_draft_mode <- reactiveVal("creation")
    md_model_draft_validation_status <- reactiveVal(NULL)
    updatingTabsetPanel <- reactiveVal(FALSE)
    
    #FUNCTIONS
    setID = function(type, id){
      sprintf("%s_%s", type, id)
    }
    
    handle_metadata_form = function(type){
      switch(type,
        "contact" = tabsetPanel(
          id = ns("contact_form"),
          type = "pills", vertical = T,
          tabPanel(
            value = "contact_identifiers",
            title = "Identifier(s)",
            fluidRow(
              column(3, selectizeInput(ns("contact_identifier_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = md_model_draft()$getAllowedKeyValuesFor("Identifier"),
                                       selected = "id"
              )),
              column(3,textInput(ns("contact_identifier"), "Identifier",value = "", width = NULL, placeholder = "Identifier")),
              column(3,
                     actionButton(ns("contact_identifier_button_add"), title="Add identifier",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("contact_identifier_button_clear"), title="Clear identifier",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("contact_identifiers_table_wrapper"))
          ),
          tabPanel(
            value = "contact_details",
            title = "Details",
            fluidRow(column(6, tags$b("Organization name")), column(6, textInput(ns("contact_org"), label = NULL, value = md_model_draft()$organizationName, width = NULL, placeholder = "Organization name"))),
            fluidRow(column(6, tags$b("First name")), column(6, textInput(ns("contact_firstname"), label = NULL, value = md_model_draft()$firstName, width = NULL, placeholder = "First name"))),
            fluidRow(column(6, tags$b("Last name")), column(6, textInput(ns("contact_lastname"), label = NULL, value = md_model_draft()$lastName, width = NULL, placeholder = "Last name"))),
            fluidRow(column(6, tags$b("Position name")), column(6, textInput(ns("contact_positionname"), label = NULL, value = md_model_draft()$positionName, width = NULL, placeholder = "Position name"))),
            fluidRow(column(6, tags$b("Postal address")), column(6, textInput(ns("contact_postaladdress"), label = NULL, value = md_model_draft()$postalAddress, width = NULL, placeholder = "Postal address"))),
            fluidRow(column(6, tags$b("Postal code")), column(6, textInput(ns("contact_postalcode"), label = NULL, value = md_model_draft()$postalCode, width = NULL, placeholder = "Postal code"))),
            fluidRow(column(6, tags$b("City")), column(6, textInput(ns("contact_city"), label = NULL, value = md_model_draft()$city, width = NULL, placeholder = "City"))),
            fluidRow(column(6, tags$b("Country")), column(6, textInput(ns("contact_country"), label = NULL, value = md_model_draft()$country, width = NULL, placeholder = "Country")))
          ),
          tabPanel(
            value = "contact_info",
            title = "Information",
            fluidRow(column(6, tags$b("Email")), column(6, textInput(ns("contact_email"), label = NULL, value = "", width = NULL, placeholder = "Email"))),
            fluidRow(column(6, tags$b("Phone number")), column(6, textInput(ns("contact_voice"), label = NULL, value = md_model_draft()$voice, width = NULL, placeholder = "Phone number"))),
            fluidRow(column(6, tags$b("Facsimile")), column(6, textInput(ns("contact_facsimile"), label = NULL, value = md_model_draft()$facsimile, width = NULL, placeholder = "Facsimile"))),
            fluidRow(column(6, tags$b("Website URL")), column(6, textInput(ns("contact_websiteurl"), label = NULL, value = md_model_draft()$websiteUrl, width = NULL, placeholder = "Website URL"))),
            fluidRow(column(6, tags$b("Website name")), column(6, textInput(ns("contact_websitename"), label = NULL, value = md_model_draft()$websiteName, width = NULL, placeholder = "Website name")))
          )
        ),
        "entity" = tabsetPanel(
          id = ns("entity_form"),
          type = "pills", vertical = T,
          tabPanel(
            title = "Identifier",
            "TODO"
          ),
          tabPanel(
            title = "Title",
            "TODO"
          ),
          tabPanel(
            title = "Description",
            "TODO"
          ),
          tabPanel(
            title = "Subject",
            "TODO"
          ),
          tabPanel(
            title = "Creator",
            "TODO"
          ),
          tabPanel(
            title = "Date",
            "TODO"
          ),
          tabPanel(
            title = "Type",
            "TODO"
          ),
          tabPanel(
            title = "Language",
            "TODO"
          ),
          tabPanel(
            title = "SpatialCoverage",
            "TODO"
          ),
          tabPanel(
            title = "TemporalCoverage",
            "TODO"
          ),
          tabPanel(
            title = "Relation",
            "TODO"
          ),
          tabPanel(
            title = "Rights",
            "TODO"
          ),
          tabPanel(
            title = "Format",
            "TODO"
          ),
          tabPanel(
            title = "Provenance",
            "TODO"
          ),
          tabPanel(
            title = "Data",
            "TODO"
          )
        ),
        "dictionary" = "COMING SOON"
      )
      
    }
    
    #UIs
    
    #metadata editor info
    output$metadata_editor_info <- renderText({
      session$userData$module("metadata-editor")
      updateModuleUrl(session, "metadata-editor")
      text <- i18n()$t("METADATA_EDITOR_TITLE")
      pageLoaded(TRUE)
      text
    })
    
    #meta edition choices (entity, contact, dictionary)
    output$meta_editor_choices <- renderUI({
      fluidRow(
        bs4Dash::bs4ValueBox(
          width = 4,
          value = h4("Contacts"), 
          subtitle = "Create contacts to attach to datasets", 
          color = "primary",
          icon = icon("users"),
          footer = shiny::tagList(
            bs4Dash::actionButton(inputId = ns("create_contact_table"), label = "Create table"), #TODO
            bs4Dash::actionButton(inputId = ns("load_contact_table"), label = "Load table") #TODO
          )
        ),
        bs4Dash::bs4ValueBox(
          width = 4,
          value = h4("Entities"), 
          subtitle = "Create entities to describe datasets", 
          color = "primary",
          icon = icon("table"),
          footer = shiny::tagList(
            bs4Dash::actionButton(inputId = ns("create_entity_table"), label = "Create table"), #TODO
            bs4Dash::actionButton(inputId = ns("load_entity_table"), label = "Load table") #TODO
          )
        ),
        bs4Dash::bs4ValueBox(
          width = 4,
          value = h4("Dictionary"), 
          subtitle = "Create a data dictionary", 
          color = "primary",
          icon = icon("table-list"),
          footer = shiny::tagList(
            bs4Dash::actionButton(inputId = ns("create_dictionary_table"), label = "Create table"), #TODO
            bs4Dash::actionButton(inputId = ns("load_dictionary_table"), label = "Load table") #TODO
          )
        )
      )
      
    })
    
    #metadata editor
    output$meta_editor <- renderUI({
      req(!is.null(md_model_type()))
      
      shiny::tagList(
        fluidRow(
          bs4Dash::actionButton(inputId = ns(paste0(sprintf("create_%s", md_model_type()))), label = paste("Create", md_model_type()))
        ),
        fluidRow(
          tabBox(
            width = 6,
            id = paste0("tabbox_", md_model_type(), "_form_view"),
            type = "tabs", solidHeader = FALSE, status = "teal",
            tabPanel(
              icon = icon("pencil"),
              value = paste0("tabbox_", md_model_type(), "_form_editor"),
              title = switch(md_model_draft_mode(),
                "creation" = paste0("Create new ",md_model_type()),
                "edition" = paste0("Edit ", md_model_type()," ", md_model_draft_idx())
              ),
              handle_metadata_form(type = md_model_type()),hr(),
              bs4Dash::actionButton(inputId = ns(paste0("save_",md_model_type())), label = "Save"),br(),
              if(!is.null(md_model_draft_validation_status())){
                bs4Dash::bs4Badge(
                  if("ERROR" %in% md_model_draft_validation_status()$type) "Validation errors" else "Validation warnings",
                  color = if("ERROR" %in% md_model_draft_validation_status()$type) "danger" else "warning"
                )
              }else{
                NULL
              }
            ),
            tabPanel(
              value = paste0("tabbox_", md_model_type(), "_form_validator"),
              title = "Validation report",
              uiOutput(ns("validation_issues_table_wrapper"))
            )
          ),
          tabBox(
            width = 6,
            id = ns(paste0("tabbox_", md_model_type(), "_table_view")),
            type = "tabs", solidHeader = FALSE, status = "teal",
            maximizable = TRUE,
            tabPanel(
              title = "Table view",
              rhandsontable::rHandsontableOutput(ns("meta_table")),hr(),
              bs4Dash::actionButton(inputId = ns(paste0("download_",md_model_type(),"s")), label = "Download (CSV)")
            )
          )
        )
      )
    })
    
    #meta_table (geoflow pivot table format rendered as RHandsontable)
    output$meta_table <- rhandsontable::renderRHandsontable({
      req(!is.null(md_model_type()))
      metatbl = NULL
      if(length(md_model())==0){
        WARN("No row in metadata table, creating an empty dataframe")
        metatbl = switch(md_model_type(),
          "contact" = geoflow_contact$new()$asDataFrame(),
          "entity" = geoflow_entity$new()$asDataFrame(),
          "dictionary" = data.frame(todo = NA)
        )
      }else{
        INFO("Convert md_model to dataframe")
        metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
      }
      
      out_tbl <- rhandsontable::rhandsontable(
        metatbl, 
        readOnly = TRUE
      ) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(
          fixedColumnsLeft = 1,
          colWidths = 200,
          manualColumnResize = TRUE
        )
      print(out_tbl)
      out_tbl
    })
    
    #RENDERERS
    output$validation_issues_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        md_model_draft_validation_status(), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$validation_issues_table_wrapper <-renderUI({
      if(!is.null(md_model_draft_validation_status())){
        DTOutput(ns("validation_issues_table"))
      }else{
        tags$em("No validation issues")
      }
    })
    #contact
    output$contact_identifiers_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(names(md_model_draft()$identifiers), function(idname){
          data.frame(key = idname, id = md_model_draft()$identifiers[[idname]])
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$contact_identifiers_table_wrapper <-renderUI({
      if(length(md_model_draft()$identifiers)>0){
        DTOutput(ns("contact_identifiers_table"))
      }else{NULL}
    })
    
    #EVENTS
    #entities
    observeEvent(input$create_entity_table, {
      md_model_type("entity")
      INFO(sprintf("Select editor for type '%s'", md_model_type()))
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(1L)
    })
    observeEvent(input$create_entity, {
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(length(md_model())+1)
      md_model_draft_mode("creation")
    })
    observeEvent(input$save_entity,{
      INFO(sprintf("Save %s to metadata table", md_model_type()))
      print(md_model_draft())
      entities = md_model()
      if(md_model_draft_idx()==0){
        entities[[length(entities)+1]] = md_model_draft()
      }else{
        entities[[md_model_draft_idx()]] = md_model_draft()
      }
      md_model(entities)
      md_model_draft_mode("edition")
    })
    
    #contacts
    observeEvent(input$create_contact_table, {
      md_model_type("contact")
      INFO(sprintf("Select editor for type '%s'", md_model_type()))
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(1L)
      # hideTab(
      #   inputId = paste0("tabbox_", md_model_type(), "_form_view"),
      #   target = paste0("tabbox_", md_model_type(), "_form_validator")
      # )
    })
    observeEvent(input$create_contact,{
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(length(md_model())+1)
      md_model_draft_mode("creation")
      # hideTab(
      #   inputId = paste0("tabbox_", md_model_type(), "_form_view"),
      #   target = paste0("tabbox_", md_model_type(), "_form_validator")
      # )
    })
    observeEvent(input$save_contact,{
      INFO(sprintf("Save %s to metadata table", md_model_type()))
      
      
      req(!is.null(md_model_type()))
      validation_errors = switch(md_model_type(),
             "contact" = {
               contact = md_model_draft()
               contact$setOrganizationName(input$contact_org)
               contact$setLastName(input$contact_lastname)
               contact$setFirstName(input$contact_firstname)
               contact$setPositionName(input$contact_positionname)
               contact$setPostalAddress(input$contact_postaladdress)
               contact$setPostalCode(input$contact_postalcode)
               contact$setCity(input$contact_city)
               contact$setCountry(input$contact_country)
               contact$setEmail(input$contact_email)
               contact$setVoice(input$contact_voice)
               contact$setFacsimile(input$contact_facsimile)
               contact$setWebsiteUrl(input$contact_websiteurl)
               contact$setWebsiteName(input$contact_websitename)
               md_model_draft(contact$clone(deep = T))
               
               #perform validation
               qa_errors = geoflow_validator_contacts$new(source = md_model_draft()$asDataFrame())$validate_content()
               if(nrow(qa_errors)==0){
                 qa_errors = NULL
               }
               qa_errors
             }

      )
      md_model_draft_validation_status(validation_errors)
      
      if(!is.null(validation_errors)){
        ERROR(paste0("There are validation errors with the ", md_model_type(),". Aborting saving the data to geoflow pivot model"))
        # showTab(
        #   inputId = paste0("tabbox_", md_model_type(), "_form_view"),
        #   target = paste0("tabbox_", md_model_type(), "_form_validator")
        # )
      }else{
        INFO(sprintf("There are no validation errors/warnings with the ", md_model_type(),". Aborting saving the data to geoflow pivot model"))
        # hideTab(
        #   inputId = paste0("tabbox_", md_model_type(), "_form_view"),
        #   target = paste0("tabbox_", md_model_type(), "_form_validator")
        # )
      }
      
      save_model = TRUE
      if(!is.null(validation_errors)) {
        if("ERROR" %in% validation_errors$type){
          ERROR("There are validation errors, abort saving the model!")
          save_model = FALSE
        }
      }
      if(save_model){
        INFO("Saving model to metadata table")
        contacts = md_model()
        if(md_model_draft_idx()==0){
          contacts[[length(contacts)+1]] = md_model_draft()
        }else{
          contacts[[md_model_draft_idx()]] = md_model_draft()
        }
        md_model(contacts)
        md_model_draft_mode("edition")
      }
      
    })
    
    #dictionary
    observeEvent(input$create_dictionary_table, {
      md_model_type("dictionary")
      INFO(sprintf("Select editor for type '%s'", md_model_type()))
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(1L)
    })
    observeEvent(input$create_dictionary,{
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(length(md_model())+1)
      md_model_draft_mode("creation")
    })
    #TODO save dictionary
    
    #contact events
    observeEvent(input$contact_form, {
      active_contact_form_tab(input$contact_form)
    })
    
    observe({
      print(input$contact_form)
      if(!is.null(input$contact_form)) if(input$contact_form != active_contact_form_tab()){
        isolate({
          updateTabsetPanel(session, ns("contact_form"), selected = active_contact_form_tab()) 
        })
      }
    })
    
    observeEvent(input$contact_identifier_button_add,{
      print("Add event")
      contact = md_model_draft()
      contact$setIdentifier(
        key = input$contact_identifier_type,
        id = input$contact_identifier
      )
      md_model_draft(contact$clone(deep = T))
    })
    
    observeEvent(input$contact_identifier_button_clear,{
      print("Clear event")
      contact = md_model_draft()
      contact$identifiers = list()
      print(contact$identifiers)
      md_model_draft(contact$clone(deep = T))
    })
    
  })
  
}