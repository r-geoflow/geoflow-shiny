#metadata_editor_server
metadata_editor_server<- function(id, auth_info, i18n, geoflow_configs, parent.session){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    #templates
    contact_tpl = geoflow_contact$new()
    entity_tpl = geoflow_entity$new()
    
    #reactives
    #generic reactives
    pageLoaded <- reactiveVal(FALSE)
    md_model <- reactiveVal(list())
    md_model_type <- reactiveVal(NULL)
    md_model_draft <- reactiveVal(NULL)
    md_model_draft_idx <- reactiveVal(0L)
    md_model_draft_mode <- reactiveVal("creation")
    md_model_draft_valid <- reactiveVal(NULL)
    md_model_draft_validation_report <- reactiveVal(NULL)
    md_model_subject_selection <- reactiveVal(NULL)
    md_model_subject_draft <- reactiveVal(NULL)
    md_model_bbox <- reactiveVal(NULL)
    cache_vocabs <- reactiveVal(list())
    update_meta_editor <- reactiveVal(TRUE)
    #model specific reactives
    active_contact_form_tab <- reactiveVal("contact_identifiers")
    active_entity_form_tab <- reactiveVal("entity_identifiers")
    ref_hierarchy <- reactiveVal(NULL)
    
    #FUNCTIONS
    setID = function(type, id){
      sprintf("%s_%s", type, id)
    }
    
    handle_metadata_form = function(type, model = NULL){
      switch(type,
        "contact" = tabsetPanel(
          width = 3,
          id = ns("contact_form"),
          type = "pills", vertical = T,
          tabPanel(
            value = "contact_identifiers",
            title = "Identifier(s)",
            fluidRow(
              column(3, selectizeInput(ns("contact_identifier_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = contact_tpl$getAllowedKeyValuesFor("Identifier"),
                                       selected = "id"
              )),
              column(6,textInput(ns("contact_identifier"), "Identifier",value = "", width = NULL, placeholder = "Identifier")),
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
            fluidRow(column(6, tags$b("Organization name")), column(6, textInput(ns("contact_org"), label = NULL, value = if(!is.null(model)) model$organizationName else "", width = NULL, placeholder = "Organization name"))),
            fluidRow(column(6, tags$b("First name")), column(6, textInput(ns("contact_firstname"), label = NULL, value = if(!is.null(model)) model$firstName else "", width = NULL, placeholder = "First name"))),
            fluidRow(column(6, tags$b("Last name")), column(6, textInput(ns("contact_lastname"), label = NULL, value = if(!is.null(model)) model$lastName else "", width = NULL, placeholder = "Last name"))),
            fluidRow(column(6, tags$b("Position name")), column(6, textInput(ns("contact_positionname"), label = NULL, value = if(!is.null(model)) model$positionName else "", width = NULL, placeholder = "Position name"))),
            fluidRow(column(6, tags$b("Postal address")), column(6, textInput(ns("contact_postaladdress"), label = NULL, value = if(!is.null(model)) model$postalAddress else "", width = NULL, placeholder = "Postal address"))),
            fluidRow(column(6, tags$b("Postal code")), column(6, textInput(ns("contact_postalcode"), label = NULL, value = if(!is.null(model)) model$postalCode else "", width = NULL, placeholder = "Postal code"))),
            fluidRow(column(6, tags$b("City")), column(6, textInput(ns("contact_city"), label = NULL, value = if(!is.null(model)) model$city else "", width = NULL, placeholder = "City"))),
            fluidRow(column(6, tags$b("Country")), column(6, textInput(ns("contact_country"), label = NULL, value = if(!is.null(model)) model$country else "", width = NULL, placeholder = "Country")))
          ),
          tabPanel(
            value = "contact_info",
            title = "Information",
            fluidRow(column(6, tags$b("Email")), column(6, textInput(ns("contact_email"), label = NULL, value = if(!is.null(model)) model$email else "", width = NULL, placeholder = "Email"))),
            fluidRow(column(6, tags$b("Phone number")), column(6, textInput(ns("contact_voice"), label = NULL, value = if(!is.null(model)) model$voice else "", width = NULL, placeholder = "Phone number"))),
            fluidRow(column(6, tags$b("Facsimile")), column(6, textInput(ns("contact_facsimile"), label = NULL, value = if(!is.null(model)) model$facsimile else "", width = NULL, placeholder = "Facsimile"))),
            fluidRow(column(6, tags$b("Website URL")), column(6, textInput(ns("contact_websiteurl"), label = NULL, value = if(!is.null(model)) model$websiteUrl else "", width = NULL, placeholder = "Website URL"))),
            fluidRow(column(6, tags$b("Website name")), column(6, textInput(ns("contact_websitename"), label = NULL, value = if(!is.null(model)) model$websiteName else "", width = NULL, placeholder = "Website name")))
          )
        ),
        "entity" = tabsetPanel(
          width = 3,
          id = ns("entity_form"),
          type = "pills", vertical = T,
          tabPanel(
            value = "entity_identifiers",
            title = "Identifier",
            fluidRow(
              column(3, selectizeInput(ns("entity_identifier_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Identifier"),
                                       selected = "id"
              )),
              column(6,textInput(ns("entity_identifier"), "Identifier",value = "", width = NULL, placeholder = "Identifier")),
              column(3,
                     actionButton(ns("entity_identifier_button_add"), title="Add identifier",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_identifier_button_clear"), title="Clear identifier",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_identifiers_table_wrapper"))
          ),
          tabPanel(
            value = "entity_titles",
            title = "Title",
            fluidRow(
              column(3, selectizeInput(ns("entity_title_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Title"),
                                       selected = "id"
              )),
              column(7,textInput(ns("entity_title"), "Title",value = "", width = NULL, placeholder = "Title")),
              column(2,
                     actionButton(ns("entity_title_button_add"), title="Add title",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_title_button_clear"), title="Clear title",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_titles_table_wrapper"))
          ),
          tabPanel(
            value = "entity_descriptions",
            title = "Description",
            fluidRow(
              column(3, selectizeInput(ns("entity_description_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Description"),
                                       selected = "id"
              )),
              column(7,textAreaInput(ns("entity_description"), "Description",value = "", width = NULL, placeholder = "Description")),
              column(2,
                     actionButton(ns("entity_description_button_add"), title="Add description",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_description_button_clear"), title="Clear description",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_descriptions_table_wrapper"))
          ),
          tabPanel(
            value = "entity_subjects",
            title = "Subject",
            fluidRow(
              column(3, selectizeInput(ns("entity_subject_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = c(geometa::ISOKeywordType$values(), "taxonomy"),
                                       selected = "theme"
              )),
              column(6, selectizeInput(ns("entity_vocabulary_server"),
                                       label="Vocabulary server",
                                       multiple = F,
                                       choices = {
                                         vocabs = list_vocabularies()
                                         setNames(c(vocabs$id, "custom"), nm = c(vocabs$def, "Custom"))
                                        },
                                       selected = "custom"
              ))
            ),
            fluidRow(
              style = "height:400px;overflow-y:auto;",
              column(12,
                uiOutput(ns("entity_vocabulary_custom")),
                uiOutput(ns("entity_vocabulary_tree_wrapper"))
              )
            ),
            fluidRow(
              column(3,
                     actionButton(ns("entity_subject_button_add"), title="Add subject",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_subject_button_clear"), title="Clear subject",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_subjects_table_wrapper"))
          ),
          tabPanel(
            value = "entity_contacts",
            title = "Creator",
            fluidRow(
              column(3, selectizeInput(ns("entity_contact_type"),
                                       label="Role",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Creator"),
                                       selected = "id"
              )),
              column(7,textInput(ns("entity_contact"), "Contact",value = "", width = NULL, placeholder = "Contact")),
              column(2,
                     actionButton(ns("entity_contact_button_add"), title="Add contact",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_contact_button_clear"), title="Clear contact",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_contacts_table_wrapper"))
          ),
          tabPanel(
            value = "entity_dates",
            title = "Date",
            fluidRow(
              column(3, selectizeInput(ns("entity_date_type"),
                                       label="Date type",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Date"),
                                       selected = "creation"
              )),
              column(7,dateInput(ns("entity_date"), "Date",value = Sys.Date(), width = NULL)),
              column(2,
                     actionButton(ns("entity_date_button_add"), title="Add date",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_date_button_clear"), title="Clear date",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_dates_table_wrapper"))
          ),
          tabPanel(
            value = "entity_types",
            title = "Type",
            fluidRow(
              column(3, selectizeInput(ns("entity_resource_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Type"),
                                       selected = "generic"
              )),
              column(7,textInput(ns("entity_resource"), "Type",value = "dataset", width = NULL, placeholder = "Type")),
              column(2,
                     actionButton(ns("entity_type_button_add"), title="Add type",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_type_button_clear"), title="Clear type",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_types_table_wrapper"))
          ),
          tabPanel(
            value = "entity_languages",
            title = "Language",
            fluidRow(
              column(6, selectizeInput(ns("entity_language"),
                                       label="Language",
                                       multiple = F,
                                       choices = {
                                         languages = geometa::ISOLanguage$values(labels = T)
                                         setNames(languages[,1], nm = languages[,2])
                                       },
                                       selected = "eng"
              ))
            )
          ),
          tabPanel(
            value = "entity_spatialcoverages",
            title = "SpatialCoverage",
            fluidRow(
              column(6, selectizeInput(ns("entity_srid"),
                                       label="SRID",
                                       multiple = F,
                                       choices = {
                                         setNames(c(4326), nm = c("WGS 84 (EPSG:4326)"))
                                       },
                                       selected = 4326
              ))
            ),
            fluidRow(
              style = "height:450px;overflow-y:auto;",
              column(12,
                leafletOutput(ns("entity_map"), height = "400px"),
                uiOutput(ns("entity_wkt_wrapper"))
              )
            )
          ),
          tabPanel(
            value = "entity_temporalcoverages",
            title = "TemporalCoverage",
            "TODO"
          ),
          tabPanel(
            value = "entity_relations",
            title = "Relation",
            fluidRow(
              column(3, selectizeInput(ns("entity_relation_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Relation"),
                                       selected = "id"
              )),
              column(7,textInput(ns("entity_relation_name"), "Name",value = "", width = NULL, placeholder = "Name"))
            ),
            fluidRow(
              column(3),
              column(7,textAreaInput(ns("entity_relation_description"), "Description",value = "", width = NULL, placeholder = "Description"))
            ),
            fluidRow(
              column(3),
              column(7, textInput(ns("entity_relation_link"), "Link", value = "", width = NULL, placeholder = "Link")),
              column(2,
                     actionButton(ns("entity_relation_button_add"), title="Add relation",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_relation_button_clear"), title="Clear relation",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_relations_table_wrapper"))
          ),
          tabPanel(
            value = "entity_rights",
            title = "Rights",
            "TODO"
          ),
          tabPanel(
            value = "entity_formats",
            title = "Format",
            fluidRow(
              column(3, selectizeInput(ns("entity_format_type"),
                                       label="Format type",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Relation"),
                                       selected = "id"
              )),
              column(7,selectizeInput(ns("entity_format_name"),
                                      label="Format",
                                      multiple = F,
                                      
                                      choices = as.character(mime::mimemap),
                                      selected = NULL
              ))
            ),
            fluidRow(
              column(3),
              column(7,textAreaInput(ns("entity_format_description"), "Description",value = "", width = NULL, placeholder = "Description"))
            ),
            fluidRow(
              column(3),
              column(7, textInput(ns("entity_format_link"), "Link", value = "", width = NULL, placeholder = "Link")),
              column(2,
                     actionButton(ns("entity_format_button_add"), title="Add format",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_format_button_clear"), title="Clear format",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_formats_table_wrapper"))
          ),
          tabPanel(
            value = "entity_provenances",
            title = "Provenance",
            "TODO"
          ),
          tabPanel(
            value = "entity_datasets",
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
            bs4Dash::actionButton(inputId = ns("create_contact_table"), label = "Create table"),
            bs4Dash::actionButton(inputId = ns("load_contact_table"), label = "Load table"), #TODO
            bs4Dash::actionButton(inputId = ns("create_contact"), label = "Create contact")
            
          )
        ),
        bs4Dash::bs4ValueBox(
          width = 4,
          value = h4("Entities"), 
          subtitle = "Create entities to describe datasets", 
          color = "primary",
          icon = icon("table"),
          footer = shiny::tagList(
            bs4Dash::actionButton(inputId = ns("create_entity_table"), label = "Create table"),
            bs4Dash::actionButton(inputId = ns("load_entity_table"), label = "Load table"), #TODO
            bs4Dash::actionButton(inputId = ns("create_entity"), label = "Create entity")
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
            bs4Dash::actionButton(inputId = ns("load_dictionary_table"), label = "Load table"), #TODO
            bs4Dash::actionButton(inputId = ns("create_dictionary"), label = "Create dictionary")
          )
        )
      )
      
    })
    
    #metadata editor
    output$meta_editor <- renderUI({
      req(!is.null(md_model_type()))
      print("render meta editor")
      shiny::tagList(
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
              handle_metadata_form(type = md_model_type(), model = if(md_model_draft_mode() == "edition") md_model_draft() else NULL),hr(),
              bs4Dash::actionButton(inputId = ns("check_model"), label = "Check"),
              bs4Dash::actionButton(inputId = ns("save_model"), label = "Save", disabled = TRUE),br(),
              uiOutput(ns("meta_editor_validation_status"))
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
    
    output$meta_editor_validation_status <- renderUI({
      if(!is.null(md_model_draft_valid())) if(!md_model_draft_valid()){
        bs4Dash::bs4Badge(
          if("ERROR" %in% md_model_draft_validation_report()$type) "Validation errors" else "Validation warnings",
          color = if("ERROR" %in% md_model_draft_validation_report()$type) "danger" else "warning"
        )
      }else{
        bs4Dash::bs4Badge(
          "No validation issues",
          color = "success"
        )
      }
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
      out_tbl
    })
    
    #RENDERERS
    #validation (model agnostic)
    output$validation_issues_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        md_model_draft_validation_report(), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$validation_issues_table_wrapper <-renderUI({
      if(!is.null(md_model_draft_validation_report())){
        DTOutput(ns("validation_issues_table"))
      }else{
        tags$em("No validation issues")
      }
    })
    #entity
    #entity -> Identifier
    output$entity_identifiers_table <- DT::renderDT(server = FALSE, {
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
    output$entity_identifiers_table_wrapper <-renderUI({
      if(length(md_model_draft()$identifiers)>0){
        DTOutput(ns("entity_identifiers_table"))
      }else{NULL}
    })
    #entity -> Title
    output$entity_titles_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(names(md_model_draft()$titles), function(idname){
          data.frame(key = idname, id = md_model_draft()$titles[[idname]])
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_titles_table_wrapper <-renderUI({
      if(length(md_model_draft()$titles)>0){
        DTOutput(ns("entity_titles_table"))
      }else{NULL}
    })
    #entity -> Description
    output$entity_descriptions_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(names(md_model_draft()$descriptions), function(idname){
          data.frame(key = idname, id = md_model_draft()$descriptions[[idname]])
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_descriptions_table_wrapper <-renderUI({
      if(length(md_model_draft()$descriptions)>0){
        DTOutput(ns("entity_descriptions_table"))
      }else{NULL}
    })
    #entity -> Contact
    output$entity_contacts_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(md_model_draft()$contacts, function(contact){
          data.frame(key = contact$role, id = if(length(contact$identifiers)>0) contact$identifiers[[1]] else "")
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_contacts_table_wrapper <-renderUI({
      if(length(md_model_draft()$contacts)>0){
        DTOutput(ns("entity_contacts_table"))
      }else{NULL}
    })
    #entity -> Subject
    output$entity_vocabulary_tree <- jsTreeR::renderJstree({
      req(input$entity_vocabulary_server != "custom")
      cached_vocabs = cache_vocabs()
      hierarchy = cached_vocabs[[input$entity_vocabulary_server]]
      if(is.null(hierarchy)) {
        vocab = md_model_subject_selection()
        hierarchy = vocab$get_concepts_hierarchy(
          lang = i18n()$get_translation_language(),
          method = if(!is.null(vocab$rdf)) "R" else "SPARQL",
          out_format = "list"
        )
        cached_vocabs[[input$entity_vocabulary_server]] = hierarchy
        cache_vocabs(cached_vocabs)
      }
      jsTreeR::jstree(hierarchy$children, theme = "proton", checkboxes = T, checkWithText = T, multiple = T, selectLeavesOnly = T)
    })
    output$entity_vocabulary_tree_wrapper <- renderUI({
      if(input$entity_vocabulary_server != "custom"){
        withSpinner(jsTreeR::jstreeOutput(ns("entity_vocabulary_tree")))
      }else{
        NULL
      }
    })
    output$entity_vocabulary_custom <- renderUI({
      req(input$entity_vocabulary_server == "custom")
      shiny::tagList(
        fluidRow(
          column(4,textInput(ns("custom_vocab_thesaurus_name"), "Thesaurus name",value = NULL, width = NULL, placeholder = "Thesaurus name")),
          column(4,textInput(ns("custom_vocab_thesaurus_uri"), "Thesaurus URI",value = NULL, width = NULL, placeholder = "Thesaurus URI"))
        ),
        fluidRow(
          column(4,textInput(ns("custom_vocab_keyword_name"), "Keyword", value = NULL, width = NULL, placeholder = "Keyword")),
          column(4,textInput(ns("custom_vocab_keyword_uri"), "Keyword URI", value = NULL, width = NULL, placeholder = "Keyword URI")),
          column(4,
                 actionButton(ns("custom_vocab_keyword_button_add"), title="Add keyword",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                 actionButton(ns("custom_vocab_keyword_button_clear"), title="Clear keyword",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
          )
        ),
        uiOutput(ns("entity_vocabulary_custom_keyword_table_wrapper"))
      )
    })
    
    output$entity_vocabulary_custom_keyword_table <- DT::renderDT(server = FALSE, {
      req(!is.null(md_model_subject_draft()))
      DT::datatable(
        do.call("rbind", lapply(md_model_subject_draft()$keywords, function(kwd){
          data.frame(keyword = kwd$name, uri = if(!is.null(kwd$uri)) kwd$uri else "")
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_vocabulary_custom_keyword_table_wrapper <-renderUI({
      if(input$entity_vocabulary_server == "custom" & length(md_model_subject_draft()$keywords)>0){
        DTOutput(ns("entity_vocabulary_custom_keyword_table"))
      }else{NULL}  
    })
    output$entity_subjects_table <- DT::renderDT(server = FALSE, {
      req(!is.null(md_model_draft()))
      DT::datatable(
        do.call("rbind", lapply(md_model_draft()$subjects, function(subj){
          print(subj$name)
          print(subj$uri)
          print(sapply(subj$keywords, function(x){x$name}))
          data.frame(key = subj$key, title = subj$name, keywords = paste0(sapply(subj$keywords, function(kwd){kwd$name}),collapse=","))
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_subjects_table_wrapper <-renderUI({
      if(length(md_model_draft()$subjects) > 0){
        DTOutput(ns("entity_subjects_table"))
      }else{NULL}  
    })
    #entity -> Date
    output$entity_dates_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(md_model_draft()$dates, function(date){
          data.frame(dateType = date$key, date = date$value)
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_dates_table_wrapper <-renderUI({
      if(length(md_model_draft()$dates)>0){
        DTOutput(ns("entity_dates_table"))
      }else{NULL}
    })
    #entity -> Type
    output$entity_types_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(names(md_model_draft()$types), function(idname){
          data.frame(key = idname, type = md_model_draft()$types[[idname]])
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_types_table_wrapper <-renderUI({
      if(length(md_model_draft()$types)>0){
        DTOutput(ns("entity_types_table"))
      }else{NULL}
    })
    #entity -> SpatialCoverage
    output$entity_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addDrawToolbar(
          singleFeature = TRUE,
          targetGroup = "draw",
          rectangleOptions = TRUE,
          circleMarkerOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()
        ) %>%
        setView(lng = 0, lat = 0, zoom = 2)
    })
    output$entity_wkt_wrapper <- renderUI({
      if (!is.null(md_model_bbox())) {
        shiny::textInput(ns("entity_wkt"), label = "WKT", placeholder = "WKT", value = md_model_bbox())
      } else {
        shiny::tagList(
          tags$em("Draw a rectangle on the map to see the WKT representation."),
          shiny::textInput(ns("entity_wkt"), label = "WKT", placeholder = "WKT", value = NULL)
        )
      }
    })
    #entity -> TemporalCoverage
    #entity -> Relation
    output$entity_relations_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(md_model_draft()$relations, function(rel){
          data.frame(
            key = rel$key, 
            name = rel$name, 
            description = if(!is.null(rel$description)) rel$description else "", 
            link = rel$link
          )
          
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_relations_table_wrapper <-renderUI({
      if(length(md_model_draft()$relations)>0){
        DTOutput(ns("entity_relations_table"))
      }else{NULL}
    })
    #entity -> Format
    output$entity_formats_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(md_model_draft()$formats, function(format){
          data.frame(
            key = format$key, 
            name = format$name, 
            description = if(!is.null(format$description)) format$description else "", 
            uri = if(!is.null(format$uri)) format$uri else ""
          )
        })), 
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering=F
        )
      )
    })
    output$entity_formats_table_wrapper <-renderUI({
      if(length(md_model_draft()$formats)>0){
        DTOutput(ns("entity_formats_table"))
      }else{NULL}
    })
    
    #contact
    #contact -> Identifier
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
    #core - check_metadata
    observeEvent(input$check_model,{
      INFO(sprintf("Check %s validity", md_model_type()))
      req(!is.null(md_model_type()))
      
      #perform validation
      meta_validator = switch(md_model_type(),
        "contact" = geoflow_validator_contacts$new(source = md_model_draft()$asDataFrame()),
        "entity" = geoflow_validator_entities$new(source = md_model_draft()$asDataFrame())
      )
      qa_errors = meta_validator$validate_content()
      if(nrow(qa_errors)==0){
        md_model_draft_valid(TRUE)
        shinyjs::enable("save_model")
        qa_errors = NULL
      }else{
        md_model_draft_valid(FALSE)
        if(nrow(qa_errors[qa_errors$type == "ERROR",]>0)){
          shinyjs::disable("save_model")
        }else{
          shinyjs::enable("save_model")
        }
      }
      md_model_draft_validation_report(qa_errors)
      if(!is.null(qa_errors)){
        ERROR(paste0("Validation errors with the ", md_model_type(),". Aborting saving the data to geoflow pivot model"))
      }else{
        INFO(paste0("No validation errors/warnings with the ", md_model_type(),". Aborting saving the data to geoflow pivot model"))
      }
    })
    #core - save_metadata
    observeEvent(input$save_model,{
      INFO(sprintf("Save %s to metadata table", md_model_type()))
      req(!is.null(md_model_type()))
      if(md_model_type()=="entity"){
        md_model_subject_draft(NULL)
      }
      qa_errors = md_model_draft_validation_report()
      save_model = TRUE
      if(!is.null(qa_errors)) {
        if("ERROR" %in% qa_errors$type){
          ERROR("There are validation errors, abort saving the model!")
          save_model = FALSE
        }
      }
      if(save_model){
        INFO("Saving model to metadata table")
        meta_elements = md_model()
        if(md_model_draft_idx()==0){
          meta_elements[[length(meta_elements)+1]] = md_model_draft()
        }else{
          meta_elements[[md_model_draft_idx()]] = md_model_draft()
        }
        md_model(meta_elements)
        md_model_draft_mode("edition")
      }
      
    })
    
    #entities
    observeEvent(input$create_entity_table, {
      md_model(list())
      md_model_draft(NULL)
      md_model_draft_idx(0L)
      md_model_draft_mode("creation")
      md_model_draft_valid(NULL)
      md_model_draft_validation_report(NULL)
      md_model_type("entity")
      INFO(sprintf("Select editor for type '%s'", md_model_type()))
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      print(md_model_draft())
      md_model_draft_idx(1L)
    })
    observeEvent(input$create_entity, {
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(length(md_model())+1)
      md_model_draft_mode("creation")
    })
    
    #contacts
    observeEvent(input$create_contact_table, {
      md_model(list())
      md_model_draft(NULL)
      md_model_draft_idx(0L)
      md_model_draft_mode("creation")
      md_model_draft_valid(NULL)
      md_model_draft_validation_report(NULL)
      md_model_type("contact")
      INFO(sprintf("Select editor for type '%s'", md_model_type()))
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(1L)
    })
    observeEvent(input$create_contact,{
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(length(md_model())+1)
      md_model_draft_mode("creation")
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
    
    #SPECIFIC FORM EVENTS
    #entity specific form events
    #----------------------------
    observeEvent(input$entity_form, {
      INFO(sprintf("Selecting tab %s", input$entity_form))
      active_entity_form_tab(input$entity_form)
    })
    observe({
      print(input$entity_form)
      if(!is.null(input$entity_form)) if(input$entity_form != active_entity_form_tab()) {
        isolate({
          updateTabsetPanel(session, ns("entity_form"), selected = active_entity_form_tab()) 
        })
      }
    })
    observeEvent(c(
      input$contact_org,
      input$contact_lastname,
      input$contact_firstname,
      input$contact_positionname,
      input$contact_postaladdress,
      input$contact_postalcode,
      input$contact_city,
      input$contact_country,
      input$contact_email,
      input$contact_voice,
      input$contact_facsimile,
      input$contact_websiteurl,
      input$contact_websitename
    ),{
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
    })
    #events entity -> Identifier
    observeEvent(input$entity_identifier_button_add,{
      entity = md_model_draft()
      entity$setIdentifier(
        key = input$entity_identifier_type,
        id = input$entity_identifier
      )
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_identifier_button_clear,{
      entity = md_model_draft()
      entity$identifiers = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Title
    observeEvent(input$entity_title_button_add,{
      update_meta_editor(FALSE)
      entity = md_model_draft()
      entity$setTitle(
        key = input$entity_title_type,
        title = input$entity_title
      )
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_title_button_clear,{
      entity = md_model_draft()
      entity$titles = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Description
    observeEvent(input$entity_description_button_add,{
      entity = md_model_draft()
      entity$setDescription(
        key = input$entity_description_type,
        description = input$entity_description
      )
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_description_button_clear,{
      entity = md_model_draft()
      entity$descriptions = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Creator
    observeEvent(input$entity_contact_button_add,{
      entity = md_model_draft()
      contact = geoflow_contact$new()
      contact$setRole(input$entity_contact_type)
      contact$setIdentifier("id", input$entity_contact)
      entity$addContact(contact)
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_contact_button_clear,{
      entity = md_model_draft()
      entity$contacts = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Subject
    #custom vocab
    observeEvent(input$custom_vocab_keyword_button_add,{
      INFO("Add a keyword for custom vocab")
      if(is.null(md_model_subject_draft())){
        md_model_subject_draft(geoflow_subject$new())
      }
      print(input$custom_vocab_thesaurus_name)
      print(input$custom_vocab_thesaurus_uri)
      print(input$custom_vocab_keyword_name)
      print(input$custom_vocab_keyword_uri)
      subj = md_model_subject_draft()
      subj$setKey(input$entity_subject_type)
      if(input$custom_vocab_thesaurus_name != "") subj$setName(input$custom_vocab_thesaurus_name)
      if(input$custom_vocab_thesaurus_uri != "") subj$setUri(input$custom_vocab_thesaurus_uri)
      subj$addKeyword(
        keyword = input$custom_vocab_keyword_name,
        uri = if(!is.null(input$custom_vocab_keyword_uri) & input$custom_vocab_keyword_uri != "") input$custom_vocab_keyword_uri else NULL
      )
      md_model_subject_draft(subj$clone(deep = T))
      print(sapply(md_model_subject_draft()$keywords, function(x){x$name}))
    })
    observeEvent(input$custom_vocab_keyword_button_clear,{
      INFO("Clear subject model draft")
      md_model_subject_draft(NULL)
    })
    #existing vocab
    observeEvent(input$entity_vocabulary_server,{
      req(input$entity_vocabulary_server != "custom")
      md_model_subject_draft(NULL)
      vocabs = geoflow::list_vocabularies(T) 
      vocab = vocabs[sapply(vocabs, function(x){
        x$id == input$entity_vocabulary_server
      })][[1]]
      md_model_subject_selection(vocab)
    })
    observe({
      req(input$entity_vocabulary_server != "custom")
      if(is.null(md_model_subject_draft())){
        md_model_subject_draft(geoflow_subject$new())
      }
      subj = md_model_subject_draft()
      subj$setKey(input$entity_subject_type)
      subj$setName(md_model_subject_selection()$def)
      subj$setUri(md_model_subject_selection()$id) #we put here the vocabulary Id
      kwds = sapply(input$entity_vocabulary_tree_checked, function(x){x$text})
      kwds = kwds[kwds != ""]
      subj$keywords = lapply(kwds, function(x){geoflow_keyword$new(name = x)})
      md_model_subject_draft(subj)
      print(subj)
    })
    observeEvent(input$entity_subject_button_add,{
      INFO("Add subject to entity")
      entity = md_model_draft()
      subj = md_model_subject_draft()
      same_subject = sapply(entity$subjects, function(x){x$key == subj$key & x$name == subj$name})
      if(any(same_subject)){
        entity$subjects[[which(same_subject)]] <- subj
      }else{
        entity$addSubject(subj)
      }
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_subject_button_clear,{
      INFO("Clear subjects from entity")
      entity = md_model_draft()
      entity$subjects = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Date
    observeEvent(input$entity_date_button_add,{
      update_meta_editor(FALSE)
      entity = md_model_draft()
      entity$addDate(
        dateType = input$entity_date_type,
        date = input$entity_date
      )
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_date_button_clear,{
      entity = md_model_draft()
      entity$dates = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Type
    observeEvent(input$entity_type_button_add,{
      update_meta_editor(FALSE)
      entity = md_model_draft()
      entity$setType(
        key = input$entity_resource_type,
        type = input$entity_resource
      )
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_type_button_clear,{
      entity = md_model_draft()
      entity$types = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Language
    observeEvent(input$entity_language,{
      entity = md_model_draft()
      entity$setLanguage(input$entity_language)
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> SpatialCoverage
    observeEvent(input$entity_map_draw_new_feature, {
      feature <- input$entity_map_draw_new_feature
      if (feature$geometry$type == "Polygon") {
        coords <- feature$geometry$coordinates[[1]]
        bbox_polygon <- sf::st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE)))
        bbox_sfc <- sf::st_sfc(bbox_polygon) # Convert to sfc
        md_model_bbox(sf::st_as_text(bbox_sfc)) # Convert to WKT
        entity = md_model_draft()
        entity$setSrid(input$entity_srid)
        entity$setSpatialBbox(wkt = md_model_bbox())
        entity$setSpatialExtent(wkt = md_model_bbox())
      }
    })
    observeEvent(input$entity_wkt,{
      # Parse WKT and extract bounding box coordinates
      bbox_polygon <- try(sf::st_as_sfc(input$entity_wkt, crs = input$entity_srid), silent = TRUE) # Convert WKT to sfc object
      if(!is(bbox_polygon, "try-error")){
        bbox_coords <- sf::st_bbox(bbox_polygon) # Get bounding box (xmin, ymin, xmax, ymax)
        
        # Draw bounding box on the map
        leafletProxy("entity_map") %>%
          clearShapes() %>% # Clear previous drawings
          addRectangles(
            lng1 = bbox_coords["xmin"], lat1 = bbox_coords["ymin"],
            lng2 = bbox_coords["xmax"], lat2 = bbox_coords["ymax"],
            color = "blue", fillOpacity = 0.2
          ) %>%
          setView(
            lng = mean(c(bbox_coords["xmin"], bbox_coords["xmax"])),
            lat = mean(c(bbox_coords["ymin"], bbox_coords["ymax"])),
            zoom = 2
          )
      }
    })
    #events entity -> TemporalCoverage
    #events entity -> Relation
    observeEvent(input$entity_relation_button_add,{
      entity = md_model_draft()
      rel = geoflow_relation$new()
      rel$setKey(input$entity_relation_type)
      rel$setName(input$entity_relation_name)
      if(input$entity_relation_description != "") rel$setDescription(input$entity_relation_description)
      rel$setLink(input$entity_relation_link)
      entity$addRelation(rel)
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_relation_button_clear,{
      entity = md_model_draft()
      entity$relations = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Format
    observeEvent(input$entity_format_button_add,{
      entity = md_model_draft()
      form = geoflow_format$new()
      form$setKey(input$entity_format_type)
      form$setName(input$entity_format_name)
      if(input$entity_format_description != "") form$setDescription(input$entity_format_description)
      if(input$entity_format_link != "") form$setLink(input$entity_format_link)
      entity$addFormat(form)
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_format_button_clear,{
      entity = md_model_draft()
      entity$formats = list()
      md_model_draft(entity$clone(deep = T))
    })
    
    #contact specific form events
    #----------------------------
    observeEvent(input$contact_form, {
      active_contact_form_tab(input$contact_form)
    })
    observe({
      print(input$contact_form)
      if(!is.null(input$contact_form)) if(input$contact_form != active_contact_form_tab()) {
        isolate({
          updateTabsetPanel(session, ns("contact_form"), selected = active_contact_form_tab()) 
        })
      }
    })
    observeEvent(input$contact_identifier_button_add,{
      contact = md_model_draft()
      contact$setIdentifier(
        key = input$contact_identifier_type,
        id = input$contact_identifier
      )
      md_model_draft(contact$clone(deep = T))
    })
    observeEvent(input$contact_identifier_button_clear,{
      contact = md_model_draft()
      contact$identifiers = list()
      md_model_draft(contact$clone(deep = T))
    })
    
  })
  
}