#metadata_editor_server
metadata_editor_server<- function(id, auth_info, i18n, geoflow_configs, parent.session){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
    
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
    #model specific reactives
    active_contact_form_tab <- reactiveVal("contact_identifiers")
    active_entity_form_tab <- reactiveVal("entity_identifiers")
    ref_hierarchy <- reactiveVal(NULL)
    ref_contacts <- reactiveVal(NULL)
    
    #FUNCTIONS
    setID = function(type, id){
      sprintf("%s_%s", type, id)
    }
    
    #handle_metadata_form
    handle_metadata_form = function(type, model = NULL){
      switch(type,
        "contact" = bs4Dash::tabsetPanel(
          width = 3,
          id = ns("contact_form"),
          type = "pills", vertical = T,
          tabPanel(
            value = "contact_identifiers",
            title = "Identifier(s)",
            fluidRow(
              column(3, selectInput(ns("contact_identifier_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = contact_tpl$getAllowedKeyValuesFor("Identifier"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(6,textInput(ns("contact_identifier"), "Identifier",value = "", width = NULL, placeholder = "Identifier")),
              column(3,
                     actionButton(ns("contact_identifier_button_add"), title="Add identifier",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("contact_identifiers_table"))
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
        "entity" = bs4Dash::tabsetPanel(
          width = 3,
          id = ns("entity_form"),
          type = "pills", vertical = T,
          tabPanel(
            value = "entity_identifiers",
            title = "Identifier",
            fluidRow(
              column(3, selectInput(ns("entity_identifier_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Identifier"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(6,textInput(ns("entity_identifier"), "Identifier",value = "", width = NULL, placeholder = "Identifier")),
              column(3,
                     actionButton(ns("entity_identifier_button_add"), title="Add identifier",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_identifiers_table"))
          ),
          tabPanel(
            value = "entity_titles",
            title = "Title",
            fluidRow(
              column(3, selectInput(ns("entity_title_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Title"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,textInput(ns("entity_title"), "Title",value = "", width = NULL, placeholder = "Title")),
              column(2,
                     actionButton(ns("entity_title_button_add"), title="Add title",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_titles_table"))
          ),
          tabPanel(
            value = "entity_descriptions",
            title = "Description",
            fluidRow(
              column(3, selectInput(ns("entity_description_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Description"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,textAreaInput(ns("entity_description"), "Description",value = "", width = NULL, placeholder = "Description")),
              column(2,
                     actionButton(ns("entity_description_button_add"), title="Add description",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_descriptions_table"))
          ),
          tabPanel(
            value = "entity_subjects",
            title = "Subject",
            fluidRow(
              column(3, selectInput(ns("entity_subject_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = c(geometa::ISOKeywordType$values(), "taxonomy"),
                                       selected = "theme",
                                       selectize = FALSE
              )),
              column(6, selectInput(ns("entity_vocabulary_server"),
                                       label="Vocabulary server",
                                       multiple = F,
                                       choices = {
                                         vocabs = list_vocabularies()
                                         setNames(c(vocabs$id, "custom"), nm = c(vocabs$def, "Custom"))
                                        },
                                       selected = "custom",
                                       selectize = FALSE
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
              column(3,
                     actionButton(ns("entity_contact_load"), title = "Load contacts", size = "sm", label="", icon=icon("users"))
              )
            ),
            fluidRow(
              column(3, selectInput(ns("entity_contact_type"),
                                       label="Role",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Creator"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,uiOutput(ns("entity_contact_wrapper"))),
              column(2,
                     actionButton(ns("entity_contact_button_add"), title="Add contact",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"))
            ),
            DTOutput(ns("entity_contacts_table"))
          ),
          tabPanel(
            value = "entity_dates",
            title = "Date",
            fluidRow(
              column(3, selectInput(ns("entity_date_type"),
                                       label="Date type",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Date"),
                                       selected = "creation",
                                       selectize = FALSE
              )),
              column(7,dateInput(ns("entity_date"), "Date",value = Sys.Date(), width = NULL)),
              column(2,
                     actionButton(ns("entity_date_button_add"), title="Add date",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_dates_table"))
          ),
          tabPanel(
            value = "entity_types",
            title = "Type",
            fluidRow(
              column(3, selectInput(ns("entity_resource_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Type"),
                                       selected = "generic",
                                       selectize = FALSE
              )),
              column(7,textInput(ns("entity_resource"), "Type",value = "dataset", width = NULL, placeholder = "Type")),
              column(2,
                     actionButton(ns("entity_type_button_add"), title="Add type",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_types_table"))
          ),
          tabPanel(
            value = "entity_languages",
            title = "Language",
            fluidRow(
              column(6, selectInput(ns("entity_language"),
                                       label="Language",
                                       multiple = F,
                                       choices = {
                                         languages = geometa::ISOLanguage$values(labels = T)
                                         setNames(languages[,1], nm = languages[,2])
                                       },
                                       selected = "eng",
                                       selectize = FALSE
              ))
            )
          ),
          tabPanel(
            value = "entity_spatialcoverages",
            title = "SpatialCoverage",
            fluidRow(
              column(6, selectInput(ns("entity_srid"),
                                       label="SRID",
                                       multiple = F,
                                       choices = {
                                         setNames(c(4326), nm = c("WGS 84 (EPSG:4326)"))
                                       },
                                       selected = 4326,
                                       selectize = FALSE
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
            fluidRow(
              fluidRow(
                column(12, dateRangeInput(ns("entity_temporalcoverage"), label = "Temporal coverage"))
              )
            )
          ),
          tabPanel(
            value = "entity_relations",
            title = "Relation",
            fluidRow(
              column(3, selectInput(ns("entity_relation_type"),
                                       label="Key",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Relation"),
                                       selected = "id",
                                       selectize = FALSE
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
            fluidRow(
              column(3, selectInput(ns("entity_right_type"),
                                       label = "Right type",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Rights"),
                                       selected = "license",
                                       selectize = FALSE
              )),
              column(7, uiOutput(ns("entity_right_wrapper"))),
              column(2,
                     actionButton(ns("entity_right_button_add"), title="Add right",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_right_button_clear"), title="Clear right",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_rights_table_wrapper"))
          ),
          tabPanel(
            value = "entity_formats",
            title = "Format",
            fluidRow(
              column(3, selectInput(ns("entity_format_type"),
                                       label="Format type",
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Format"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,selectInput(ns("entity_format_name"),
                                      label="Format",
                                      multiple = F,
                                      
                                      choices = as.character(mime::mimemap),
                                      selected = NULL,
                                      selectize = FALSE
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
            fluidRow(
              column(12, textInput(ns("entity_prov_statement"), "Statement", value = "", width = NULL, placeholder = "Statement")),
            ),
            hr(),
            fluidRow(
              column(5, textInput(ns("entity_prov_process_rationale"), "Process rationale", value = "", width = NULL, placeholder = "Process rationale")),
              column(5, textAreaInput(ns("entity_prov_process_description"), "Process description", value = "", width = NULL, placeholder = "Process description")),
              column(2,
                     actionButton(ns("entity_prov_process_button_add"), title="Add process",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                     actionButton(ns("entity_prov_process_button_clear"), title="Clear processes",size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
              )
            ),
            uiOutput(ns("entity_processes_table_wrapper"))
          ),
          tabPanel(
            value = "entity_datasets",
            title = "Data",
            fluidRow(
              bs4Dash::accordion(
                id = "entity_data_blocks",
                bs4Dash::accordionItem(
                  title = tags$span(icon("download"), " ", "Data access"),
                  fluidRow(
                    column(4, selectInput(ns("entity_data_access"),
                                          label="Access",
                                          multiple = F,
                                          choices = {
                                            geoflow::list_data_accessors()$id
                                          },
                                          selected = if(!is.null(model)) model$data$access else NULL,
                                          selectize = FALSE
                    )),
                    column(4, selectInput(ns("entity_data_type"),
                                          label="Mode",
                                          multiple = F,
                                          choices = {
                                            setNames(
                                              c("source", "dir"),
                                              nm = c("File(s)", "Directory")
                                            )
                                          },
                                          selected = "source",
                                          selectize = FALSE
                    )),
                    column(4, selectInput(ns("entity_data_sourcetype"),
                                          label="Source type",
                                          multiple = F,
                                          choices = {
                                            geoflow::geoflow_data$new()$getAllowedSourceTypes()
                                          },
                                          selected = if(!is.null(model)) model$data$sourceType else NULL,
                                          selectize = FALSE
                    ))
                  ),
                  hr(),
                  uiOutput(ns("entity_data_type_entry")),
                  hr(),
                  fluidRow(
                    column(12, textAreaInput(
                      ns("entity_data_sourcesql"),
                      label = "Source SQL",
                      value = if(!is.null(model)) model$data$sourceSql else NULL,
                      width = NULL,
                      placeholder = "Source SQL"
                    ))
                  )
                ),
                bs4Dash::accordionItem(
                  title = tags$span(icon("table-list"), " ", "Data characteristics"),
                  icon = icon("table-list"),
                  fluidRow(
                    column(6, selectInput(ns("entity_data_spatialrepresentationtype"),
                                          label="Spatial Representation type",
                                          multiple = F,
                                          choices = {
                                            c("vector","grid")
                                          },
                                          selected = if(!is.null(model)) model$data$spatialRepresentationType else "vector",
                                          selectize = FALSE
                    )),
                    column(4, textInput(ns("entity_data_featuretype"),
                                          label="Feature type",
                                          width = NULL,
                                          value = if(!is.null(model)) model$data$featureType else NULL,
                                          placeholder = "Feature type"
                    )),
                  )
                ),
                bs4Dash::accordionItem(
                  title = tags$span(icon("upload"), " ", "Upload configuration"),
                  fluidRow(
                    column(4, selectInput(ns("entity_data_upload"),
                                          label="Upload?",
                                          multiple = F,
                                          choices = {
                                            c(TRUE,FALSE)
                                          },
                                          selected = if(!is.null(model)) model$data$upload else TRUE,
                                          selectize = FALSE
                    )),
                    column(4, selectInput(ns("entity_data_uploadtype"),
                                          label="Upload type",
                                          multiple = F,
                                          choices = {
                                            geoflow::geoflow_data$new()$getAllowedUploadTypes()
                                          },
                                          selected = if(!is.null(model)) model$data$uploadType else NULL,
                                          selectize = FALSE
                    ))
                  ),
                  hr(),
                  fluidRow(
                    column(6, textInput(
                      ns("entity_data_uploadsource"),
                      label = "Upload source",
                      value = if(!is.null(model) & !is.null(model$data$uploadSource)) model$data$uploadSource[[1]] else NULL,
                      width = NULL,
                      placeholder = "Source to upload"
                    ))
                  )
                ),
                bs4Dash::accordionItem(
                  title = tags$span(icon("upload"), icon("globe"), " ", "Upload configuration - GeoServer settings"),
                  h6("Layer identification"),
                  fluidRow(
                    column(6,textInput(ns("entity_data_layername"),
                                       label = "Name",
                                       value = if(!is.null(model)) model$data$layername else NULL,
                                       width = NULL,
                                       placeholder = "Name"
                    )),
                    column(6,textInput(ns("entity_data_layeruri"),
                                       label = "URI",
                                       value = if(!is.null(model)) model$data$layeruri else NULL,
                                       width = NULL,
                                       placeholder = "URI"
                    ))
                  ),
                  fluidRow(
                    column(12,textInput(ns("entity_data_layertitle"),
                                        label = "Title",
                                        value = if(!is.null(model)) model$data$layertitle else NULL,
                                        width = NULL,
                                        placeholder = "Title"
                    ))
                  ),
                  fluidRow(
                    column(12,textAreaInput(
                      ns("entity_data_layerdesc"),
                      label = "Description",
                      value = if(!is.null(model)) model$data$layerdesc else NULL,
                      width = NULL,
                      placeholder = "Description"
                    ))
                  ),
                  h6("SQL View layer settings"),
                  fluidRow(
                    column(12, textAreaInput(
                      ns("entity_data_sql"),
                      label = "SQL (for Geoserver SQL views)",
                      value = if(!is.null(model)) model$data$sql else NULL,
                      width = NULL,
                      placeholder = "SQL (for Geoserver SQL views)"
                    ))
                  ),
                  fluidRow(
                    column(6, textInput(
                      ns("entity_data_geometry_field"),
                      label = "Geometry field",
                      value = if(!is.null(model)) model$data$geometryField else NULL,
                      width = NULL,
                      placeholder = "Geometry field"
                    )),
                    column(6, selectInput(ns("entity_data_geometry_type"),
                      label="Geometry type",
                      multiple = F,
                      choices = {
                        c("Geometry", "GeometryCollection", "Point","MultiPoint","LineString","MultiLineString","Polygon","MultiPolygon")
                      },
                      selected =  if(!is.null(model)) model$data$geometryType else "Geometry",
                      selectize = FALSE
                    ))
                  ),
                  fluidRow(
                    column(3, textInput(ns("entity_data_parameter_fieldname"),
                      label = "Parameter fieldname",
                      value = NULL,
                      width = NULL,
                      placeholder = "Parameter fieldname"
                    )),
                    column(3, textInput(ns("entity_data_parameter_alias"),
                      label = "Parameter alias (optional)",
                      value = NULL,
                      width = NULL,
                      placeholder = "Parameter alias"
                    )),
                    column(3, textInput(ns("entity_data_parameter_regexp"),
                      label = "Parameter control (regexp)",
                      value = NULL,
                      width = NULL,
                      placeholder = "Parameter control (regexp)"
                    )),
                    column(2, textInput(ns("entity_data_parameter_defaultvalue"),
                      label = "Default value",
                      value = NULL,
                      width = NULL,
                      placeholder = "Default value"
                    )),
                    column(1,
                           actionButton(ns("entity_data_parameter_button_add"), title="Add parameter",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
                    )
                  ),
                  uiOutput(ns("entity_data_parameters_table_wrapper"))
                ),
                bs4Dash::accordionItem(
                  title = tags$span(icon("upload"), icon("cloud"), " ", "Upload configuration - Cloud settings"),
                  fluidRow(
                  )
                ),
                bs4Dash::accordionItem(
                  title = "Dictionary settings",
                  fluidRow(
                  )
                )
              )
            )
          )
        ),
        "dictionary" = "COMING SOON"
      )
      
    }
    
    #check_model
    check_model = function(type, model){
      INFO(sprintf("Check %s validity", type))
      req(!is.null(type))
      INFO("Copying view to model")
      switch(type,
             "contact" = {
               contact = model
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
             },
             "entity" = {
               entity = model
               #main dc fields managed through observers
               #data simple fields
               #=> data -> data access fields
               entity$data$setAccess(input$entity_data_access)
               switch(input$entity_data_type,
                "dir" = {
                  entity$data$source = NULL
                  entity$data$dir = input$entity_data_dir
                },
                "source"= {
                  entity$data$dir = NULL
                  #sources are managed through observer
                }
               )
               entity$data$setSourceType(input$entity_data_sourcetype)
               #=> data -> data characteristics
               if(nzchar(input$entity_data_spatialrepresentationtype)) entry$data$setSpatialRepresentationType(input$entity_data_spatialrepresentationtype)
               if(nzchar(input$entity_data_featuretype)) entity$data$setFeatureType(input$entity_data_featuretype)
               #=> data -> upload fields
               if(nzchar(input$entity_data_uploadtype)) entity$data$setUploadType(input$entity_data_uploadtype)
               if(nzchar(input$entity_data_uploadsource)) entity$data$setUploadSource(input$entity_data_uploadsource)
               #=> data -> Geoserver fields
               if(nzchar(input$entity_data_layername)) entity$data$setLayername(input$entity_data_layername)
               if(nzchar(input$entity_data_layertitle)) entity$data$setLayertitle(input$entity_data_layertitle)
               if(nzchar(input$entity_data_layerdesc)) entity$data$setLayerdesc(input$entity_data_layerdesc)
               if(nzchar(input$entity_data_layeruri)) entity$data$setLayeruri(input$entity_data_layeruri)
               if(nzchar(input$entity_data_sql)) entity$data$setSql(input$entity_data_sql)
               if(nzchar(input$entity_data_geometry_field)) entity$data$setGeometryField(input$entity_data_geometry_field)
               if(nzchar(input$entity_data_geometry_type)) entity$data$setGeometryType(input$entity_data_geometry_type)
               #parameters managed through observer
               
               md_model_draft(entity$clone(deep = T))
             }
      )
      
      #perform validation
      meta_validator = switch(type,
                              "contact" = geoflow_validator_contacts$new(source = md_model_draft()$asDataFrame()),
                              "entity" = geoflow_validator_entities$new(source = md_model_draft()$asDataFrame())
      )
      qa_errors = meta_validator$validate_content()
      print(qa_errors)
      valid = FALSE
      if(nrow(qa_errors)==0){
        INFO(paste0("No validation errors with the ", type,". Saving data to geoflow pivot model"))
        valid = TRUE
      }else{
        if(nrow(qa_errors[qa_errors$type == "ERROR",]>0)){
          ERROR(paste0("Validation errors with the ", type,". Aborting saving the data to geoflow pivot model"))
          valid = FALSE
        }else{
          WARN(paste0("Validation warnings with the ", type,". Saving data to geoflow pivot model"))
          valid = TRUE
        }
      }
      md_model_draft_valid(valid)
      md_model_draft_validation_report(qa_errors)
    }
    
    #render_field_elements_table
    render_field_elements_table = function(field, object_field = NULL, field_model = c("kvp","attributes"), list_elements = list(),
                                           field_key = NULL, field_value = NULL, field_value_list = FALSE, 
                                           btn_remove_id){
      objs <- md_model_draft()[[field]]
      if(!is.null(object_field)) objs <- objs[[object_field]]
      if (is.null(objs) || length(objs) == 0) {
        tbl <- tibble::tibble(key = character(0), value = character(0), delete = character(0))
      } else {
        tbl <- switch(field_model,
          "kvp" = {
            is_named_list = !is.null(names(objs))
            if(is_named_list){
              if(length(list_elements)==0){
                do.call(rbind, lapply(names(objs), function(objname) {
                  tibble::tibble(key = objname, value = objs[[objname]])
                }))
              }else{
                print(names(objs))
                do.call(rbind, lapply(names(objs), function(objname){
                  tibble::as_tibble(
                    do.call(cbind, lapply(list_elements, function(x){
                      col_tbl = tibble::tibble(col = if(!is.null(objs[[objname]][[x]])) objs[[objname]][[x]] else "-")
                      names(col_tbl) = x
                      return(col_tbl)
                    }))
                  )
                }))
              }
            }else{
              do.call("rbind", lapply(objs, function(obj){
                data.frame(key = obj[[field_key]], value = if(field_value_list) obj[[field_value]][[1]] else obj[[field_value]])
              }))
            }
          },
          "attributes" = {
            do.call("rbind", lapply(objs, function(obj){
              data.frame(
                name = obj,
                description = if(!is.null(attr(obj, "description"))) attr(obj, "description") else "-",
                uri = if(!is.null(attr(obj, "uri"))) attr(obj, "uri") else "-"
              )
            }))
          }
        )
        tbl$delete <- sprintf(
          '<button class="delete_btn btn btn-link" style="color:red;" data-row="%s" title="Delete">
        <span class="glyphicon glyphicon-trash" aria-hidden="true"></span>
      </button>', seq_len(length(objs))
        )
      }
      names(tbl)[length(names(tbl))] <- "Actions"
      if(nrow(tbl)>0) DT::datatable(
        tbl,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          dom = 't'
        ),
        callback = JS(
          sprintf(
            "table.on('click', 'button.delete_btn', function() {
               var row = $(this).data('row');
               Shiny.setInputValue('%s', row, {priority: 'event'});
             });", btn_remove_id
          )
        )
      )
    }
    #handle_field_element_remove_event
    handle_field_element_remove_event = function(field, object_field = NULL, input_btn_remove){
      row <- as.numeric(input_btn_remove)
      model <- md_model_draft()
      if(is.null(object_field)){
        if (!is.null(model[[field]]) && length(model[[field]]) > 0 && !is.na(row) && row >= 1 && row <= length(model[[field]])) {
          model[[field]] <- model[[field]][-row]
        } else {
          model[[field]] <- list()
        }
        if(length(model[[field]])==0) model[[field]] = list()
      }else{
        if (!is.null(model[[field]][[object_field]]) && length(model[[field]][[object_field]]) > 0 && !is.na(row) && row >= 1 && row <= length(model[[field]][[object_field]])) {
          model[[field]][[object_field]] <- model[[field]][[object_field]][-row]
        } else {
          model[[field]][[object_field]] <- list()
        }
        if(length(model[[field]][[object_field]])==0) model[[field]][[object_field]] = list()
      }
      check_model(type = md_model_type(), model = model)
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
            bs4Dash::actionButton(inputId = ns("load_contact_table"), label = "Load table"),
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
      print("render meta editor")
      req(!is.null(md_model_type()))
      valid = md_model_draft_valid()
      print(valid)
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
              bs4Dash::actionButton(inputId = ns("save_model"), label = "Save", style= if(is.null(valid) || (is.logical(valid) & !valid)) "display:none;" else {NULL}),br(),
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
    
    output$meta_editor_entry_selector_wrapper <- renderUI({
      req(length(md_model())>0)
      selectInput(ns("meta_editor_entry_selector"),
                     label = sprintf("Edit %s entry", md_model_type()),
                     multiple = F,
                     choices = {
                       switch(md_model_type(),
                         "contact" = {
                           if(length(md_model())>0){
                             setNames(
                               object = sapply(md_model(), function(x){if(length(x$identifiers)>0) x$identifiers[[1]] else "?"}),
                               nm = sapply(md_model(), function(x){
                                 if(nzchar(x$firstName) & !is.na(x$firstName) & nzchar(x$lastName) & !is.na(x$lastName)){
                                   paste(x$firstName, x$lastName)
                                 }else{
                                   if(nzchar(x$organizationName)){
                                     x$organizationName
                                   }else{
                                     if(length(x$identifiers)>0) x$identifiers[[1]] else "?"
                                   }
                                 }
                              })
                             )
                           }else{
                             c()
                           }
                         },
                         "entity" = {
                           if(length(md_model())>0){
                             sapply(md_model(), function(x){if(length(x$identifiers)>0) x$identifiers[[1]] else "?"})
                           }else{
                             c()
                           }
                           
                         }
                       )
                     },
                     selected = if(md_model_draft_mode() == "edition" & length(md_model_draft()$identifiers)>0) md_model_draft()$identifiers[[1]] else NULL,
                     selectize = FALSE
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
        INFO("Conversion done!")
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
      render_field_elements_table(
        field = "identifiers", 
        field_model = "kvp",
        btn_remove_id = ns("entity_identifier_button_remove")
      )
    })

    #entity -> Title
    output$entity_titles_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "titles",
        field_model = "kvp",
        btn_remove_id = ns("entity_title_button_remove")
      )
    })
    
    #entity -> Description
    output$entity_descriptions_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "descriptions",
        field_model = "kvp",
        btn_remove_id = ns("entity_description_button_remove")
      )
    })
    
    #entity -> Contact
    output$entity_contact_wrapper <- renderUI({
      if(is.null(ref_contacts())){
        textInput(ns("entity_contact"), "Contact",value = "", width = NULL, placeholder = "Contact")
      }else{
        selectInput(ns("entity_contact"),
                       label="Contact",
                       multiple = F,
                       choices = {
                         setNames(
                           sapply(ref_contacts(), function(x){x$identifiers[[1]]}), 
                           nm = sapply(ref_contacts(), function(x){
                             if(nzchar(x$firstName) & !is.na(x$firstName) & nzchar(x$lastName) & !is.na(x$lastName)){
                               paste(x$firstName, x$lastName)
                             }else{
                               if(nzchar(x$organizationName)){
                                 x$organizationName
                               }else{
                                 if(length(x$identifiers)>0) x$identifiers[[1]] else "?"
                               }
                             }
                           })
                         )
                       },
                       selected = NULL,
                       selectize = FALSE
        )
      }
    })
    output$entity_contacts_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "contacts",
        field_model = "kvp",
        field_key = "role", field_value = "identifiers", field_value_list = TRUE,
        btn_remove_id = ns("entity_contact_button_remove")
      )
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
          data.frame(key = subj$key, title = if(!is.null(subj$name)) subj$name else "-", keywords = paste0(sapply(subj$keywords, function(kwd){kwd$name}),collapse=","))
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
      render_field_elements_table(
        field = "dates",
        field_model = "kvp",
        field_key = "key", field_value = "value",
        btn_remove_id = ns("entity_date_button_remove")
      )
    })
  
    #entity -> Type
    output$entity_types_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "types",
        field_model = "kvp",
        btn_remove_id = ns("entity_type_button_remove")
      )
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
    #entity -> Rights
    output$entity_right_wrapper <- renderUI({
      switch(input$entity_right_type,
        "license" = {
          selectInput(ns("entity_right"),
                         label="License",
                         multiple = F,
                         choices = {
                           licenses = zen4R::get_licenses()
                           setNames(licenses$id, nm = licenses$title)
                         },
                         selected = "cc-by-4.0",
                         selectize = FALSE
          )
        },
        "useConstraint" = {
          selectInput(ns("entity_right"),
                         label="Use constraint",
                         multiple = F,
                         choices = geometa::ISORestriction$values(),
                         selected = NULL,
                         selectize = FALSE
          )
        },
        "accessConstraint" = {
          selectInput(ns("entity_right"),
                         label="Access constraint",
                         multiple = F,
                         choices = geometa::ISORestriction$values(),
                         selected = NULL,
                         selectize = FALSE
          )
        },
        "otherConstraint" = {
          textInput(ns("entity_right"), "Other constraint",value = "", width = NULL, placeholder = "Other constraint")
        },
        "use" = {
          textInput(ns("entity_right"), "Use limitation",value = "", width = NULL, placeholder = "Use limitation")
        },
        "useLimitation" = {
          textInput(ns("entity_right"), "Use limitation",value = "", width = NULL, placeholder = "Use limitation")
        },
        "accessRight" = {
          textInput(ns("entity_right"), "Access right",value = "", width = NULL, placeholder = "Access right")
        },
        "accessConditions" = {
          textInput(ns("entity_right"), "Access conditions",value = "", width = NULL, placeholder = "Access conditions")
        }
      )
    })
    output$entity_rights_table <- DT::renderDT(server = FALSE, {
      DT::datatable(
        do.call("rbind", lapply(md_model_draft()$rights, function(right){
          data.frame(
            key = right$key, 
            name = right$values[[1]]
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
    output$entity_rights_table_wrapper <-renderUI({
      if(length(md_model_draft()$rights)>0){
        DTOutput(ns("entity_rights_table"))
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
    #entity -> Provenance
    output$entity_processes_table <- DT::renderDT(server = FALSE, {
      req(!is.null(md_model_draft()$provenance))
      DT::datatable(
        do.call("rbind", lapply(md_model_draft()$provenance$processes, function(process){
          data.frame(
            rationale = process$rationale, 
            description = process$description
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
    output$entity_processes_table_wrapper <-renderUI({
      if(length(md_model_draft()$provenance$processes)>0){
        DTOutput(ns("entity_processes_table"))
      }else{NULL}
    })
    #entity -> Data
    output$entity_data_type_entry <- renderUI({
      if(input$entity_data_type == "source"){
        tagList(
          fluidRow(
            column(4,textInput(ns("entity_data_source_name"), "Source name",value = NULL, width = NULL)),
            column(6,textInput(ns("entity_data_source_uri"), "Source path/URL",value = NULL, width = NULL)),
            column(1,
                   actionButton(ns("entity_data_source_button_add"), title="Add source",size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
            )
          ),
          hr(),
          uiOutput(ns("entity_data_sources_table_wrapper"))
        )
      }else if(input$entity_data_type == "dir"){
        fluidRow(
          column(12,textInput(ns("entity_data_dir"), "Directory",value = NULL, width = NULL)),
        )
      }
    })
    output$entity_data_sources_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "data",
        object_field = "source",
        field_model = "attributes",
        btn_remove_id = ns("entity_data_source_button_remove")
      )
    })
    output$entity_data_sources_table_wrapper <-renderUI({
      if(length(md_model_draft()$data$source)>0){
        DTOutput(ns("entity_data_sources_table"))
      }else{NULL}
    })
    output$entity_data_parameters_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "data",
        object_field = "parameters",
        field_model = "kvp",
        list_elements = c("name", "fieldname", "regexp", "defaultvalue"),
        btn_remove_id = ns("entity_data_parameter_button_remove")
      )
    })
    output$entity_data_parameters_table_wrapper <-renderUI({
      if(length(md_model_draft()$data$parameters)>0){
        DTOutput(ns("entity_data_parameters_table"))
      }else{NULL}
    })
    
    
    #contact
    #contact -> Identifier
    output$contact_identifiers_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "identifiers",
        field_model = "kvp",
        btn_remove_id = ns("contact_identifier_button_remove")
      )
    })
    
    #EVENTS
    #core - check_metadata
    observeEvent(input$check_model,{
      check_model(type = md_model_type(), model = md_model_draft())
    })
    #core - save_metadata
    observeEvent(input$save_model,{
      INFO(sprintf("Save %s to metadata table", md_model_type()))
      req(!is.null(md_model_type()))
      if(md_model_type()=="entity"){
        md_model_subject_draft(NULL)
        md_model_bbox(NULL)
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
        print(length(meta_elements))
        if(md_model_draft_idx()==0){
          meta_elements[[length(meta_elements)+1]] = md_model_draft()
        }else{
          meta_elements[[md_model_draft_idx()]] = md_model_draft()
        }
        md_model(meta_elements)
        md_model_draft_mode("edition")#triggers twice the render model
        updateSelectInput(inputId = "meta_editor_entry_selector", selected = if(length(md_model_draft()$identifiers)>0) md_model_draft()$identifiers[[1]] else NULL)
      }
      
    })
    
    #core - select entry (meta_editor_entry_selector)
    observeEvent(input$meta_editor_entry_selector,{
      has_entry = sapply(md_model(), function(x){ input$meta_editor_entry_selector %in% c(x$identifiers,"?") })
      selected_entry = md_model()[has_entry][[1]]
      md_model_draft_idx(which(has_entry))
      md_model_draft(selected_entry$clone(deep = TRUE))
    })
    
    #entities
    observeEvent(input$create_entity_table, {
      md_model(list())
      md_model_draft(NULL)
      md_model_draft_idx(0L)
      md_model_draft_mode("creation")
      md_model_draft_valid(NULL)
      md_model_draft_validation_report(NULL)
      md_model_subject_selection(NULL)
      md_model_subject_draft(NULL)
      md_model_bbox(NULL)
      md_model_type("entity")
      INFO(sprintf("Select editor for type '%s'", md_model_type()))
      entity = geoflow::geoflow_entity$new()
      entity$data = geoflow::geoflow_data$new()
      md_model_draft( entity )
      md_model_draft_idx(1L)
    })
    observeEvent(input$load_entity_table, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Load list of entities",
          if(appConfig$auth){
            withSpinner(
              tagList(
                jsTreeR::jstreeOutput(ns("entities_load_tree")),
                actionButton(ns("entities_load_tree_select"), label = "Select", style = "float:right")
              )   
            )
          }else{
            tagList(
              fileInput(ns("entities_local_file"), label = "File",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = "Choose file"),
              actionButton(ns("entities_local_file_select"), label = "Select", style = "float:right")
            )
          },
          easyClose = TRUE, footer = NULL 
        )
      )
    })
    output$entities_load_tree <- jsTreeR::renderJstree({
      jsTreeR::jstree(
        list(
          build_tree_data_dir(
            auth_api = AUTH_API, 
            root = appConfig$data_dir_remote,
            mime_types = c(".csv", ".xlsx", ".xls")
          )
        ),
        types = list(
          file = list(icon = "jstree-file"),
          folder = list(icon = "jstree-folder")
        ),
        selectLeavesOnly = TRUE,
        checkboxes = FALSE,
        multiple = FALSE,
        search = TRUE
      )
    })
    observeEvent(input$entities_load_tree_select,{
      selected_resource = input$entities_load_tree_selected
      
      config = list()
      config$profile$id = "load_ocs_entities"
      config$software$input$ocs = AUTH_API
      config = geoflow::add_config_utils(config)
      entity_handler = geoflow::geoflow_handler$new(yaml = system.file("metadata/entity", "entity_handler_ocs.yml", package = "geoflow"))
      entities = entity_handler$fun(
        handler = entity_handler,
        source = selected_resource[[1]]$data,
        config = config
      )
      md_model_type("entity")
      md_model(entities)
      md_model_draft_mode("edition")#triggers twice the render model
      updateSelectInput(inputId = "meta_editor_entry_selector", selected = NULL)
      
      shiny::removeModal()
    })
    observeEvent(input$entities_local_file_select,{
      req(!is.null(input$entities_local_file))
      
      config = list()
      config$profile$id = "load_local_entities"
      config = geoflow::add_config_utils(config)
      entity_handler = switch(mime::guess_type(input$entities_local_file$datapath),
                               "text/csv" = geoflow::geoflow_handler$new(yaml = system.file("metadata/entity", "entity_handler_csv.yml", package = "geoflow")),
                               "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = geoflow::geoflow_handler$new(yaml = system.file("metadata/entity", "entity_handler_excel.yml", package = "geoflow")),
                               "application/vn.ms-excel" = geoflow::geoflow_handler$new(yaml = system.file("metadata/entity", "entity_handler_excel.yml", package = "geoflow"))
      )
      entities = entity_handler$fun(
        handler = entity_handler,
        source = input$entities_local_file$datapath,
        config = config
      )
      md_model_type("entity")
      md_model(entities)
      md_model_draft_mode("edition")#triggers twice the render model
      updateSelectInput(inputId = "meta_editor_entry_selector", selected = NULL)
      
      shiny::removeModal()
      
    })
    observeEvent(input$create_entity, {
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(length(md_model())+1)
      md_model_draft_mode("creation")
      md_model_draft_valid(NULL)
      md_model_draft_validation_report(NULL)
    })
    
    #contacts
    observeEvent(input$create_contact_table, {
      md_model(list())
      md_model_draft(NULL)
      md_model_draft_idx(0L)
      md_model_draft_mode("creation")
      md_model_draft_valid(NULL)
      md_model_draft_validation_report(NULL)
      md_model_subject_selection(NULL)
      md_model_subject_draft(NULL)
      md_model_bbox(NULL)
      md_model_type("contact")
      INFO(sprintf("Select editor for type '%s'", md_model_type()))
      contact = geoflow::geoflow_contact$new()
      md_model_draft( contact )
      md_model_draft_idx(1L)
    })
    observeEvent(input$load_contact_table, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Load list of contacts",
          if(appConfig$auth){
            withSpinner(
              tagList(
                jsTreeR::jstreeOutput(ns("contacts_load_tree")),
                actionButton(ns("contacts_load_tree_select"), label = "Select", style = "float:right")
              )   
            )
          }else{
            tagList(
              fileInput(ns("contacts_local_file"), label = "File",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = "Choose file"),
              actionButton(ns("contacts_local_file_select"), label = "Select", style = "float:right")
            )
          },
          easyClose = TRUE, footer = NULL 
        )
      )
    })
    output$contacts_load_tree <- jsTreeR::renderJstree({
      jsTreeR::jstree(
        list(
          build_tree_data_dir(
            auth_api = AUTH_API, 
            root = appConfig$data_dir_remote,
            mime_types = c(".csv", ".xlsx", ".xls")
          )
        ),
        types = list(
          file = list(icon = "jstree-file"),
          folder = list(icon = "jstree-folder")
        ),
        selectLeavesOnly = TRUE,
        checkboxes = FALSE,
        multiple = FALSE,
        search = TRUE
      )
    })
    observeEvent(input$contacts_load_tree_select,{
      selected_resource = input$contacts_load_tree_selected
      
      config = list()
      config$profile$id = "load_ocs_contacts"
      config$software$input$ocs = AUTH_API
      config = geoflow::add_config_utils(config)
      contact_handler = geoflow::geoflow_handler$new(yaml = system.file("metadata/contact", "contact_handler_ocs.yml", package = "geoflow"))
      contacts = contact_handler$fun(
        handler = contact_handler,
        source = selected_resource[[1]]$data,
        config = config
      )
      md_model_type("contact")
      md_model(contacts)
      md_model_draft_mode("edition")#triggers twice the render model
      updateSelectInput(inputId = "meta_editor_entry_selector", selected = NULL)

      shiny::removeModal()
    })
    observeEvent(input$contacts_local_file_select,{
      req(!is.null(input$contacts_local_file))
      
      config = list()
      config$profile$id = "load_local_contacts"
      config = geoflow::add_config_utils(config)
      contact_handler = switch(mime::guess_type(input$contacts_local_file$datapath),
                               "text/csv" = geoflow::geoflow_handler$new(yaml = system.file("metadata/contact", "contact_handler_csv.yml", package = "geoflow")),
                               "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = geoflow::geoflow_handler$new(yaml = system.file("metadata/contact", "contact_handler_excel.yml", package = "geoflow")),
                               "application/vn.ms-excel" = geoflow::geoflow_handler$new(yaml = system.file("metadata/contact", "contact_handler_excel.yml", package = "geoflow"))
      )
      contacts = contact_handler$fun(
        handler = contact_handler,
        source = input$contacts_local_file$datapath,
        config = config
      )
      md_model_type("contact")
      md_model(contacts)
      md_model_draft_mode("edition")#triggers twice the render model
      updateSelectInput(inputId = "meta_editor_entry_selector", selected = NULL)
      
      shiny::removeModal()
      
    })
    observeEvent(input$create_contact,{
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(length(md_model())+1)
      md_model_draft_mode("creation")
      md_model_draft_valid(NULL)
      md_model_draft_validation_report(NULL)
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
    #events entity -> Identifier
    observeEvent(input$entity_identifier_button_add,{
      entity = md_model_draft()
      entity$setIdentifier(
        key = input$entity_identifier_type,
        id = input$entity_identifier
      )
      check_model(type = md_model_type(), model = entity)
    })
    observeEvent(input$entity_identifier_button_remove,{
      handle_field_element_remove_event(field = "identifiers", input_btn_remove = input$entity_identifier_button_remove)
    })
    #events entity -> Title
    observeEvent(input$entity_title_button_add,{
      entity = md_model_draft()
      entity$setTitle(
        key = input$entity_title_type,
        title = input$entity_title
      )
      check_model(type = md_model_type(), model = entity)
    })
    observeEvent(input$entity_title_button_remove,{
      handle_field_element_remove_event(field = "titles", input_btn_remove = input$entity_title_button_remove)
    })
    #events entity -> Description
    observeEvent(input$entity_description_button_add,{
      entity = md_model_draft()
      entity$setDescription(
        key = input$entity_description_type,
        description = input$entity_description
      )
      check_model(type = md_model_type(), model = entity)
    })
    observeEvent(input$entity_description_button_remove,{
      handle_field_element_remove_event(field = "descriptions", input_btn_remove = input$entity_description_button_remove)
    })
    #events entity -> Creator
    observeEvent(input$entity_contact_load,{
      shiny::showModal(
        shiny::modalDialog(
          title = "Load list of contacts",
          if(appConfig$auth){
            withSpinner(
              tagList(
                jsTreeR::jstreeOutput(ns("entity_contacts_load_tree")),
                actionButton(ns("entity_contacts_load_tree_select"), label = "Select", style = "float:right")
              )   
            )
          }else{
            tagList(
              fileInput(ns("entity_contacts_local_file"), label = "File",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = "Choose file"),
              actionButton(ns("entity_contacts_local_file_select"), label = "Select", style = "float:right")
            )
          },
          easyClose = TRUE, footer = NULL 
        )
      )
    })
    output$entity_contacts_load_tree <- jsTreeR::renderJstree({
      jsTreeR::jstree(
        list(
          build_tree_data_dir(
            auth_api = AUTH_API, 
            root = appConfig$data_dir_remote,
            mime_types = c(".csv", ".xlsx", ".xls")
          )
        ),
        types = list(
          file = list(icon = "jstree-file"),
          folder = list(icon = "jstree-folder")
        ),
        selectLeavesOnly = TRUE,
        checkboxes = FALSE,
        multiple = FALSE,
        search = TRUE
      )
    })
    observeEvent(input$entity_contacts_load_tree_select,{
      selected_resource = input$entity_contacts_load_tree_selected
      
      config = list()
      config$profile$id = "load_ocs_contacts"
      config$software$input$ocs = AUTH_API
      config = geoflow::add_config_utils(config)
      contact_handler = geoflow::geoflow_handler$new(yaml = system.file("metadata/contact", "contact_handler_ocs.yml", package = "geoflow"))
      contacts = contact_handler$fun(
        handler = contact_handler,
        source = selected_resource[[1]]$data,
        config = config
      )
      ref_contacts(contacts)
      shiny::removeModal()
    })
    observeEvent(input$entity_contacts_local_file_select,{
      req(!is.null(input$entity_contacts_local_file))
      
      config = list()
      config$profile$id = "load_local_contacts"
      config = geoflow::add_config_utils(config)
      contact_handler = switch(mime::guess_type(input$entity_contacts_local_file$datapath),
        "text/csv" = geoflow::geoflow_handler$new(yaml = system.file("metadata/contact", "contact_handler_csv.yml", package = "geoflow")),
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = geoflow::geoflow_handler$new(yaml = system.file("metadata/contact", "contact_handler_excel.yml", package = "geoflow")),
        "application/vn.ms-excel" = geoflow::geoflow_handler$new(yaml = system.file("metadata/contact", "contact_handler_excel.yml", package = "geoflow"))
      )
      contacts = contact_handler$fun(
        handler = contact_handler,
        source = input$entity_contacts_local_file$datapath,
        config = config
      )
      ref_contacts(contacts)
      shiny::removeModal()
      
    })
    observeEvent(input$entity_contact_button_add,{
      entity = md_model_draft()
      contact = geoflow_contact$new()
      contact$setRole(input$entity_contact_type)
      contact$setIdentifier("id", input$entity_contact)
      entity$addContact(contact)
      check_model(type = md_model_type(), model = entity)
    })
    observeEvent(input$entity_contact_button_remove,{
      handle_field_element_remove_event(field = "contacts", input_btn_remove = input$entity_contact_button_remove)
    })
    #events entity -> Subject
    #custom vocab
    observeEvent(input$custom_vocab_keyword_button_add,{
      INFO("Add a keyword for custom vocab")
      if(is.null(md_model_subject_draft())){
        md_model_subject_draft(geoflow_subject$new())
      }
      subj = md_model_subject_draft()
      subj$setKey(input$entity_subject_type)
      if(input$custom_vocab_thesaurus_name != "") subj$setName(input$custom_vocab_thesaurus_name)
      if(input$custom_vocab_thesaurus_uri != "") subj$setUri(input$custom_vocab_thesaurus_uri)
      subj$addKeyword(
        keyword = input$custom_vocab_keyword_name,
        uri = if(!is.null(input$custom_vocab_keyword_uri) & input$custom_vocab_keyword_uri != "") input$custom_vocab_keyword_uri else NULL
      )
      md_model_subject_draft(subj$clone(deep = T))
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
      entity = md_model_draft()
      entity$addDate(
        dateType = input$entity_date_type,
        date = input$entity_date
      )
      check_model(type = md_model_type(), model = entity)
    })
    observeEvent(input$entity_date_button_remove,{
      handle_field_element_remove_event(field = "dates", input_btn_remove = input$entity_date_button_remove)  
    })
    #events entity -> Type
    observeEvent(input$entity_type_button_add,{
      entity = md_model_draft()
      entity$setType(
        key = input$entity_resource_type,
        type = input$entity_resource
      )
      check_model(type = md_model_type(), model = entity)
    })
    observeEvent(input$entity_type_button_remove,{
      handle_field_element_remove_event(field = "types", input_btn_remove = input$entity_type_button_remove)  
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
    observeEvent(input$entity_temporalcoverage,{
      time = input$entity_temporalcoverage
      if(any(!is.na(time))){
        entity = md_model_draft()
        if(any(is.na(time))){
          time = time[!is.na(time)]
          entity$setTemporalExtent(str = time)
        }else{
          entity$setTemporalExtent(str = paste0(time, collapse="/"))
        }
        md_model_draft(entity$clone(deep = T))
      }
    })
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
    #events entity -> Rights
    observeEvent(input$entity_right_button_add,{
      entity = md_model_draft()
      right = geoflow_right$new()
      right$setKey(input$entity_right_type)
      right$setValues(input$entity_right)
      entity$addRight(right)
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_format_button_clear,{
      entity = md_model_draft()
      entity$rights = list()
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Format
    observeEvent(input$entity_format_button_add,{
      entity = md_model_draft()
      form = geoflow_format$new()
      form$setKey(input$entity_format_type)
      form$setName(input$entity_format_name)
      if(input$entity_format_description != "") form$setDescription(input$entity_format_description)
      if(input$entity_format_link != "") form$setUri(input$entity_format_link)
      entity$addFormat(form)
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_format_button_clear,{
      entity = md_model_draft()
      entity$formats = list()
      md_model_draft(entity$clone(deep = T))
    })
    
    #events entity -> Provenance
    observeEvent(input$entity_prov_process_button_add,{
      entity = md_model_draft()
      prov = geoflow_provenance$new()
      if(!is.null(entity$provenance)){
        prov = entity$provenance
      }
      prov$setStatement(input$entity_prov_statement)
      process = geoflow_process$new()
      process$setRationale(input$entity_prov_process_rationale)
      process$setDescription(input$entity_prov_process_description)
      prov$addProcess(process)
      entity$setProvenance(prov)
      md_model_draft(entity$clone(deep = T))
    })
    observeEvent(input$entity_prov_process_button_clear,{
      entity = md_model_draft()
      entity$provenance = NULL
      md_model_draft(entity$clone(deep = T))
    })
    #events entity -> Data
    observeEvent(input$entity_data_source_button_add,{
      entity = md_model_draft()
      edata = geoflow_data$new() 
      if(!is.null(entity$data)){
        edata = entity$data
      }
      source = input$entity_data_source_name
      if(!is.null(input$entity_data_source_uri)) if(nzchar(input$entity_data_source_uri)){
        attr(source, "uri") <- input$entity_data_source_uri
      }
      if(!is.null(source)) if(nzchar(source)){
        edata$addSource(source)
        entity$setData(edata)
        print(entity$data)
        md_model_draft(entity$clone(deep = T))
      }
    })
    observeEvent(input$entity_data_source_button_remove,{
      handle_field_element_remove_event(field = "data", object_field = "source", input_btn_remove = input$entity_data_source_button_remove)
    })
    observeEvent(input$entity_data_parameter_button_add,{
      entity = md_model_draft()
      edata = geoflow_data$new() 
      if(!is.null(entity$data)){
        edata = entity$data
      }
      fieldname = input$entity_data_parameter_fieldname
      alias = input$entity_data_parameter_alias
      if(!nzchar(alias)) alias = fieldname
      regexp = input$entity_data_parameter_regexp
      defaultValue = input$entity_data_parameter_defaultvalue
      if(nzchar(fieldname) & nzchar(regexp) & nzchar(defaultValue)){
        edata$setParameter(alias, fieldname, regexp, defaultValue)
        entity$setData(edata)
        print(entity$data)
        md_model_draft(entity$clone(deep = T))
      }
    })
    observeEvent(input$entity_data_parameter_button_remove,{
      handle_field_element_remove_event(field = "data", object_field = "parameters", input_btn_remove = input$entity_data_parameter_button_remove)
    })
    #contact specific form events
    #----------------------------
    observeEvent(input$contact_identifier_button_add,{
      contact = md_model_draft()
      contact$setIdentifier(
        key = input$contact_identifier_type,
        id = input$contact_identifier
      )
      check_model(type = md_model_type(), model = contact)
    })
    observeEvent(input$contact_identifier_button_remove,{
      row <- as.numeric(input$contact_identifier_button_remove)
      contact <- md_model_draft()
      if (!is.null(contact$identifiers) && length(contact$identifiers) > 0 && !is.na(row) && row >= 1 && row <= length(contact$identifiers)) {
        contact$identifiers <- contact$identifiers[-row]
      } else {
        contact$identifiers <- list()
      }
      if(length(contact$identifiers)==0) contact$identifiers = list()
      check_model(type = md_model_type(), model = contact)
    })
  })
  
}