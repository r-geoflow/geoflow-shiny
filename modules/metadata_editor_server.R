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
    cloud_overwriting_danger <- reactiveVal(FALSE)
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
          id = ns("contact_form"),
          type = "pills", vertical = T,
          tabPanel(
            value = "contact_identifiers",
            title = i18n()$t("MD_EDITOR_C_IDENTIFIERS"),
            fluidRow(
              column(3, selectInput(ns("contact_identifier_type"),
                                       label= i18n()$t("MD_EDITOR_KEY"),
                                       multiple = F,
                                       choices = contact_tpl$getAllowedKeyValuesFor("Identifier"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(6,textInput(ns("contact_identifier"), i18n()$t("MD_EDITOR_C_IDENTIFIER"),value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_IDENTIFIER"))),
              column(3,
                     actionButton(ns("contact_identifier_button_add"), title=i18n()$t("MD_EDITOR_C_IDENTIFIER_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("contact_identifiers_table"))
          ),
          tabPanel(
            value = "contact_details",
            title = i18n()$t("MD_EDITOR_C_DETAILS"),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_ORGNAME"))), column(6, textInput(ns("contact_org"), label = NULL, value = if(!is.null(model)) model$organizationName else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_ORGNAME")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_FIRSTNAME"))), column(6, textInput(ns("contact_firstname"), label = NULL, value = if(!is.null(model)) model$firstName else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_FIRSTNAME")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_LASTNAME"))), column(6, textInput(ns("contact_lastname"), label = NULL, value = if(!is.null(model)) model$lastName else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_LASTNAME")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_POSITIONNAME"))), column(6, textInput(ns("contact_positionname"), label = NULL, value = if(!is.null(model)) model$positionName else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_POSITIONNAME")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_POSTALADDRESS"))), column(6, textInput(ns("contact_postaladdress"), label = NULL, value = if(!is.null(model)) model$postalAddress else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_POSTALADDRESS")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_POSTALCODE"))), column(6, textInput(ns("contact_postalcode"), label = NULL, value = if(!is.null(model)) model$postalCode else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_POSTALCODE")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_CITY"))), column(6, textInput(ns("contact_city"), label = NULL, value = if(!is.null(model)) model$city else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_CITY")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_COUNTRY"))), column(6, textInput(ns("contact_country"), label = NULL, value = if(!is.null(model)) model$country else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_COUNTRY"))))
          ),
          tabPanel(
            value = "contact_info",
            title = i18n()$t("MD_EDITOR_C_INFO"),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_EMAIL"))), column(6, textInput(ns("contact_email"), label = NULL, value = if(!is.null(model)) model$email else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_EMAIL")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_PHONENUMBER"))), column(6, textInput(ns("contact_voice"), label = NULL, value = if(!is.null(model)) model$voice else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_PHONENUMBER")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_FACSIMILE"))), column(6, textInput(ns("contact_facsimile"), label = NULL, value = if(!is.null(model)) model$facsimile else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_FACSIMILE")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_WEBSITE_URL"))), column(6, textInput(ns("contact_websiteurl"), label = NULL, value = if(!is.null(model)) model$websiteUrl else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_WEBSITE_URL")))),
            fluidRow(column(6, tags$b(i18n()$t("MD_EDITOR_C_WEBSITE_NAME"))), column(6, textInput(ns("contact_websitename"), label = NULL, value = if(!is.null(model)) model$websiteName else "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_WEBSITE_NAME"))))
          )
        ),
        "entity" = bs4Dash::tabsetPanel(
          id = ns("entity_form"),
          type = "pills", vertical = T,
          tabPanel(
            value = "entity_identifiers",
            title = i18n()$t("MD_EDITOR_E_IDENTIFIERS"),
            fluidRow(
              column(3, selectInput(ns("entity_identifier_type"),
                                       label = i18n()$t("MD_EDITOR_KEY"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Identifier"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(6,textInput(ns("entity_identifier"), i18n()$t("MD_EDITOR_E_IDENTIFIER"), value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_E_IDENTIFIER"))),
              column(3,
                     actionButton(ns("entity_identifier_button_add"), title = i18n()$t("MD_EDITOR_E_IDENTIFIER_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_identifiers_table"))
          ),
          tabPanel(
            value = "entity_titles",
            title = i18n()$t("MD_EDITOR_E_TITLE"),
            fluidRow(
              column(3, selectInput(ns("entity_title_type"),
                                       label = i18n()$t("MD_EDITOR_KEY"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Title"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,textInput(ns("entity_title"), i18n()$t("MD_EDITOR_E_TITLE"),value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_E_TITLE"))),
              column(2,
                     actionButton(ns("entity_title_button_add"), title=i18n()$t("MD_EDITOR_E_TITLE_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_titles_table"))
          ),
          tabPanel(
            value = "entity_descriptions",
            title = i18n()$t("MD_EDITOR_E_DESCRIPTION"),
            fluidRow(
              column(3, selectInput(ns("entity_description_type"),
                                       label = i18n()$t("MD_EDITOR_KEY"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Description"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,textAreaInput(ns("entity_description"), i18n()$t("MD_EDITOR_E_DESCRIPTION"),value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_E_DESCRIPTION"))),
              column(2,
                     actionButton(ns("entity_description_button_add"), title=i18n()$t("MD_EDITOR_E_DESCRIPTION_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_descriptions_table"))
          ),
          tabPanel(
            value = "entity_subjects",
            title = i18n()$t("MD_EDITOR_E_SUBJECTS"),
            fluidRow(
              column(3, selectInput(ns("entity_subject_type"),
                                       label = i18n()$t("MD_EDITOR_KEY"),
                                       multiple = F,
                                       choices = c(geometa::ISOKeywordType$values(), "taxonomy"),
                                       selected = "theme",
                                       selectize = FALSE
              )),
              column(6, selectInput(ns("entity_vocabulary_server"),
                                       label = i18n()$t("MD_EDITOR_E_VOCABULARY"),
                                       multiple = F,
                                       choices = {
                                         vocabs = list_vocabularies()
                                         setNames(c(vocabs$id, "custom"), nm = c(vocabs$def, i18n()$t("MD_EDITOR_E_VOCABULARY_CUSTOM")))
                                        },
                                       selected = "custom",
                                       selectize = FALSE
              ))
            ),
            fluidRow(
              style = "height:500px;overflow-y:auto;",
              column(12,
                uiOutput(ns("entity_vocabulary_custom")),
                uiOutput(ns("entity_vocabulary_tree_wrapper"))
              )
            ),
            fluidRow(
              column(3,
                     actionButton(ns("entity_subject_button_add"), title = i18n()$t("MD_EDITOR_E_SUBJECT_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
              )
            ),
            DTOutput(ns("entity_subjects_table"))
          ),
          tabPanel(
            value = "entity_contacts",
            title = i18n()$t("MD_EDITOR_E_CONTACTS"),
            fluidRow(
              column(3,
                     actionButton(ns("entity_contact_load"), title = i18n()$t("MD_EDITOR_E_CONTACTS_LOAD"), size = "sm", label="", icon=icon("users"))
              )
            ),
            fluidRow(
              column(3, selectInput(ns("entity_contact_type"),
                                       label = i18n()$t("MD_EDITOR_E_CONTACT"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Creator"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,uiOutput(ns("entity_contact_wrapper"))),
              column(2,
                     actionButton(ns("entity_contact_button_add"), title = i18n()$t("MD_EDITOR_E_CONTACT_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"))
            ),
            DTOutput(ns("entity_contacts_table"))
          ),
          tabPanel(
            value = "entity_dates",
            title = i18n()$t("MD_EDITOR_E_DATES"),
            fluidRow(
              column(3, selectInput(ns("entity_date_type"),
                                       label = i18n()$t("MD_EDITOR_E_DATETYPE"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Date"),
                                       selected = "creation",
                                       selectize = FALSE
              )),
              column(7,dateInput(ns("entity_date"), i18n()$t("MD_EDITOR_E_DATE"),value = Sys.Date(), width = NULL)),
              column(2,
                     actionButton(ns("entity_date_button_add"), title = i18n()$t("MD_EDITOR_E_DATE_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_dates_table"))
          ),
          tabPanel(
            value = "entity_types",
            title = i18n()$t("MD_EDITOR_E_TYPE"),
            fluidRow(
              column(3, selectInput(ns("entity_resource_type"),
                                       label = i18n()$t("MD_EDITOR_KEY"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Type"),
                                       selected = "generic",
                                       selectize = FALSE
              )),
              column(7,textInput(ns("entity_resource"), "Type",value = "dataset", width = NULL, placeholder = "Type")),
              column(2,
                     actionButton(ns("entity_type_button_add"), title=i18n()$t("MD_EDITOR_E_TYPE_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_types_table"))
          ),
          tabPanel(
            value = "entity_languages",
            title = i18n()$t("MD_EDITOR_E_LANGUAGE"),
            fluidRow(
              column(6, selectInput(ns("entity_language"),
                                       label = i18n()$t("MD_EDITOR_E_LANGUAGE"),
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
            title = i18n()$t("MD_EDITOR_E_SPATIALCOVERAGE"),
            fluidRow(
              column(6, selectInput(ns("entity_srid"),
                                       label = "SRID",
                                       multiple = F,
                                       choices = {
                                         setNames(c(4326), nm = c("WGS 84 (EPSG:4326)"))
                                       },
                                       selected = 4326,
                                       selectize = FALSE
              ))
            ),
            fluidRow(
              style = "height:500px;overflow-y:auto;",
              column(12,
                leafletOutput(ns("entity_map"), height = "400px"),
                uiOutput(ns("entity_wkt_wrapper"))
              )
            )
          ),
          tabPanel(
            value = "entity_temporalcoverages",
            title = i18n()$t("MD_EDITOR_E_TEMPORALCOVERAGE"),
            fluidRow(
              fluidRow(
                column(12, dateRangeInput(ns("entity_temporalcoverage"), label = i18n()$t("MD_EDITOR_E_TEMPORALCOVERAGE")))
              )
            )
          ),
          tabPanel(
            value = "entity_relations",
            title = i18n()$t("MD_EDITOR_E_RELATIONS"),
            fluidRow(
              column(3, selectInput(ns("entity_relation_type"),
                                       label = i18n()$t("MD_EDITOR_KEY"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Relation"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,textInput(ns("entity_relation_name"), i18n()$t("MD_EDITOR_NAME"),value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_NAME")))
            ),
            fluidRow(
              column(3),
              column(7,textAreaInput(ns("entity_relation_description"), i18n()$t("MD_EDITOR_DESCRIPTION"),value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_DESCRIPTION")))
            ),
            fluidRow(
              column(3),
              column(7, textInput(ns("entity_relation_link"), i18n()$t("MD_EDITOR_URI"), value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_URI"))),
              column(2,
                     actionButton(ns("entity_relation_button_add"), title=i18n()$t("MD_EDITOR_E_RELATION_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_relations_table"))
          ),
          tabPanel(
            value = "entity_rights",
            title = i18n()$t("MD_EDITOR_E_RIGHTS"),
            fluidRow(
              column(3, selectInput(ns("entity_right_type"),
                                       label = i18n()$t("MD_EDITOR_E_RIGHTTYPE"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Rights"),
                                       selected = "license",
                                       selectize = FALSE
              )),
              column(7, uiOutput(ns("entity_right_wrapper"))),
              column(2,
                     actionButton(ns("entity_right_button_add"), title = i18n()$t("MD_EDITOR_E_RIGHT_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_rights_table"))
          ),
          tabPanel(
            value = "entity_formats",
            title = i18n()$t("MD_EDITOR_E_FORMATS"),
            fluidRow(
              column(3, selectInput(ns("entity_format_type"),
                                       label = i18n()$t("MD_EDITOR_E_FORMATTYPE"),
                                       multiple = F,
                                       choices = entity_tpl$getAllowedKeyValuesFor("Format"),
                                       selected = "id",
                                       selectize = FALSE
              )),
              column(7,selectInput(ns("entity_format_name"),
                                      label = i18n()$t("MD_EDITOR_E_FORMAT"),
                                      multiple = F,
                                      choices = as.character(mime::mimemap),
                                      selected = NULL,
                                      selectize = FALSE
              ))
            ),
            fluidRow(
              column(3),
              column(7,textAreaInput(ns("entity_format_description"), i18n()$t("MD_EDITOR_DESCRIPTION"),value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_DESCRIPTION")))
            ),
            fluidRow(
              column(3),
              column(7, textInput(ns("entity_format_link"), i18n()$t("MD_EDITOR_URI"), value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_URI"))),
              column(2,
                     actionButton(ns("entity_format_button_add"), title = i18n()$t("MD_EDITOR_E_FORMAT_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("entity_formats_table"))
          ),
          tabPanel(
            value = "entity_provenances",
            title = i18n()$t("MD_EDITOR_E_PROV"),
            fluidRow(
              column(12, textInput(ns("entity_prov_statement"), i18n()$t("MD_EDITOR_E_PROV_STATEMENT"), value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_E_PROV_STATEMENT"))),
            ),
            hr(),
            fluidRow(
              column(5, textInput(ns("entity_prov_process_rationale"), i18n()$t("MD_EDITOR_E_PROV_PROCESS_RATIONALE"), value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_E_PROV_PROCESS_RATIONALE"))),
              column(5, textAreaInput(ns("entity_prov_process_description"), i18n()$t("MD_EDITOR_E_PROV_PROCESS_DESCRIPTION"), value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_E_PROV_PROCESS_DESCRIPTION"))),
              column(2,
                     actionButton(ns("entity_prov_process_button_add"), title = i18n()$t("MD_EDITOR_E_PROV_PROCESS_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
              )
            ),
            DTOutput(ns("entity_processes_table"))
          ),
          tabPanel(
            value = "entity_datasets",
            title = i18n()$t("MD_EDITOR_E_DATA"),
            fluidRow(
              bs4Dash::accordion(
                id = "entity_data_blocks",
                bs4Dash::accordionItem(
                  title = tags$span(icon("download"), " ", i18n()$t("MD_EDITOR_E_DATA_ACCESS_INFORMATION")),
                  fluidRow(
                    column(4, selectInput(ns("entity_data_access"),
                                          label= i18n()$t("MD_EDITOR_E_DATA_ACCESS"),
                                          multiple = F,
                                          choices = {
                                            geoflow::list_data_accessors()$id
                                          },
                                          selected = if(!is.null(model)) model$data$access else NULL,
                                          selectize = FALSE
                    )),
                    column(4, selectInput(ns("entity_data_type"),
                                          label = i18n()$t("MD_EDITOR_E_DATA_ACCESS_MODE"),
                                          multiple = F,
                                          choices = {
                                            setNames(
                                              c("source", "dir"),
                                              nm = c(i18n()$t("MD_EDITOR_E_DATA_ACCESS_MODE_FILE"), i18n()$t("MD_EDITOR_E_DATA_ACCESS_MODE_DIR"))
                                            )
                                          },
                                          selected = "source",
                                          selectize = FALSE
                    )),
                    column(4, selectInput(ns("entity_data_sourcetype"),
                                          label = i18n()$t("MD_EDITOR_E_DATA_SOURCETYPE"),
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
                      label = i18n()$t("MD_EDITOR_E_DATA_SOURCESQL"),
                      value = if(!is.null(model)) model$data$sourceSql else NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_E_DATA_SOURCESQL")
                    ))
                  )
                ),
                bs4Dash::accordionItem(
                  title = tags$span(icon("table-list"), " ", i18n()$t("MD_EDITOR_E_DATA_CHARACTERISTICS")),
                  icon = icon("table-list"),
                  fluidRow(
                    column(6, selectInput(ns("entity_data_spatialrepresentationtype"),
                                          label = i18n()$t("MD_EDITOR_E_DATA_SRT"),
                                          multiple = F,
                                          choices = {
                                            c("vector","grid")
                                          },
                                          selected = if(!is.null(model)) model$data$spatialRepresentationType else "vector",
                                          selectize = FALSE
                    )),
                    column(4, textInput(ns("entity_data_featuretype"),
                                          label = i18n()$t("MD_EDITOR_E_DATA_FT"),
                                          width = NULL,
                                          value = if(!is.null(model)) model$data$featureType else NULL,
                                          placeholder = "Feature type"
                    )),
                  )
                ),
                bs4Dash::accordionItem(
                  title = tags$span(icon("upload"), " ", i18n()$t("MD_EDITOR_E_DATA_UPLOAD_CONFIGURATION")),
                  fluidRow(
                    column(4, selectInput(ns("entity_data_upload"),
                                          label = i18n()$t("MD_EDITOR_E_DATA_UPLOAD"),
                                          multiple = F,
                                          choices = {
                                            c(TRUE,FALSE)
                                          },
                                          selected = if(!is.null(model)) model$data$upload else TRUE,
                                          selectize = FALSE
                    )),
                    column(4, selectInput(ns("entity_data_uploadtype"),
                                          label = i18n()$t("MD_EDITOR_E_DATA_UPLOAD_TYPE"),
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
                      label = i18n()$t("MD_EDITOR_E_DATA_UPLOAD_SOURCE"),
                      value = if(!is.null(model) & !is.null(model$data$uploadSource)) model$data$uploadSource[[1]] else NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_E_DATA_UPLOAD_SOURCE")
                    ))
                  )
                ),
                bs4Dash::accordionItem(
                  title = tags$span(icon("upload"), icon("globe"), " ", paste0(i18n()$t("MD_EDITOR_E_DATA_UPLOAD_CONFIGURATION"), " - GeoServer")),
                  h6(i18n()$t("MD_EDITOR_E_DATA_LAYER_IDENTIFICATION")),
                  fluidRow(
                    column(6,textInput(ns("entity_data_layername"),
                                       label = i18n()$t("MD_EDITOR_NAME"),
                                       value = if(!is.null(model)) model$data$layername else NULL,
                                       width = NULL,
                                       placeholder = i18n()$t("MD_EDITOR_NAME")
                    )),
                    column(6,textInput(ns("entity_data_layeruri"),
                                       label = i18n()$t("MD_EDITOR_URI"),
                                       value = if(!is.null(model)) model$data$layeruri else NULL,
                                       width = NULL,
                                       placeholder = i18n()$t("MD_EDITOR_URI")
                    ))
                  ),
                  fluidRow(
                    column(12,textInput(ns("entity_data_layertitle"),
                                        label = i18n()$t("MD_EDITOR_TITLE"),
                                        value = if(!is.null(model)) model$data$layertitle else NULL,
                                        width = NULL,
                                        placeholder = i18n()$t("MD_EDITOR_TITLE")
                    ))
                  ),
                  fluidRow(
                    column(12,textAreaInput(
                      ns("entity_data_layerdesc"),
                      label = i18n()$t("MD_EDITOR_DESCRIPTION"),
                      value = if(!is.null(model)) model$data$layerdesc else NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_DESCRIPTION")
                    ))
                  ),
                  h6("SQL View layer settings"),
                  fluidRow(
                    column(12, textAreaInput(
                      ns("entity_data_sql"),
                      label = i18n()$t("MD_EDITOR_E_DATA_VIEWSQL"),
                      value = if(!is.null(model)) model$data$sql else NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_E_DATA_VIEWSQL")
                    ))
                  ),
                  fluidRow(
                    column(6, textInput(
                      ns("entity_data_geometry_field"),
                      label = i18n()$t("MD_EDITOR_E_DATA_GEOMETRY_FIELD"),
                      value = if(!is.null(model)) model$data$geometryField else NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_E_DATA_GEOMETRY_FIELD")
                    )),
                    column(6, selectInput(ns("entity_data_geometry_type"),
                      label = i18n()$t("MD_EDITOR_E_DATA_GEOMETRY_TYPE"),
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
                      label = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_FIELDNAME"),
                      value = NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_FIELDNAME")
                    )),
                    column(3, textInput(ns("entity_data_parameter_alias"),
                      label = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_ALIAS"),
                      value = NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_ALIAS")
                    )),
                    column(3, textInput(ns("entity_data_parameter_regexp"),
                      label = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_REGEXP"),
                      value = NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_REGEXP")
                    )),
                    column(2, textInput(ns("entity_data_parameter_defaultvalue"),
                      label = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_DEFAULTVALUE"),
                      value = NULL,
                      width = NULL,
                      placeholder = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_DEFAULTVALUE")
                    )),
                    column(1,
                           actionButton(ns("entity_data_parameter_button_add"), title = i18n()$t("MD_EDITOR_E_DATA_PARAMETER_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
                    )
                  ),
                  uiOutput(ns("entity_data_parameters_table_wrapper"))
                ),
                bs4Dash::accordionItem(
                  title = tags$span(icon("upload"), icon("cloud"), " ", paste0(i18n()$t("MD_EDITOR_E_DATA_UPLOAD_CONFIGURATION")," - Cloud")),
                  fluidRow(
                  )
                )
              )
            )
          )
        ),
        "featuretype" =  bs4Dash::tabsetPanel(
          id = ns("dictionary_form"),
          type = "pills", vertical = T,
          tabPanel(
            value = "dictionary_featuretype",
            title = i18n()$t("MD_EDITOR_IDENTIFIER"),
            fluidRow(
              column(12,
                textInput(
                  inputId = ns("featuretype_identifier"),
                  label = "Identifier",
                  value = if(!is.null(model)) model$id else NULL,
                  width = NULL,
                  placeholder = i18n()$t("MD_EDITOR_IDENTIFIER")
                )
              )
            )
          ),
          tabPanel(
            value = "dictionary_members",
            title = i18n()$t("MD_EDITOR_D_MEMBERS"),
            fluidRow(
              column(6,
                textInput(
                  inputId = ns("featuremember_code"),
                  label = i18n()$t("MD_EDITOR_CODE"),
                  value = NULL,
                  width = NULL,
                  placeholder = i18n()$t("MD_EDITOR_CODE")
                )       
              ),
              column(6,
               textInput(
                 inputId = ns("featuremember_name"),
                 label = i18n()$t("MD_EDITOR_NAME"),
                 value = NULL,
                 width = NULL,
                 placeholder = i18n()$t("MD_EDITOR_NAME")
               )                      
              )
            ),
            fluidRow(
              column(4,
                     selectInput(
                       inputId = ns("featuremember_type"),
                       label= i18n()$t("MD_EDITOR_TYPE"),
                       multiple = F,
                       choices = {
                         fm_types = c("attribute", "variable",
                                      "gml:PointPropertyType", "gml:MultiPointPropertyType",
                                      "gml:LineStringPropertyType", "gml:MultiLineStringPropertyType",
                                      "gml:PolygonPropertyType", "gml:MultiPolygonPropertyType")
                         setNames(
                           fm_types, 
                           nm = c(i18n()$t("MD_EDITOR_D_ATTRIBUTE"), i18n()$t("MD_EDITOR_D_VARIABLE"),
                                  i18n()$t("MD_EDITOR_D_GEOM_POINT"), i18n()$t("MD_EDITOR_D_GEOM_MULTIPOINT"),
                                  i18n()$t("MD_EDITOR_D_GEOM_LINESTRING"), i18n()$t("MD_EDITOR_D_GEOM_MULTILINESTRING"),
                                  i18n()$t("MD_EDITOR_D_GEOM_POLYGON"), i18n()$t("MD_EDITOR_D_GEOM_MULTIPOLYGON"))
                         )
                       },
                       selected = "attribute",
                       selectize = FALSE
                     )
              ),
              column(2,
                selectInput(
                  inputId = ns("featuremember_minoccurs"),
                  label = i18n()$t("MD_EDITOR_D_MINOCCURS"),
                  multiple = F,
                  choices = {
                    setNames(c(0,1), nm = c("0","1"))
                  },
                  selected = 0,
                  selectize = FALSE
                )       
              ),
              column(2,
                     selectInput(
                       inputId = ns("featuremember_maxoccurs"),
                       label = i18n()$t("MD_EDITOR_D_MAXOCCURS"),
                       multiple = F,
                       choices = {
                         setNames(c(1,Inf), nm = c("1","Inf"))
                       },
                       selected = 1,
                       selectize = FALSE
                     )       
              ),
              column(4,
                     textInput(inputId = ns("featuremember_measurementunit"), label = "Unit", value = NULL, width = NULL, placeholder = i18n()$t("MD_EDITOR_D_MEASUREMENTUNIT"))     
              )
            ),
            fluidRow(
              column(6,
                     textInput(
                       inputId = ns("featuremember_def"),
                       label = i18n()$t("MD_EDITOR_D_DEFINITION"),
                       value = NULL,
                       width = NULL,
                       placeholder = i18n()$t("MD_EDITOR_D_DEFINITION")
                     )       
              ),  
              column(6,
                textInput(
                  inputId = ns("featuremember_definitionsource"), 
                  label = i18n()$t("MD_EDITOR_D_DEFINITION_SOURCE"), 
                  value = NULL, 
                  width = NULL, 
                  placeholder = i18n()$t("MD_EDITOR_D_DEFINITION_SOURCE")
                )           
              )
            ),
            fluidRow(
              column(5,
                textInput(
                  inputId = ns("featuremember_registerid"), 
                  label = i18n()$t("MD_EDITOR_D_REGISTER_FUNCTION"), 
                  value = NULL, 
                  width = NULL, 
                  placeholder = i18n()$t("MD_EDITOR_D_REGISTER_FUNCTION")
                )
              ),
              column(5,
                textInput(
                  inputId = ns("featuremember_registerscript"), 
                  label = i18n()$t("MD_EDITOR_D_REGISTER_SCRIPT"), 
                  value = NULL, 
                  width = NULL, 
                  placeholder = i18n()$t("MD_EDITOR_D_REGISTER_SCRIPT")
                )
              ),
              column(2,
                actionButton(ns("featuretype_member_button_add"), title = i18n()$t("MD_EDITOR_D_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
              )
            ),
            DTOutput(ns("featuretype_members_table"))
          )
        )
      )
      
    }
    
    #check_model
    check_model = function(type, model, validate = TRUE){
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
               if(nzchar(input$entity_data_spatialrepresentationtype)) entity$data$setSpatialRepresentationType(input$entity_data_spatialrepresentationtype)
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
             },
             "featuretype" = {
               featuretype = model
               if(nzchar(input$featuretype_identifier)) featuretype$id = input$featuretype_identifier
               #members manager through observer
               md_model_draft(featuretype$clone(deep = T))
             }
      )
      
      #perform validation
      meta_validator = switch(type,
                              "contact" = geoflow_validator_contacts$new(source = md_model_draft()$asDataFrame()),
                              "entity" = geoflow_validator_entities$new(source = md_model_draft()$asDataFrame())
      )
      if(!is.null(meta_validator) & validate){
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
      }else{
        md_model_draft_valid(TRUE)
        md_model_draft_validation_report(
          data.frame(
            row = character(0),
            col = character(0),
            type = character(0),
            message = character(0),
            stringsAsFactors = F
          )
        )
      }
    }
    
    #render_field_elements_table
    render_field_elements_table = function(field, object_field = NULL, field_model = c("kvp","attributes"), list_elements = list(),
                                           field_key = NULL, field_value = NULL, field_value_object_field = NULL, field_value_list = FALSE, field_value_list_strategy = c("first","collapse"),
                                           field_other_props = c(),
                                           btn_remove_id){
      objs <- md_model_draft()[[field]]
      if(!is.null(object_field)) objs <- objs[[object_field]]
      if (is.null(objs) || length(objs) == 0) {
        tbl <- tibble::tibble(key = character(0), value = character(0), delete = character(0))
        names(tbl) <- c(i18n()$t("MD_EDITOR_KEY"),i18n()$t("MD_EDITOR_VALUE"),i18n()$t("MD_EDITOR_ACTIONS"))
      } else {
        tbl <- switch(field_model,
          "kvp" = {
            is_named_list = !is.null(names(objs))
            if(is_named_list){
              if(length(list_elements)==0){
                out_tbl = do.call(rbind, lapply(names(objs), function(objname) {
                  tibble::tibble(key = objname, value = objs[[objname]])
                }))
                names(out_tbl) <- c(i18n()$t("MD_EDITOR_KEY"),i18n()$t("MD_EDITOR_VALUE"))
                out_tbl
              }else{
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
              out_tbl = do.call("rbind", lapply(objs, function(obj){
                r_tbl = tibble::tibble(key = obj[[field_key]])
                names(r_tbl)[length(names(r_tbl))] <- i18n()$t("MD_EDITOR_KEY")
                if(length(field_other_props)>0) for(field_other_prop in field_other_props){
                  r_tbl[[field_other_prop]]  = if(!is.null(obj[[field_other_prop]])) obj[[field_other_prop]] else "-"
                  names(r_tbl)[length(names(r_tbl))] <- i18n()$t(paste0("MD_EDITOR_", toupper(field_other_prop)))
                }
                r_tbl$value = if(field_value_list) {
                  switch(field_value_list_strategy,
                    "first" = if(!is.null(field_value_object_field)) obj[[field_value]][[1]][[field_value_object_field]] else obj[[field_value]][[1]],
                    "collapse" = if(!is.null(field_value_object_field)){
                      paste(sapply(obj[[field_value]], function(x){x[[field_value_object_field]]}), collapse=",")
                    }else{
                      paste(obj[[field_value]], collapse=",")
                    }
                  )
                }else{
                  obj[[field_value]]
                }
                names(r_tbl)[length(names(r_tbl))] <- i18n()$t("MD_EDITOR_VALUE")
                r_tbl
              }))
              out_tbl
            }
          },
          "attributes" = {
            out_tbl = do.call("rbind", lapply(objs, function(obj){
              data.frame(
                name = obj,
                description = if(!is.null(attr(obj, "description"))) attr(obj, "description") else "-",
                uri = if(!is.null(attr(obj, "uri"))) attr(obj, "uri") else "-"
              )
            }))
            names(out_tbl) <- c(i18n()$t("MD_EDITOR_NAME"),i18n()$t("MD_EDITOR_DESCRIPTION"),i18n()$t("MD_EDITOR_URI"))
            out_tbl
          }
        )
        tbl$delete <- sprintf(
          '<button class="delete_btn btn btn-link" style="color:red;" data-row="%s" title="Delete">
        <span class="glyphicon glyphicon-trash" aria-hidden="true"></span>
      </button>', seq_len(length(objs))
        )
      }
      names(tbl)[length(names(tbl))] <- i18n()$t("MD_EDITOR_ACTIONS")
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
      check_model(type = md_model_type(), model = model, validate = FALSE)
    }
    
    #loadCloudTree
    loadCloudTree = function(id, leavesOnly = FALSE){
      output[[id]] <- jsTreeR::renderJstree({
        jsTreeR::jstree(
          nodes = list(
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
          selectLeavesOnly = leavesOnly,
          checkboxes = FALSE,
          multiple = FALSE,
          search = TRUE
        )
      })
    }
    
    #UIs
    
    #metadata editor info
    output$metadata_editor_info <- renderText({
      session$userData$module("metadata-editor")
      updateModuleUrl(session, "metadata-editor")
      text <- i18n()$t("MD_EDITOR_HEADER")
      pageLoaded(TRUE)
      text
    })
    
    #meta edition choices (entity, contact, dictionary)
    output$meta_editor_choices <- renderUI({
      print(md_model_type())
      fluidRow(
        bs4Dash::bs4ValueBox(
          width = 4,
          value = h5(i18n()$t("MD_EDITOR_C"), style = "font-weight:bold;"), 
          subtitle = i18n()$t("MD_EDITOR_C_SUBTITLE"), 
          color = ifelse(is.null(md_model_type()), "white", ifelse(md_model_type()=="contact", "primary", "white")),
          icon = icon("users"),
          footer = shiny::tagList(
            bs4Dash::actionButton(inputId = ns("create_contact_table"), label = i18n()$t("MD_EDITOR_TABLE_CREATE")),
            bs4Dash::actionButton(inputId = ns("load_contact_table"), label = i18n()$t("MD_EDITOR_TABLE_LOAD")),
            bs4Dash::actionButton(inputId = ns("create_contact"), label = i18n()$t("MD_EDITOR_C_CREATE"))
          )
        ),
        bs4Dash::bs4ValueBox(
          width = 4,
          value = h5(i18n()$t("MD_EDITOR_E"), style = "font-weight:bold;"), 
          subtitle = i18n()$t("MD_EDITOR_E_SUBTITLE"), 
          color = ifelse(is.null(md_model_type()), "white", ifelse(md_model_type()=="entity", "primary", "white")),
          icon = icon("table"),
          footer = shiny::tagList(
            bs4Dash::actionButton(inputId = ns("create_entity_table"), label = i18n()$t("MD_EDITOR_TABLE_CREATE")),
            bs4Dash::actionButton(inputId = ns("load_entity_table"), label = i18n()$t("MD_EDITOR_TABLE_LOAD")),
            bs4Dash::actionButton(inputId = ns("create_entity"), label = i18n()$t("MD_EDITOR_E_CREATE"))
          )
        ),
        bs4Dash::bs4ValueBox(
          width = 4,
          value = h5(i18n()$t("MD_EDITOR_D"), style = "font-weight:bold;"), 
          subtitle = i18n()$t("MD_EDITOR_D_SUBTITLE"),
          color = ifelse(is.null(md_model_type()), "white", ifelse(md_model_type()=="featuretype", "primary", "white")),
          icon = icon("table-list"),
          footer = shiny::tagList(
            bs4Dash::actionButton(inputId = ns("create_dictionary_table"), label = i18n()$t("MD_EDITOR_TABLE_CREATE")),
            bs4Dash::actionButton(inputId = ns("load_dictionary_table"), label = i18n()$t("MD_EDITOR_TABLE_LOAD")),
            bs4Dash::actionButton(inputId = ns("create_dictionary"), label = i18n()$t("MD_EDITOR_D_CREATE"))
          )
        )
      )
      
    })
    
    output$meta_editor_hrcustom <- renderUI({
      handle_style = function(type){
        blank_style = ""
        default_style = "margin:-25px 0px 0px 0px;padding:0px;height:25px;border-bottom:2px solid #007bff;"
        ifelse(
          is.null(md_model_type()), blank_style,
          ifelse(md_model_type() != type, default_style,
            paste0(default_style, "background:linear-gradient(#007bff, #007bff) no-repeat center/2px 100%")
          )
        )
      }
      
      fluidRow(
        column(width = 4, style = handle_style("contact")),
        column(width = 4, style = handle_style("entity")),
        column(width = 4, style = handle_style("featuretype")),
        style = "margin:0px;"
      )
    })
    
    #metadata editor
    output$meta_editor <- renderUI({
      print("render meta editor")
      req(!is.null(md_model_type()))
      valid = md_model_draft_valid()
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
                "creation" = switch(md_model_type(),
                                    "contact" = i18n()$t("MD_EDITOR_C_NEW"),
                                    "entity" = i18n()$t("MD_EDITOR_E_NEW"),
                                    "featuretype" = i18n()$t("MD_EDITOR_D_NEW")
                ),
                "edition" = paste0(switch(md_model_type(),
                                          "contact" = i18n()$t("MD_EDITOR_C_EDIT"),
                                          "entity" = i18n()$t("MD_EDITOR_E_EDIT"),
                                          "featuretype" = i18n()$t("MD_EDITOR_D_EDIT")
                                          )," ", md_model_draft_idx())
              ),
              handle_metadata_form(type = md_model_type(), model = if(md_model_draft_mode() == "edition") md_model_draft() else NULL),hr(),
              bs4Dash::actionButton(inputId = ns("check_model"), label = i18n()$t("MD_EDITOR_CHECK")),
              bs4Dash::actionButton(inputId = ns("save_model"), label = i18n()$t("MD_EDITOR_SAVE"), style= if(is.null(valid) || (is.logical(valid) & !valid)) "display:none;" else {NULL}),br(),
              uiOutput(ns("meta_editor_validation_status"))
            ),
            tabPanel(
              value = paste0("tabbox_", md_model_type(), "_form_validator"),
              title = i18n()$t("MD_EDITOR_VALIDATION_REPORT"),
              uiOutput(ns("validation_issues_table_wrapper"))
            )
          ),
          tabBox(
            width = 6,
            id = ns(paste0("tabbox_", md_model_type(), "_table_view")),
            type = "tabs", solidHeader = FALSE, status = "teal",
            maximizable = TRUE,
            tabPanel(
              title = i18n()$t("MD_EDITOR_TABLE_VIEW"),
              rhandsontable::rHandsontableOutput(ns("meta_table")),hr(),
              downloadButton(
                outputId = ns(paste0("download_",md_model_type(),"_table_csv")), label = i18n()$t("MD_EDITOR_TABLE_DOWNLOAD_CSV"),
                icon = icon("file-csv", class = "fa-lg", style = "color:#476ec5")
              ),
              downloadButton(
                outputId = ns(paste0("download_",md_model_type(),"_table_excel")), label = i18n()$t("MD_EDITOR_TABLE_DOWNLOAD_EXCEL"),
                icon = icon("file-excel", class = "fa-lg", style = "color:#217346;")
              ),
              if(appConfig$auth){
                bs4Dash::actionButton(
                  inputId = ns(paste0("upload_", md_model_type(),"_table")), 
                  label = i18n()$t("MD_EDITOR_TABLE_UPLOAD_CLOUD"),
                  icon = icon("cloud-arrow-up")
                )
              }else{""}
            )
          )
        )
      )
    })
    
    output$meta_editor_entry_selector_wrapper <- renderUI({
      req(length(md_model())>0)
      selectInput(ns("meta_editor_entry_selector"),
                     label = switch(md_model_type(),
                                    "contact" = i18n()$t("MD_EDITOR_C_EDIT"),
                                    "entity" =  i18n()$t("MD_EDITOR_E_EDIT"),
                                    "featuretype" = i18n()$t("MD_EDITOR_D_EDIT")),
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
                         },
                         "featuretype" = {
                           if(length(md_model())>0){
                             sapply(md_model(), function(x){if(!is.null(x$id)) x$id else "?"})
                           }else{
                             c()
                           }
                         }
                       )
                     },
                     selected = {
                       switch(md_model_type(),
                        "contact" = if(md_model_draft_mode() == "edition" & length(md_model_draft()$identifiers)>0) md_model_draft()$identifiers[[1]] else NULL,
                        "entity" = if(md_model_draft_mode() == "edition" & length(md_model_draft()$identifiers)>0) md_model_draft()$identifiers[[1]] else NULL,
                        "featuretype" = if(md_model_draft_mode() == "edition" & !is.null(md_model_draft()$id)) md_model_draft()$id else NULL
                       )
                     },
                     selectize = FALSE
      )
    })
    
    output$meta_editor_validation_status <- renderUI({
      if(!is.null(md_model_draft_valid())) if(!md_model_draft_valid()){
        bs4Dash::bs4Badge(
          if("ERROR" %in% md_model_draft_validation_report()$type) i18n()$t("MD_EDITOR_VALIDATION_ERRORS") else i18n()$t("MD_EDITOR_VALIDATION_WARNINGS"),
          color = if("ERROR" %in% md_model_draft_validation_report()$type) "danger" else "warning"
        )
      }else{
        bs4Dash::bs4Badge(
          i18n()$t("MD_EDITOR_NOVALIDATIONISSUES"),
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
          "featuretype" = geoflow_featuretype$new()$asDataFrame()
        )
      }else{
        INFO("Convert md_model to dataframe")
        metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
        INFO("Conversion done!")
      }
      print(metatbl)
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
        tags$em(i18n()$t("MD_EDITOR_NOVALIDATIONISSUES"))
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
        textInput(ns("entity_contact"), i18n()$t("MD_EDITOR_C_ONE"),value = "", width = NULL, placeholder = i18n()$t("MD_EDITOR_C_ONE"))
      }else{
        selectInput(ns("entity_contact"),
                       label = i18n()$t("MD_EDITOR_C_ONE"),
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
        field_key = "role", field_value = "identifiers", 
        field_value_list = TRUE, field_value_list_strategy = "first",
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
          column(4,textInput(ns("custom_vocab_thesaurus_name"), i18n()$t("MD_EDITOR_E_THESAURUS_NAME"),value = NULL, width = NULL, placeholder = i18n()$t("MD_EDITOR_E_THESAURUS_NAME"))),
          column(4,textInput(ns("custom_vocab_thesaurus_uri"), i18n()$t("MD_EDITOR_E_THESAURUS_URI"),value = NULL, width = NULL, placeholder = i18n()$t("MD_EDITOR_E_THESAURUS_URI")))
        ),
        fluidRow(
          column(4,textInput(ns("custom_vocab_keyword_name"), i18n()$t("MD_EDITOR_E_KEYWORD"), value = NULL, width = NULL, placeholder = i18n()$t("MD_EDITOR_E_KEYWORD"))),
          column(4,textInput(ns("custom_vocab_keyword_uri"), i18n()$t("MD_EDITOR_E_KEYWORD_URI"), value = NULL, width = NULL, placeholder = i18n()$t("MD_EDITOR_E_KEYWORD_URI"))),
          column(4,
                 actionButton(ns("custom_vocab_keyword_button_add"), title=i18n()$t("MD_EDITOR_E_KEYWORD_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;"),
                 actionButton(ns("custom_vocab_keyword_button_clear"), title=i18n()$t("MD_EDITOR_E_KEYWORD_CLEAR"),size="sm",label="",icon=icon("trash"),class = "btn-warning", style = "margin-top:35px;")
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
      render_field_elements_table(
        field = "subjects", 
        field_model = "kvp",
        field_key = "key", field_value = "keywords", field_value_object_field = "name",
        field_value_list = TRUE, field_value_list_strategy = "collapse",
        field_other_props = "name",
        btn_remove_id = ns("entity_subject_button_remove")
      )
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
        shiny::tagList(
          shinyWidgets::textInputIcon(
            inputId = ns("entity_wkt"), 
            label = "WKT", placeholder = "WKT", 
            value = md_model_bbox(),
            icon = if(is(try(sf::st_as_sfc(input$entity_wkt, crs = input$entity_srid), silent = TRUE), "try-error")){
              icon("warning", style = "color:orange;", title = i18n()$t("MD_EDITOR_E_WKT_INVALID"))
            }else{
              if(sf::st_is_valid(sf::st_as_sfc(input$entity_wkt, crs = input$entity_srid), silent = TRUE)){
                icon("check", style = "color:green;", title = i18n()$t("MD_EDITOR_E_WKT_VALID")) 
              }else{
                icon("warning", style = "color:orange;", title = i18n()$t("MD_EDITOR_E_WKT_INVALID")) 
              }
            }
          )
        )
      } else {
        shiny::tagList(
          tags$em(i18n()$t("MD_EDITOR_E_BBOX_HELPTEXT")),
          shinyWidgets::textInputIcon(ns("entity_wkt"), label = "WKT", placeholder = "WKT", value = NULL)
        )
      }
    })
    #entity -> TemporalCoverage
    #entity -> Relation
    output$entity_relations_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "relations",
        field_model = "kvp",
        field_key = "key", field_value = "link", 
        field_other_props = c("name", "description"),
        btn_remove_id = ns("entity_relation_button_remove")
      )
    })
    #entity -> Rights
    output$entity_right_wrapper <- renderUI({
      switch(input$entity_right_type,
        "license" = {
          selectInput(ns("entity_right"),
                         label = i18n()$t("MD_EDITOR_E_RIGHT_LICENSE"),
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
                         label = i18n()$t("MD_EDITOR_E_RIGHT_USECONSTRAINT"),
                         multiple = F,
                         choices = geometa::ISORestriction$values(),
                         selected = NULL,
                         selectize = FALSE
          )
        },
        "accessConstraint" = {
          selectInput(ns("entity_right"),
                         label = i18n()$t("MD_EDITOR_E_RIGHT_ACCESSCONSTRAINT"),
                         multiple = F,
                         choices = geometa::ISORestriction$values(),
                         selected = NULL,
                         selectize = FALSE
          )
        },
        "otherConstraint" = {
          textInput(ns("entity_right"), 
                    i18n()$t("MD_EDITOR_E_RIGHT_OTHERCONSTRAINT"),
                    value = "", width = NULL, 
                    placeholder = i18n()$t("MD_EDITOR_E_RIGHT_OTHERCONSTRAINT"))
        },
        "use" = {
          textInput(ns("entity_right"),
                    i18n()$t("MD_EDITOR_E_RIGHT_USELIMITATION"),
                    value = "", width = NULL, 
                    placeholder = i18n()$t("MD_EDITOR_E_RIGHT_USELIMITATION"))
        },
        "useLimitation" = {
          textInput(ns("entity_right"),
                    i18n()$t("MD_EDITOR_E_RIGHT_USELIMITATION"),
                    value = "", width = NULL, 
                    placeholder = i18n()$t("MD_EDITOR_E_RIGHT_USELIMITATION"))
        },
        "accessRight" = {
          textInput(ns("entity_right"),
                    i18n()$t("MD_EDITOR_E_RIGHT_ACCESSRIGHT"),
                    value = "", width = NULL, 
                    placeholder = i18n()$t("MD_EDITOR_E_RIGHT_ACCESSRIGHT"))
        },
        "accessConditions" = {
          textInput(ns("entity_right"),
                    i18n()$t("MD_EDITOR_E_RIGHT_ACCESSCONDITIONS"),
                    value = "", width = NULL, 
                    placeholder = i18n()$t("MD_EDITOR_E_RIGHT_ACCESSCONDITIONS"))
        },
        "termsOfUse" = {
          textInput(ns("entity_right"),
                    i18n()$t("MD_EDITOR_E_RIGHT_TERMSOFUSE"),
                    value = "", width = NULL, 
                    placeholder = i18n()$t("MD_EDITOR_E_RIGHT_TERMSOFUSE"))
        },
        "disclaimer" = {
          textInput(ns("entity_right"),
                    i18n()$t("MD_EDITOR_E_RIGHT_DISCLAIMER"),
                    value = "", width = NULL, 
                    placeholder = i18n()$t("MD_EDITOR_E_RIGHT_DISCLAIMER"))
        },
        "citation" = {
          textInput(ns("entity_right"),
                    i18n()$t("MD_EDITOR_E_RIGHT_CITATION"),
                    value = "", width = NULL, 
                    placeholder = i18n()$t("MD_EDITOR_E_RIGHT_CITATION"))
        }
      )
    })
    output$entity_rights_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "rights",
        field_model = "kvp",
        field_key = "key", field_value = "values", field_value_list_strategy = "collapse", 
        btn_remove_id = ns("entity_right_button_remove")
      )
    })
    #entity -> Format
    output$entity_formats_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "formats",
        field_model = "kvp",
        field_key = "key", field_value = "name",
        field_other_props = c("description", "uri"),
        btn_remove_id = ns("entity_format_button_remove")
      )
    })
    #entity -> Provenance
    output$entity_processes_table <- DT::renderDT(server = FALSE, {
      render_field_elements_table(
        field = "provenance", object_field = "processes",
        field_model = "kvp",
        field_key = "rationale", field_value = "description", field_value_list_strategy = "collapse", 
        btn_remove_id = ns("entity_prov_process_button_remove")
      )
    })
    #entity -> Data
    output$entity_data_type_entry <- renderUI({
      if(input$entity_data_type == "source"){
        tagList(
          fluidRow(
            column(4,textInput(ns("entity_data_source_name"), i18n()$t("MD_EDITOR_E_DATA_SOURCE_NAME"),value = NULL, width = NULL)),
            column(6,textInput(ns("entity_data_source_uri"), i18n()$t("MD_EDITOR_E_DATA_SOURCE_URL"),value = NULL, width = NULL)),
            column(1,
                   actionButton(ns("entity_data_source_button_add"), title=i18n()$t("MD_EDITOR_E_DATA_SOURCE_ADD"),size="sm",label="",icon=icon("plus"),class = "btn-success", style = "margin-top:35px;")
            )
          ),
          hr(),
          uiOutput(ns("entity_data_sources_table_wrapper"))
        )
      }else if(input$entity_data_type == "dir"){
        fluidRow(
          column(12,textInput(ns("entity_data_dir"), i18n()$t("MD_EDITOR_E_DATA_SOURCE_DIRECTORY"),value = NULL, width = NULL)),
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
    
    #dictionary/featuretype
    #featuretype -> members
    output$featuretype_members_table <- DT::renderDT(server = FALSE, {
      print("testing")
      render_field_elements_table(
        field = "members",
        field_model = "kvp",
        field_key = "code", field_value = "name",
        btn_remove_id = ns("featuretype_member_button_remove")
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
        updateSelectInput(
          inputId = "meta_editor_entry_selector", 
          selected = {
            switch(md_model_type(),
              "contact" = if(length(md_model_draft()$identifiers)>0) md_model_draft()$identifiers[[1]] else NULL,
              "entity" = if(length(md_model_draft()$identifiers)>0) md_model_draft()$identifiers[[1]] else NULL,
              "featuretype" = if(!is.null(md_model_draft()$id)) md_model_draft()$id else NULL
            )
          })
      }
      
    })
    
    #core - select entry (meta_editor_entry_selector)
    observeEvent(input$meta_editor_entry_selector,{
      has_entry = sapply(md_model(), function(x){ 
        if(md_model_type() %in% c("contact","entity")){
          input$meta_editor_entry_selector %in% c(x$identifiers,"?") 
        }else{
          input$meta_editor_entry_selector %in% c(x$id,"?") 
        }
        
      })
      selected_entry = md_model()[has_entry][[1]]
      md_model_draft_idx(which(has_entry))
      entry = selected_entry$clone(deep = TRUE)
      md_model_draft(entry)
      if(is(entry, "geoflow_entity")){
        bbox = entry$spatial_bbox
        if(!is.null(bbox)){
          geom = sf::st_polygon(list(
            rbind(
              c(bbox$xmin, bbox$ymin),
              c(bbox$xmin, bbox$ymax),
              c(bbox$xmax, bbox$ymax),
              c(bbox$xmax, bbox$ymin),
              c(bbox$xmin, bbox$ymin))
          ))
          geom_wkt = sf::st_as_text(geom)
          md_model_bbox(geom_wkt)
        }
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
          title = i18n()$t("MD_EDITOR_LOAD_ENTITIES"),
          if(appConfig$auth){
            tagList(
              jsTreeR::jstreeOutput(ns("entities_load_tree_leavesonly")),
              actionButton(ns("entities_load_tree_leavesonly_select"), label = i18n()$t("MD_EDITOR_SELECT"), status = "primary", style = "float:right"),
              actionButton(ns("entities_load_tree_leavesonly_cancel"), label = i18n()$t("MD_EDITOR_CANCEL"), style = "float:right")
            )   
          }else{
            tagList(
              fileInput(ns("entities_local_file"), label = "File",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = i18n()$t("MD_EDITOR_CHOOSEFILE")),
              actionButton(ns("entities_local_file_select"), label = i18n()$t("MD_EDITOR_SELECT"), style = "float:right")
            )
          },
          easyClose = FALSE, footer = NULL 
        )
      )
    })
    
    loadCloudTree(id = "entities_load_tree", leavesOnly = FALSE)
    loadCloudTree(id = "entities_load_tree_leavesonly", leavesOnly = TRUE)
    
    observeEvent(input$entities_load_tree_leavesonly_cancel,{
      shiny::removeModal()
    })
    observeEvent(input$entities_load_tree_leavesonly_select,{
      selected_resource = input$entities_load_tree_leavesonly_selected
      
      config = list()
      config$profile$id = "load_ocs_entities"
      config$software$input$ocs = AUTH_API
      config = geoflow::add_config_logger(config)
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
      loadCloudTree(id = "entities_load_tree_leavesonly", leavesOnly = TRUE)
    })
    observeEvent(input$entities_local_file_select,{
      req(!is.null(input$entities_local_file))
      
      config = list()
      config$profile$id = "load_local_entities"
      config = geoflow::add_config_logger(config)
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
    output$download_entity_table_csv <- downloadHandler(
      filename = function() {
        "new_entities.csv"
      },
      content = function(file) {
        metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
        readr::write_csv(metatbl, file)
      }
    )
    output$download_entity_table_excel <- downloadHandler(
      filename = function() {
        "new_entities.xlsx"
      },
      content = function(file) {
        metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
        writexl::write_xlsx(metatbl, file)
      }
    )
    
    output$entities_load_tree_upload_action <- renderUI({
      if(length(input$entities_load_tree_selected)>0){
        actionButton(ns("entities_load_tree_upload"), label = i18n()$t("MD_EDITOR_UPLOAD"), status = "primary", style = "float:right")
      }else{
        disabled(actionButton(ns("entities_load_tree_upload"), label = i18n()$t("MD_EDITOR_UPLOAD"), 
                              title = i18n()$t("MD_EDITOR_UPLOAD_TOOLTIP"),
                              status = "primary", style = "float:right"))
      }
    })
    observeEvent(input$upload_entity_table, {
      #method only available for Cloud interaction
      cloud_overwriting_danger(FALSE)
      req(appConfig$auth)
      shiny::showModal(
        shiny::modalDialog(
          title = i18n()$t("MD_EDITOR_E_CLOUD_UPLOAD"),
          tagList(
            textInput(ns("entity_table_filename"), label = i18n()$t("MD_EDITOR_FILENAME"), value = "new_entities.csv", width = NULL),
            hr(),
            jsTreeR::jstreeOutput(ns("entities_load_tree")),
            uiOutput(ns("entities_load_tree_upload_action")),
            actionButton(ns("entities_load_tree_cancel"), label = i18n()$t("MD_EDITOR_CANCEL"), style = "float:right")
          ),
          easyClose = FALSE, footer = uiOutput(ns("overwriting_file_danger")) 
        )
      )
    })
    observe({
      if(length(input$entities_load_tree_selected)>0){
        selected_resource = input$entities_load_tree_selected[[1]]
        if(selected_resource$type == "file"){
          shiny::updateTextInput(inputId = "entity_table_filename", value = basename(selected_resource$data))
          cloud_overwriting_danger(TRUE)
        }else if(selected_resource$type == "folder"){
          files = AUTH_API$listFiles(relPath = selected_resource$data)
          if(input$entity_table_filename %in% files$name){
            cloud_overwriting_danger(TRUE)
          }else{
            cloud_overwriting_danger(FALSE)
          }
        }
      }
    })
    observeEvent(input$entities_load_tree_cancel,{
      shiny::removeModal()
      cloud_overwriting_danger(FALSE)
    })
    observeEvent(input$entities_load_tree_upload,{
      req(length(input$entities_load_tree_selected)>0)
      selected_resource = input$entities_load_tree_selected[[1]]
      
      metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
      switch(mime::guess_type(input$entity_table_filename),
             "text/csv" = {
               readr::write_csv(metatbl, file.path(tempdir(), input$entity_table_filename))
             },
             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
               writexl::write_xlsx(metatbl, file.path(tempdir(), input$entity_table_filename))
             }
      )
      uploaded = AUTH_API$uploadFile(
        filename = file.path(tempdir(), input$entity_table_filename),
        relPath = if(selected_resource$type == "folder"){
          selected_resource$data
        }else if(selected_resource$type == "file"){
          dirname(selected_resource$data)
        }
      )
      shiny::removeModal()
      loadCloudTree(id = "entities_load_tree", leavesOnly = FALSE)
      cloud_overwriting_danger(FALSE)
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
          title = i18n()$t("MD_EDITOR_LOAD_CONTACTS"),
          if(appConfig$auth){
            tagList(
              jsTreeR::jstreeOutput(ns("contacts_load_tree_leavesonly")),
              actionButton(ns("contacts_load_tree_leavesonly_select"), label = i18n()$t("MD_EDITOR_SELECT"), status = "primary", style = "float:right"),
              actionButton(ns("contacts_load_tree_leavesonly_cancel"), label = i18n()$t("MD_EDITOR_CANCEL"), style = "float:right")
            )   
          }else{
            tagList(
              fileInput(ns("contacts_local_file"), label = "File",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = i18n()$t("MD_EDITOR_CHOOSEFILE")),
              actionButton(ns("contacts_local_file_select"), label = i18n()$t("MD_EDITOR_SELECT"), style = "float:right")
            )
          },
          easyClose = FALSE, footer = NULL 
        )
      )
    })
    
    loadCloudTree(id = "contacts_load_tree", leavesOnly = FALSE)
    loadCloudTree(id = "contacts_load_tree_leavesonly", leavesOnly = TRUE)
    
    observeEvent(input$contacts_load_tree_leavesonly_cancel,{
      shiny::removeModal()
    })
    observeEvent(input$contacts_load_tree_leavesonly_select,{
      selected_resource = input$contacts_load_tree_leavesonly_selected
      
      config = list()
      config$profile$id = "load_ocs_contacts"
      config$software$input$ocs = AUTH_API
      config = geoflow::add_config_logger(config)
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
      loadCloudTree(id = "contacts_load_tree_leavesonly", leavesOnly = TRUE)
    })
    observeEvent(input$contacts_local_file_select,{
      req(!is.null(input$contacts_local_file))
      
      config = list()
      config$profile$id = "load_local_contacts"
      config = geoflow::add_config_logger(config)
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
    output$download_contact_table_csv <- downloadHandler(
      filename = function() {
        "new_contacts.csv"
      },
      content = function(file) {
        metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
        readr::write_csv(metatbl, file)
      }
    )
    output$download_contact_table_excel <- downloadHandler(
      filename = function() {
        "new_contacts.xlsx"
      },
      content = function(file) {
        metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
        writexl::write_xlsx(metatbl, file)
      }
    )
    output$contacts_load_tree_upload_action <- renderUI({
      if(length(input$contacts_load_tree_selected)>0){
        actionButton(ns("contacts_load_tree_upload"), label = i18n()$t("MD_EDITOR_UPLOAD"), status = "primary", style = "float:right")
      }else{
        disabled(actionButton(ns("contacts_load_tree_upload"), label = i18n()$t("MD_EDITOR_UPLOAD"), 
                              title = i18n()$t("MD_EDITOR_UPLOAD_TOOLTIP"),
                              status = "primary", style = "float:right"))
      }
    })
    observeEvent(input$upload_contact_table, {
      #method only available for Cloud interaction
      cloud_overwriting_danger(FALSE)
      req(appConfig$auth)
      shiny::showModal(
        shiny::modalDialog(
          title = i18n()$t("MD_EDITOR_C_CLOUD_UPLOAD"),
          tagList(
            textInput(ns("contact_table_filename"), label = i18n()$t("MD_EDITOR_FILENAME"), value = "new_contacts.csv", width = NULL),
            hr(),
            jsTreeR::jstreeOutput(ns("contacts_load_tree")),
            uiOutput(ns("contacts_load_tree_upload_action")),
            actionButton(ns("contacts_load_tree_cancel"), label = "Cancel", style = "float:right")
          ),
          easyClose = FALSE, footer = uiOutput(ns("overwriting_file_danger")) 
        )
      )
    })
    observe({
      if(length(input$contacts_load_tree_selected)>0){
        selected_resource = input$contacts_load_tree_selected[[1]]
        if(selected_resource$type == "file"){
          shiny::updateTextInput(inputId = "contact_table_filename", value = basename(selected_resource$data))
          cloud_overwriting_danger(TRUE)
        }else if(selected_resource$type == "folder"){
          files = AUTH_API$listFiles(relPath = selected_resource$data)
          if(input$contact_table_filename %in% files$name){
            cloud_overwriting_danger(TRUE)
          }else{
            cloud_overwriting_danger(FALSE)
          }
        }
      }
    })
    observeEvent(input$contacts_load_tree_cancel,{
      shiny::removeModal()
      jsTreeR::jstreeDestroy(session = session, id = ns("contacts_load_tree"))
      cloud_overwriting_danger(FALSE)
    })
    observeEvent(input$contacts_load_tree_upload,{
      req(length(input$contacts_load_tree_selected)>0)
      selected_resource = input$contacts_load_tree_selected[[1]]
      
      metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
      switch(mime::guess_type(input$contact_table_filename),
             "text/csv" = {
               readr::write_csv(metatbl, file.path(tempdir(), input$contact_table_filename))
             },
             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
               writexl::write_xlsx(metatbl, file.path(tempdir(), input$contact_table_filename))
             }
      )
      uploaded = AUTH_API$uploadFile(
        filename = file.path(tempdir(), input$contact_table_filename),
        relPath = if(selected_resource$type == "folder"){
          selected_resource$data
        }else if(selected_resource$type == "file"){
          dirname(selected_resource$data)
        }
      )
      shiny::removeModal()
      loadCloudTree(id = "contacts_load_tree", leavesOnly = FALSE)
      cloud_overwriting_danger(FALSE)
    })
    
    
    #dictionary
    observeEvent(input$create_dictionary_table, {
      md_model(list())
      md_model_type("featuretype")
      INFO(sprintf("Select editor for type '%s'", md_model_type()))
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(1L)
    })
    observeEvent(input$load_dictionary_table, {
      shiny::showModal(
        shiny::modalDialog(
          title = i18n()$t("MD_EDITOR_LOAD_DICTIONARY"),
          if(appConfig$auth){
            tagList(
              jsTreeR::jstreeOutput(ns("featuretypes_load_tree_leavesonly")),
              actionButton(ns("featuretypes_load_tree_leavesonly_select"), label = i18n()$t("MD_EDITOR_SELECT"), status = "primary", style = "float:right"),
              actionButton(ns("featuretypes_load_tree_leavesonly_cancel"), label = i18n()$t("MD_EDITOR_CANCEL"), style = "float:right")
            )   
          }else{
            tagList(
              fileInput(ns("featuretypes_local_file"), label = "File",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = i18n()$t("MD_EDITOR_CHOOSEFILE")),
              actionButton(ns("featuretypes_local_file_select"), label = i18n()$t("MD_EDITOR_SELECT"), style = "float:right")
            )
          },
          easyClose = FALSE, footer = NULL 
        )
      )
    })
    
    loadCloudTree(id = "featuretypes_load_tree", leavesOnly = FALSE)
    loadCloudTree(id = "featuretypes_load_tree_leavesonly", leavesOnly = TRUE)
    
    observeEvent(input$featuretypes_load_tree_leavesonly_cancel,{
      shiny::removeModal()
    })
    observeEvent(input$featuretypes_load_tree_leavesonly_select,{
      selected_resource = input$featuretypes_load_tree_leavesonly_selected
      
      config = list()
      config$profile$id = "load_ocs_featuretypes"
      config$software$input$ocs = AUTH_API
      config = geoflow::add_config_logger(config)
      dictionary_handler = geoflow::geoflow_handler$new(yaml = system.file("metadata/dictionary", "dictionary_handler_ocs.yml", package = "geoflow"))
      dict = dictionary_handler$fun(
        handler = dictionary_handler,
        source = selected_resource[[1]]$data,
        config = config
      )
      md_model_type("featuretype")
      md_model(dict$featuretypes)
      md_model_draft_mode("edition")#triggers twice the render model
      updateSelectInput(inputId = "meta_editor_entry_selector", selected = NULL)
      
      shiny::removeModal()
      loadCloudTree(id = "featuretypes_load_tree_leavesonly", leavesOnly = TRUE)
    })
    observeEvent(input$featuretypes_local_file_select,{
      req(!is.null(input$featuretypes_local_file))
      
      config = list()
      config$profile$id = "load_local_featuretypes"
      config = geoflow::add_config_logger(config)
      dictionary_handler = switch(mime::guess_type(input$featuretypes_local_file$datapath),
                              "text/csv" = geoflow::geoflow_handler$new(yaml = system.file("metadata/entity", "dictionary_handler_csv.yml", package = "geoflow")),
                              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = geoflow::geoflow_handler$new(yaml = system.file("metadata/entity", "dictionary_handler_excel.yml", package = "geoflow")),
                              "application/vn.ms-excel" = geoflow::geoflow_handler$new(yaml = system.file("metadata/entity", "dictionary_handler_excel.yml", package = "geoflow"))
      )
      dict = dictionary_handler$fun(
        handler = dictionary_handler,
        source = input$featuretypes_local_file$datapath,
        config = config
      )
      md_model_type("featuretype")
      md_model(dict$featuretypes)
      md_model_draft_mode("edition")#triggers twice the render model
      updateSelectInput(inputId = "meta_editor_entry_selector", selected = NULL)
      
      shiny::removeModal()
      
    })
    observeEvent(input$create_dictionary,{
      md_model_draft( eval(parse(text = sprintf("geoflow::geoflow_%s$new()", md_model_type()))) )
      md_model_draft_idx(length(md_model())+1)
      md_model_draft_mode("creation")
    })
    output$download_featuretype_table_csv <- downloadHandler(
      filename = function() {
        "new_dictionary.csv"
      },
      content = function(file) {
        metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
        readr::write_csv(metatbl, file)
      }
    )
    output$download_featuretype_table_excel <- downloadHandler(
      filename = function() {
        "new_dictionary.xlsx"
      },
      content = function(file) {
        metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
        writexl::write_xlsx(metatbl, file)
      }
    )
    output$featuretypes_load_tree_upload_action <- renderUI({
      if(length(input$featuretypes_load_tree_selected)>0){
        actionButton(ns("featuretypes_load_tree_upload"), label = i18n()$t("MD_EDITOR_UPLOAD"), status = "primary", style = "float:right")
      }else{
        disabled(actionButton(ns("featuretypes_load_tree_upload"), label = i18n()$t("MD_EDITOR_UPLOAD"), 
                              title = i18n()$t("MD_EDITOR_UPLOAD_TOOLTIP"),
                              status = "primary", style = "float:right"))
      }
    })
    observeEvent(input$upload_featuretype_table, {
      #method only available for Cloud interaction
      cloud_overwriting_danger(FALSE)
      req(appConfig$auth)
      shiny::showModal(
        shiny::modalDialog(
          title = i18n()$t("MD_EDITOR_D_CLOUD_UPLOAD"),
          tagList(
            textInput(ns("featuretype_table_filename"), label = i18n()$t("MD_EDITOR_FILENAME"), value = "new_dictionary.csv", width = NULL),
            hr(),
            jsTreeR::jstreeOutput(ns("featuretypes_load_tree")),
            uiOutput(ns("featuretypes_load_tree_upload_action")),
            actionButton(ns("featuretypes_load_tree_cancel"), label = i18n()$t("MD_EDITOR_CANCEL"), style = "float:right")
          ),
          easyClose = FALSE, footer = uiOutput(ns("overwriting_file_danger")) 
        )
      )
    })
    observe({
      if(length(input$featuretypes_load_tree_selected)>0){
        req(length(input$featuretypes_load_tree_selected)>0)
        selected_resource = input$featuretypes_load_tree_selected[[1]]
        if(selected_resource$type == "file"){
          shiny::updateTextInput(inputId = "featuretype_table_filename", value = basename(selected_resource$data))
          cloud_overwriting_danger(TRUE)
        }else if(selected_resource$type == "folder"){
          files = AUTH_API$listFiles(relPath = selected_resource$data)
          if(input$featuretype_table_filename %in% files$name){
            cloud_overwriting_danger(TRUE)
          }else{
            cloud_overwriting_danger(FALSE)
          }
        }
      }
    })
    observeEvent(input$featuretypes_load_tree_cancel,{
      shiny::removeModal()
      jsTreeR::jstreeDestroy(session = session, id = ns("featuretypes_load_tree"))
      cloud_overwriting_danger(FALSE)
    })
    observeEvent(input$featuretypes_load_tree_upload,{
      selected_resource = input$featuretypes_load_tree_selected[[1]]
      
      metatbl = do.call("rbind", lapply(md_model(), function(x){x$asDataFrame()}))
      switch(mime::guess_type(input$featuretype_table_filename),
             "text/csv" = {
               readr::write_csv(metatbl, file.path(tempdir(), input$featuretype_table_filename))
             },
             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
               writexl::write_xlsx(metatbl, file.path(tempdir(), input$featuretype_table_filename))
             }
      )
      uploaded = AUTH_API$uploadFile(
        filename = file.path(tempdir(), input$featuretype_table_filename),
        relPath = if(selected_resource$type == "folder"){
          selected_resource$data
        }else if(selected_resource$type == "file"){
          dirname(selected_resource$data)
        }
      )
      shiny::removeModal()
      loadCloudTree(id = "featuretypes_load_tree", leavesOnly = FALSE)
      cloud_overwriting_danger(FALSE)
    })
    
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
          title = i18n()$t("MD_EDITOR_LOAD_CONTACTS"),
          if(appConfig$auth){
            tagList(
              jsTreeR::jstreeOutput(ns("entity_contacts_load_tree")),
              actionButton(ns("entity_contacts_load_tree_select"), label = i18n()$t("MD_EDITOR_SELECT"), style = "float:right")
            )   
          }else{
            tagList(
              fileInput(ns("entity_contacts_local_file"), label = i18n()$t("MD_EDITOR_FILENAME"),multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = i18n()$t("MD_EDITOR_CHOOSEFILE")),
              actionButton(ns("entity_contacts_local_file_select"), label = i18n()$t("MD_EDITOR_SELECT"), style = "float:right")
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
      config = geoflow::add_config_logger(config)
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
      config = geoflow::add_config_logger(config)
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
      req(!is.null(md_model_subject_selection()))
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
    observeEvent(input$entity_subject_button_remove,{
      md_model_subject_draft(NULL)
      md_model_subject_selection(NULL)
      handle_field_element_remove_event(field = "subjects", input_btn_remove = input$entity_subject_button_remove)  
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
        if(sf::st_is_valid(bbox_polygon)){
          bbox_coords <- sf::st_bbox(bbox_polygon) # Get bounding box (xmin, ymin, xmax, ymax)
          md_model_bbox(input$entity_wkt)
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
        }else{
          md_model_bbox(input$entity_wkt)
          WARN(sprintf("Invalid geometry: %s", input$entity_wkt))
          leafletProxy("entity_map") %>% clearShapes()
        }
      }else{
        md_model_bbox(input$entity_wkt)
        WARN(sprintf("Invalid geometry: %s", input$entity_wkt))
        leafletProxy("entity_map") %>% clearShapes()
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
    observeEvent(input$entity_relation_button_remove,{
      handle_field_element_remove_event(field = "relations", input_btn_remove = input$entity_relation_button_remove)
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
    observeEvent(input$entity_right_button_remove,{
      handle_field_element_remove_event(field = "rights", input_btn_remove = input$entity_right_button_remove)
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
    observeEvent(input$entity_format_button_remove,{
      handle_field_element_remove_event(field = "formats", input_btn_remove = input$entity_format_button_remove)
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
    observeEvent(input$entity_prov_process_button_remove,{
      handle_field_element_remove_event(field = "provenance", object_field = "processes", input_btn_remove = input$entity_prov_process_button_remove)
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
    #dictionary/featuretype specific form events
    #-------------------------------------------
    #events featuretype -> member
    observeEvent(input$featuretype_member_button_add,{
      ft = md_model_draft()
      fm = geoflow::geoflow_featuremember$new(
        type = input$featuremember_type,
        code = input$featuremember_code,
        name = input$featuremember_name,
        def = input$featuremember_definition,
        defSource = input$featuremember_definitionsource,
        minOccurs = input$featuremember_minoccurs,
        maxOccurs = input$featuremember_maxoccurs,
        uom = input$featuremember_measurementunit,
        registerId = input$featuremember_registerid,
        registerScript = input$featuremember_registerscript
      )
      ft$addMember(fm)
      check_model(type = md_model_type(), model = ft)
    })
    observeEvent(input$featuretype_member_button_remove,{
      handle_field_element_remove_event(field = "members", input_btn_remove = input$featuretype_member_button_remove)  
    })
    
    #Miscs
    output$overwriting_file_danger <- renderUI({
      if(cloud_overwriting_danger()){
        bs4Dash::callout(
          i18n()$t("MD_EDITOR_D_CLOUD_UPLOAD_OVERWRITE"),
          title = i18n()$t("MD_EDITOR_DANGER"),
          status = "danger",
          width = 12
        )
      }else{
        NULL
      }
    })
    
  })
  
}
