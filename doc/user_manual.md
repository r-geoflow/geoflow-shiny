User Manual
================

**Table of contents**

[1. Installation](#1-installation)<br/>    [1.1. Local
installation](#installation_local)<br/>    [1.2. Docker
installation](#installation_docker)<br/> [2.
Configuration](#2-configuration)<br/>    [2.1. Configuration
file](#configuration_file)<br/>    [2.2. Use in anonymous
mode](#config_mode_anonymous)<br/>    [2.3. Use in authenticated
mode](#config_mode_auth)<br/> [3. Modules](#3-modules)<br/>    [3.1.
Configuration editor](#module_editor)<br/>       [3.1.1.
Profile](#module_editor_profile)<br/>       [3.1.2.
Metadata](#module_editor_metadata)<br/>       [3.1.3.
Software](#module_editor_software)<br/>       [3.1.4.
Actions](#module_editor_actions)<br/>    [3.2. Configuration
browser](#module_browser)<br/> [4. Advanced features](#advanced)<br/>
   [4.1. User cloud interaction](#advanced_cloud)<br/>

In its first stage,
[geoflow-shiny](https://github.com/r-geoflow/geoflow-shiny) offers a
user interface with 2 modules:

-   a workflow configuration editor (initial purpose of geoflow-shiny)
-   a basic workflow browser with the capacity to execute a workflow
    (likely to be enriched in the future)

<a name="installation"/>

# 1 Installation

<a name="installation_local"/>

## 1.1 Local installation

[geoflow-shiny](https://github.com/r-geoflow/geoflow-shiny) can be
downloaded from Github.

By default all files managed by the app, including configurations newly
created and results of geoflow jobs will be stored in the R working
directory where the app is launched. This working directory can be
modified in the `geoflow-shiny` data directory.

To run it on your local machine, just run the `app.R` script, this will
open the Shiny application in your browser.

<a name="installation_docker"/>

## 1.2 Docker installation

[geoflow-shiny](https://github.com/r-geoflow/geoflow-shiny) includes a
Dockerfile for those interested in Docker installation/deployment. An
image of the latest shiny application is available in the [Docker
Hub](https://hub.docker.com/r/eblondel/geoflow-shiny)

To pull the image

    docker pull eblondel/geoflow-shiny

<a name="configuration"/>

# 2 Configuration

<a name="configuration_file"/>

## 2.1 Configuration file

To be launched, the app requires a configuration file (YAML file). A
default configuration file is provided
[here](https://github.com/r-geoflow/geoflow-shiny/blob/main/resources/config.yml)

<a name="config_mode_anonymous"/>

## 2.2 Use in anonymous mode

By default, the configuration provided to launch the application is set
for anonymous usage, meaning the the application modules will be
directlly accessed by user, without the need to authenticate. The
default configuration file provided
[here](https://github.com/r-geoflow/geoflow-shiny/blob/main/resources/config.yml)
corresponds to this mode:

``` yaml
title: Geoflow UI
lang: en
auth: false
data_dir_local: .
logger: INFO
```

-   The `title` allows to customize the app title that will appear as
    header.
-   The `lang` parameter is a placeholder for forthcoming multi-lingual
    support (*not yet supported*)
-   The `auth` clarifies whether if we want to run the app in
    authenticated mode.
-   The `data_dir_local` specifies the local directory where
    [geoflow](https://github.com/r-geoflow/geoflow) execution jobs will
    be stored. By default it will be the app root directory.
-   The `logger` specifies the level of logs wished for the application

<a name="config_mode_auth"/>

## 2.3 Use in authenticated mode

The application can also be run in authenticated mode, meaning the user
will have to login to access the application modules.

At this stage, 2 authentication methods are supported:

-   *authentication using a login form*: only one possible
    authentication provider implemented for now, and called `ocs` (for
    Open Collaboration Services) that allows to connect to institutional
    clouds such as NextCloud or Owncloud through the
    [ocs4R](https://github.com/eblondel/ocs4R) R interface to OCS API
-   *authentication without form*, specifically tailored to ShinyProxy
    deployments, inheriting *SHINYPROXY_OIDC_ACCESS_TOKEN*: only one
    possible authentication provider implemented to connect to the
    [D4science e-infrastructure](https://www.d4science.org) through the
    [d4storagehub4R](https://github.com/eblondel/d4storagehub4R) R
    package

To support the authenticated mode, a different configuration has to be
provided to the application, specifying `auth: true`.

To support the authentication mode through login form, one or more
`auth_endpoints` can be defined. Each endpoint should include its type
(`auth_type`) for now limited to `ocs`, its name (`auth_name`) and base
URL (`auth_url`). The list of endpoints will appear as dropdown list in
the login form.

Additional properties allow to control the appearance of the login form.

The `data_dir_local` is still to be configured, as it will be the place
where geoflow will store the `jobs`. Indeed, at now execution `jobs`
will not be stored on the user cloud space but will still be stored
locally (where the application is run/deployed). Nonetheless, the
*configuration files* managed through the application will be stored in
the cloud in the remoted directory configured as `data_dir_remote`.

Example of configuration to let users connect through three cloud
providers:

``` yaml
title: Geoflow UI
lang: en
auth: true
auth_cookie_expiry: 30
auth_endpoints:
    - 
      auth_name: INRAE NextCloud
      auth_type: ocs
      auth_url: https://nextcloud.inrae.fr
    - 
      auth_name: Ifremer OwnCloud
      auth_type: ocs
      auth_url: https://cloud.ifremer.fr
    - 
      auth_name: CNRS NextCloud
      auth_type: ocs
      auth_url: https://mycore.core-cloud.net
auth_background: "linear-gradient(rgba(0, 140, 142, 0.9), rgba(255, 255, 255, 0.5))"
auth_title: "Geoflow"
auth_footer: "<br><p>This Geoflow was set-up for the French Zone Ateliers network / BED project. To access the service, you will need to authenticate with your cloud user account.</p>"
data_dir_local: .
data_dir_remote: MyDrive/geoflow
logger: INFO
```

When launching the application, the login form will appear as follows:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_auth_form.jpg"/>

<a name="modules"/>

# 3 Modules

<a name="module_editor"/>

## 3.1 Configuration editor

The **configuration editor** suggests a user interface to create a
[geoflow](https://github.com/r-geoflow/geoflow) configuration that later
can be either downloaded (for direct use with
[geoflow](https://github.com/r-geoflow/geoflow)) or saved (for execution
through the [geoflow-shiny](https://github.com/r-geoflow/geoflow-shiny)
workflow browser).

For users having existing
[geoflow](https://github.com/r-geoflow/geoflow) workflow configuration
files, it is also possible to load it for easy editing and maintenance:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_load_file.jpg"/>

**Overview**

The **configuration editor** follows the structure defined by
[geoflow](https://github.com/r-geoflow/geoflow), with 4 main sections:
\* **Profile**: General information about the workflow execution and
metadata. For more information see the
[geoflow](https://github.com/r-geoflow/geoflow) documentation \*
**Metadata**: Metadata information to provide for an `entity`-based
workflow. This includes information about *contacts*, *entities*
(metadata + data), and an associated data *dictionary* . For more
information see the [geoflow](https://github.com/r-geoflow/geoflow)
documentation \* **Software**: List of input/software software that will
be used by [geoflow](https://github.com/r-geoflow/geoflow) to interact
with. Input software are used to fetch data from the workflow (eg. a
database, a google drive). Output software are those where geoflow will
perform publication actions. For more information see the
[geoflow](https://github.com/r-geoflow/geoflow) documentation \*
**Actions**: List of actions that will be run by
[geoflow](https://github.com/r-geoflow/geoflow)

<a name="module_editor_profile"/>

### 3.1.1 Profile

The *Profile* editor is split between: \* Execution parameters,
including the workflow `id` (a unique string, preferrably without
space/special characters), the execution `mode` (‘entity’ or ‘raw’) and
eventual execution `options`. \* Workflow metadata: that may be recycled
through [geoflow](https://github.com/r-geoflow/geoflow) actions. Note:
for the timebeing workflow metadata is rather at *experimental* stage in
geoflow. See [geoflow](https://github.com/r-geoflow/geoflow)
documentation for more details

-   Profile section overview:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_profile_empty.jpg"/>

-   Profile section overview (once filled):

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_profile_filled.jpg"/>

<a name="module_editor_metadata"/>

### 3.1.2 Metadata

The *Metadata* editor is split into three tabs corresponding the type of
metadata that are referenced in geoflow (*contacts*, *entities*,
*dictionary*). For each of them, you can add one or more sources of
metadata, and eventually modify or delete. Whatever type of metadata
considered (*contact*, *entity*, *dictionary*), the logic to add,
modify, delete a metadata source is the same.

Metadata *contacts* and *entities* can also be checked to verify
metadata syntax and content.

-   Metadata section overview:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_empty.jpg"/>

#### 3.1.2.1 Add a metadata source

For adding a source, click on “Add a new <metadata> source”, where
<metadata> is a contact, entity or dictionary. This will open a form to
add the source.

Each metadata source form will request a *handler* (type of source
handled by [geoflow](https://github.com/r-geoflow/geoflow)) and the
*source* (usually the link or file path). See
[geoflow](https://github.com/r-geoflow/geoflow) documentation for more
details

Once filled, click on “Add”, and the metadata source will be added in
the table.

-   Contact entry form

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form1.jpg"/>

-   Contact entry form with list of available contact *handlers*

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form2.jpg"/>

-   Contact entry form filled:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form3.jpg"/>

#### 3.1.2.2 Modify a metadata source

To modify a metadata source, select the row in the metadata table
corresponding to the metadata source you want to **modify**, and then
click on “Modify <metadata> source”. This will open the entry form to
modify the selected source. Once edited, click on “Modify”.

-   Contact metadata source selected:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form4.jpg"/>

-   Form for metadata source modification:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form5.jpg"/>

#### 3.1.2.3 Delete a metadata source

To delete a metadata source, select the row in the metadata table
corresponding to the source you want to **delete**, and then click on
“Delete <metadata> source”. A dialog will open asking for confirming the
deletion:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_delete.jpg"/>

#### 3.1.2.4 Validate a metadata source

To validate a metadata source, click on the ‘Check metadata’ button on
the metadata source. It will open a dialog to display a validation
report:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_entity_validate.jpg"/>

The metadata validation report includes two validation steps: \*
validate the metadata table *structure*: in case the structure doesn’t
match the one expected by *geoflow*, a message will be displayed. If the
structure is valid, the validator will move to the next step \* validate
the metadata table *syntax* and *content*: in case there is any warning
and/or error, a metadata validation report will be displayed.

The metadata table *syntax*/*content* validation report can be displayed
in two ways: \* *smart view*: a view over the metadata table with color
codes to indicate the validation status: ‘green’ when there is no issue,
‘yellow’ in case of warnings, ‘red’ in case of errors. For warnings and
errors, a comment is displayed over the table cell to inform about the
issue.

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_entity_validation_report_smartview.jpg"/>

-   *raw report*: as table listing all warnings and errors by row and
    column. This report is also displayed in the logs when executing the
    geoflow workflow considered.

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_entity_validation_report_raw.jpg"/>

<a name="module_editor_software"/>

### 3.1.3 Software

Within a workflow, [geoflow](https://github.com/r-geoflow/geoflow))
allows to plug one or more software. geoflow will interact with these
software to either read data from the source (eg. a database where to
read data) or to write/publish data (eg. a database where to store a
dataset, a web application where to publish data).

The user interface to add, modify or delete a software follows the same
logic as for metadata sources.

-   Software section overview

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_empty.jpg"/>

#### 3.1.3.1 Add a software

For adding a **software**, click on “Add a new software”. This will open
a form to add the software.

Each software comes with **parameters** and eventually additional
**properties**. These are specific to each software. The form will
change when selecting the type of software.

Once filled, click on “Add”, and the software will be added in the
table. In the table, click on **(+)** to expand the software and see the
parameters and eventual properties.

-   Software entry form:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_form1.jpg"/>

-   Software entry form filled:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_form2.jpg"/>

#### 3.1.3.2 Modify a software:

To modify a software, select the row in the software table corresponding
to the software you want to **modify**, and then click on “Modify
software”. This will open the entry form to modify the selected
software. Once edited, click on “Modify”.

-   Software selected:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_form3.jpg"/>

-   Form for software modification:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_form4.jpg"/>

#### 3.1.3.3 Delete a software

To delete a software, select the row in the software table corresponding
to the software you want to **delete**, and then click on “Delete
software”. A dialog will open asking for confirming the deletion:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_delete.jpg"/>

<a name="module_editor_actions"/>

### 3.1.4 Actions

Within a workflow, [geoflow](https://github.com/r-geoflow/geoflow))
allows to plug one or more actions. geoflow will execute these actions,
either as sequence (in `raw` mode), or as sequence iterating over
entities (in `entity` mode).

The user interface to add, modify or delete an action follows the same
logic as for metadata sources and software.

-   Actions section overview

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_empty.jpg"/>

#### 3.1.4.1 Add an action

For adding an **action**, click on “Add a new action”. This will open a
form to add the action.

Each action comes with **options**, that are specific. The form will
change when selecting the action you want to perfom.

Once filled, click on “Add”, and the action will be added in the table.
In the table, click on **(+)** to expand the action and see the eventual
options chosen for the actions execution.

-   Action entry form (example):

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_form1.jpg"/>

#### 3.1.4.2 Modify an action:

To modify an action, select the row in the actions table corresponding
to the action you want to **modify**, and then click on “Modify action”.
This will open the entry form to modify the selected action. Once
edited, click on “Modify”.

-   Action selected:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_form2.jpg"/>

-   Form for action modification:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_form3.jpg"/>

#### 3.1.4.3 Delete an action

To delete an action, select the row in the actions table corresponding
to the action you want to **delete**, and then click on “Delete action”.
A dialog will open asking for confirming the deletion:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_delete.jpg"/>

<a name="module_browser"/>

### 3.1.5 List of configurations

The workflow browser will list all configuration files that are
available within your geoflow data directory. From there, you will be
able to trigger/execute the workflow with
[geoflow](https://github.com/r-geoflow/geoflow).

Overview of the workflow browser:

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_workflows.jpg"/>

Each configuration file can be edited in the configuration *editor* or
executed. The execution of a workflow can be followed in the *Console*
that will dynamically show the following information:

-   computation logs, structured in three parts: R session info;
    Initialization; Execution,
-   status (including the status of workflow *initialization* and
    overall *execution*), with color code (red if failure, green if
    success)

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_console.jpg"/>

Each section of the execution is expandable/collapsible:

-   *R Session Info*

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_console_sessioninfo.jpg"/>

-   *Initialization*

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_console_initialization.jpg"/>

-   *Execution*

<img src="https://github.com/r-geoflow/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_console_execution.jpg"/>

<a name="advanced"/>

### 3.1.6 Advanced features

<a name="advanced_cloud"/>

#### 3.1.6.1 User cloud interaction

In authenticated mode,
[geoflow-shiny](https://github.com/r-geoflow/geoflow-shiny) is capable
to share the Shiny session user information to
[geoflow](https://github.com/r-geoflow) to let this one interact with
the cloud within a workflow. This is made possible thanks to
[geoflow](https://github.com/r-geoflow/geoflow) Shiny awareness and its
capability to evaluate a Shiny session user environment.

Hence, [geoflow-shiny](https://github.com/r-geoflow) will share to
[geoflow](https://github.com/r-geoflow/geoflow) a session `userData`
environment that includes the following information:

-   `GEOFLOW_SHINY_AUTH_URL`: the auth cloud endpoint URL used
-   `GEOFLOW_SHINY_AUTH_USER`: the auth cloud user that is using
    [geoflow-shiny](https://github.com/r-geoflow/geoflow-shiny)
-   `GEOFLOW_SHINY_AUTH_PWD`: the auth cloud user password

By principle, configuration files that are managed by user are not
modified/altered by the application itself; and at this stage, it is
required to make sure an OCS software is defined in the configuration
for which user wants to share his user information to connect to cloud
through [geoflow](https://github.com/r-geoflow/geoflow). To do so, an
OCS software definition containing the above environment variable names
(as Mustache syntax), should be added as follows:

``` json
  {
      "id": "ocs-cloud",
      "type": "input",
      "software_type": "ocs",
      "parameters": {
        "url": "{{GEOFLOW_SHINY_AUTH_URL}}",
        "user": "{{GEOFLOW_SHINY_AUTH_USER}}",
        "pwd": "{{GEOFLOW_SHINY_AUTH_PWD}}",
        "logger": "DEBUG"
      }
    }
```
