# geoflow-shiny

[geoflow-shiny](https://github.com/eblondel/geoflow-shiny) is a Shiny application to faciliate the management and execution of (meta)data workflows based on the [geoflow](https://github.com/eblondel/geoflow) R package.

Question about using `geoflow-shiny`? You can ask create a "Discussion" [here](https://github.com/eblondel/geoflow-shiny/discussions). If it deals with `geoflow`, please use the geoflow 'discussions [here](https://github.com/eblondel/geoflow/discussions)

## Sponsors

Many thanks to the following organizations that have supported :

<div style="float:left;">
  <a href="http://www.cnrs.fr"><img src="http://www.cnrs.fr/themes/custom/cnrs/logo.svg" height=200 width=200/></a>
  <a href="https://inee.cnrs.fr/fr/zones-ateliers"><img src="https://inee.cnrs.fr/sites/institut_inee/files/inline-images/logo-za_0_0.jpg" height=150 width=300/></a>
</div>

The following projects have contributed to strenghten ``geoflow-shiny``:

<a href="https://www.blue-cloud.org"><img height=100 width=300 src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png"/></a>

_Blue-Cloud has received funding from the European Union's Horizon programme call BG-07-2019-2020, topic: [A] 2019 - Blue Cloud services, Grant Agreement No.862409._

For geoflow sponsoring/funding new developments, enhancements, support requests, please contact me by [e-mail](mailto:eblondel.pro@gmail.com)

## Citation

We thank in advance people that use ``geoflow-shiny`` for citing it in their work / publication(s). For this, please use the citation provided at this link [![DOI](https://zenodo.org/badge/DOI//10.5281/zenodo.4704563.svg)](https://doi.org//10.5281/zenodo.4704563)


# Installation

[geoflow-shiny](https://github.com/eblondel/geoflow-shiny) can be downloaded from Github. 

By default all files managed by the app, including configurations newly created and results of geoflow jobs will be stored in the R working directory where the app is launched. This working directory can be modified in the ``geoflow-shiny`` data directory.

To run it on your local machine, just run the `app.R` script, this will open the Shiny application in your browser.


#Issue reporting

Issues can be reported at https://github.com/eblondel/geoflow-shiny/issues Issues should be used to report bugs and/or request enhancements / new features only.


# User guide

In its first stage, [geoflow-shiny](https://github.com/eblondel/geoflow-shiny) offers a user interface with 2 modules:
* a workflow configuration editor (initial purpose of geoflow-shiny)
* a basic workflow browser with the capacity to execute a workflow (likely to be enriched in the future)

[1. Configuration editor](#configuration_editor)<br/>
&nbsp;&nbsp;&nbsp;[1.1. Profile](#configuration_editor_profile)<br/>
&nbsp;&nbsp;&nbsp;[1.2. Metadata](#configuration_editor_metadata)<br/>
&nbsp;&nbsp;&nbsp;[1.3. Software](#configuration_editor_software)<br/>
&nbsp;&nbsp;&nbsp;[1.4. Actions](#configuration_editor_actions)<br/>
[2. Configuration browser](#configuration_browser)<br/>


<a name="configuration_editor"/>

## 1. Configuration editor

The **configuration editor** suggests a user interface to create a [geoflow](https://github.com/eblondel/geoflow) configuration that later can be either downloaded (for direct use with [geoflow](https://github.com/eblondel/geoflow)) or saved (for execution through the [geoflow-shiny](https://github.com/eblondel/geoflow-shiny) workflow browser).

For users having existing [geoflow](https://github.com/eblondel/geoflow) workflow configuration files, it is also possible to load it for easy editing and maintenance:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_load_file.jpg"/>


**Overview**

The **configuration editor** follows the structure defined by [geoflow](https://github.com/eblondel/geoflow), with 4 main sections:
* **Profile**: General information about the workflow execution and metadata. For more information see the [geoflow](https://github.com/eblondel/geoflow) documentation
* **Metadata**: Metadata information to provide for an `entity`-based workflow. This includes information about _contacts_, _entities_ (metadata + data), and an associated data _dictionary_ . For more information see the [geoflow](https://github.com/eblondel/geoflow) documentation
* **Software**: List of input/software software that will be used by [geoflow](https://github.com/eblondel/geoflow) to interact with. Input software are used to fetch data from the workflow (eg. a database, a google drive). Output software are those where geoflow will perform publication actions. For more information see the [geoflow](https://github.com/eblondel/geoflow) documentation
* **Actions**: List of actions that will be run by [geoflow](https://github.com/eblondel/geoflow)

<a name="configuration_editor_profile"/>

### 1.1. Profile

The _Profile_ editor is split between:
* Execution parameters, including the workflow `id` (a unique string, preferrably without space/special characters), the execution `mode` ('entity' or 'raw') and eventual execution `options`.
* Workflow metadata: that may be recycled through [geoflow](https://github.com/eblondel/geoflow) actions. Note: for the timebeing workflow metadata is rather at _experimental_ stage in geoflow. See [geoflow](https://github.com/eblondel/geoflow) documentation for more details

* Profile section overview:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_profile_empty.jpg"/>

* Profile section overview (once filled):

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_profile_filled.jpg"/>

<a name="configuration_editor_metadata"/>

### 1.2 Metadata

The _Metadata_ editor is split into three tabs corresponding the type of metadata that are referenced in geoflow (_contacts_, _entities_, _dictionary_). For each of them, you can add one or more sources of metadata, and eventually modify or delete. Whatever type of metadata considered (_contact_, _entity_, _dictionary_), the logic to add, modify, delete a metadata source is the same.

Metadata _contacts_ and _entities_ can also be checked to verify metadata syntax and content.

* Metadata section overview:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_empty.jpg"/>

#### Add a metadata source

For adding a source, click on "Add a new <metadata> source", where <metadata> is a contact, entity or dictionary. This will open a form to add the source.

Each metadata source form will request a _handler_ (type of source handled by [geoflow](https://github.com/eblondel/geoflow)) and the _source_ (usually the link or file path). See [geoflow](https://github.com/eblondel/geoflow) documentation for more details 

Once filled, click on "Add", and the metadata source will be added in the table.

* Contact entry form

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form1.jpg"/>

* Contact entry form with list of available contact _handlers_

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form2.jpg"/>

* Contact entry form filled:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form3.jpg"/>

#### Modify a metadata source

To modify a metadata source, select the row in the metadata table corresponding to the metadata source you want to **modify**, and then click on "Modify <metadata> source". This will open the entry form to modify the selected source. Once edited, click on "Modify".

* Contact metadata source selected:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form4.jpg"/>

* Form for metadata source modification: 

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_form5.jpg"/>

#### Delete a metadata source

To delete a metadata source, select the row in the metadata table corresponding to the source you want to **delete**, and then click on "Delete <metadata> source". A dialog will open asking for confirming the deletion:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_contact_delete.jpg"/>

#### Validate a metadata source

To validate a metadata source, click on the 'Check metadata' button on the metadata source. It will open a dialog to display a validation report:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_entity_validate.jpg"/>

The metadata validation report includes two validation steps:
* validate the metadata table *structure*: in case the structure doesn't match the one expected by _geoflow_, a message will be displayed. If the structure is valid, the validator will move to the next step
* validate the metadata table *syntax* and *content*: in case there is any warning and/or error, a metadata validation report will be displayed.

The metadata table *syntax*/*content* validation report can be displayed in two ways: 
* *smart view*: a view over the metadata table with color codes to indicate the validation status: 'green' when there is no issue, 'yellow' in case of warnings, 'red' in case of errors. For warnings and errors, a comment is displayed over the table cell to inform about the issue.

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_entity_validation_report_smartview.jpg"/>

* *raw report*: as table listing all warnings and errors by row and column. This report is also displayed in the logs when executing the geoflow workflow considered.

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_metadata_entity_validation_report_raw.jpg"/>

<a name="configuration_editor_software"/>

### 1.3 Software

Within a workflow, [geoflow](https://github.com/eblondel/geoflow)) allows to plug one or more software. geoflow will interact with these software to either read data from the source (eg. a database where to read data) or to write/publish data (eg. a database where to store a dataset, a web application where to publish data).

The user interface to add, modify or delete a software follows the same logic as for metadata sources.

* Software section overview

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_empty.jpg"/>


#### Add a software

For adding a **software**, click on "Add a new software". This will open a form to add the software. 

Each software comes with **parameters** and eventually additional **properties**. These are specific to each software. The form will change when selecting the type of software.

Once filled, click on "Add", and the software will be added in the table. In the table, click on **(+)** to expand the software and see the parameters and eventual properties.

* Software entry form:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_form1.jpg"/>

* Software entry form filled:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_form2.jpg"/>


#### Modify a software:

To modify a software, select the row in the software table corresponding to the software you want to **modify**, and then click on "Modify software". This will open the entry form to modify the selected software. Once edited, click on "Modify".

* Software selected:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_form3.jpg"/>

* Form for software modification: 

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_form4.jpg"/>


#### Delete a software

To delete a software, select the row in the software table corresponding to the software you want to **delete**, and then click on "Delete software". A dialog will open asking for confirming the deletion:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_software_delete.jpg"/>


<a name="configuration_editor_actions"/>

### 1.4 Actions

Within a workflow, [geoflow](https://github.com/eblondel/geoflow)) allows to plug one or more actions. geoflow will execute these actions, either as sequence (in `raw` mode), or as sequence iterating over entities (in `entity` mode).

The user interface to add, modify or delete an action follows the same logic as for metadata sources and software.

* Actions section overview

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_empty.jpg"/>


#### Add an action

For adding an **action**, click on "Add a new action". This will open a form to add the action. 

Each action comes with **options**, that are specific. The form will change when selecting the action you want to perfom.

Once filled, click on "Add", and the action will be added in the table. In the table, click on **(+)** to expand the action and see the eventual options chosen for the actions execution.

* Action entry form (example):

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_form1.jpg"/>


#### Modify an action:

To modify an action, select the row in the actions table corresponding to the action you want to **modify**, and then click on "Modify action". This will open the entry form to modify the selected action. Once edited, click on "Modify".

* Action selected:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_form2.jpg"/>

* Form for action modification: 

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_form3.jpg"/>


#### Delete an action

To delete an action, select the row in the actions table corresponding to the action you want to **delete**, and then click on "Delete action". A dialog will open asking for confirming the deletion:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_actions_delete.jpg"/>


<a name="configuration_browser"/>

## 2. Configuration browser

The workflow browser will list all configuration files that are available within your geoflow data directory. From there, you will be able to trigger/execute the workflow with [geoflow](https://github.com/eblondel/geoflow).

Overview of the workflow browser:

<img src="https://github.com/eblondel/geoflow-shiny/raw/main/doc/screenshots/geoflow-shiny_workflows.jpg"/>
