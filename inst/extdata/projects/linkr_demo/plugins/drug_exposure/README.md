# 1) Introduction

The "Drug exposure" plugin allows you to display **medication administration data** in the form of a **timeline**.

<table align="center"><tr><td>
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/drug_exposure/en_figure.gif"
        alt="Dygraphs timeline",
        width="750",
        style="border: solid 1px #ccc; padding: 5px;"
    />
</td></tr></table>

# 2) Features

## Figure settings

<table align="center"><tr><td>
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/drug_exposure/en_figure_settings.png"
        alt="Figure settings",
        width="400",
        style="border: solid 1px #ccc; padding: 5px;"
    />
</td></tr></table>

Here are the parameters you can adjust:

- **Data to display**: choose whether you want to display patient data or stay data

- **Concepts to display**:
    - *All concepts*: if you want to display all medications received by the patient during the period
    - *Concept classes*: if you want to display only one class of concepts. In this case, a dropdown menu allows you to choose which class(es) to display.
    - *Selected concepts*: if you want to display only certain medications, which you will choose from the concepts dropdown menu

- **Synchronize timelines**: several plugins work with a timeline (the scroll bar below the timeline where you can choose the period to display).
This feature allows you to synchronize the timelines of several widgets on the same tab.

## Code

<table align="center"><tr><td>
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/drug_exposure/figure_and_code.png"
        alt="Figure and code",
        width="750",
        style="border: solid 1px #ccc; padding: 5px;"
    />
</td></tr></table>

As with **all plugins** on LinkR, the **low-code** interface allows you to generate code from the graphical interface.

To do this, go to the "Figure settings" tab, and once you have chosen your parameters, click on "Display figure".

This will:

- **generate the code** corresponding to the chosen parameters
- then **execute the code** and display the figure corresponding to the code

You can then **modify the code** directly and save it.

If you click on "Run code" from the tab with the code editor, it will display the figure corresponding to the code.

However, if you are on the tab corresponding to the figure settings, it will erase the current code and replace it
with the code corresponding to the current parameters.

## Save files

<table align="center"><tr><td>
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/drug_exposure/en_settings_files.png"
        alt="Settings files",
        width="280",
        style="border: solid 1px #ccc; padding: 5px;"
    />
</td></tr></table>

You can **save** the figure parameters and code from the save files management page.

To do this, click on the name of the selected file at the top of the widget ("Save File 1" in the screenshot above).
You can then **add** or **delete** save files.

Then return to the "Figure settings" or "Code" tabs and save your parameters and code.

## General settings

<table align="center"><tr><td>
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/drug_exposure/en_general_settings.png"
        alt="General settings",
        width="260",
        style="border: solid 1px #ccc; padding: 5px;"
    />
</td></tr></table>

The general settings are divided into:

- **Display Settings**

    - **Show save file**: show or hide the name of the selected save file
    - **Figure and settings/editor side by side**: do you want the figure to appear side by side
    with the figure settings or the figure code? This avoids going back and forth between
    the different tabs during the widget configuration phase.

- **Code Execution**

    - **Run code when loading a save file**: this allows the code to be executed when
    the selected save file changes, which is useful when loading a project:
    this allows all widgets with this option to be loaded
    - **Run code when data is updated**: for this widget, this means that
    the data is updated when you change patient or visit, depending on the value of the 
    "Data to display" parameter
