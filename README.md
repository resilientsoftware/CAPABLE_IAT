# CAPABLE_IAT
This is a RStudio project that aims to provide an Integrated Analysis Tool to study the results of the pilots of CAPABLE project.
The Main app is the app.R file that is the core Shiny app that provides visualizations. 
The app can work with fake and real data under this conditions:
github only contains fake_data folder that shows an example of the data. If the user has access to real data it is important to mantain the same namefile es the fake_data structure. The real data folder (data) must not be pushed on github (nowaday there is a git-ignore command on data folder).

Description of other files in the project.
-------
config.R contains the setting of the data.
dataframe.R prepares read the fake or real data and prepares the dataframes. 
