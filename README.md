# CAPABLE_IAT
This is a RStudio project that aims to providean Integrated Analysis Tool to study the results of the pilots of CAPABLE project.
The Main app is the app.R file that is the core Shiny app that provide visualizations. 
The app can work with fake and real data under this conditions:
github only contains fake_data folder that show an example od the data. If the user has access to real data it is important to mantain the same namefile es the fake_data structure. real data folder (data) must not pushed on github (nowaday there is a githum ignore command on data folder)
config.R contains the setting of the data 
dataframe.R prepares read the fake or real data and prepares the dataframes. 
