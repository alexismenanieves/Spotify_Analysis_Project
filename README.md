## README

This project contains code files for creating an unsupervised analysis of song similarity using data retrieved from Spotify. Some concepts are addressed, like EDA, clustering and dendograms. You are expected to know how to run R. Tidyverse and httr are required among other packages.

### Structure

The main files are:

-   `Spotify_Analysis_Script`: R script that contains the data cleaning, a basic EDA and a clustering of song data, from some artists like Pearl Jam and Pedro Suarez-Vertiz
-   `Spotify_Analysis_Connection`: R script which contains functions for creating the connection to Spotify and retrieves data like artists, albums and tracks
-   `Spotify_Analysis_Plot_Config`: R script with the ggplot theme configuration
-   `README.md`: A brief overview of the project

### Some plots from the analysis
![Image of Pearl Jam song similarities](Images/Pearl_Jam_Song_Similarity.png)
![Image of Pedro Suarez-Vertiz song similarities](Images/Pedro_Suarez_Song_Similarity.png)

### Further reading

Spotify for Developers. (2021, May 26). *Web API Reference*, Retrieved from <https://developer.spotify.com/documentation/web-api/reference/>
