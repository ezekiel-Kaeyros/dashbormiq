# Base R Shiny image
FROM rocker/shiny

# Installation de l'openjdk
RUN apt-get update && apt-get install -y openjdk-8-jdk

# Installation des dépendances R spécifiées
RUN R -e "install.packages(c('dplyr', 'htmlwidgets', 'jsonlite', 'keyring', 'tidyr', 'shiny.fluent', 'leaflet', 'leaflet.minicharts', 'magrittr', 'plotly', 'ggplot2', 'reactable', 'rhino', 'rjson', 'sf', 'shiny', 'shiny.router', 'shinyjs', 'shinymanager', 'shinythemes', 'tm', 'wordcloud2', 'wordcloud', 'lubridate', 'stringr', 'upstartr', 'mongolite', 'glue', 'janitor'))"

RUN R -e "install.packages('topicmodels')"

# Installation du package LDAvis
RUN R -e "install.packages('LDAvis')"
RUN R -e "install.packages('quanteda')"

# Make a directory in the container
WORKDIR /app

# Copy your files into the container
COPY . /app

# Install libglpk40
RUN apt-get update && apt-get install -y libglpk40

RUN apt-get update && apt-get install -y libsecret-1-0

# Installation des dépendances système pour les packages R
# Installation des dépendances système pour les packages R
RUN apt-get update && \
    apt-get install -y libudunits2-dev libproj-dev libgdal-dev libgeos-dev libgsl-dev
# Installation de libgsl





# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 8180)"]
