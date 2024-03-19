# Base R Shiny image
FROM rocker/shiny

# Installation de l'openjdk
RUN apt-get update && apt-get install -y openjdk-8-jdk

# Installation des dépendances R spécifiées
RUN R -e "install.packages(c('dplyr', 'htmlwidgets', 'jsonlite', 'keyring', 'tidyr', 'shiny.fluent', 'leaflet', 'leaflet.minicharts', 'magrittr', 'plotly', 'ggplot2', 'reactable', 'rhino', 'rjson', 'sf', 'shiny', 'shiny.router', 'shinyjs', 'shinymanager', 'shinythemes', 'tm', 'wordcloud2', 'wordcloud', 'lubridate', 'stringr', 'upstartr', 'mongolite', 'glue', 'janitor'))"

# Make a directory in the container
WORKDIR /app

COPY . /app

# Install libglpk40
RUN apt-get update && apt-get install -y libglpk40

RUN apt-get update && apt-get install -y libsecret-1-0

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 8180)"]
