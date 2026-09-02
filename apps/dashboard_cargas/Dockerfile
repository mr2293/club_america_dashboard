# Use official Rocker image for Shiny, platform fixed
#FROM --platform=linux/amd64 rocker/shiny:4.2.1

# Maintainer info (optional)
#LABEL maintainer="Mateo Rodriguez Cortina <your_email@example.com>"

# Install system dependencies needed for R packages
#RUN apt-get update && apt-get install -y \
#    libcurl4-openssl-dev \
#    libssl-dev \
#    libxml2-dev \
#    libgit2-dev \
#    libfontconfig1-dev \
#    libfreetype6-dev \
#    libpng-dev \
#    libjpeg-dev \
#    libtiff5-dev \
#    libv8-dev \
#    && rm -rf /var/lib/apt/lists/*

# Set working directory
#WORKDIR /home/dashboard_cargas

# Copy renv files first for dependency installation
#COPY renv.lock renv/ ./

# Install renv and restore packages from renv.lock
#RUN R -e "install.packages('renv', repos='https://cloud.r-project.org'); renv::restore(prompt = FALSE)"

# Copy the rest of your app files
#COPY app.R dashboard.R deploy.R dashboard_cargas.Rproj ./
#COPY data ./data
#COPY micros ./micros
#COPY www ./www
#COPY rsconnect ./rsconnect

# Expose Shiny default port
#EXPOSE 3838

# Default command to run the deploy script
#CMD ["Rscript", "deploy.R"]



# Use official Rocker image for Shiny
#FROM --platform=linux/amd64 rocker/shiny:4.2.1

# Install system dependencies needed for R packages
#RUN apt-get update && apt-get install -y \
#    libcurl4-openssl-dev \
#    libssl-dev \
#    libxml2-dev \
#    libgit2-dev \
#    libfontconfig1-dev \
#    libfreetype6-dev \
#    libpng-dev \
#    libjpeg-dev \
#    libtiff5-dev \
#    && rm -rf /var/lib/apt/lists/*

# Install required R packages in a single line
#RUN R -e "install.packages(c('tidyverse','zoo','reshape2','gt','ggrepel','lubridate','readxl','dplyr','shiny','plotly','shinythemes','bslib','reactable','tibble','stringr','rsconnect','data.table','DT','curl','xml2'), repos='https://cloud.r-project.org')"

# Set working directory
#WORKDIR /home/dashboard_cargas

# Copy app files
#COPY app.R dashboard.R deploy.R dashboard_cargas.Rproj ./

# Copy folders
#COPY data /home/dashboard_cargas/data
#COPY micros /home/dashboard_cargas/micros
#COPY www/player_images /home/dashboard_cargas/www/player_images
#COPY rsconnect /home/dashboard_cargas/rsconnect

# Default command to run the deploy script
#CMD ["Rscript", "deploy.R"]


# Use official Rocker image for Shiny (remove --platform)
FROM rocker/shiny:4.4.1

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libv8-dev \
    g++ \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /home/dashboard_cargas

# Copy R project + renv.lock + R scripts
COPY dashboard_cargas.Rproj renv.lock .rscignore ./
COPY app.R dashboard.R deploy.R ./

# Copy data, www, micros folders
COPY data data
COPY www www
COPY micros micros
COPY rsconnect rsconnect

# Install renv first and restore packages
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt = FALSE)"

# Default command to deploy
CMD ["Rscript", "deploy.R"]
