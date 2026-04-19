FROM rocker/shiny:4.5.2

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
    python3 \
    && rm -rf /var/lib/apt/lists/* 

WORKDIR /home/cargas_fisicas_7

COPY cargas_fisicas_7.Rproj renv.lock fix_lock.py ./
COPY app.R cargas7.R deploy.R ./
COPY data data

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt = FALSE)" && \
    R -e "install.packages('rsconnect', repos='https://cloud.r-project.org')" && \
    R -e "renv::snapshot(prompt = FALSE)"

# Strip non-standard array fields from renv.lock so rsconnect can parse it
RUN python3 fix_lock.py

CMD ["Rscript", "deploy.R"]