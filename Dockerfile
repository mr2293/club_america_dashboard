# Combined image for club_america_dashboard: one navbarPage app.R at the
# repo root mounts the three original apps (apps/dashboard_cargas,
# apps/cargas_fisicas_7, apps/session_calculator) as Shiny modules.
#
# Base image pinned to match renv.lock's R version (4.5.2) -- note this is
# newer than apps/dashboard_cargas/Dockerfile's own rocker/shiny:4.4.1,
# matching apps/cargas_fisicas_7/Dockerfile's rocker/shiny:4.5.2 instead.
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

WORKDIR /home/club_america_dashboard

# Root-level R project files
COPY renv.lock deploy.R app.R club_america_dashboard.Rproj* ./

# Each original app's full subtree (code + data + www/micros as applicable)
COPY apps/dashboard_cargas apps/dashboard_cargas
COPY apps/cargas_fisicas_7 apps/cargas_fisicas_7
COPY apps/session_calculator apps/session_calculator

# Install renv and restore the merged lockfile (renv.lock already tracks
# rsconnect, so renv::restore() installs it -- do not additionally
# install.packages("rsconnect") here, it overwrites the locked version with
# latest-CRAN and makes rsconnect's own deploy-time dependency check fail
# with "Library and lockfile are out of sync".
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt = FALSE)"

CMD ["Rscript", "deploy.R"]
