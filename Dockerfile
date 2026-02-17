# Start from the geospatial base (which already has many system deps)
FROM rocker/geospatial

# 1. Install system dependencies for plumber and specific R packages
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Set up the working directory
WORKDIR /app

# 3. Install renv and restore the environment
# We copy only the files needed for installation first to leverage Docker caching
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Define the renv cache location
ENV RENV_PATHS_LIBRARY renv/library

# Restore the R packages (this replaces your manual list)
RUN R -e "renv::restore()"

# 4. Copy the rest of the application
COPY . .

# 5. Setup Entrypoint
RUN cp entrypoint.sh /usr/local/bin/entrypoint.sh && \
    chmod +x /usr/local/bin/entrypoint.sh

EXPOSE 7006

ENTRYPOINT ["entrypoint.sh"]