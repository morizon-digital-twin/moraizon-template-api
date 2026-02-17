# syntax=docker/dockerfile:1
FROM rocker/geospatial

# 1. System-Abhangigkeiten
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# 2. Renv Cache-Pfad fur den Container definieren
ENV RENV_PATHS_CACHE=/root/.local/share/renv

# 3. Nur die fur die Installation notigen Dateien kopieren
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# 4. Restore mit Cache-Mount und Noble-Binaries
# RENV_CONFIG_REPOS_OVERRIDE stellt sicher, dass renv das Repo beim Restore wirklich nutzt
RUN --mount=type=cache,target=/root/.local/share/renv \
    RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/noble/latest" \
    R -e "renv::restore()"

# 5. Restlichen Code kopieren
COPY . .

# 6. Entrypoint Setup
RUN cp entrypoint.sh /usr/local/bin/entrypoint.sh && \
    chmod +x /usr/local/bin/entrypoint.sh

EXPOSE 7006

ENTRYPOINT ["entrypoint.sh"]