# Select R version
FROM r-base:4.2.0

# Setup environment
ENV R_SHINY_PORT=8080
WORKDIR /app
COPY . .

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
    libcurl4-openssl-dev \
    libgmp-dev \
    libmpfr-dev \
    libssl-dev \
    libxml2-dev

# Install R dependencies
RUN R -e 'renv::restore()'

# Set entrypoint
ENTRYPOINT ["Rscript", "app.R"]
