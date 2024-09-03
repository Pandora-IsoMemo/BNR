# Select R version
FROM inwt/r-shiny:4.2.1

# Setup environment
ENV R_SHINY_PORT=3838
WORKDIR /workspace

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
    libcurl4-openssl-dev \
    libgmp-dev \
    libmpfr-dev \
    libssl-dev \
    libxml2-dev

# Install R dependencies
COPY .Rprofile .
COPY renv.lock .
COPY renv/activate.R ./renv/activate.R
RUN R -e 'renv::restore()'

# Copy R sourcers
COPY . .

# Set entrypoint
ENTRYPOINT ["Rscript", "app.R"]
