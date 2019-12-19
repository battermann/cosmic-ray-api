FROM haskell:8.6.5
RUN apt update && apt upgrade -y && apt install -y postgresql postgresql-contrib libpq-dev
RUN mkdir -p /app
COPY . /app
WORKDIR /app
RUN stack build
CMD stack exec connect-four-api-exe
