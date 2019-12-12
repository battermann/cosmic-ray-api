FROM haskell:8.6.5
RUN mkdir -p /opt/build
COPY ./connect-four-api/ /opt/build/
WORKDIR /opt/build
RUN stack build
CMD stack exec connect-four-api-exe