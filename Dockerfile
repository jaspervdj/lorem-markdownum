FROM haskell:9.8.2-buster as build
WORKDIR /opt/work
RUN cabal update
COPY ./lorem-markdownum.cabal /opt/work/
RUN cabal build --only-dependencies -j4
COPY LICENSE /opt/work
COPY ./lib /opt/work/lib
COPY ./src /opt/work/src
RUN cabal install
RUN mkdir -p /opt/build/bin && \
    cp /root/.local/bin/lorem-markdownum-web /opt/build/bin/

FROM debian:buster-slim
COPY --from=build /opt/build/bin/* /bin/
RUN mkdir -p /usr/share/lorem-markdownum
COPY ./data /usr/share/lorem-markdownum/data
COPY ./static /usr/share/lorem-markdownum/static
ENV LOREM_MARKDOWNUM_DATA_DIR /usr/share/lorem-markdownum/data
ENV LOREM_MARKDOWNUM_STATIC_DIR /usr/share/lorem-markdownum/static
ENV LOREM_MARKDOWNUM_BIND_ADDRESS 0.0.0.0
ENV LOREM_MARKDOWNUM_BIND_PORT 80
CMD ["/bin/lorem-markdownum-web"]
