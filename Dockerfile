FROM haskell:9.4.7-buster
WORKDIR /app
COPY . .
RUN cabal update
RUN make install
CMD compiled/lemmatchers-web
EXPOSE 4217
