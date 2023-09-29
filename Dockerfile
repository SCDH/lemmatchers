FROM haskell:8.10.7-buster
WORKDIR /app
COPY . .
RUN cabal update
RUN make install
CMD compiled/lemmatchers-web
EXPOSE 4217
