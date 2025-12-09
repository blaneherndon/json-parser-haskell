FROM haskell:9.12.2

COPY main.hs /

CMD ["runghc", "main.hs"]
