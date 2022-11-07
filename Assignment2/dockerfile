# Run the following from the commandline:
# docker build -t haskell-pl . 
# docker run -it --rm haskell-pl
FROM haskell:8
WORKDIR /
RUN cabal update;
RUN cabal install BNFC;
RUN cabal install alex;
RUN cabal install happy;
CMD /bin/bash