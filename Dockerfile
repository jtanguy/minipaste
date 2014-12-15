FROM divarvel/archlinux-haskell:latest

RUN /usr/bin/useradd -m deploy

ADD . /home/deploy/minipaste

RUN chown -R deploy:deploy /home/deploy/minipaste

ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

USER deploy
ENV HOME /home/deploy
WORKDIR /home/deploy/minipaste

RUN cabal update && cabal sandbox init
RUN cabal install -j4 --only-dependencies
RUN cabal configure && cabal build 

EXPOSE 8080

ENTRYPOINT ["/home/deploy/minipaste/dist/build/minipaste/minipaste"]

