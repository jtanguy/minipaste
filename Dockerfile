FROM fpco/stack-build:latest

RUN /usr/bin/useradd -m deploy

ADD . /home/deploy/minipaste

RUN chown -R deploy:deploy /home/deploy/minipaste

USER deploy
ENV HOME /home/deploy
WORKDIR /home/deploy/minipaste

RUN stack build

EXPOSE 8080

ENTRYPOINT ["stack exec -- minipaste"]

