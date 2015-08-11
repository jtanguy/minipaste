FROM fpco/stack-build:latest

ADD . /opt/minipaste

WORKDIR /opt/minipaste

RUN stack setup

RUN stack build --only-snapshot --jobs 2

RUN stack build

RUN stack install

EXPOSE 8080

CMD stack exec -- minipaste
