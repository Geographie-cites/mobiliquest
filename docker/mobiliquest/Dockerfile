FROM rainist/sbt:latest as mobiliquest

RUN apk update && apk add bash git sudo nodejs-npm

RUN adduser mobiliquest -g "" -D -h /var/mobiliquest/

USER mobiliquest

RUN cd /var/mobiliquest && \
    git config --global http.sslVerify "false" && \
    git clone https://github.com/Geographie-cites/mobiliquest.git && \
    cd mobiliquest && \
    git checkout master
RUN cd /var/mobiliquest/mobiliquest/ && sbt buildUI

