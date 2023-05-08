FROM node:lts-slim

MAINTAINER Daniel Schraudner <daniel.schraudner@fau.de>

WORKDIR /usr/app/

RUN npm install n3 xhr2

COPY output output
COPY package.json .
COPY index.js .

CMD node index.js