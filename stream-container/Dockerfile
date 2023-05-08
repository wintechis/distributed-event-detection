FROM node:lts-slim

MAINTAINER Daniel Schraudner <daniel.schraudner@fau.de>

ENV PORT=8080

WORKDIR /usr/app/

RUN npm install n3

COPY output output
COPY package.json .
COPY index.js .

EXPOSE 8080

CMD node index.js -p 8080