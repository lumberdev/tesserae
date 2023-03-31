FROM node:lts-alpine AS node

RUN apk add --update \
    git

WORKDIR /tesserae/node

# copy package.json for JS deps
COPY ./package.json /tesserae/node

# download JS deps (avoid shadow-cljs downloading them)
RUN npm install

FROM clojure:temurin-18-tools-deps-alpine

WORKDIR /tesserae

COPY --from=node /tesserae/node ./

# copy just deps.edn for clojure deps without busting cache with other files
COPY ./deps.edn /tesserae

# download clojure deps (avoid downloading in CMD step)
# use clojure over clj to avoid rlwrap

RUN clojure -A:shadow-cljs -Stree

# Copy all source code
COPY . /tesserae

# Compile app
RUN clojure -M:shadow-cljs release tesserae --verbose

CMD clojure -M:prod -m tesserae.serve --port $PORT