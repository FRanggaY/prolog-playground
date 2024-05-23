## Prerequisites

Install SWI Prolog
https://www.swi-prolog.org/download/stable


## Web Prolog

Open SWI Prolog

consult file index.pl

Landing Page

```sh
http://localhost:8000
```

Add Store

```sh
http://localhost:8000/add_store
```

Specific Store by Id

```sh
http://localhost:8000/store?id={id}
```

## Static api with express js

change directory to api

```sh
cd api
```

Install package

```sh
npm install
```

Serve

```sh
node server.js
```

Hard Reload Serve

```sh
node --watch server.js
```