## Prerequisites

Install SWI Prolog
https://www.swi-prolog.org/download/stable


## List URL

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


## Database Location

look up on folder Documents/Prolog and you will find data_umkm.db

## Check data using fetch public api

Open SWI Prolog

consult file fetch_public_api.pl

```sh
read_data.
```


## Static api with express js

```sh
cd api
```

```sh
npm install
```

```sh
node server.js
```

## Running with web that implement api

Open SWI Prolog

consult file index_with_api.pl

Landing Page

```sh
http://localhost:8000
```
