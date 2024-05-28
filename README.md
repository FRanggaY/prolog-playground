## Prerequisites

Install SWI Prolog
https://www.swi-prolog.org/download/stable

## Static API

This for static data so when restart server, data will be reset

change directory to api-static

```sh
cd api-static
```

Install package

```sh
npm install
```

Serve with hard reload

```sh
npm run start
```

Opsionally, you can use nodemon by 

```sh
npm run start:nodemon
```

This api is include:
- Store
- Store Review
- Store Recommendations (TBC)

## Dynamic API

This live is connected to MYSQL Database so be sure to prepare that mysql connection already exists

change directory to api-dynamic

```sh
cd api-dynamic
```

Install package

```sh
npm install
```

Serve with hard reload

```sh
npm run start
```

Opsionally, you can use nodemon by 

```sh
npm run start:nodemon
```

Migrate database table.sql to mysql


This api is include:
- Store
- Store Review
- Store Recommendations

- Postman support import

## Web Prolog for frontend

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
