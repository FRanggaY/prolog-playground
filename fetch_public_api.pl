:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

% URL of the public API
api_url('https://api-berita-indonesia.vercel.app/').

% Fetch data from the public API
fetch_data(Data) :-
    api_url(URL),
    http_open(URL, In, []),
    json_read_dict(In, Data),
    close(In).

% Read data
read_data :-
    fetch_data(Data),
    process_endpoints(Data.endpoints).

% Process endpoints
process_endpoints([]).
process_endpoints([Endpoint|Endpoints]) :-
    process_endpoint(Endpoint),
    process_endpoints(Endpoints).

% Process a single endpoint
process_endpoint(Endpoint) :-
    writeln('Endpoint:'),
    writeln(Endpoint.name), % Print endpoint name
    writeln('--------------------------------------'),
    writeln('Paths:'),
    process_paths(Endpoint.paths), % Process paths
    writeln('---------------------').

% Process paths
process_paths([]).
process_paths([Path|Paths]) :-
    writeln('--------------------------------------'),
    writeln(Path.name), % Print path name
    writeln(Path.path), % Print path path
    writeln('--------------------------------------'),
    process_paths(Paths).
