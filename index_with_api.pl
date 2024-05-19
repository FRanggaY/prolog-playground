:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(persistency)).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).

:- use_module(library(http/http_json)).

% Declare dynamic predicates
:- dynamic store_list/1.
:- dynamic table_rows/1.
:- dynamic table_row/1.

% handler home or landing page
:- http_handler(root(.), home_handler, []).

% Handler store page
:- http_handler(root(store), store_handler, []).

% Handler store add page
:- http_handler(root(add_store), add_store_handler, []).

% home or landing page
home_handler(_Request) :-
    reply_html_page(
        title('Beranda'),
        \html_bootstrap_head, % add boostrap link
        div([  
            div(class='p-4 bg-primary text-white', [
                div([
                    h5('Website UMKM')
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', 'Selamat Datang di Website UMKM'),
                    p(class='', 'Silahkan jelajahi website ini'),
                    div(class='d-flex gap-2 flex-wrap',[
                        a([class='btn btn-primary', href='/store'], 'Toko')
                    ])
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', 'Pencarian rekomendasi'),
                    form([method='GET'], [
                        div(class='mb-3', [
                            label([class='form-label', for='inputLokasi'], 'Lokasi'),
                            input([class='form-control', id='inputLokasi', type='text', name='lokasi'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputKategori'], 'Kategori'),
                            input([class='form-control', id='inputKategori', type='text', name='kategori'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputPenilaian'], 'Penilaian'),
                            select([class="form-select", name='penilaian'], [
                                option(value='produk', 'Produk'),
                                option(value='fasilitas', 'Fasilitas'),
                                option(value='layanan', 'Layanan')
                            ])
                        ]),
                        button([type="submit", class="btn btn-primary"], 'Cari')
                    ])
                ])
            ])
        ])
    ).


% store page
store_handler(Request) :-
    % Fetch data from API using http_open/3
    setup_call_cleanup(
        http_open('http://localhost:3000/stores', Stream, []),
        json_read_dict(Stream, Stores),
        close(Stream)
    ),
    % Generate HTML
    reply_html_page(
        title('UMKM | Toko'),
        \html_bootstrap_head, % add boostrap link
        div([  
            div(class='p-4 bg-primary text-white', [
                div([
                    h5('Website UMKM')
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', 'Toko UMKM'),
                    div(class='d-flex gap-2 flex-wrap',[
                        a([class='btn btn-primary', href='/'], 'Kembali'),
                        a([class='btn btn-primary', href='/add_store'], 'Tambah')
                    ])
                ])
            ]),
            div(class('card mx-5 mt-4'), [
                \store_list(Stores)
            ])
        ])
    ).

% Generate store list table
store_list(Stores) -->
    html([
        table(class('table'), [
            \table_header,
            tbody(\table_rows(Stores))
        ])
    ]).

% Table header for the store list
table_header -->
    html(thead(tr([
        th('ID'),
        th('Name'),
        th('Details')
    ]))).

% Generate table rows for each store
table_rows([]) --> [].
table_rows([Store|Rest]) -->
    table_row(Store),
    table_rows(Rest).

% Generate a table row for a single store
table_row(Store) -->
    html(tr([
        td(Store.id),
        td(Store.name),
        td(a([href('/store_detail?id=' + Store.id)], 'View Details'))
    ])).

% store add page
add_store_handler(_Request) :-
    reply_html_page(
        title('UMKM | Tambah Toko'),
        \html_bootstrap_head, % add boostrap link
        div([  
            div(class='p-4 bg-primary text-white', [
                div([
                    h5('Website UMKM')
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', 'Toko UMKM'),
                    div(class='d-flex gap-2 flex-wrap',[
                        a([class='btn btn-primary', href='/store'], 'Kembali')
                    ])
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    form([action='/submit_store', method='POST'], [
                        div(class='mb-3', [
                            label([class='form-label', for='inputId'], 'Id'),
                            input([class='form-control', id='inputId', type='text', name='id'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputName'], 'Nama'),
                            input([class='form-control', id='inputName', type='text', name='name'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputOwnerName'], 'Nama Pemilik'),
                            input([class='form-control', id='inputOwnerName', type='text', name='owner_name'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputDescription'], 'Deskripsi'),
                            input([class='form-control', id='inputDescription', type='text', name='description'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputAddress'], 'Alamat'),
                            input([class='form-control', id='inputAddress', type='text', name='address'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputCategory'], 'Kategori'),
                            input([class='form-control', id='inputCategory', type='text', name='category'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputCreatedAt'], 'Dibuat pada'),
                            input([class='form-control', id='inputCreatedAt', type='date', name='created_at'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputUpdatedAt'], 'Diupdated pada'),
                            input([class='form-control', id='inputUpdatedAt', type='date', name='updated_at'])
                        ]),
                        button([type="submit", class="btn btn-primary"], 'Tambah')
                    ])
                ])
            ])
        ])
    ).

% Handler saving store
:- http_handler(root(submit_store), submit_store_handler, [method(post)]).

% Define json_data/2 predicate to construct a JSON object
json_data(JSON, Data) :-
    dict_create(JSON, json, Data).

submit_store_handler(Request) :-
    http_parameters(Request, [id(Id, []), name(Name, [])]), % Extract form parameters
    
    % Now, you can send the data to the API endpoint (localhost:8000/stores) using HTTP client predicates like http_post/4
    json_data(JSON, [id=Id, name=Name]),
    % Send POST request to the API endpoint
    catch(
        http_post('http://localhost:3000/stores', json(JSON), Response, []),
        Error,
        handle_error(Error)
    ),

    % If there was no error, print success message
    (   var(Error) % Check if there was no error
    ->  reply_html_page(
        title('UMKM | Tambah Toko'),
        \html_bootstrap_head, % add boostrap link
        div([  
            div(class='p-4 bg-primary text-white', [
                div([
                    h5('Website UMKM')
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', ['Store dengan nama ', Name, ' berhasil ditambahkan.']),
                    div(class='d-flex gap-2 flex-wrap',[
                        a([class='btn btn-primary', href='/store'], 'Kembali')
                    ])
                ])
            ])
        ])
    )
    ;   true % Do nothing if there was an error, as its already handled
    ).

% Handle HTTP request errors
handle_error(Error) :-
    format('Failed to submit store data: ~w~n', [Error]).

% handle not found page
http_404(Request) :-
    memberchk(method(Method), Request),
    memberchk(location(Relative), Request),
    format('Content-type: text/plain~n~n'),
    format('404 Not Found: ~w ~w~n', [Method, Relative]).

% start server on specific port
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% start server on port 8000
:- initialization(server(8000)).

% CDN Boostrap
html_bootstrap_head -->
    html([ link([rel('stylesheet'), href('https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css')]) ]).