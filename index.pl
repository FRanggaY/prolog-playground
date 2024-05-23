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

% Handler store specific name page
:- http_handler(root(store_edit), store_edit_handler, []).

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
        th('Actions')
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
        td(div([
            a([class('btn btn-warning'), href('/store_edit?id=' + Store.id)], 'Edit'),
            button([type(button), class('btn btn-danger'), onclick('deleteStore(' + Store.id + ')')], 'Delete')
        ]))
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
                        button([type="submit", class="btn btn-primary"], 'Tambah')
                    ])
                ])
            ])
        ])
    ).


% Handler for the /store_edit endpoint
store_edit_handler(Request) :-
    % Extract the store ID from the request parameters
    http_parameters(Request, [id(Id, [])]),
    % Construct the URL for the API request
    atom_concat('http://localhost:3000/stores/', Id, Url),
    % Attempt to fetch data from the API
    (   catch(
            (   http_open(Url, Stream, []),
                json_read_dict(Stream, Data),
                close(Stream)
            ),
            Error,
            (   format(user_output, 'Failed to fetch data: ~w', [Error]),
                fail
            )
        )
    ->  % If successful, generate the edit form with pre-filled data
        reply_html_page(
            title('UMKM | Edit Toko'),
            \html_bootstrap_head, % Add Bootstrap link
            div([
                div(class='p-4 bg-primary text-white', [
                    div([ h5('Website UMKM') ])
                ]),
                div(class='card mx-5 mt-4', [
                    div(class='card-body', [
                        h5(class='card-title mt-2', 'Toko UMKM'),
                        div(class='d-flex gap-2 flex-wrap', [
                            a([class='btn btn-primary', href='/store'], 'Kembali')
                        ])
                    ])
                ]),
                div(class='card mx-5 mt-4', [
                    div(class='card-body', [
                        form([action='/submit_edit_store', method='POST'], [
                            div(class='mb-3', [
                                label([class='form-label', for='inputId'], 'Id'),
                                input([class='form-control', id='inputId', type='text', name='id', value=Data.get(id), readonly])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputName'], 'Nama'),
                                input([class='form-control', id='inputName', type='text', name='name', value=Data.get(name)])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputOwnerName'], 'Nama Pemilik'),
                                input([class='form-control', id='inputOwnerName', type='text', name='owner_name', value=Data.get(owner_name)])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputDescription'], 'Deskripsi'),
                                input([class='form-control', id='inputDescription', type='text', name='description', value=Data.get(description)])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputAddress'], 'Alamat'),
                                input([class='form-control', id='inputAddress', type='text', name='address', value=Data.get(address)])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputCategory'], 'Kategori'),
                                input([class='form-control', id='inputCategory', type='text', name='category', value=Data.get(category)])
                            ]),
                            button([type="submit", class="btn btn-primary"], 'Update')
                        ])
                    ])
                ])
            ])
        )
    ;   % If data fetch failed, display an error message
        reply_html_page(
            title('UMKM | Edit Toko'),
            \html_bootstrap_head, % Add Bootstrap link
            div([
                div(class='p-4 bg-primary text-white', [
                    div([ h5('Website UMKM') ])
                ]),
                div(class='card mx-5 mt-4', [
                    div(class='card-body', [
                        h5(class='card-title mt-2', 'Toko UMKM tidak ditemukan'),
                        div(class='d-flex gap-2 flex-wrap', [
                            a([class='btn btn-primary', href='/store'], 'Kembali')
                        ])
                    ])
                ])
            ])
        )
    ).


% Handler saving store
:- http_handler(root(submit_store), submit_store_handler, [method(post)]).

% Define json_data/2 predicate to construct a JSON object
json_data(JSON, Data) :-
    dict_create(JSON, json, Data).

submit_store_handler(Request) :-
    http_parameters(Request, [id(Id, []), name(Name, []), owner_name(OwnerName, []), description(Description, []), address(Address, []), category(Category, [])]), % Extract form parameters
    
    % Now, you can send the data to the API endpoint (localhost:8000/stores) using HTTP client predicates like http_post/4
    json_data(JSON, [id=Id, name=Name, owner_name=OwnerName, description=Description, address=Address, category=Category]),
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

% Handler edit store
:- http_handler(root(submit_edit_store), submit_edit_store_handler, [method(post)]).

json_edit_data(json(Data), json(Data)).

% Handler for submitting the edited store
submit_edit_store_handler(Request) :-
    % Extract form parameters
    (   http_parameters(Request, [
            id(Id, []),
            name(Name, []),
            owner_name(OwnerName, []),
            description(Description, []),
            address(Address, []),
            category(Category, [])
        ])
    ->  % Ensure all parameters are properly instantiated
        json_edit_data(json([id=Id, name=Name, owner_name=OwnerName, description=Description, address=Address, category=Category]), JSON),
        format(atom(Url), 'http://localhost:3000/stores/~w', [Id]),
        % Attempt to send the PUT request to the API endpoint
        (   catch(
                http_put(Url, json(JSON), Response, []),
                Error,
                (handle_error(Error), fail)
            )
        ->  reply_html_page(
                title('UMKM | Edit Toko'),
                \html_bootstrap_head, % add bootstrap link
                div([  
                    div(class='p-4 bg-primary text-white', [
                        div([
                            h5('Website UMKM')
                        ])
                    ]),
                    div(class='card mx-5 mt-4', [
                        div(class='card-body', [
                            h5(class='card-title mt-2', ['Store dengan nama ', Name, ' berhasil diupdate.']),
                            div(class='d-flex gap-2 flex-wrap',[
                                a([class='btn btn-primary', href='/store'], 'Kembali')
                            ])
                        ])
                    ])
                ])
            )
        ;   reply_html_page(
                title('UMKM | Edit Toko'),
                \html_bootstrap_head, % add bootstrap link
                div([  
                    div(class='p-4 bg-primary text-white', [
                        div([
                            h5('Website UMKM')
                        ])
                    ]),
                    div(class='card mx-5 mt-4', [
                        div(class='card-body', [
                            h5(class='card-title mt-2', 'Terjadi kesalahan saat mengupdate store.'),
                            div(class='d-flex gap-2 flex-wrap',[
                                a([class='btn btn-primary', href='/store'], 'Kembali')
                            ])
                        ])
                    ])
                ])
            )
        )
    ;   % If parameter extraction fails, show an error message
        reply_html_page(
            title('UMKM | Edit Toko'),
            \html_bootstrap_head, % add bootstrap link
            div([  
                div(class='p-4 bg-primary text-white', [
                    div([
                        h5('Website UMKM')
                    ])
                ]),
                div(class='card mx-5 mt-4', [
                    div(class='card-body', [
                        h5(class='card-title mt-2', 'Terjadi kesalahan saat mengupdate store. Parameter tidak lengkap.'),
                        div(class='d-flex gap-2 flex-wrap',[
                            a([class='btn btn-primary', href='/store'], 'Kembali')
                        ])
                    ])
                ])
            ])
        )
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
    html(head([
        link([rel('stylesheet'), href('https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css')]),
        script(type('text/javascript'), "
            async function deleteStore(id) {
            if (confirm('Are you sure you want to delete this store?')) {
                try {
                    const response = await fetch('http://localhost:3000/stores/' + id, {
                        method: 'DELETE'
                    });

                    if (response.ok) {
                        alert('Store deleted successfully');
                        location.reload();
                    } else {
                        alert('Failed to delete store');
                    }
                } catch (error) {
                    alert(error);
                    console.error('Error deleting store:', error);
                    alert('Failed to delete store');
                }
            }
        }
        ")
    ])).