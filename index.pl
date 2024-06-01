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
:- dynamic table_store_rows/1.
:- dynamic table_store_row/1.

:- dynamic store_review_list/1.
:- dynamic table_store_review_rows/1.
:- dynamic table_store_review_row/1.

% handler home or landing page
:- http_handler(root(.), home_handler, []).

% Handler store page
:- http_handler(root(store), store_handler, []).

% Handler store add page
:- http_handler(root(add_store), add_store_handler, []).

% Handler saving store
:- http_handler(root(submit_store), submit_store_handler, [method(post)]).

% Handler store edit specific name page
:- http_handler(root(store_edit), store_edit_handler, []).

% Handler submit edit store
:- http_handler(root(submit_edit_store), submit_edit_store_handler, [method(post)]).

% Handler store detail specific name page
:- http_handler(root(store_detail), store_detail_handler, []).

% Handler saving store
:- http_handler(root(submit_store_review), submit_store_review_handler, [method(post)]).

% Handler recommendation store
:- http_handler(root(recommendation), recommendation_store_handler, []).

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
                    form([method='GET', action="/recommendation", autocomplete='off'], [
                        div(class='mb-3', [
                            label([class='form-label', for='inputLokasi'], 'Lokasi'),
                            input([class='form-control', id='inputLokasi', type='text', name='lokasi', placeholder='Bogor'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputKategori'], 'Kategori'),
                            input([class='form-control', id='inputKategori', type='text', name='kategori', placeholder='Otomotif'])
                        ]),
                        button([type="submit", class="btn btn-primary"], 'Cari')
                    ])
                ])
            ])
        ])
    ).

recommendation_store_handler(Request) :-
    setup_call_cleanup(
        http_open('http://localhost:3000/store-recommendation?address=&category=', Stream, []),
        json_read_dict(Stream, Stores),
        close(Stream)
    ),
    reply_html_page(
        title('Rekomendasi UMKM'),
        \html_bootstrap_head,
        div([  
            div(class='p-4 bg-primary text-white', [
                div([
                    h5('Website UMKM')
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', 'Rekomendasi Toko UMKM'),
                    div(class='d-flex gap-2 flex-wrap',[
                        a([class='btn btn-primary', href='/'], 'Kembali')
                    ])
                ])
            ]),
            div(class('card mx-5 mt-4'), [
                \store_recommended(Stores)
            ])
        ])
    ).

store_recommended(Stores) -->
    html([
        table(class('table'), [
            \table_recommended_header,
            tbody(\table_recommended_rows(Stores))
        ])
    ]).

table_recommended_header -->
    html(thead(tr([
        th('Name'),
        th('Owner'),
        th('Description'),
        th('Rating')
    ]))).

table_recommended_rows([]) --> [].
table_recommended_rows([Store|Rest]) -->
    table_recommended_row(Store),
    table_recommended_rows(Rest).

table_recommended_row(Store) -->
    html(tr([
        td(Store.name),
        td(Store.owner_name),
        td(Store.description),
        td(Store.average_rating)
    ])).

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
            \table_store_header,
            tbody(\table_store_rows(Stores))
        ])
    ]).

% Table header for the store list
table_store_header -->
    html(thead(tr([
        th('Code'),
        th('Name'),
        th('Actions')
    ]))).

% Generate table rows for each store
table_store_rows([]) --> [].
table_store_rows([Store|Rest]) -->
    table_store_row(Store),
    table_store_rows(Rest).

% Generate a table row for a single store
table_store_row(Store) -->
    html(tr([
        td(Store.code),
        td(Store.name),
        td(div(class='d-flex gap-2',[
            a([class('btn btn-primary'), href('/store_detail?store_code=' + Store.code)], 'Detail'),
            a([class('btn btn-warning'), href('/store_edit?store_code=' + Store.code)], 'Edit'),
            button([type(button), class('btn btn-danger'), onclick('deleteStore("' + Store.code + '")')], 'Delete')
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
                    form([action='/submit_store', method='POST', autocomplete='off'], [                        
                        div(class='mb-3', [
                            label([class='form-label', for='inputCode'], 'Code'),
                            input([class='form-control', id='inputCode', type='text', name='code'])
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
    http_parameters(Request, [store_code(StoreCode, [])]),
    % Construct the URL for the API request
    atom_concat('http://localhost:3000/stores/', StoreCode, Url),
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
                        form([action='/submit_edit_store', method='POST', autocomplete='off'], [
                            div(class='mb-3', [
                                label([class='form-label', for='inputCode'], 'Code'),
                                input([class='form-control', id='inputCode', type='text', name='code', value=Data.get(code), readonly])
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





% Define json_data/2 predicate to construct a JSON object
json_data(JSON, Data) :-
    dict_create(JSON, json, Data).

submit_store_handler(Request) :-
    http_parameters(Request, [code(Code, []), name(Name, []), owner_name(OwnerName, []), description(Description, []), address(Address, []), category(Category, [])]), % Extract form parameters
    
    % Now, you can send the data to the API endpoint (localhost:8000/stores) using HTTP client predicates like http_post/4
    json_data(JSON, [code=Code, name=Name, owner_name=OwnerName, description=Description, address=Address, category=Category]),
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

json_edit_data(json(Data), json(Data)).

% Handler for submitting the edited store
submit_edit_store_handler(Request) :-
    % Extract form parameters
    (   http_parameters(Request, [
            code(Code, []),
            name(Name, []),
            owner_name(OwnerName, []),
            description(Description, []),
            address(Address, []),
            category(Category, [])
        ])
    ->  % Ensure all parameters are properly instantiated
        json_edit_data(json([code=Code, name=Name, owner_name=OwnerName, description=Description, address=Address, category=Category]), JSON),
        format(atom(Url), 'http://localhost:3000/stores/~w', [Code]),
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

% Handler for the /store_detail endpoint
store_detail_handler(Request) :-
    % Extract the store ID from the request parameters
    http_parameters(Request, [store_code(StoreCode, [])]),
    % Construct the URL for the API request
    atom_concat('http://localhost:3000/stores/', StoreCode, Url),
    % Attempt to fetch data from the API
    (   catch(
            (   http_open(Url, StreamStore, []),
                json_read_dict(StreamStore, Data),
                close(StreamStore)
            ),
            Error,
            (   format(user_output, 'Failed to fetch data: ~w', [Error]),
                fail
            )
        )
    ->  % If successful, generate the detail form with pre-filled data and this should be dynamic code
        
        % Fetch data from API using http_open/3
        atom_concat('http://localhost:3000/store-reviews/', StoreCode, UrlReview),
        setup_call_cleanup(
            http_open(UrlReview, StreamReview, []),
            json_read_dict(StreamReview, StoreReviews),
            close(StreamReview)
        ),

        reply_html_page(
            title('UMKM | Detail Toko'),
            \html_bootstrap_head, % Add Bootstrap link
            div([
                div(class='p-4 bg-primary text-white', [
                    div([ h5('Website UMKM') ])
                ]),
                div(class='card mx-5 mt-4', [
                    div(class='card-body', [
                        h5(class='card-title mt-2', 'Detail Toko UMKM'),
                        div(class='d-flex gap-2 flex-wrap', [
                            a([class='btn btn-primary', href='/store'], 'Kembali')
                        ])
                    ])
                ]),
                div(class='card mx-5 mt-4', [
                    div(class='card-body', [
                        div(class='mb-3', [
                            table(class('table'), [
                                tr([
                                    td('Nama Toko'),
                                    td(Data.get(name))
                                ]),
                                tr([
                                    td('Nama Pemilik'),
                                    td(Data.get(owner_name))
                                ]),
                                tr([
                                    td('Deskripsi'),
                                    td(Data.get(description))
                                ]),
                                tr([
                                    td('Alamat'),
                                    td(Data.get(address))
                                ]),
                                tr([
                                    td('Kategori'),
                                    td(Data.get(category))
                                ])
                            ])
                        ]),
                        div(class='mt-5', [ h5('Tambah Ulasan') ]),
                        form([action='/submit_store_review', method='POST', autocomplete='off'], [
                            div(class='mb-3', [
                                input([class='form-control', id='inputStoreCode', type='text', name='store_code', value=StoreCode, hidden=true ])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputName'], 'Nama'),
                                input([class='form-control', id='inputName', type='text', name='name' ])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputDescription'], 'Deskripsi'),
                                input([class='form-control', id='inputDescription', type='text', name='description' ])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputRating'], 'Rating'),
                                input([class='form-control', id='inputRating', type='number', name='rating' ])
                            ]),
                            button([type="submit", class="btn btn-primary"], 'Tambah')
                        ]),
                        div(class='mt-5', [ h5('Ulasan') ]),
                        div(class('card mx-5 mt-4'), [
                            \store_review_list(StoreReviews)
                        ])
                    ])
                ])
            ])
        )
    ;   % If data fetch failed, display an error message
        reply_html_page(
            title('UMKM | Detail Toko'),
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

% Generate store review list table
store_review_list(StoreReviews) -->
    html([
        table(class('table'), [
            \table_store_review_header,
            tbody(\table_store_review_rows(StoreReviews))
        ])
    ]).

% Table header for the store review list
table_store_review_header -->
    html(thead(tr([
        th('Code'),
        th('Name'),
        th('Description'),
        th('Rating')
    ]))).

% Generate table rows for each store review
table_store_review_rows([]) --> [].
table_store_review_rows([StoreReview|Rest]) -->
    table_store_review_row(StoreReview),
    table_store_review_rows(Rest).

% Generate a table row for a single store review
table_store_review_row(StoreReview) -->
    html(tr([
        td(StoreReview.store_code),
        td(StoreReview.name),
        td(StoreReview.description),
        td(StoreReview.rating)
    ])).

submit_store_review_handler(Request) :-
    http_parameters(Request, [name(Name, []), description(Description, []), rating(Rating, []), store_code(StoreCode, [])]), % Extract form parameters
    
    % Now, you can send the data to the API endpoint (localhost:8000/stores) using HTTP client predicates like http_post/4
    json_data(JSON, [name=Name, rating=Rating, description=Description, store_code=StoreCode]),
    % Send POST request to the API endpoint
    catch(
        http_post('http://localhost:3000/store-reviews', json(JSON), Response, []),
        Error,
        handle_error(Error)
    ),

    % If there was no error, print success message
    (   var(Error) % Check if there was no error
    ->  reply_html_page(
        title('UMKM | Tambah Ulasan Toko'),
        \html_bootstrap_head, % add boostrap link
        div([  
            div(class='p-4 bg-primary text-white', [
                div([
                    h5('Website UMKM')
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', ['Ulasan Toko dengan nama ', Name, ' berhasil ditambahkan.']),
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
    html(head([
        link([rel('stylesheet'), href('https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css')]),
        script(type('text/javascript'), "
            async function deleteStore(storeCode) {
            if (confirm('Are you sure you want to delete this store?')) {
                try {
                    const response = await fetch('http://localhost:3000/stores/' + storeCode, {
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