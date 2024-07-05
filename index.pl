:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(persistency)).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).

:- use_module(library(http/http_json)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_header)).

% Declare dynamic predicates
:- dynamic store_list/1.
:- dynamic table_store_rows/1.
:- dynamic table_store_row/1.

:- dynamic store_review_list/1.
:- dynamic table_store_review_rows/1.
:- dynamic table_store_review_row/1.

% handler home or landing page
:- http_handler(root(.), home_handler, []).

% handler login page
:- http_handler(root(login), login_handler, []).

% Handler Action Login
:- http_handler(root(submit_login), submit_login_handler, [method(post)]).

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

% home or landing page
home_handler(Request) :-
    http_parameters(Request, [address(Address, [default('')]), category(Category, [default('')])]),
    % Parameters are provided, perform the API call
    format(atom(Url), 'http://localhost:3000/store-recommendation?address=~w&category=~w', [Address, Category]),
    catch(
        setup_call_cleanup(
            http_open(Url, Stream, []),
            json_read_dict(Stream, Stores),
            close(Stream)
        ),
        Error,
        (   format('Error fetching recommendations: ~w~n', [Error]),
            fail
        )
    ),
    DisplayRecommendations = true,
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
                        a([class='btn btn-primary', href='/login'], 'Login'),
                        a([class='btn btn-primary', href='/store'], 'Toko')
                    ])
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', 'Pencarian rekomendasi'),
                    form([method='GET', action="/", autocomplete='off'], [
                        div(class='mb-3', [
                            label([class='form-label', for='inputAlamat'], 'Alamat'),
                            input([class='form-control', id='inputAlamat', type='text', name='address', placeholder='Bogor', required='required'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputKategori'], 'Kategori'),
                            input([class='form-control', id='inputKategori', type='text', name='category', placeholder='Rumah Makan', required='required'])
                        ]),
                        div(class='mb-3 d-flex gap-2', [
                            button([type="submit", class="btn btn-primary"], 'Cari'),
                            a([href="/", class="btn btn-primary"], 'Reset')
                        ])
                    ])
                ])
            ]),
            % Conditionally render the recommendations section if parameters are provided
            \conditional_recommendations_section(DisplayRecommendations, Category, Address, Stores)
        ])
    ).

% Helper to render the recommendations section conditionally
conditional_recommendations_section(true, Category, Address, Stores) -->
    html(div(class='card mx-5 mt-4', [
        div(class='card-body', [
            h5(class='card-title mt-2', ['Hasil Pencarian : ', Category, ' ',  Address])
        ])
    ])),
    html(div(class('card mx-5 mt-4'), [
        \store_recommended(Stores)
    ])).

conditional_recommendations_section(false, _, _, _) -->
    html([]).

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
        th('Category'),
        th('Description'),
        th('Address'),
        th('Rating'),
        th('Aksi')
    ]))).

table_recommended_rows([]) --> [].
table_recommended_rows([Store|Rest]) -->
    table_recommended_row(Store),
    table_recommended_rows(Rest).

table_recommended_row(Store) -->
    {
        format(atom(GoogleMapsUrl), 'http://maps.google.com/maps?q=~w,~w', [Store.lattidue, Store.longitude])
    },
    html(tr([
        td(Store.name),
        td(Store.owner_name),
        td(Store.category),
        td(Store.description),
        td(Store.address),
        td(Store.average_rating),
        td(div(class='d-flex gap-2',[
            a([class('btn btn-primary'), target('_blank'), href(GoogleMapsUrl)], 'Menuju Peta')
        ]))
    ])).

% login page
login_handler(_Request) :-
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
                    h5(class='card-title mt-2', 'Login di Website UMKM'),
                    p(class='', 'Silahkan login website ini'),
                    div(class='d-flex gap-2 flex-wrap',[
                        a([class='btn btn-primary', href='/'], 'Rekomendasi')
                    ])
                ])
            ]),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    form([method='POST', action='/submit_login', autocomplete='off'], [
                        div(class='mb-3', [
                            label([class='form-label', for='inputUsername'], 'Username'),
                            input([class='form-control', id='inputUsername', type='text', name='username', placeholder='Username'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputPassword'], 'Password'),
                            input([class='form-control', id='inputPassword', type='password', name='password', placeholder='Password'])
                        ]),
                        div(class='mb-3 d-flex gap-2', [
                            button([type="submit", class="btn btn-primary"], 'Login')
                        ])
                    ])
                ])
            ])
        ])
    ).

% Submit Login
submit_login_handler(Request) :-
    http_parameters(Request, [
        username(Username, [string]),
        password(Password, [string])
    ]),
    
    (   catch(
            http_post('http://localhost:3000/login',
                      json(json([username=Username, password=Password])),
                      Reply, [json_object(dict)]),
            Error,
            (   format(string(ErrorMessage), '~w', [Error]),
                reply_html_page(
                    title('Login Gagal'),
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
                                p('Tidak dapat menghubungi server:'),
                                p(ErrorMessage),
                                div(class='d-flex gap-2 flex-wrap',[
                                    a([class='btn btn-primary', href='/login'], 'Kembali')
                                ])
                            ])
                        ])
                    ])
                ), fail
            )
        )
    ->  (   Reply.get(status) == true
        ->  % Set a cookie with the token
            format(string(Cookie), 'token=~w; Path=/; HttpOnly', [Reply.data.get(token)]),
            format('Set-Cookie: ~w~n', [Cookie]),  % Debug: Print the Set-Cookie header
            reply_html_page(
                title('Login Berhasil'),
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
                            p(['Selamat datang, ', Username]),
                            div(class='d-flex gap-2 flex-wrap',[
                                a([class='btn btn-primary', href='/store'], 'Toko')
                            ])
                        ])
                    ])
                ])
            )
        ;   reply_html_page(
                title('Login Gagal'),
                [h1('Login Gagal'),
                 p('Username atau password salah.')]
            )
        )
    ;   % Handle unexpected errors gracefully
        reply_html_page(
            title('Login Gagal'),
            div(class='card mx-5 mt-4', [
                div(class='card-body', [
                    h5(class='card-title mt-2', 'Login Gagal'),
                    p('Terjadi kesalahan yang tidak terduga. Silakan coba lagi nanti.')
                ])
            ])
        )
    ).


% store page
% Extracts a cookie value from the SWI HTTP request object
% Fails if either no cookie list in request or cookie Name not found
http_cookie_value(Request, Name, Value) :-
    % cookie list must exist in the request, and will live in CookieList
    memberchk(cookie(CookieList), Request),
    % find one cookie in the cookie list whose Name is Value
    nth1(_, CookieList, Name=Value).

store_handler(Request) :-
    % Extract token from cookies using http_cookie_value/3
    (   http_cookie_value(Request, token, _Token)
    ->  TokenPresent = true
    ;   TokenPresent = false
    ),
    % Fetch data from API using http_open/3
    setup_call_cleanup(
        http_open('http://localhost:3000/stores', Stream, []),
        json_read_dict(Stream, Stores),
        close(Stream)
    ),
    % Generate HTML
    reply_html_page(
        title('UMKM | Toko'),
        \html_bootstrap_head, % add bootstrap link
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
                        \maybe_add_button(TokenPresent)
                    ])
                ])
            ]),
            div(class('card mx-5 mt-4'), [
                \store_list(Stores, TokenPresent)
            ])
        ])
    ).

% Conditionally render the "Tambah" button
maybe_add_button(true) -->
    html(a([class='btn btn-primary', href='/add_store'], 'Tambah')).
maybe_add_button(false) --> [].

% Generate store list table
store_list(Stores, TokenPresent) -->
    html([
        table(class('table'), [
            \table_store_header(TokenPresent),
            tbody(\table_store_rows(Stores, TokenPresent))
        ])
    ]).

% Table header for the store list
table_store_header(true) -->
    html(thead(tr([
        th('Code'),
        th('Name'),
        th('Actions')
    ]))).
table_store_header(false) -->
    html(thead(tr([
        th('Code'),
        th('Name')
    ]))).

% Generate table rows for each store
table_store_rows([], _) --> [].
table_store_rows([Store|Rest], TokenPresent) -->
    table_store_row(Store, TokenPresent),
    table_store_rows(Rest, TokenPresent).

% Generate a table row for a single store
table_store_row(Store, true) -->
    html(tr([
        td(Store.code),
        td(Store.name),
        td(div(class='d-flex gap-2',[
            a([class('btn btn-primary'), href('/store_detail?store_code=' + Store.code)], 'Detail'),
            a([class('btn btn-warning'), href('/store_edit?store_code=' + Store.code)], 'Edit'),
            button([type(button), class('btn btn-danger'), onclick('deleteStore("' + Store.code + '")')], 'Delete')
        ]))
    ])).
table_store_row(Store, false) -->
    html(tr([
        td(Store.code),
        td(Store.name),
        td(div(class='d-flex gap-2',[
            a([class('btn btn-primary'), href('/store_detail?store_code=' + Store.code)], 'Detail')
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
                            input([class='form-control', id='inputCode', type='text', name='code', required='required'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputName'], 'Nama'),
                            input([class='form-control', id='inputName', type='text', name='name', required='required'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputOwnerName'], 'Nama Pemilik'),
                            input([class='form-control', id='inputOwnerName', type='text', name='owner_name', required='required'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputDescription'], 'Deskripsi'),
                            input([class='form-control', id='inputDescription', type='text', name='description', required='required'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputAddress'], 'Alamat'),
                            input([class='form-control', id='inputAddress', type='text', name='address', required='required'])
                        ]),
                        div(class='mb-3', [
                            label([class='form-label', for='inputCategory'], 'Kategori'),
                            input([class='form-control', id='inputCategory', type='text', name='category', required='required'])
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
                                input([class='form-control', id='inputName', type='text', name='name', value=Data.get(name), required='required'])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputOwnerName'], 'Nama Pemilik'),
                                input([class='form-control', id='inputOwnerName', type='text', name='owner_name', value=Data.get(owner_name), required='required'])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputDescription'], 'Deskripsi'),
                                input([class='form-control', id='inputDescription', type='text', name='description', value=Data.get(description), required='required'])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputAddress'], 'Alamat'),
                                input([class='form-control', id='inputAddress', type='text', name='address', value=Data.get(address), required='required'])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputCategory'], 'Kategori'),
                                input([class='form-control', id='inputCategory', type='text', name='category', value=Data.get(category), required='required'])
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
                                input([class='form-control', id='inputName', type='text', name='name', required='required' ])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputDescription'], 'Deskripsi'),
                                input([class='form-control', id='inputDescription', type='text', name='description', required='required' ])
                            ]),
                            div(class='mb-3', [
                                label([class='form-label', for='inputRating'], 'Rating'),
                                select([class='form-control', id='inputRating', name='rating', required='required'], [
                                    option([value='1'], '1'),
                                    option([value='2'], '2'),
                                    option([value='3'], '3'),
                                    option([value='4'], '4'),
                                    option([value='5'], '5')
                                ])
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
    html([
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
    ]).