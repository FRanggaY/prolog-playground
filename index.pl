:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(persistency)).

% fact
:- persistent(
    store(id:atom, name:atom, owner_name:atom, description:atom, address:atom, category:atom, created_at:atom, updated_at:atom)
).

% location db
:- db_attach('data_umkm.db', []).

% handler home or landing page
:- http_handler(root(.), home_handler, []).

% Handler store page
:- http_handler(root(store), store_handler, []).

% Handler store specific name page
:- http_handler(root(store_detail), store_detail_handler, []).

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
            div(class='card mx-5 mt-4', [
                p(['disini list tokonya, bisa pakai kartu/table semudahnya. untuk liat detail /store_detail?id={idyangdipilih} ']) % add table here (Coming Soon)
            ])
        ])
    ).

% store specific id page
store_detail_handler(Request) :-
    http_parameters(Request, [id(Id, [])]),
    (   store(Id, Name, OwnerName, Description, Address, Category, CreatedAt, UpdatedAt)
    ->  reply_html_page(
            title('UMKM | Detail Toko'),
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
                        p(['Nama: ', Name]),
                        p(['Nama Pemilik: ', OwnerName]),
                        p(['Deskripsi: ', Description]),
                        p(['Alamat: ', Address]),
                        p(['Kategori: ', Category]),
                        p(['Dibuat pada: ', CreatedAt]),
                        p(['Diupdate pada: ', UpdatedAt])
                    ])
                ])
            ])
        )
    ;   reply_html_page(
            title('UMKM | Detail Toko'),
            \html_bootstrap_head, % add boostrap link
            div([  
                div(class='p-4 bg-primary text-white', [
                    div([
                        h5('Website UMKM')
                    ])
                ]),
                div(class='card mx-5 mt-4', [
                    div(class='card-body', [
                        h5(class='card-title mt-2', 'Toko UMKM tidak ditemukan'),
                        div(class='d-flex gap-2 flex-wrap',[
                            a([class='btn btn-primary', href='/store'], 'Kembali')
                        ])
                    ])
                ])
            ])
        )
    ).

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

submit_store_handler(Request) :-
    http_parameters(Request, [id(Id, []), name(Name, []), owner_name(OwnerName, []), description(Description, []), address(Address, []), category(Category, []), created_at(CreatedAt, []), updated_at(UpdatedAt, [])]),
    assert_store(Id, Name, OwnerName, Description, Address, Category, CreatedAt, UpdatedAt),
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
                    h5(class='card-title mt-2', ['Store dengan id ', Id, ' berhasil ditambahkan.']),
                    div(class='d-flex gap-2 flex-wrap',[
                        a([class='btn btn-primary', href='/store'], 'Kembali')
                    ])
                ])
            ])
        ])
    ).

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