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

% Handler store specific id page
:- http_handler(root(store), store_handler, []).

% Handler store add page
:- http_handler(root(add_store), add_store_handler, []).

% home or landing page
home_handler(_Request) :-
    reply_html_page(
        title('Beranda'),
        [ h1('Selamat Datang di Website UMKM'),
          p('Silakan jelajahi website kami.')
        ]
    ).

% store specific id page
store_handler(Request) :-
    http_parameters(Request, [id(Id, [])]),
    (   store(Id, Name, OwnerName, Description, Address, Category, CreatedAt, UpdatedAt)
    ->  reply_html_page(
            title('Toko'),
            [ h1('Detail'),
              p(['Nama: ', Name]),
              p(['Nama Pemilik: ', OwnerName]),
              p(['Deskripsi: ', Description]),
              p(['Alamat: ', Address]),
              p(['Kategori: ', Category]),
              p(['Dibuat pada: ', CreatedAt]),
              p(['Diupdate pada: ', UpdatedAt])
            ]
        )
    ;   reply_html_page(
            title('Toko Tidak Ditemukan'),
            [ h1('Toko Tidak Ditemukan'),
              p(['Toko dengan id ', Id, ' tidak ditemukan.'])
            ]
        )
    ).

% store add page
add_store_handler(_Request) :-
    reply_html_page(
        title('Add Store'),
        [ h1('Add Store'),
          form([action='/submit_store', method='POST'],
               [ p(['Id: ', input([type='text', name='id'])]),
                 p(['Nama: ', input([type='text', name='name'])]),
                 p(['Nama Pemilik: ', input([type='text', name='owner_name'])]),
                 p(['Deskripsi: ', input([type='text', name='description'])]),
                 p(['Alamat: ', input([type='text', name='address'])]),
                 p(['Kategori: ', input([type='text', name='category'])]),
                 p(['Dibuat pada: ', input([type='datetime', name='created_at'])]),
                 p(['Diupdated Pada: ', input([type='datetime', name='updated_at'])]),
                 p(input([type='submit', value='Tambah']))
               ])
        ]
    ).

% Handler saving store
:- http_handler(root(submit_store), submit_store_handler, [method(post)]).

submit_store_handler(Request) :-
    http_parameters(Request, [id(Id, []), name(Name, []), owner_name(OwnerName, []), description(Description, []), address(Address, []), category(Category, []), created_at(CreatedAt, []), updated_at(UpdatedAt, [])]),
    assert_store(Id, Name, OwnerName, Description, Address, Category, CreatedAt, UpdatedAt),
    reply_html_page(
        title('Store Ditambahkan'),
        [ h1('Store Ditambahkan'),
          p(['Store dengan id ', Id, ' berhasil ditambahkan.']),
          p('Kembali ke halaman '),
          a([href('/store?id='), Id], 'store'),
          p('.')
        ]
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