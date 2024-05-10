% IDK WORK OR Not

:- use_module(library(odbc)).

% Establish connection to MySQL database
connect :-
    odbc_connect('your_data_source_name', _, [user('username'), password('password')]).

% Create table in MySQL database
create_table :-
    connect,
    odbc_query('your_data_source_name', 'CREATE TABLE example_table (id INT, name VARCHAR(255))', _),
    odbc_disconnect('your_data_source_name').

% Insert data into table
insert_data(Id, Name) :-
    connect,
    atomic_list_concat(['INSERT INTO example_table VALUES (', Id, ', \'', Name, '\')'], InsertQuery),
    odbc_query('your_data_source_name', InsertQuery, _),
    odbc_disconnect('your_data_source_name').

% Read data from table
read_data :-
    connect,
    odbc_query('your_data_source_name', 'SELECT * FROM example_table', Row),
    process_rows(Row),
    odbc_disconnect('your_data_source_name').

process_rows(end_of_file) :- !.
process_rows(Row) :-
    writeln(Row),
    odbc_query('your_data_source_name', 'FETCH', NewRow),
    process_rows(NewRow).

% Example usage
:- initialization((create_table, insert_data(1, 'Alice'), insert_data(2, 'Bob'), read_data)).


% PROVIDE READ DATA TO WEB IN TABLE
read_data :-
    connect,
    odbc_query('your_data_source_name', 'SELECT * FROM example_table', Rows),
    odbc_disconnect('your_data_source_name'),
    reply_html_page(
        title('Data from MySQL Table'),
        \html_table(Rows)
    ).

html_table([]) --> []. % Base case for empty list
html_table([Row|Rows]) --> % Recursive case
    { Row =.. [row|Data] }, % Convert row to list
    html(tr(\html_cells(Data))), % Generate HTML table row
    html_table(Rows).

html_cells([]) --> []. % Base case for empty list
html_cells([Cell|Cells]) --> % Recursive case
    html(td(Cell)), % Generate HTML table cell
    html_cells(Cells).
