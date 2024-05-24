CREATE TABLE store (
    code varchar(5) PRIMARY KEY NOT NULL,
    name varchar(156) NOT NULL,
    owner_name varchar(156) NOT NULL,
    description varchar(256),
    address varchar(256) NOT NULL,
    category varchar(156) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

CREATE TABLE store_review (
    id INT AUTO_INCREMENT PRIMARY KEY,
    store_code varchar(5) NOT NULL,
    name varchar(156) NOT NULL,
    description varchar(512),
    rating INT NOT NULL CHECK (rating BETWEEN 0 AND 10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (store_code) REFERENCES store(code)
);
