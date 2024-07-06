CREATE TABLE store (
    code varchar(10) PRIMARY KEY NOT NULL,
    name varchar(156) NOT NULL,
    owner_name varchar(156) NOT NULL,
    description varchar(256),
    lattidue varchar(100),
    longitude varchar(100),
    address varchar(256) NOT NULL,
    category varchar(156) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

INSERT INTO store (code, name, owner_name, description, address, category, lattidue, longitude) VALUES
('WRT', 'Warung Ratu', 'Ratna', 'Warung makan yang menyediakan masakan khas Sunda.', 'Bandung, Jawa Barat', 'Makanan', '-6.917464', '107.619125'),
('TKG', 'Toko Kue Gembira', 'Budi', 'Menjual berbagai jenis kue tradisional dan modern.', 'Jakarta Selatan, DKI Jakarta', 'Kue', '-6.175110', '106.865036'),
('BGR', 'Bengkel Gita Raya', 'Ahmad', 'Bengkel servis umum untuk berbagai jenis motor.', 'Bogor, Jawa Barat', 'Otomotif', '-6.597147', '106.806038'),
('CSM', 'Casa Mia', 'Maria', 'Restoran Italia dengan chef asli dari Italia.', 'Ubud, Bali', 'Makanan', '-8.508230', '115.263100'),
('KIM', 'Kedai Ikan Mas', 'Siti', 'Spesialis masakan ikan air tawar lokal.', 'Depok, Jawa Barat', 'Makanan', '-6.402484', '106.794243'),
('PST', 'Pusat Oleh-Oleh Istana', 'Joko', 'Menjual berbagai oleh-oleh khas Yogyakarta.', 'Yogyakarta, DIY', 'Souvenir', '-7.797929', '110.371556'),
('SDN', 'Soto Ayam Ndelik', 'Rudi', 'Soto ayam bumbu kuning dengan resep tradisional.', 'Solo, Jawa Tengah', 'Makanan', '-7.4944009', '110.4632616'),
('TRB', 'Terang Bulan Master', 'Asep', 'Menjual martabak manis dengan berbagai topping.', 'Medan, Sumatera Utara', 'Makanan', '3.583033', '98.666816'),
('PKS', 'Pak Sastro Ayam Bakar', 'Sastro', 'Ayam bakar dengan bumbu rahasia keluarga.', 'Samarinda, Kalimantan Timur', 'Makanan', '-0.5003', '117.150149'),
('RSC', 'Rumah Sambal Citarasa', 'Dewi', 'Warung spesialis sambal dan masakan pedas.', 'Makassar, Sulawesi Selatan', 'Makanan', '-5.132293', '119.4240869');

CREATE TABLE store_review (
    id INT AUTO_INCREMENT PRIMARY KEY,
    store_code varchar(10) NOT NULL,
    name varchar(156) NOT NULL,
    description varchar(512),
    rating INT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (store_code) REFERENCES store(code)
);

INSERT INTO store_review (store_code, name, description, rating) VALUES
('WRT', 'Review Ratu 1', 'Makanannya sangat enak dan pelayanannya ramah.', 5),
('WRT', 'Review Ratu 2', 'Cocok untuk makan keluarga, tempatnya juga bersih.', 4),
('TKG', 'Review Kue 1', 'Kue-kuenya unik dan rasanya lezat.', 5),
('TKG', 'Review Kue 2', 'Variasi kue banyak, tapi harga sedikit mahal.', 3),
('BGR', 'Review Bengkel 1', 'Servis cepat dan harga bersahabat.', 5),
('BGR', 'Review Bengkel 2', 'Bagus servisnya, mekaniknya kompeten.', 4),
('CSM', 'Review Casa 1', 'Makanan Italia autentik, pizza dan pasta terbaik!', 5),
('CSM', 'Review Casa 2', 'Suasana restoran sangat nyaman dan mewah.', 5),
('KIM', 'Review Ikan 1', 'Ikan mas bakar dan ikan mas gorengnya juara!', 5),
('KIM', 'Review Ikan 2', 'Sambalnya pas, lokasinya juga mudah dijangkau.', 4),
('PST', 'Review Oleh-Oleh 1', 'Tempatnya strategis, produk lokal sangat kaya.', 4),
('PST', 'Review Oleh-Oleh 2', 'Pelayanan ramah dan oleh-olehnya autentik.', 5),
('SDN', 'Review Soto 1', 'Sotonya mantap, kuahnya gurih meresap.', 5),
('SDN', 'Review Soto 2', 'Porsinya besar, cocok untuk makan siang yang mengenyangkan.', 4),
('TRB', 'Review Terang Bulan 1', 'Martabak manisnya lembut dan tidak terlalu manis.', 4),
('TRB', 'Review Terang Bulan 2', 'Variasi topping banyak, tapi antreannya panjang.', 3),
('PKS', 'Review Ayam Bakar 1', 'Ayam bakarnya enak, bumbunya meresap sempurna.', 5),
('PKS', 'Review Ayam Bakar 2', 'Suasana nyaman, ayam bakarnya juga tidak terlalu berminyak.', 4),
('RSC', 'Review Sambal 1', 'Sambalnya pedas meresap, sangat cocok dengan nasi hangat.', 5),
('RSC', 'Review Sambal 2', 'Varian sambal banyak, makanannya juga halal.', 4);

CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name varchar(100) NOT NULL,
    username varchar(100) NOT NULL,
    password varchar(100) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);