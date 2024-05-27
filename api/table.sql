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

INSERT INTO store (code, name, owner_name, description, address, category) VALUES
('WRT', 'Warung Ratu', 'Ratna', 'Warung makan yang menyediakan masakan khas Sunda.', 'Bandung, Jawa Barat', 'Makanan'),
('TKG', 'Toko Kue Gembira', 'Budi', 'Menjual berbagai jenis kue tradisional dan modern.', 'Jakarta Selatan, DKI Jakarta', 'Kue'),
('BGR', 'Bengkel Gita Raya', 'Ahmad', 'Bengkel servis umum untuk berbagai jenis motor.', 'Bogor, Jawa Barat', 'Otomotif'),
('CSM', 'Casa Mia', 'Maria', 'Restoran Italia dengan chef asli dari Italia.', 'Ubud, Bali', 'Makanan'),
('KIM', 'Kedai Ikan Mas', 'Siti', 'Spesialis masakan ikan air tawar lokal.', 'Depok, Jawa Barat', 'Makanan'),
('PST', 'Pusat Oleh-Oleh Istana', 'Joko', 'Menjual berbagai oleh-oleh khas Yogyakarta.', 'Yogyakarta, DIY', 'Souvenir'),
('SDN', 'Soto Ayam Ndelik', 'Rudi', 'Soto ayam bumbu kuning dengan resep tradisional.', 'Solo, Jawa Tengah', 'Makanan'),
('TRB', 'Terang Bulan Master', 'Asep', 'Menjual martabak manis dengan berbagai topping.', 'Medan, Sumatera Utara', 'Makanan'),
('PKS', 'Pak Sastro Ayam Bakar', 'Sastro', 'Ayam bakar dengan bumbu rahasia keluarga.', 'Samarinda, Kalimantan Timur', 'Makanan'),
('RSC', 'Rumah Sambal Citarasa', 'Dewi', 'Warung spesialis sambal dan masakan pedas.', 'Makassar, Sulawesi Selatan', 'Makanan');

CREATE TABLE store_review (
    id INT AUTO_INCREMENT PRIMARY KEY,
    store_code varchar(5) NOT NULL,
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

