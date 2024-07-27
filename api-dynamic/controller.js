// controller.js
const pool = require("./database");
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const uuid = require('uuid').v4;

function queryDatabase(sql, params) {
	return new Promise((resolve, reject) => {
		pool.query(sql, params, (err, results) => {
			if (err) return reject(err);
			resolve(results);
		});
	});
}
// CRUD STORE BEGIN
exports.createStore = async (req, res) => {
	const {name, owner_name, description, address, category, lattidue, longitude } = req.body;
	const code = uuid();
	const sql = `INSERT INTO store (code, name, owner_name, description, address, category, lattidue, longitude) VALUES (?, ?, ?, ?, ?, ?, ?, ?)`;
	try {
		const checkStoreCode = await queryDatabase("SELECT * FROM store WHERE code = ?", [code]);
		if (checkStoreCode.length === 0) {
			await queryDatabase(sql, [code, name, owner_name, description, address, category, lattidue, longitude]);
			return this.getAllStores(req, res);
		} else {
			await queryDatabase(sql, [`${code}${checkStoreCode.length + 1}`, name, owner_name, description, address, category]);
			return this.getAllStores(req, res);
		}
	} catch (error) {
		res.status(500).json({ message: "Error creating store", error: error.message });
	}
};

exports.getAllStores = async (req, res) => {
	const sql = `SELECT * FROM store`;
	try {
		const results = await queryDatabase(sql);
		res.json(results);
	} catch (error) {
		res.status(500).json({ message: "Error fetching stores", error: error.message });
	}
};

exports.getStoresByCode = async (req, res) => {
	const { code } = req.params;
	const sql = `SELECT * FROM store where code = ?`;
	try {
		const results = await queryDatabase(sql, [code]);
		if (results.length === 0) {
			res.status(500).send("store not found");
		} else {
			res.json(results[0]);
		}
	} catch (error) {
		res.status(500).json({ message: "Error fetching stores", error: error.message });
	}
};

exports.updateStore = async (req, res) => {
	const { name, owner_name, description, address, category, lattidue, longitude, code } = req.body;
	const sql = `UPDATE store 
	set name = ?,
	owner_name = ?,
	description = ?,
	address = ?,
	category = ?,
	lattidue = ?,
	longitude = ?
	where code = ?`;
	try {
		await queryDatabase(sql, [name, owner_name, description, address, category, lattidue, longitude, code]);
		return this.getAllStores(req, res);
	} catch (error) {
		res.status(500).json({ message: "Error fetching stores", error: error.message });
	}
};

exports.deleteStore = async (req, res) => {
	const { code } = req.params;
	const sqlDeleteStoreReview = `DELETE FROM store_review WHERE store_code = ?`;
	const sqlDeleteStore = `DELETE FROM store WHERE code = ?`;

	const connection = await pool.promise().getConnection();

	try {
		await connection.beginTransaction();

		await queryDatabase(sqlDeleteStoreReview, [code], connection);
		await queryDatabase(sqlDeleteStore, [code], connection);

		await connection.commit();
		connection.release();

		return this.getAllStores(req, res);
	} catch (error) {
		await connection.rollback();
		connection.release();
		console.log(error);
		res.status(500).json({ message: "Error deleting store", error: error.message });
	}
};

// CRUD STORE END

exports.createStoreReview = async (req, res) => {
	const { store_code, name, description, rating } = req.body;
	const sql = `INSERT INTO store_review (store_code, name, description, rating) VALUES (?, ?, ?, ?)`;
	try {
		await queryDatabase(sql, [store_code, name, description, rating]);
		return this.getStoreReviews(req, res);
	} catch (error) {
		res.status(500).json({ message: "Error adding review", error: error.message });
	}
};

exports.getStoreReviews = async (req, res) => {
	const { store_code } = req.params;
	const sql = `SELECT * FROM store_review WHERE store_code = ?`;
	try {
		const results = await queryDatabase(sql, [store_code]);
		res.json(results);
	} catch (error) {
		res.status(500).json({ message: "Error fetching reviews", error: error.message });
	}
};

exports.getStoreRecommendation = async (req, res) => {
	const { address, category } = req.query;
	// Recomendation lebih dari rating 4
	const sql = `SELECT 
			ROUND(AVG(sr.rating), 1) AS average_rating,
			COUNT(sr.id) AS total_reviews,
			s.*
		FROM 
			store s
		INNER JOIN 
			store_review sr ON sr.store_code = s.code
		WHERE 
			UPPER(s.address) LIKE ? AND UPPER(s.category) LIKE ?
		GROUP BY 
			s.code
		HAVING 
			ROUND(AVG(sr.rating), 1) >= 4
		ORDER BY 
			average_rating DESC,
			s.name ASC
		LIMIT 10;
		`;
	try {
		const results = await queryDatabase(sql, [`%${address.toUpperCase()}%`, `%${category.toUpperCase()}%`]);
		res.json(results);
	} catch (error) {
		res.status(500).json({ message: "Error fetching reviews", error: error.message });
	}
};

// START REGISTER

exports.register = async (req, res) => {
	const body = req.body;
	const saltArround = 10;
	
	// Validation field
	if (!body.username || body.username === "" || body.username === undefined) return res.status(400).json({message: "username is required", code : 400, status: false});
	if (!body.password || body.password === "" || body.password === undefined) return res.status(400).json({message: "password is required", code : 400, status: false});

	try {
		// Check existing
		const queryExist = `SELECT * FROM users WHERE username LIKE ?`;
		const checkExist = await queryDatabase(queryExist, [`%${body.username}%`]);
		if (checkExist.length > 0) return res.status(400).json({message : "username already used", code: 400, status: false});

		let salt = await bcrypt.genSaltSync(saltArround);
		const newPassword = bcrypt.hashSync(body.password, salt);

		const queryInsert = `INSERT INTO users (name, username, password) VALUE (?, ? ,?)`;
		await queryDatabase(queryInsert, [body.name, body.username, newPassword]);

		return res.status(200).json({message : "Success register", code: 200, status: true});

	} catch (error) {
		return res.status(500).json({ message: "Error fetching reviews", error: error.message });
	}
}

exports.login = async (req, res) => {
	const body = req.body;
	try {
		// Check existing
		const queryExist = `SELECT * FROM users WHERE username LIKE ?`;
		const checkExist = await queryDatabase(queryExist, [`%${body.username}%`]);
		if (checkExist.length < 1) return res.status(404).json({message : "User not found", code: 404, status: false});

		// Validation Password
		const hashedPass = checkExist[0].password;
		const inputPassword = body.password;

		let compire = await bcrypt.compareSync(inputPassword, hashedPass);

		if (!compire) return res.status(400).json({message : "Password Not Match", code: 400, status: false});
		const token = await jwt.sign(body, process.env.JWT_SECRET_KEY, {expiresIn : '1h'});

		return res.status(200).json({message : "Success login", data : {token} ,code: 200, status: true}); 
	
	} catch (error) {
		return res.status(500).json({ message: "Error fetching reviews", error: error.message });
	}
}

exports.getProfile = async (req, res) => {
	const body = req.body;
	try {
		const queryExist = `SELECT id, name, username FROM users WHERE username LIKE ?`;
		const checkExist = await queryDatabase(queryExist, [`%${body.user_token}%`]);
		if (checkExist.length < 1) return res.status(404).json({message : "user not found", code: 404, status: false});

		return res.json(checkExist);
	} catch (error) {
		return res.status(500).json({ message: "Error fetching reviews", error: error.message });
	}
}