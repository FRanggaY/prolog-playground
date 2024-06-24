// controller.js
const pool = require("./database");

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
	const { code, name, owner_name, description, address, category } = req.body;
	const sql = `INSERT INTO store (code, name, owner_name, description, address, category) VALUES (?, ?, ?, ?, ?, ?)`;
	try {
		const checkStoreCode = await queryDatabase("SELECT * FROM store WHERE code = ?", [code]);
		if (checkStoreCode.length === 0) {
			await queryDatabase(sql, [code, name, owner_name, description, address, category]);
			return this.getAllStores(req, res);
		} else {
			res.status(500).json({ message: "Duplicate Code!", error: true });
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
	const { name, owner_name, description, address, category, code } = req.body;
	const sql = `UPDATE store 
	set name = ?,
	owner_name = ?,
	description = ?,
	address = ?,
	category = ?
	where code = ?`;
	try {
		await queryDatabase(sql, [name, owner_name, description, address, category, code]);
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
	const sql = `SELECT 
			ROUND(AVG(sr.rating), 1) AS average_rating,
			COUNT(sr.id) AS total_reviews,
			s.*
		FROM 
			store s
		INNER JOIN 
			store_review sr ON sr.store_code = s.code
		WHERE UPPER(address) LIKE ?
		GROUP BY 
			s.code
		ORDER BY 
			average_rating DESC
		LIMIT 10`;
	try {
		const results = await queryDatabase(sql, [`%${address.toUpperCase()}%`]);
		res.json(results);
	} catch (error) {
		res.status(500).json({ message: "Error fetching reviews", error: error.message });
	}
};
