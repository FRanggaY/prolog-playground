const express = require("express");
const cors = require("cors");
const morgan = require("morgan");
const app = express();
const port = 3000;

app.use(cors());

app.use(express.json());

app.use(morgan("dev"));

// Static variable to store data
let stores = [
	{
		code: '1',
		name: "Baso Enak",
		owner_name: "yulia",
		description: "Enak ini",
		address: "jalan test",
		category: "Rumah Makan",
	},
	{
		code: '2',
		name: "Mie Ayam",
		owner_name: "budi gas",
		description: "Enak ini",
		address: "jalan test new",
		category: "Rumah Makan",
	},
];

let storeReviews = [
	{ code: '1', store_code: '1', name: "andi", description: "gk enak", rating: 1 },
	{ code: '2', store_code: '1', name: "budi", description: "gk enak", rating: 3 },
];

const calculateAverageRating = (storeCode) => {
	const reviews = storeReviews.filter(review => review.store_code === storeCode);
	const totalReviews = reviews.length;
	const totalRating = reviews.reduce((acc, review) => acc + review.rating, 0);
	return totalReviews ? (totalRating / totalReviews) : 0;
};

app.get("/store-recommendation", (req, res) => {
	const { address, category } = req.query;

	let filteredStores = stores.filter(store => {
		return (!address || store.address.toLowerCase().includes(address.toLowerCase())) &&
			(!category || store.category.toLowerCase().includes(category.toLowerCase()));
	});

	let storeRatings = filteredStores.map(store => {
		return {
			...store,
			average_rating: calculateAverageRating(store.code),
			total_reviews: storeReviews.filter(review => review.store_code === store.code).length
		};
	});

	// Sort stores by average rating in descending order and get the top 5
	storeRatings.sort((a, b) => b.average_rating - a.average_rating);
	let topStores = storeRatings.slice(0, 5);

	res.json(topStores);
});

// Create: Add a new store
app.post("/stores", (req, res) => {
	const newstore = {
		code: req.body.code,
		name: req.body.name,
		owner_name: req.body.owner_name,
		description: req.body.description,
		address: req.body.address,
		category: req.body.category,
	};

	stores.push(newstore);
	res.status(201).json(newstore);
});

// Read: Get all stores
app.get("/stores", (req, res) => {
	res.json(stores);
});

// Read: Get a single store by code
app.get("/stores/:code", (req, res) => {
	const store = stores.find((u) => u.code === String(req.params.code));
	if (!store) return res.status(404).send("store not found");
	res.json(store);
});

// Create: Add a new store reviews
app.post("/store-reviews", (req, res) => {
	const newstoreReview = {
		code: storeReviews.length + 1,
		store_code: String(req.body.store_code),
		name: req.body.name,
		description: req.body.description,
		rating: req.body.rating,
	};
	storeReviews.push(newstoreReview);
	res.status(201).json(newstoreReview);
});

// Read: Get a store reviews by store code
app.get("/store-reviews/:code", (req, res) => {
	const store = storeReviews.filter((u) => u.store_code == String(req.params.code));
	if (!store) return res.status(404).send("store not found");
	res.json(store);
});

// Update: Update a store by code
app.put("/stores/:code", (req, res) => {
	const store = stores.find((u) => u.code === String(req.params.code));
	if (!store) return res.status(404).send("store not found");

	store.name = req.body.name;
	store.owner_name = req.body.owner_name;
	store.description = req.body.description;
	store.address = req.body.address;
	store.category = req.body.category;
	res.json(store);
});

// Delete: Delete a store by code
app.delete("/stores/:code", (req, res) => {
	const storeIndex = stores.findIndex((u) => u.code === String(req.params.code));
	if (storeIndex === -1) return res.status(404).send("store not found");

	const deletedstore = stores.splice(storeIndex, 1);
	res.json(deletedstore);
});

app.listen(port, () => {
	console.log(`Server running at http://localhost:${port}`);
});
