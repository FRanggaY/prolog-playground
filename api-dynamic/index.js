// index.js
require('dotenv').config();
const express = require("express");
const cors = require("cors");
const morgan = require("morgan");
const controller = require("./controller");
const middleware = require("./middleware/verify");

const app = express();
const port = 3000;

app.use(cors());
app.use(express.json());
app.use(morgan("dev"));

// Store Routes
app.get("/ping", (req, res) => {
	res.send("PONG!");
});

// Auth
app.post("/register", controller.register);
app.post("/login", controller.login);

app.get("/profile", middleware.validToken, controller.getProfile);

app.get("/stores", controller.getAllStores);
app.get("/stores/:code", controller.getStoresByCode);
app.post("/stores", middleware.validToken ,controller.createStore);
app.put("/stores/:code", middleware.validToken, controller.updateStore);
app.delete("/stores/:code", middleware.validToken, controller.deleteStore);

// Store Review Routes
app.get("/store-reviews/:store_code", controller.getStoreReviews);
app.post("/store-reviews", controller.createStoreReview);

// Store Recommendation
app.get("/store-recommendation", controller.getStoreRecommendation);

app.listen(port, () => {
	console.log(`Server running at http://localhost:${port}`);
});
