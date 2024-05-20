const express = require('express');
const cors = require('cors');
const app = express();
const port = 3000;


app.use(cors());

app.use(express.json());

// Static variable to store data
let stores = [
    // { id: 1, name: 'Baso Enak' },
    // { id: 2, name: 'Mie Kikil' }
];

// Create: Add a new store
app.post('/stores', (req, res) => {
    const newstore = {
        id: stores.length + 1,
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
app.get('/stores', (req, res) => {
    res.json(stores);
});

// Read: Get a single store by id
app.get('/stores/:id', (req, res) => {
    const store = stores.find(u => u.id === parseInt(req.params.id));
    if (!store) return res.status(404).send('store not found');
    res.json(store);
});

// Update: Update a store by id
app.put('/stores/:id', (req, res) => {
    const store = stores.find(u => u.id === parseInt(req.params.id));
    if (!store) return res.status(404).send('store not found');

    store.name = req.body.name;
    store.owner_name = req.body.owner_name;
    store.description = req.body.description;
    store.address = req.body.address;
    store.category = req.body.category;
    res.json(store);
});

// Delete: Delete a store by id
app.delete('/stores/:id', (req, res) => {
    const storeIndex = stores.findIndex(u => u.id === parseInt(req.params.id));
    if (storeIndex === -1) return res.status(404).send('store not found');

    const deletedstore = stores.splice(storeIndex, 1);
    res.json(deletedstore);
});

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
});
