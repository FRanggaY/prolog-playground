const express = require('express');
const app = express();
const port = 3000;

app.use(express.json());

// Static variable to store data
let stores = [
    { id: 1, name: 'Baso Enak' },
    { id: 2, name: 'Mie Kikil' }
];

// Create: Add a new store
app.post('/stores', (req, res) => {
    const newstore = {
        id: stores.length + 1,
        name: req.body.name
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
