const jwt = require('jsonwebtoken');

exports.validToken = async (req, res, next) => {
    const headers = req.headers;
    try {
        if (!headers.authorization || headers.authorization === undefined || headers.authorization === "") return res.status(400).json({message: "Invalid token", code : 400, status: false});

        const token = headers.authorization.split(" ")[1];

        jwt.verify(token, process.env.JWT_SECRET_KEY, (err, decode) => {
            if (err) return res.status(400).json({message: "Token Is Expired", code : 400, status: false});

            req.body.user_token = decode.username;

            next();
        })

    } catch (error) {
		return res.status(500).json({ message: "Error fetching reviews", error: error.message }); 
    }
}