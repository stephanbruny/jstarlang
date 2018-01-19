// Import http module 
let http = import 'http';

// A simple function to send a text-response
let sendText = fun (res) => fun (text) {
    send res (200, { 'Content-Type': 'text/html', 'Content-Length': text.length }, text);
};

// Create a listener process
let listener = receive (req, res) {
    let response = sendText(res);
    match req.get.path as route with
    | '/': 
        response('Hello, World');
    | http.route('/:name'): 
        response(`Hello, ${http.query(route, 'name')}`);
    | _: 
        send res (404, { 'Content-Type': 'text/html' }, 'I don\'t know this route');
}

// Initialize an http-server with config-table and listener-process
let server = http.server({ port: 8080 }, listener);