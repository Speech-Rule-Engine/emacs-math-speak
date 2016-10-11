//Commentary:

// Expose a simple REPL as a server to emacspeak-maths.
// Usage Model:
// The Emacs  client sends requests of the form:
// command: args --- where args are comma-separated.
// The server responds with a Lisp S-expression.
// This S-expression is an annotated string.
// Annotations are ACSS property/value specifications.
// At any given time, the server loop is working with a given math expression,
// Emacs issues browse/render commands against that expression.

// Code:

// Load the TCP Library
var net = require('net');

// Initialize Math rendering system.
var mjx = require('mathjax-node');
var sre = require('speech-rule-engine');
sre.setupEngine({markup: 'acss'});

// Handle to connected Emacspeak-maths client:
var client;

// table of request handlers:

var handlers = {};

// Add the various handlers:

// Accept a LaTeX math expression:
handlers.enter = function(expr, socket) {
    mjx.typeset({math: expr,
                 format: 'TeX',
                 mml: true},
                function(data) {socket.write(sre.walk(data.mml)); }
               );
    // Find a better alternative to passing raw kbd values.
  // sre.move(9);
};

// Implement Handlers:
handlers.up = function () {};
handlers.down = function () {};
handlers.left = function () {};
handlers.right = function () {};
handlers.repeat = function () {};
handlers.root = function () {};

// Start a TCP Server on port 5000
net.createServer(function (socket) {
    // Identify this client
    socket.name = socket.remoteAddress + ":" + socket.remotePort;

    // Record this client:
    client = socket;

    // Method: respond
    function respond(message, sender) {
        // message is of the form:
        //cmd: args, args, args
        var request = message.toString();
        var cmd = request.split(':', 1)[0];
        var args = request.slice(cmd.length + 1);
        var handler = handlers[cmd];
        if (handler !== undefined) {
          var result = handler.call(null, args, socket);
            if (result !== undefined ) {
                sender.write(result);
            } else {
                process.stdout.write("Handler " + cmd + " returned undefined. \n");
            }
            } else {
                process.stdout.write("Handler for " + request[0] + " not defined.\n");
            }
    }

        // Announce yourself:
        socket.write("(Welcome " + socket.name + ")\n");
        // Handle incoming messages from Emacs:
        socket.on('data', function (data) {
            respond(data, socket);
        });

        // Shutdown server on disconnect:
        socket.on('end', function () {
            socket.destroy();
            process.exit();
        });

    }).listen(5000);

                 // Put a friendly message on the terminal of the server.
                 console.log("Math server running at port 5000\n");
