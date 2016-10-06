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
//Warning: this errors out.

handlers.enter = function (expr) {
  mjx.typeset({math: expr,
               format: 'TeX',
               mml:true},
              function(data) {sre.walk(data.mml)}
             );
  // Find a better alternative to passing raw kbd values.
  sre.move(9);
  return "done";
};

// Implement Handlers:
handlers.up = function () {};
handlers.down = function () {};
handlers.left = function () {};
handlers.right = function () {};
handlers.repeat = function () {};

// Start a TCP Server
net.createServer(function (socket) {
  // Identify this client
  socket.name = socket.remoteAddress + ":" + socket.remotePort 

  // Record this client:
  client = socket;

  // Announce yourself:
  socket.write("Welcome " + socket.name + "\n");
  // Handle incoming messages from Emacs::
  socket.on('data', function (data) {
    respond(data, socket);
  });

  // Shutdown server on disconnect:
  socket.on('end', function () {
    // server.shutdown();
  });
  
  // Send out response:
  function respond(message, sender ) {
    process.stdout.write("type: " + typeof(message.value) +"\n");
    // handle requests that have single argument for now:
    var request = message.split(':');
    var handler =handlers[request[0]];
    if (handler != undefined) {
    var result = handler.apply(request[1]);
    sender.write(result);
    // Debug: Log it to the server output too
    process.stdout.write(result);
      } else {
        process.stdout.write("Handler " + handler +"not defined. ");
        }
  }

}).listen(5000);

// Put a friendly message on the terminal of the server.
console.log("Math server running at port 5000\n");
