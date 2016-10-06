// Load the TCP Library
net = require('net');

// Initialize Math rendering system.
var mjx = require('mathjax-node');
var sre = require('speech-rule-engine');
sre.setupEngine({markup: 'acss'});


// Handle to connected Emacspeak-maths client:
var client;

// Start a TCP Server
net.createServer(function (socket) {
  // Identify this client
  socket.name = socket.remoteAddress + ":" + socket.remotePort 

  // Record this client:
  client = socket;

  // Send a nice welcome message and announce
  socket.write("Welcome " + socket.name + "\n");
  // Handle incoming messages from Emacs:.
  socket.on('data', function (data) {
    respond(data, socket);
  });

  // Shutdown server on disconnect:
  socket.on('end', function () {
    
    // server.shutdown();
  });
  
  // Send out response:
  function respond(message, sender ) {
    // ToDo: write handlers for the different actions:
    var result ="received: " +  message;
    sender.write(result);
    // Debug: Log it to the server output too
    // process.stdout.write(result);
  }

}).listen(5000);

// Put a friendly message on the terminal of the server.
console.log("Math server running at port 5000\n");
