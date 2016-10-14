// Copyright 2016 T. V. Raman, Volker Sorge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//Commentary:


/**
 * @fileoverview Server for connecting Emacs to the Speech-Rule-Engine.
 * @author tv.raman.tv@gmail.com (T. V. Raman)
 *
 *  Expose a simple REPL as a server to emacspeak-maths.
 *
 *  Usage Model:
 *
 *  The Emacs  client sends requests of the form:
 *  command: arg.
 *  The server responds with a Lisp S-expression.
 *  This S-expression is an annotated string.
 *  Annotations are ACSS property/value specifications.
 *  At any given time, the server loop is working with a given math expression,
 *  Emacs issues browse/render commands against that expression.
 */

// Code:

// Load the TCP Library
var net = require('net');

// Initialize Math rendering system.
var mjx = require('mathjax-node');
var sre = require('speech-rule-engine');
sre.setupEngine({markup: 'acss'});

// table of request handlers:

var handlers = {};

// An error output function for MathJax errors.

var errorOutput = function(errors, socket) {
  socket.write(
    errors
      .map(function(x) {return '(error "' + x.replace(/\\/g, '\\\\') + '")';})
      .join(' ')
  );
};

// Add the various handlers:

// Accept a LaTeX math expression:
handlers.enter = function(expr, socket) {
  mjx.config({
    displayErrors: false,
  });
  mjx.typeset({math: expr,
               format: 'TeX',
               mml: true},
              function(data) {
                (data.errors && data.errors.length) ?
                  errorOutput(data.errors, socket) :
                  socket.write(sre.walk(data.mml)); }
             );
};

// Implement Handlers:
handlers.up = function() {return sre.move('UP');};
handlers.down = function() {return sre.move('DOWN');};
handlers.left = function() {return sre.move('LEFT');};
handlers.right = function() {return sre.move('RIGHT');};
handlers.repeat = function() {return sre.move('REPEAT');};
handlers.root = function() {return sre.move('ROOT');};

// Start a TCP Server on port 5000
net.createServer(function(socket) {
  // Identify this client
  socket.name = socket.remoteAddress + ':' + socket.remotePort;

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
      if (result !== undefined) {
        sender.write(result);
      } else {
        process.stdout.write('Handler ' + cmd + ' returned undefined. \n');
      }
    } else {
      process.stdout.write('Handler for ' + request[0] + ' not defined.\n');
    }
  }

  // Announce yourself:
  socket.write('(welcome  \"Maths Speech  Server! \" ');
  // Handle incoming messages from Emacs:
  socket.on('data', function(data) {
    respond(data, socket);
  });

  // Shutdown server on disconnect:
  socket.on('end', function() {
    socket.destroy();
    process.exit();
  });

}).listen(5000);

// Put a friendly message on the terminal of the server.
console.log('Math server running at port 5000\n');
