// Initialize Math rendering system.
var mjx = require('mathjax-node');
var sre = require('speech-rule-engine');
sre.setupEngine({markup: 'acss'});

var runWithCounter = function(counter, callback, args) {  var result = callback.apply(sre, args);
  console.log('BEGINOUTPUT' + counter + ': ' + result + ' :ENDOUTPUT');
};
