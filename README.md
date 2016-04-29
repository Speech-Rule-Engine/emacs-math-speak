# emacs-math-speak
A bridge between [EmacsSpeak](https://github.com/tvraman/emacspeak) and the [Speech Rule Engine](https://github.com/zorkow/speech-rule-engine).


## Install

### Prerequisites

You need to have a recent installation of NodeJS and npm. Then do

    npm install speech-rule-engine
    npm install mathjax-node
    
You need the js-comint package installed for Emacs and loaded, as well as the
inferior-js program set. For example put into your .emacs file:

    (require 'js-comint)
    (setq inferior-js-program-command "node")

  
