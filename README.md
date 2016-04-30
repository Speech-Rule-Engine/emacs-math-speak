# emacs-math-speak
A bridge between [EmacsSpeak](https://github.com/tvraman/emacspeak) and the [Speech Rule Engine](https://github.com/zorkow/speech-rule-engine).


## Install

### Prerequisites

You need to have a recent installation of NodeJS and npm. Then do

    npm install speech-rule-engine
    npm install mathjax-node
    
## NVM Dances On Ubuntu 
  
The nvm installed by apt-get on Ubuntu (Node Version Manager) is out
of date.
here are the dances I had to do to get it updated and using the
right version.
  
  
  
    # .nvm holds git clone of 
    # https://github.com/creationix/nvm.git
    export NVM_DIR=/home/raman/.nvm
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" "--no-use"  
    nvm install stable 
    nvm use --delete-prefix stable --silent

