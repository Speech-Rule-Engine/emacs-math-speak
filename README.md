# emacs-math-speak
A bridge between [EmacsSpeak](https://github.com/tvraman/emacspeak) and the [Speech Rule Engine](https://github.com/zorkow/speech-rule-engine).


## Install

### PrePrerequisites

Install the latest version of nvm and npm

    git clone https://github.com/creationix/nvm.git

Move somewhere where you like it. E.g., ~/.nvm

Put into your bashrc:

    export NVM_DIR="/home/sorge/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" "--no-use"  # This loads nvm

    nvm use --delete-prefix stable --silent

Then install the latest version of node (or iojs):

    nvm install stable



### Prerequisites

You need to have a recent installation of NodeJS and npm. Then do

    npm install speech-rule-engine
    npm install mathjax-node
    
You need the js-comint package installed for Emacs and loaded, as well as the
inferior-js program set. For example put into your .emacs file:

    (require 'js-comint)
    (setq inferior-js-program-command "node")

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

## API

| Emacs function | Effect |
| ---- | ---- |
| `ems-start` | Starts the bridge to NodeJS process |
| `ems-stop` | Stops the bridge to NodeJS process |
| `ems-enter` | Given a string with a LaTeX expression, returns intial speech string and starts walking the expression. Make sure to sufficiently escape backslashes, that is per single backslash we need four backslashes. Does currently not work properly as the initialisation is asynchronous. |
| `ems-up` | Walk current expression up. Return speech string or null. |
| `ems-down` | Walk current expression down. Return speech string or null. |
| `ems-left` | Walk current expression left. Return speech string or null. |
| `ems-right` | Walk current expression right. Return speech string or null. |
| `ems-repeat` | Repeat the last speech string. |
| `ems-depth` | Return speech string with current level. |
