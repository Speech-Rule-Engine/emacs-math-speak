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
export NODE_DISABLE_COLORS=1
export NODE_NO_READLINE=1

Then install the latest version of node 

    nvm install stable



### Prerequisites

You need to have a recent installation of NodeJS and npm. Then do

    npm install speech-rule-engine
    npm install mathjax-node
    


## Emacs Interaction 

### Test server/client 

From a shell, run
    node math-server.js
    
    Then from a different shell, run a client 
        nc localhost 5000
        
        In this client connection, type 
            
                enter: a+b
                
                If everything works, you'll see an S-expression come
                back.
                

### Next Run it Through Emacs

     M-x load-file 
         emacspeak-maths.el
         
         
| Emacs function | Effect |
| ---- | ---- |
| `emacspeak-start` | Starts the bridge to NodeJS process |
| `emacspeak-shutdown` | Stops the bridge to NodeJS process |
| `emacspeak-enter` | Given a string with a LaTeX expression, returns initial speech s-expression and starts walking the expression. |
| `ems-up` | Walk current expression up. Return speech s-expression or null. |
| `ems-down` | Walk current expression down. Return speech s-expression or null. |
| `ems-left` | Walk current expression left. Return speech s-expression or null. |
| `ems-right` | Walk current expression right. Return speech s-expression or null. |


