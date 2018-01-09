# Path to your oh-my-zsh configuration.
ZSH=$HOME/dotfiles/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_CUSTOM=$HOME/dotfiles/.my-zsh
ZSH_THEME="hironemu"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git git-flow)

source $ZSH/oh-my-zsh.sh

if [ -x '/usr/libexec/path_helper' ]; then
  eval $(/usr/libexec/path_helper -s)
fi

# Customize to your needs...

# if [[ -s $HOME/.rvm/scripts/rvm ]] ; then source $HOME/.rvm/scripts/rvm ; rvm use default ; fi
if [[ -x `which rbenv` ]]; then eval "$(rbenv init -)"; fi

# for Java
if [ -f "/System/Library/Frameworks/JavaVM.framework/Versions/A/Commands/java_home" ]; then
  # "1.8" for Java8
  # "9"   for Java9
  export JAVA_HOME=`/System/Library/Frameworks/JavaVM.framework/Versions/A/Commands/java_home -v "1.8"`
  PATH=${JAVA_HOME}/bin:${PATH}
fi

# EC2
if [ -d ~/.ec2 ]; then
  export JAVA_HOME="$(/usr/libexec/java_home)"
  export EC2_PRIVATE_KEY="$(/bin/ls "$HOME"/.ec2/pk-*.pem | /usr/bin/head -1)"
  export EC2_CERT="$(/bin/ls "$HOME"/.ec2/cert-*.pem | /usr/bin/head -1)"
  export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
fi

# No correct
alias ag='nocorrect ag'

# for go lang
if [ -x "`which go`" ]; then
  export GOROOT=`go env GOROOT`
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
fi

# for Direnv
export EDITOR=vim
if [ -x "`which direnv`" ]; then
  eval "$(direnv hook zsh)"
fi

# user commands
if [ -d ~/bin ]; then
  export PATH=$PATH:~/bin
fi

# for Emacs
if [ -d ~/.emacs.d/bin ]; then
  export PATH=$PATH:~/.emacs.d/bin
fi

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

export PATH=$HOME/Library/Android/sdk/tools:$HOME/Library/Android/sdk/platform-tools:$PATH
# for Node.js
if [ -d "$HOME/.nodebrew/current/bin" ]; then
  export PATH=$HOME/.nodebrew/current/bin:$PATH
fi
# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/dev/google-cloud-sdk/path.zsh.inc" ]; then source $HOME/dev/google-cloud-sdk/path.zsh.inc; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/dev/google-cloud-sdk/completion.zsh.inc" ]; then source $HOME/dev/google-cloud-sdk/completion.zsh.inc; fi


# for Azure CLI
if [ -f "$HOME/lib/azure-cli/az.completion" ]; then
  source $HOME/lib/azure-cli/az.completion
fi

# for python
if [ -d "$HOME/.pyenv" ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
fi
if [ -x "`which pyenv`" ]; then
  eval "$(pyenv init -)"
fi

# for postgresql
if [ -d '/usr/local/opt/postgresql@9.5/bin' ]; then
  export PATH="/usr/local/opt/postgresql@9.5/bin:$PATH"
fi

if [ -d '/usr/local/Cellar/openssl/1.0.2l/bin' ]; then
  export PATH="$PATH:/usr/local/Cellar/openssl/1.0.2l/bin"
fi

# for local path
if [ -d "$HOME/bin" ]; then
  export PATH=$PATH:$HOME/bin
fi

