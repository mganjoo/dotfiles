# Since OS X adds to PATH using path_helper from /etc/zprofile, we
# add additional paths here, so that these paths get precedence.
if [ -e ~/.zprofile_local ]; then
  source ~/.zprofile_local
fi

# Add ~/bin to PATH
if [ -d $HOME/bin/ ]; then
  path=(
    $HOME/bin
    $path
  )
fi

### Python ###

# Add homebrew Python to PATH if exists
if [[ -d "$(brew --prefix)/opt/python" ]]; then
  export PATH="$(brew --prefix)/opt/python/libexec/bin:$PATH"
fi

### Pipx ###
if [[ -d "$HOME/.local/bin" ]]; then
  export PATH="$PATH:$HOME/.local/bin"
fi

# Disable prompt in terminal (rely on our own terminal prompt)
export VIRTUAL_ENV_DISABLE_PROMPT=1

# Always have pip virtualenvs be within the project directory
export PIPENV_VENV_IN_PROJECT=1 

### Tmux ###

# Add tmuxifier to PATH
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"
eval "$($HOME/.external/tmuxifier/bin/tmuxifier init -)"

### Poetry ###
export PATH="$HOME/.poetry/bin:$PATH"
