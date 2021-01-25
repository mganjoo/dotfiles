# Since OS X adds to PATH using path_helper from /etc/zprofile, we
# add additional paths here, so that these paths get precedence.
if [ -e ~/.zprofile_local ]; then
  source ~/.zprofile_local
fi

# Add ~/bin and /usr/local/sbin to PATH
if [ -d $HOME/bin/ ]; then
  path=(
    $HOME/bin
    /usr/local/sbin
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

# Always have pipenv virtualenvs be within the project directory
export PIPENV_VENV_IN_PROJECT=1

### Tmux ###

# Add tmuxifier to PATH
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"
eval "$($HOME/.external/tmuxifier/bin/tmuxifier init -)"

### Pyenv ###
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

### Google Cloud SDK ###

# Google Cloud currently requires Python 3.8
if [[ -d "$(brew --prefix)/opt/python@3.8" ]]; then
  export CLOUDSDK_PYTHON="$(brew --prefix)/opt/python@3.8/libexec/bin/python"
fi

# Add completion and path information for Google Cloud SDK
if [[ -d "$(brew --prefix)/Caskroom/google-cloud-sdk/latest" ]]; then
  source "$(brew --prefix)/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"
  source "$(brew --prefix)/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc"
fi
