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

# Disable prompt in terminal
VIRTUAL_ENV_DISABLE_PROMPT=1

### Tmux ###

# Add tmuxifier to PATH
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"
eval "$($HOME/.external/tmuxifier/bin/tmuxifier init -)"
