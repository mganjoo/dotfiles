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
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
fi

# Add option to load miniconda if it exists
if [[ -d "$(brew --prefix)/miniconda3/bin" ]]; then
  # Use new method at https://github.com/conda/conda/blob/master/CHANGELOG.md#440-2017-12-20
  . $(brew --prefix)/miniconda3/etc/profile.d/conda.sh
fi

# Add support for virtualenvwrapper
if (( $+commands[virtualenvwrapper.sh] )); then
  export WORKON_HOME="$HOME/.virtualenvs"
  export PROJECT_HOME="$HOME/workspace"
  VIRTUAL_ENV_DISABLE_PROMPT=1
  source $commands[virtualenvwrapper.sh]
fi

# Add tmuxifier to PATH
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"
eval "$($HOME/.external/tmuxifier/bin/tmuxifier init -)"
