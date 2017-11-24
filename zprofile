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

# Add anaconda or virtualenv (must override homebrew/system python) {{{2
if [[ -d "$(brew --prefix)/anaconda3/bin" ]]; then
  # anaconda
  export PATH="$(brew --prefix)/anaconda3/bin:$PATH"
elif (( $+commands[virtualenvwrapper.sh] )); then
  # virtualenvwrapper
  export WORKON_HOME="$HOME/.virtualenvs"
  export PROJECT_HOME="$HOME/workspace"
  VIRTUAL_ENV_DISABLE_PROMPT=1
  export VIRTUALENVWRAPPER_PYTHON=python2
  source $commands[virtualenvwrapper.sh]
fi

# Add tmuxifier to PATH
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"
eval "$($HOME/.external/tmuxifier/bin/tmuxifier init -)"
