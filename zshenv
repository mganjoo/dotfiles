# == Path variables ==

export MANPATH=""

if [ -x /usr/libexec/path_helper ]; then
  eval `/usr/libexec/path_helper -s`
fi

# Load any prior path environment customizations.
if [ -f "$HOME/.zshenv.before" ]; then
  source $HOME/.zshenv.before
fi

path=(
  $HOME/bin
  $HOME/.bin
  $path
)

manpath=(
  $HOME/.man
  $manpath
)

# Ensure unique values in path and manpath.
typeset -gU path manpath
