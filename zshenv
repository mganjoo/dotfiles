# == Path variables ==

if [ -x /usr/libexec/path_helper ]; then
  eval `/usr/libexec/path_helper -s`
fi

path=(
  $HOME/bin
  $HOME/.bin
  /usr/local/bin
  /usr/{bin,sbin}
  /{bin,sbin}
  $path
)

manpath=(
  $HOME/.man
  /usr/local/share/man
  /usr/share/man
  $manpath
)

# Load any prior path environment customizations.
if [ -f "$HOME/.zshenv.before" ]; then
  source $HOME/.zshenv.before
fi

# Ensure unique values in path and manpath.
typeset -gU path manpath
