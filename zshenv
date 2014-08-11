# == Path variables ==

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
  /opt/X11/share/man
  $manpath
)

# Load any prior path environment customizations.
if [ -f "$HOME/.zshenv.before" ]; then
  source $HOME/.zshenv.before
fi

# Note: additional paths from path_helper (on OS X) are already in path.

# Ensure unique values in path and manpath.
typeset -gU path manpath
