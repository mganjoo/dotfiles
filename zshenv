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
  /usr/local/share/man
  /usr/share/man
  /opt/X11/share/man
  $manpath
)

# Note: additional paths from path_helper (on OS X) are already in path.

# Ensure unique values in path and manpath.
typeset -gU path manpath
