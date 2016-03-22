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
  $path
)

# Additional applications

# ghc
export GHC_DOT_APP="/Applications/ghc-7.10.2.app"
if [ -d "$GHC_DOT_APP" ]; then
  export PATH="${HOME}/.local/bin:${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

# Ensure unique values in path and manpath.
typeset -gU path
