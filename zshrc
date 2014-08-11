# == Modularized configuration ==

# Source configuration modules.
for config_file (~/.zsh/*.zsh); do
  source $config_file
done

# == Appearance ==

# Colorize iTerm (on OS X).
if [[ "$OSTYPE" == darwin* ]]; then
  osascript ~/.scripts/iTermColorsCurrent.applescript >/dev/null
fi

# == Plugins ==

# Load virtualenvwrapper.
if (( $+commands[virtualenvwrapper.sh] )); then
  source $commands[virtualenvwrapper.sh]
fi

# Load scm_breeze plugin (exclude 'design' function)
if [[ "$TERM" != 'dumb' && $- =~ i ]]; then
  source "~/.util/scm_breeze/scm_breeze.sh"
  unset -f design
fi

# Load ZSH plugins (at the end of .zshrc). Order matters.
for plugin in \
  "zsh-syntax-highlighting" \
  "zsh-history-substring-search"
do
  source ~/.zsh/external/$plugin/$plugin.zsh
done
