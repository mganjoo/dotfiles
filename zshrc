# == Modularized configuration ==

# Source large configuration modules.
for config_file (~/.zsh/*.zsh); do
  source $config_file
done

# == Appearance ==

# Colorize iTerm (on OS X).
if [[ "$OSTYPE" == darwin* ]]; then
  osascript ~/.scripts/iTermColorsCurrent.applescript >/dev/null
fi

# Prompt.
fpath=( ~/.zsh/prompt $fpath )
autoload -Uz promptinit
promptinit
prompt 'mganjoo'

# == Plugins ==

# virtualenvwrapper
if (( $+commands[virtualenvwrapper.sh] )); then
  source $commands[virtualenvwrapper.sh]
fi

# scm_breeze plugin (exclude 'design' function)
if [[ "$TERM" != 'dumb' && $- =~ i ]]; then
  source "$HOME/.util/scm_breeze/scm_breeze.sh"
  unset -f design
fi

# zsh-users plugins (order of loading matters)
for plugin in \
  "zsh-syntax-highlighting" \
  "zsh-history-substring-search"
do
  source ~/.zsh/external/$plugin/$plugin.zsh
done
