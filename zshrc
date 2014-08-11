# == User customizations ==
if [ -d $HOME/.zsh.before/ ]; then
  if [ "$(ls -A $HOME/.zsh.before/)" ]; then
    for config_file ($HOME/.zsh.before/*.zsh) source $config_file
  fi
fi

# == Modularized configuration ==

# Source large configuration modules.
for config_file (~/.zsh/*.zsh); do
  source $config_file
done

# Load file with sensitive information that shouldn't be checked in
if [ -e ~/.secrets ]; then
  source ~/.secrets
fi

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

# scm_breeze plugin (exclude 'design' function)
if [[ "$TERM" != 'dumb' && $- =~ i ]]; then
  source "$HOME/.util/scm_breeze/scm_breeze.sh"
  unset -f design
fi

# virtualenvwrapper
if (( $+commands[virtualenvwrapper_lazy.sh] )); then
  export WORKON_HOME="$HOME/.virtualenvs"
  VIRTUAL_ENV_DISABLE_PROMPT=1
  source $commands[virtualenvwrapper_lazy.sh]
fi

# zsh-users plugins (order of loading matters)
for plugin in \
  "zsh-syntax-highlighting" \
  "zsh-history-substring-search"
do
  source ~/.zsh/external/$plugin/$plugin.zsh
done

# == User customizations ==
if [ -d $HOME/.zsh.after/ ]; then
  if [ "$(ls -A $HOME/.zsh.after/)" ]; then
    for config_file ($HOME/.zsh.after/*.zsh) source $config_file
  fi
fi
