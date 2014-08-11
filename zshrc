# == Pre-load user customizations ==
if [ -d ~/.zsh.before/ ]; then
  for config_file (~/.zsh.before/*.zsh); do
    source $config_file
  done
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

# virtualenvwrapper
if (( $+commands[virtualenvwrapper.sh] )); then
  export WORKON_HOME="$HOME/.virtualenvs"
  export PROJECT_HOME="$HOME/workspace"
  VIRTUAL_ENV_DISABLE_PROMPT=1
  source $commands[virtualenvwrapper.sh]
fi

# fasd
eval "$(fasd --init auto)"

# zsh-users plugins (order of loading matters)
for plugin in \
  "zsh-syntax-highlighting" \
  "zsh-history-substring-search"
do
  source ~/.zsh/external/$plugin/$plugin.zsh
done

# == Post-load user customizations ==
if [ -d ~/.zsh.after/ ]; then
  for config_file (~/.zsh.after/*.zsh); do
    source $config_file
  done
fi
