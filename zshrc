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

# Prompt.
fpath=( ~/.zsh/prompt $fpath )
autoload -Uz promptinit
promptinit
prompt 'mganjoo'

# Base 16 Shell configuration
BASE16_SHELL="$HOME/.colors/base16-tomorrow.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# == Plugins ==

# virtualenvwrapper
if (( $+commands[virtualenvwrapper.sh] )); then
  export WORKON_HOME="$HOME/.virtualenvs"
  export PROJECT_HOME="$HOME/workspace"
  VIRTUAL_ENV_DISABLE_PROMPT=1
  source $commands[virtualenvwrapper.sh]
fi

# zmv
autoload -U zmv

# fasd
eval "$(fasd --init auto)"

# tmuxifier
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"
eval "$($HOME/.util/tmuxifier/bin/tmuxifier init -)"

# fzf
export FZF_TMUX=0
export FZF_CTRL_T_COMMAND='(git ls-tree -r --name-only HEAD ||
  find * -name ".*" -prune -o -type f -print -o -type l -print) 2>/dev/null'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# run-help
autoload run-help
if command -v brew > /dev/null 2>&1; then
  HELPDIR=$(brew --prefix)/share/zsh/help
fi

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
