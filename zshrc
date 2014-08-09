# Source configuration modules.
for config_file (~/.zsh/*.zsh); do
  source $config_file
done

# == Plugins ==

# Load scm_breeze plugin (exclude 'design' function)
[ -s "$HOME/.scm_breeze/scm_breeze.sh" ] \
  && source "$HOME/.scm_breeze/scm_breeze.sh"
unset -f design

# Load ZSH plugins (at the end of .zshrc). Order matters.
local plugins=( \
  'zsh-syntax-highlighting' \
  'zsh-history-substring-search' \
  )
for plugin in $plugins; do
  source ${0:h}/external/$plugin/$plugin.zsh
done
