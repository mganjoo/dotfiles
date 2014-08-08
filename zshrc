# Source configuration modules.
for config_file (~/.zsh/*.zsh); do
  source $config_file
done

# == Plugins ==

# Load plugins (at the end of .zshrc). Order matters.
local plugins=( \
  'zsh-syntax-highlighting' \
  'zsh-history-substring-search' \
  )
for plugin in $plugins; do
  source ${0:h}/external/$plugin/$plugin.zsh
done
