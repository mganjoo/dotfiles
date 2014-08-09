# Source configuration modules.
for config_file (~/.zsh/*.zsh); do
  source $config_file
done

# == Plugins ==

# Load scm_breeze plugin (exclude 'design' function)
if [[ "$TERM" != 'dumb' && $- =~ i ]]; then
  if [ -s "~/.scm_breeze/scm_breeze.sh" ]; then
    source "~/.scm_breeze/scm_breeze.sh"
    unset -f design
  fi
fi

# Load ZSH plugins (at the end of .zshrc). Order matters.
for plugin in \
  "zsh-syntax-highlighting" \
  "zsh-history-substring-search"
do
  source ~/.zsh/external/$plugin/$plugin.zsh
done
