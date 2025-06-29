# Load all completions and shell configuration for third party apps here.

# Homebrew
if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
  export HOMEBREW_NO_ENV_HINTS=1
fi

# iTerm shell integration
if [[ -f ~/.iterm2_shell_integration.zsh ]]; then
  source ~/.iterm2_shell_integration.zsh
fi

# 1Password plugins
if [[ -f ~/.config/op/plugins.sh ]]; then
  source ~/.config/op/plugins.sh
fi

# uv and uvx
if (( $+commands[uv] )); then
  eval "$(uv generate-shell-completion zsh)"
fi
if (( $+commands[uvx] )); then
  eval "$(uvx --generate-shell-completion zsh)"
fi

# zoxide
if (( $+commands[zoxide] )); then
  eval "$(zoxide init zsh)"
fi

# fd (modern find)
if (( $+commands[fd] )); then
  eval "$(fd --gen-completions zsh)"
fi
