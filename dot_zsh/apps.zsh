# Load all completions and shell configuration for third party apps here.

# Homebrew
if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
  export HOMEBREW_NO_ENV_HINTS=1
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

# ripgrep
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# jujutsu vcs
if (( $+commands[jj] )); then
  source <(jj util completion zsh)
fi

# claude code
# Load CLAUDE.md from additional directories when using --add-dir
CLAUDE_CODE_ADDITIONAL_DIRECTORIES_CLAUDE_MD=1
