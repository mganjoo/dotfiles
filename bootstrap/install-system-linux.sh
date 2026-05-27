#!/usr/bin/env bash
# WorkSpace / Ubuntu bootstrap: apt-installable system packages.
#
# Run once, interactively, with sudo:
#   sudo bash bootstrap/install-system-linux.sh
#
# Idempotent — re-running just upgrades / no-ops.

set -euo pipefail

if [[ $EUID -ne 0 ]]; then
  echo "error: must be run as root (use sudo)" >&2
  exit 1
fi

export DEBIAN_FRONTEND=noninteractive

apt-get update -y

# Apt-available packages. Kept in sync with dot_Brewfile — anything that has a
# Linux apt equivalent goes here; anything without one is installed by
# run_onchange_before_install-packages-linux.sh.tmpl into ~/.local/bin.
apt-get install -y --no-install-recommends \
  zsh \
  bat \
  fd-find \
  git-delta \
  fzf \
  zoxide \
  miller \
  entr \
  hyperfine \
  pdfgrep \
  jq \
  ripgrep \
  neovim \
  1password-cli \
  xclip \
  wl-clipboard \
  build-essential \
  pkg-config \
  ca-certificates \
  curl \
  unzip \
  python3-pip \
  python3-venv

# bat installs as 'batcat' on Debian/Ubuntu (name conflict with bacula-console-bat).
# fd-find installs as 'fdfind'. Create thin symlinks so the binaries match
# what the dotfiles expect (bat, fd).
ln -sf /usr/bin/batcat /usr/local/bin/bat
ln -sf /usr/bin/fdfind /usr/local/bin/fd

# Ensure zsh paths are in /etc/shells so chsh accepts them.
for sh_path in /bin/zsh /usr/bin/zsh; do
  if [[ -x "$sh_path" ]] && ! grep -qxF "$sh_path" /etc/shells 2>/dev/null; then
    echo "$sh_path" >> /etc/shells
  fi
done

echo
echo "Done. Versions installed:"
zsh --version
batcat --version | head -1
fdfind --version
delta --version | head -1
fzf --version
zoxide --version
nvim --version | head -1
op --version
