Milind Ganjoo's dotfiles
========================

My configuration files, managed with [`chezmoi`](https://github.com/twpayne/chezmoi).

## Requirements

1. Install Homebrew (on Mac OS X):

        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

2. Install 1Password (has some secrets for configuration of dotfiles):

        /opt/homebrew/bin/brew install 1password 1password-cli

   and [set up as an SSH agent](https://developer.1password.com/docs/ssh/get-started/#step-3-turn-on-the-1password-ssh-agent).

3. Temporarily add Homebrew to path:

        eval "$(/opt/homebrew/bin/brew shellenv)"

4. Clone the repository into the home directory and install:

        sh -c "$(curl -fsLS get.chezmoi.io)" -- init --ssh --apply mganjoo

5. Install Homebrew packages:

        brew bundle --file ~/.Brewfile

## Requirements (Ubuntu / Debian Linux)

1. Install chezmoi to `~/.local/bin`:

        sh -c "$(curl -fsLS get.chezmoi.io)" -- -b ~/.local/bin

2. Authenticate gh (browser flow, or paste a token):

        gh auth login

3. Clone this repo into the chezmoi source dir (without applying):

        ~/.local/bin/chezmoi init mganjoo --apply=false

4. Install the apt-managed half of the toolchain with sudo (zsh, neovim,
   fzf, bat, fd-find, git-delta, miller, entr, hyperfine, pdfgrep,
   1password-cli, xclip, wl-clipboard, etc.):

        sudo bash ~/.local/share/chezmoi/bootstrap/install-system-linux.sh

5. Apply the dotfiles:

        ~/.local/bin/chezmoi apply

6. (Optional) Enable 1Password as an SSH agent for git commit signing:
   open the 1Password app → Settings → Developer → toggle "Use SSH agent".

   The Linux side intentionally tracks only what's in Ubuntu's default apt
   repos. Aliases / shell modules that reference Mac-only tools (eza, jj,
   difft, lazygit, etc.) degrade to "command not found" if you call them
   without installing the corresponding tool by hand. Install each as
   needed via cargo / official upstream releases.

# Post-installation

1. Set ZSH as the default shell:

        # On OS X
        chsh -s $(brew --prefix)/bin/zsh
        # On Ubuntu
        chsh -s /bin/zsh

  Before running this command, add the path to the Homebrew ZSH to
  `/etc/shells`.

# Other install steps (can be automated later):

1. Change Caps Lock key to act as Control key
2. Install licenses for various brewed casks
