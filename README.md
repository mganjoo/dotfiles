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
