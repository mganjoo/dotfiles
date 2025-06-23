Milind Ganjoo's dotfiles
========================

My configuration files, managed with [`chezmoi`](https://github.com/twpayne/chezmoi).

## Requirements

1. Install Xcode from the App Store.

2. Agree to the license terms:

        sudo xcodebuild -license

3. Install Homebrew (on Mac OS X):

        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

4. Install 1Password (has some secrets for configuration of dotfiles):

        brew install 1password 1password-cli

   and [set up as an SSH agent](https://developer.1password.com/docs/ssh/get-started/#step-3-turn-on-the-1password-ssh-agent).

4. Clone the repository into the home directory and install:

        sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply mganjoo

5. Temporarily add Homebrew to path:

        eval "$(/opt/homebrew/bin/brew shellenv)"

5. Install Homebrew packages:

        brew bundle --file ~/.Brewfile

6. Update all packages:

        $(brew --prefix)/opt/fzf/install --completion --key-bindings --no-update-rc

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
