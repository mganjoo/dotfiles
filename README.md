Milind Ganjoo's dotfiles
========================

My configuration files.

## Requirements

1. Install Xcode from the App Store.

2. Agree to the license terms:

        sudo xcodebuild -license

3. Install Homebrew (on Mac OS X):

        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

4. Clone the repository into the home directory:

        git clone git://github.com/mganjoo/dotfiles.git ~/.dotfiles

5. Install Homebrew packages:

        cd ~/.dotfiles
        brew bundle

6. Install Git submodules for repository:

        git submodule update --init --recursive

7. Install the dotfiles (which also runs the associated hooks):

        RCRC=~/.dotfiles/rcrc rcup

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
