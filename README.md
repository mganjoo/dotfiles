Milind Ganjoo's dotfiles
========================

My configuration files.

## Requirements

1. Install Xcode from the App Store.

2. Agree to the license terms:

        sudo xcodebuild -license

3. Install Homebrew (on Mac OS X):

        ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

4. Install rcm:

        # On OS X, just install all the packages
        brew bundle ~/.dotfiles/Brewfile

        # On Ubuntu
        sudo apt-add-repository ppa:martin-frost/thoughtbot-rcm
        sudo apt-get install rcm

5. Install Git submodules for repository:

        git submodule update --init --recursive

## Installation

1. Clone the repository into the home directory:

        git clone git://github.com/mganjoo/dotfiles.git ~/.dotfiles

2. Install the dotfiles (which also runs the associated hooks):

        RCRC=~/.dotfiles/rcrc rcup

# Post-installation

1. Set ZSH as the default shell:

        chsh -s $(brew --prefix)/bin/zsh

  Before running this command, add the path to the Homebrew ZSH to
  `/etc/shells`.

