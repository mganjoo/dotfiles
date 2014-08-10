Milind Ganjoo's dotfiles
========================

My configuration files.

## Requirements

1. Install Xcode from the App Store and agree to the license terms:

    sudo xcodebuild -license

## Installation

1. Clone the repository into the home directory:

    git clone git://github.com/mganjoo/dotfiles.git ~/.dotfiles

2. Install the dotfiles:

    cd ~/.dotfiles
    ./install

# Post-installation

1. Set zsh as the default shell:

    chsh -s $(brew --prefix)/bin/zsh

  Before running this command, add the path to the Homebrew `zsh` to
  `/etc/shells`

