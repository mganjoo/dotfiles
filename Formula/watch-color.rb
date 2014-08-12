require "formula"

# Documentation: https://github.com/Homebrew/homebrew/wiki/Formula-Cookbook
#                /usr/local/Library/Contributions/example-formula.rb
# PLEASE REMOVE ALL GENERATED COMMENTS BEFORE SUBMITTING YOUR PULL REQUEST!

class WatchColor < Formula
  homepage "https://github.com/whit537/watch"
  url "https://github.com/whit537/watch/archive/0.3.0.tar.gz"
  version '0.3.0'
  sha1 "1b355d16d729d5fe4c0ce90ec92b45946cfa4a5a"

  def install
    bin.install "watch"
    man.install "watch.1"
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! It's enough to just replace
    # "false" with the main program this formula installs, but it'd be nice if you
    # were more thorough. Run the test with `brew test watch`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "watch --version"
  end
end

