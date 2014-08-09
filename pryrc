# === EDITOR ===
Pry.editor = 'vi'

# === CUSTOM PROMPT ===
# Show Ruby version in prompt
Pry.prompt = [
  proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} > " },
  proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} * " }
]

# === Listing config ===
# Better colors (solarized)
Pry.config.ls.heading_color = :magenta
Pry.config.ls.public_method_color = :green
Pry.config.ls.protected_method_color = :yellow

# === COLORS ===
# Configure pry theme
begin
  require 'pry-theme'
  Pry.config.theme = "solarized"
rescue LoadError => err
  puts "Could not load pry-theme"
end

# == PLUGINS ===
# Configure syntax colorized printing and pagination
begin
  require 'awesome_print'
  Pry.config.print = proc do |output, value|
    Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output)
  end
rescue LoadError => err
  puts "Could not load awesome_print"
end

# === CONVENIENCE METHODS ===
# From https://gist.github.com/807492
class Array
  def self.toy(n=10, &block)
    block_given? ? Array.new(n,&block) : Array.new(n) {|i| i+1}
  end
end

class Hash
  def self.toy(n=10)
    Hash[Array.toy(n).zip(Array.toy(n){|c| (96+(c+1)).chr})]
  end
end

# vim: ft=ruby
