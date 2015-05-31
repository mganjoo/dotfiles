#!/usr/bin/env ruby
# encoding: utf-8
# == Synopsis
#   Requires the terminal-notifier gem ([sudo] gem install terminal-notifier)

#   growlnotify wrapper to turn Growl alerts into Mountain Lion notifications
# Uses growlnotify option syntax to keep your old scripts working with the new shiny.
#
#   If you use Growl via growlnotify in your shell scripting, this script
# will replace Growl's alerts with Mountain Lion notifications.
# Name this script `growlnotify`, make it executable and replace
# `/usr/local/bin/growlnotify` (your location may vary) with it
# Now you shouldn't have to update any of your existing tools.
#
# == Examples
#   Send a Mountain Lion notification with title and message
#     growlnotify -m "message" -t "title"
#
# == Usage
#   growlnotify -m "search string" [-[tn] "title"]
#
#   For help use: growlnotify -h
#
# == Options
#     -m, --message MESSAGE            Copy just the url, defaults to full HTML tag
#     -t, --title TITLE                Title
#     -h, --help                       Display this screen
#     -v, --version                    Display version info
#     -n, --name NAME                  Pseudonym for --title
#     -s, --sticky                     Disabled in grownlnotify-notifier
#         --appIcon APPICON            Disabled in grownlnotify-notifier
#     -i, --icon ICON                  Disabled in grownlnotify-notifier
#     -I, --iconpath ICONPATH          Disabled in grownlnotify-notifier
#         --image IMAGEFILE            Disabled in grownlnotify-notifier
#     -p, --priority PRIO              Disabled in grownlnotify-notifier
#     -d, --indentifier ID             Disabled in grownlnotify-notifier
#     -H, --host HOSTNAME              Disabled in grownlnotify-notifier
#     -P, --password PASSWORD          Disabled in grownlnotify-notifier
#     -u, --udp                        Disabled in grownlnotify-notifier
#         --port PORTNUM               Disabled in grownlnotify-notifier
#     -a, --auth DIGEST                Disabled in grownlnotify-notifier
#     -c, --crypt                      Disabled in grownlnotify-notifier
#     -w, --wait                       Disabled in grownlnotify-notifier
#         --progress VALUE             Disabled in grownlnotify-notifier
#
# == Author
#   Brett Terpstra
#
# == Copyright
#   Public Domain
#

MAJOR_VERSION = 1.0

require 'optparse'
require 'rubygems'
require 'terminal-notifier'

options = {}
optparse = OptionParser.new do|opts|
  opts.banner = "Usage: growlnotify -m \"message\" -t \"title\""
  options[:message] = false
  opts.on( '-m', '--message MESSAGE', 'Copy just the url, defaults to full HTML tag' ) do|message|
    options[:message] = message
  end
  options[:title] = false
  opts.on( '-t', '--title TITLE', 'Title' ) do |title|
   options[:title] = title
  end
  opts.on( '-h', '--help', 'Display this screen' ) do
    puts opts
    exit
  end
  opts.on('-v','--version','Display version info') do
    puts MAJOR_VERSION
    exit
  end
  opts.on('-n','--name NAME','Pseudonym for --title') do |name|
    options[:title] = name unless options[:title]
  end
  opts.on('-s','--sticky','Disabled in grownlnotify-notifier')
  opts.on('-a','--appIcon APPICON','Disabled in grownlnotify-notifier')
  opts.on('-i','--icon ICON','Disabled in grownlnotify-notifier')
  opts.on('-I','--iconpath ICONPATH','Disabled in grownlnotify-notifier')
  opts.on('--image IMAGEFILE','Disabled in grownlnotify-notifier')
  opts.on('-p', '--priority PRIO','Disabled in grownlnotify-notifier')
  opts.on('-d', '--indentifier ID','Disabled in grownlnotify-notifier')
  opts.on('-H', '--host HOSTNAME','Disabled in grownlnotify-notifier')
  opts.on('-P', '--password PASSWORD','Disabled in grownlnotify-notifier')
  opts.on('-u', '--udp','Disabled in grownlnotify-notifier')
  opts.on('--port PORTNUM','Disabled in grownlnotify-notifier')
  opts.on('-a','--auth DIGEST','Disabled in grownlnotify-notifier')
  opts.on('-c','--crypt','Disabled in grownlnotify-notifier')
  opts.on('-w','--wait','Disabled in grownlnotify-notifier')
  opts.on('--progress VALUE','Disabled in grownlnotify-notifier')
end

optparse.parse!

unless options[:message]
  puts "Message required."
  exit
end

options[:title] = "Terminal" unless options[:title]

TerminalNotifier.notify(options[:message], :title => options[:title])
