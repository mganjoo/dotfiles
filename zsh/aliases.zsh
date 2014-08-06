# List 10 most used commands (or -n <number>)
alias hist_stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"
