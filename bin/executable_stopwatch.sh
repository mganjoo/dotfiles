#!/bin/bash
# Big ASCII art stopwatch with millisecond precision
# Adaptive sizing for narrow terminals
# Requires python3

python3 - << 'PYEOF'
import time, os, sys, signal, re

GREEN  = '\033[92m'
CYAN   = '\033[96m'
DIM    = '\033[2m'
BOLD   = '\033[1m'
RESET  = '\033[0m'

# ── Large digits: 5 rows × 5 wide ──
BIG = [
    [' ███ ', '█   █', '█   █', '█   █', ' ███ '],  # 0
    ['  █  ', ' ██  ', '  █  ', '  █  ', '█████'],  # 1
    ['████ ', '    █', ' ███ ', '█    ', '█████'],   # 2
    ['████ ', '    █', ' ███ ', '    █', '████ '],   # 3
    ['█   █', '█   █', '█████', '    █', '    █'],   # 4
    ['█████', '█    ', '████ ', '    █', '████ '],   # 5
    [' ███ ', '█    ', '████ ', '█   █', ' ███ '],   # 6
    ['█████', '    █', '   █ ', '  █  ', ' █   '],   # 7
    [' ███ ', '█   █', ' ███ ', '█   █', ' ███ '],   # 8
    [' ███ ', '█   █', ' ████', '    █', ' ███ '],   # 9
]
BIG_COLON  = ['   ', ' █ ', '   ', ' █ ', '   ']
BIG_PERIOD = ['   ', '   ', '   ', '   ', ' █ ']

# ── Small digits: 3 rows × 3 wide ──
SML = [
    ['███', '█ █', '███'],  # 0
    [' █ ', '██ ', ' █ '],  # 1
    ['██ ', ' █ ', '███'],  # 2
    ['██ ', ' ██', '██ '],  # 3
    ['█ █', '███', '  █'],  # 4
    ['███', '██ ', '██ '],  # 5
    ['███', '███', '███'],  # 6
    ['███', '  █', '  █'],  # 7
    ['███', '███', '███'],  # 8
    ['███', '███', '███'],  # 9
]
SML_COLON  = ['  ', ' :', '  ']
SML_PERIOD = ['  ', '  ', ' .']

STRIP_ANSI = re.compile(r'\033\[[^m]*m')

def vlen(s):
    return len(STRIP_ANSI.sub('', s))

def render(elapsed_ms, cols, rows):
    ms  = elapsed_ms % 1000
    sec = elapsed_ms // 1000
    h, m, s = sec // 3600, (sec % 3600) // 60, sec % 60

    # Height forces downgrade: big needs ~10 rows, small ~8, text ~3
    # Width thresholds pick the widest mode that fits
    use_big   = rows >= 10 and cols >= 40
    use_small = rows >= 8  and cols >= 36

    if use_big and cols >= 70:
        D, COL, PER, gap, nrows = BIG, BIG_COLON, BIG_PERIOD, ' ', 5
        parts  = [D[h//10], D[h%10], COL,
                  D[m//10], D[m%10], COL,
                  D[s//10], D[s%10], PER,
                  D[ms//100], D[(ms//10)%10], D[ms%10]]
        colors = [GREEN]*2 + [DIM] + [GREEN]*2 + [DIM] + [GREEN]*2 + [DIM] + [CYAN]*3
    elif use_big and cols >= 52:
        D, COL, PER, gap, nrows = BIG, BIG_COLON, BIG_PERIOD, ' ', 5
        parts  = [D[m//10], D[m%10], COL,
                  D[s//10], D[s%10], PER,
                  D[ms//100], D[(ms//10)%10], D[ms%10]]
        colors = [GREEN]*2 + [DIM] + [GREEN]*2 + [DIM] + [CYAN]*3
    elif use_big:
        D, COL, PER, gap, nrows = BIG, BIG_COLON, BIG_PERIOD, '', 5
        parts  = [D[m//10], D[m%10], COL,
                  D[s//10], D[s%10], PER,
                  D[ms//100], D[(ms//10)%10]]
        colors = [GREEN]*2 + [DIM] + [GREEN]*2 + [DIM] + [CYAN]*2
    elif use_small:
        D, COL, PER, gap, nrows = SML, SML_COLON, SML_PERIOD, ' ', 3
        parts  = [D[m//10], D[m%10], COL,
                  D[s//10], D[s%10], PER,
                  D[ms//100], D[(ms//10)%10], D[ms%10]]
        colors = [GREEN]*2 + [DIM] + [GREEN]*2 + [DIM] + [CYAN]*3
    else:
        # Text fallback for very small terminals
        if h > 0:
            txt = f'{GREEN}{h:02d}{DIM}:{GREEN}{m:02d}{DIM}:{GREEN}{s:02d}{DIM}.{CYAN}{ms:03d}{RESET}'
        else:
            txt = f'{GREEN}{m:02d}{DIM}:{GREEN}{s:02d}{DIM}.{CYAN}{ms:03d}{RESET}'
        return [txt], 1

    lines = []
    for row in range(nrows):
        line = gap.join(
            color + part[row] + RESET
            for part, color in zip(parts, colors)
        )
        lines.append(line)
    return lines, nrows

def cleanup(*_):
    sys.stdout.write('\033[?25h\033[2J\033[H')
    sys.stdout.flush()
    sys.exit(0)

needs_clear = False

def on_resize(*_):
    global needs_clear
    needs_clear = True

signal.signal(signal.SIGINT, cleanup)
signal.signal(signal.SIGTERM, cleanup)
signal.signal(signal.SIGWINCH, on_resize)

sys.stdout.write('\033[2J\033[H\033[?25l')
sys.stdout.flush()

start = time.monotonic()
prev_nrows = 0

while True:
    elapsed_ms = int((time.monotonic() - start) * 1000)

    try:
        sz = os.get_terminal_size()
        cols, rows = sz.columns, sz.lines
    except OSError:
        cols, rows = 80, 24

    lines, nrows = render(elapsed_ms, cols, rows)

    # Clear on resize or mode change
    if needs_clear or nrows != prev_nrows:
        sys.stdout.write('\033[2J')
        needs_clear = False
        prev_nrows = nrows

    # Adaptive chrome: hide title/help when height is tight
    show_title = rows >= nrows + 4   # digits + title + gaps + help
    show_help  = rows >= nrows + 3   # digits + gaps + help

    title_txt = 'S T O P W A T C H'
    help_txt  = 'Ctrl+C to stop'
    if cols < 30:
        title_txt = 'STOPWATCH'
    if cols < 18:
        title_txt = ''

    # Calculate layout height
    display_h = nrows
    title_offset = 0
    if show_title and title_txt:
        display_h += 2   # title + blank line
        title_offset = 2
    if show_help:
        display_h += 2   # blank line + help
    top = max(1, (rows - display_h) // 2 + 1)

    def center(txt):
        return max(1, (cols - len(txt)) // 2 + 1)

    buf = []
    if show_title and title_txt:
        buf.append(f'\033[{top};{center(title_txt)}H{BOLD}{title_txt}{RESET}')
    for i, line in enumerate(lines):
        col = max(1, (cols - vlen(line)) // 2 + 1)
        buf.append(f'\033[{top+title_offset+i};{col}H{line}')
    if show_help:
        help_row = top + title_offset + nrows + 1
        buf.append(f'\033[{help_row};{center(help_txt)}H{DIM}{help_txt}{RESET}')

    sys.stdout.write(''.join(buf))
    sys.stdout.flush()
    time.sleep(0.033)
PYEOF
