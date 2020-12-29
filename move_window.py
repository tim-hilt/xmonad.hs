import sys
import subprocess


MON_HEIGHT = 1440
MON_WIDTH = 2560


def get_current_workspace():
    result = subprocess.check_output('wmctrl -d', shell=True)
    for i in result.split(b'\n'):
        i = i.decode('utf-8')
        if '*' in i:
            return int(i[-1])


def get_windows_on_workspace(n):
    result = subprocess.check_output('wmctrl -l -G', shell=True)
    windows = []
    for i in result.split(b'\n'):
        if len(i) > 0:
            i = i.decode('utf-8')
            i = i.split()
            if i[1] == str(n - 1):
                windows.append({'id': i[0], 'x': i[2],
                    'y': i[3], 'w': i[4], 'h': i[5]})
    return windows


def calculate_gaps(windows):
    total_width = 0
    for window in windows:
        total_width += int(window['w'])
    gapsize = (MON_WIDTH - total_width) // 3
    return gapsize


def arrange_windows(windows, gapsize):
    windows = sorted(windows, key=lambda x: int(x['x']))
    x = 0
    for i, window in enumerate(windows):
        y = MON_HEIGHT // 2 - int(window['h']) // 2
        x += gapsize
        move_string = '0,' + str(x) + ',' + str(y) + ',' + window['w'] + ',' + window['h']
        ret = subprocess.call('wmctrl -i -r ' + window['id'] + ' -e ' + move_string, shell=True)
        x += int(window['w'])

def center_window(windows):
    window = windows[0]
    x = MON_WIDTH // 2 - int(window['w']) // 2
    y = MON_HEIGHT // 2 - int(window['h']) // 2
    move_string = '0,' + str(x) + ',' + str(y) + ',' + window['w'] + ',' + window['h']
    ret = subprocess.call('wmctrl -i -r ' + window['id'] + ' -e ' + move_string, shell=True)

if __name__ == '__main__':
    current_workspace = get_current_workspace()
    current_windows = get_windows_on_workspace(current_workspace)
    wcount = len(current_windows)

    if wcount == 1:
        center_window(current_windows)
        sys.exit(0)
    elif wcount == 2:
        gapsize = calculate_gaps(current_windows)
        if gapsize < 0:
            ret = subprocess.call('dunstify Error "Gapsize too narrow. Shrink windows."', shell=True)
            sys.exit(1)
        arrange_windows(current_windows, gapsize)
        sys.exit(0)
    else:
        ret = subprocess.call('dunstify Error "More/Less than 2 windows present!"', shell=True)
        sys.exit(1)
