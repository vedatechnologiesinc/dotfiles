#———————————————————————————————————————————————————————————————————————————————
# +main.keys

#———————————————————————————————————————————————————————————————————————————————
# functions

def bind_all(key, command, normal=True, passthrough=False):
    config.bind(key, command, mode='insert')
    config.bind(key, command, mode='hint')
    config.bind(key, command, mode='caret')
    config.bind(key, command, mode='register')

    if normal == True:
        config.bind(key, command, mode='normal')

    if passthrough == True:
        config.bind(key, command, mode='passthrough')

#———————————————————————————————————————————————————————————————————————————————
# unbind

config.unbind("<Escape>", mode='insert')
config.unbind("[[")
config.unbind("]]")
config.unbind("q")
config.unbind("ad")
config.unbind("@")
config.unbind("co")
config.unbind("d")
config.unbind("D")

#———————————————————————————————————————————————————————————————————————————————
# main

config.bind(".", "config-source")
config.bind("\\", "cmd-repeat-last")
config.bind("K", 'tab-prev')
config.bind("J", 'tab-next')
config.bind("a", 'open -t')
config.bind("ww", 'open -w')
config.bind("wp", 'open -p')
config.bind("wd", 'close')
config.bind("X", 'tab-close -p')
config.bind("x", 'tab-close -n')
config.bind("s", 'cmd-set-text -s :tab-select')
config.bind("I", 'mode-enter passthrough')

config.bind("[", 'tab-prev')
config.bind("]", 'tab-next')
config.bind("{", 'tab-move -')
config.bind("}", 'tab-move +')
config.bind("<", 'navigate prev -t')
config.bind(">", 'navigate next -t')

#———————————————————————————————————————————————————————————————————————————————
# escape

bind_all("<Shift+Escape>", "mode-leave ;; jseval -q document.activeElement.blur()", True, True)
bind_all("<Escape>",  "mode-leave ;; jseval -q document.activeElement.blur()", False)

#———————————————————————————————————————————————————————————————————————————————
# command

bind_all("<Meta+r>", "reload")
bind_all("<Meta+Shift+r>", "reload -f")
bind_all("<Meta+Alt+Left>", "tab-prev")
bind_all("<Meta+Alt+Right>", "tab-next")
bind_all("<Meta+t>", "open -t")
bind_all("<Meta+w>", "tab-close -n")
bind_all("<Meta+[>", "tab-prev")
bind_all("<Meta+]>", "tab-next")
bind_all("<Meta+Left>", "back")
bind_all("<Meta+Right>", "forward")
bind_all("<Meta+Shift+[>", 'tab-move -')
bind_all("<Meta+SHift+]>", 'tab-move +')
bind_all("<Meta+1>", "tab-focus 1")
bind_all("<Meta+2>", "tab-focus 2")
bind_all("<Meta+3>", "tab-focus 3")
bind_all("<Meta+4>", "tab-focus 4")
bind_all("<Meta+5>", "tab-focus 5")
bind_all("<Meta+6>", "tab-focus 6")
bind_all("<Meta+7>", "tab-focus 7")
bind_all("<Meta+8>", "tab-focus 8")
bind_all("<Meta+9>", "tab-focus 9")
bind_all("<Meta+0>", "tab-focus -1")

config.bind("g1", 'tab-focus 1', mode='normal')
config.bind("g2", 'tab-focus 2', mode='normal')
config.bind("g3", 'tab-focus 3', mode='normal')
config.bind("g4", 'tab-focus 4', mode='normal')
config.bind("g5", 'tab-focus 5', mode='normal')
config.bind("g6", 'tab-focus 6', mode='normal')
config.bind("g7", 'tab-focus 7', mode='normal')
config.bind("g8", 'tab-focus 8', mode='normal')
config.bind("g9", 'tab-focus 9', mode='normal')
config.bind("g0", 'tab-focus -1', mode='normal')

#———————————————————————————————————————————————————————————————————————————————
# t

config.bind('td', 'config-cycle colors.webpage.darkmode.enabled true false ;; config-cycle colors.webpage.bg "black" "#FBFAF2"')
config.bind('tj', 'config-cycle -p -t -u *://{url:host}/* content.javascript.enabled ;; reload')
config.bind('tc', 'config-cycle content.user_stylesheets "~/.doom.d/org.css" ""')

#———————————————————————————————————————————————————————————————————————————————
# ,

config.bind(',M', 'spawn mpv {url}')
config.bind(',m', 'hint links spawn mpv {hint-url}')
config.bind(',I', 'spawn iina {url}')
config.bind(',i', 'hint links spawn iina {hint-url}')
config.bind(',S', 'spawn open -a safari {url}')
config.bind(',s', 'hint links spawn open -a safari {hint-url}')
config.bind(',F', 'spawn open -a "Firefox Nightly" {url}')
config.bind(',f', 'hint links spawn open -a "Firefox Nightly" {hint-url}')
config.bind(',o', 'spawn org {url}')
config.bind(',O', 'hint links spawn org {url}')
