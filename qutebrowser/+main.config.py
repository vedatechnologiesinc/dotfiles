#———————————————————————————————————————————————————————————————————————————————
# +main.config

import os

c.url.default_page = "~/.qutebrowser/pages/index.html"
c.content.pdfjs = True
c.downloads.location.prompt = True
c.auto_save.session = True
c.completion.height = "20%"
c.completion.cmd_history_max_items = -1
c.content.autoplay = False
c.tabs.background = True
c.tabs.new_position.unrelated = "next"

c.content.desktop_capture = False
c.content.geolocation = False
c.content.mouse_lock = False
c.content.persistent_storage = True
c.content.register_protocol_handler = False
c.content.tls.certificate_errors = "ask"
c.completion.open_categories = ["history", "quickmarks", "bookmarks", "searchengines"]
c.hints.chars = "aoeuidhtns"
c.window.hide_decoration = False
c.zoom.levels = ["25%", "33%", "50%", "60%", "70%", "80%", "90%", "100%", "110%", "125%", "150%", "175%", "200%", "250%", "300%", "400%", "500%"]
c.scrolling.smooth = True
c.input.mouse.back_forward_buttons = True

with config.pattern('http://gigamonkeys.com/book/') as p:
    p.content.images = False

os.environ['PATH'] = "/var/setuid-wrappers:/run/wrappers/bin:/Users/ebzzry/.nix-profile/bin:/Users/ebzzry/.nix-profile/sbin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/opt/homebrew/bin:/opt/homebrew/sbin:/opt/homebrew/opt/openjdk/bin:/opt/homebrew/opt/grep/libexec/gnubin:/bin:/sbin:/opt/bin:/usr/local/bin:/usr/bin:/usr/sbin:/usr/games:/usr/X11R6/bin:/Users/ebzzry/bin:/Users/ebzzry/.cargo/bin:/Users/ebzzry/.local/bin:/Users/ebzzry/.node/bin:/Users/ebzzry/.roswell/bin:/Users/ebzzry/go/bin:/Users/ebzzry/.emacs.d/bin:/Library/TeX/texbin"

c.tabs.position = 'top'
c.tabs.title.format = "{current_title}"
c.tabs.title.format_pinned = "📌{current_title}"
c.tabs.padding = dict(bottom=4, top=6, left=6, right=8)
c.tabs.select_on_remove = "next"
c.tabs.show = 'multiple'
c.tabs.pinned.frozen = True
c.tabs.mode_on_change = 'restore'
c.tabs.indicator.width = 2
c.tabs.indicator.padding = dict(bottom=1, top=1, left=3, right=6)
c.tabs.width = '15%'
c.tabs.close_mouse_button = "middle"
c.tabs.close_mouse_button_on_bar = "ignore"
c.colors.webpage.darkmode.policy.images = "never"

c.colors.webpage.darkmode.enabled = False
# c.colors.webpage.bg = "black"

config.source("themes/ebzzry/ebzzry.py")
# config.source("themes/base16-qutebrowser/themes/default/base16-gruvbox-light.config.py")
# config.source("themes/base16-qutebrowser/themes/default/base16-gruvbox-dark.config.py")
