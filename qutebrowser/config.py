c = c  # noqa: F821 pylint: disable=E0602,C0103

config = config  # noqa: F821 pylint: disable=E0602,C0103
# pylint settings included to disable linting errors

import subprocess

def read_xresources(prefix):
    props = {}
    x = subprocess.run(['xrdb', '-query'], capture_output=True, check=True, text=True)
    lines = x.stdout.split('\n')
    for line in filter(lambda l: l.startswith(prefix), lines):
        prop, _, value = line.partition(':\t')
        props[prop] = value
    return props

xresources = read_xresources("*")

# Configuration settings
#c.colors.statusbar.normal.bg = xresources["*.background"]
#c.colors.statusbar.command.bg = xresources["*.background"]
c.colors.statusbar.command.fg = xresources["*color13"]
c.colors.statusbar.normal.fg = xresources["*.background"]
c.colors.statusbar.insert.bg = xresources["*.background"]  
c.colors.statusbar.insert.fg = xresources["*.foreground"]  
c.colors.statusbar.passthrough.fg = xresources["*color14"]
c.colors.statusbar.url.fg = xresources["*color13"]
c.colors.statusbar.url.success.https.fg = xresources["*color13"]
c.colors.statusbar.url.hover.fg = xresources["*color12"]
# c.statusbar.show = "always"
c.colors.tabs.even.bg = xresources["*color13"]
c.colors.tabs.even.bg = xresources["*.background"]
c.colors.tabs.odd.bg = xresources["*.background"]
c.colors.tabs.even.fg = xresources["*color0"]
c.colors.tabs.odd.fg = xresources["*color0"]
c.colors.tabs.selected.even.bg = xresources["*color13"]
c.colors.tabs.selected.odd.bg = xresources["*color13"]
c.colors.tabs.selected.even.fg = xresources["*color14"]
c.colors.tabs.selected.odd.fg = xresources["*color14"]
c.colors.hints.bg = xresources["*.background"]
c.colors.hints.fg = xresources["*.foreground"]
c.tabs.show = "multiple"

c.colors.completion.item.selected.match.fg = xresources["*color6"]
c.colors.completion.match.fg = xresources["*color6"]

c.colors.tabs.indicator.start = xresources["*color10"]
c.colors.tabs.indicator.stop = xresources["*color8"]
c.colors.completion.odd.bg = xresources["*.background"]
c.colors.completion.even.bg = xresources["*.background"]
c.colors.completion.fg = xresources["*.foreground"]
c.colors.completion.category.bg = xresources["*.background"]
c.colors.completion.category.fg = xresources["*.foreground"]
c.colors.completion.item.selected.bg = xresources["*.background"]
c.colors.completion.item.selected.fg = xresources["*.foreground"]

c.colors.messages.info.bg = xresources["*.background"]
c.colors.messages.info.fg = xresources["*.foreground"]
c.colors.messages.error.bg = xresources["*.background"]
c.colors.messages.error.fg = xresources["*.foreground"]
c.colors.downloads.error.bg = xresources["*.background"]
c.colors.downloads.error.fg = xresources["*.foreground"]

c.colors.downloads.bar.bg = xresources["*.background"]
c.colors.downloads.start.bg = xresources["*color10"]
c.colors.downloads.start.fg = xresources["*.foreground"]
c.colors.downloads.stop.bg = xresources["*color8"]
c.colors.downloads.stop.fg = xresources["*.foreground"]

c.colors.tooltip.bg = xresources["*.background"]
c.colors.webpage.bg = xresources["*.background"]
c.hints.border = xresources["*.foreground"]

# c.url.start_pages = ""
# c.url.default_page = ""

c.tabs.title.format = "{audio}{current_title}"
c.fonts.web.size.default = 20

c.url.searchengines = {
# note - if you use duckduckgo, you can make use of its built in bangs, of which there are many! https://duckduckgo.com/bangs
        'DEFAULT': 'https://duckduckgo.com/?q={}',
        '!dw': 'https://www.debian.org/?search={}',
        '!ad': 'http://10.24.93.65/?search={}',
        '!dpkg': 'https://packages.debian.org/search?keywords={}',
        '!gh': 'https://github.com/search?o=desc&q={}&s=stars',
        '!su': 'https://suckless.org/search?o=desc&q={}&s=stars',
        '!yt': 'https://www.youtube.com/results?search_query={}',
        }

c.completion.open_categories = ['searchengines', 'quickmarks', 'bookmarks', 'history', 'filesystem']

config.load_autoconfig() # load settings done via the gui

c.auto_save.session =  False# save tabs on quit/restart

# keybinding changes
config.bind('=', 'cmd-set-text -s :open')
config.bind('h', 'history')
config.bind('cs', 'cmd-set-text -s :config-source')
config.bind('tH', 'config-cycle tabs.show multiple never')
config.bind('sH', 'config-cycle statusbar.show always never')
config.bind('T', 'hint links tab')
config.bind('pP', 'open -- {primary}')
config.bind('pp', 'open -- {clipboard}')
config.bind('pt', 'open -t -- {clipboard}')
config.bind('qm', 'macro-record')
config.bind('<ctrl-y>', 'spawn --userscript ytdl.sh')
config.bind('tT', 'config-cycle tabs.position top left')
config.bind('gJ', 'tab-move +')
config.bind('gK', 'tab-move -')
config.bind('gm', 'tab-move')
config.bind('<Ctrl+c>', 'yank url')
config.bind('<Ctrl-d>', 'open https://discord.com')
config.bind('<Ctrl-a>', 'open http://10.24.93.65/')
config.bind('<Ctrl-r>', 'config-source')
config.bind('<Ctrl-y>', 'cmd-set-text -s :open -t https://www.youtube.com/results?search_query=')


# dark mode setup
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.algorithm = 'lightness-cielab'
c.colors.webpage.darkmode.policy.images = 'never'
config.set('colors.webpage.darkmode.enabled', False, 'file://*')

# styles, cosmetics
# c.content.user_stylesheets = ["~/.config/qutebrowser/styles/youtube-tweaks.css"]
c.hints.padding = {'top': 0, 'bottom': 0, 'left': 0, 'right': 0}
c.tabs.padding = {'top': 0, 'bottom': 0, 'left': 2, 'right': 2}  # Adjust tab padding
c.tabs.indicator.width = 0 # no tab indicators
# c.window.transparent = True # apparently not needed
#c.window.startup_mode = 'default'
c.window.hide_decoration = False  
c.tabs.width = '3%'
c.window.hide_decoration = True 

# fonts
c.fonts.default_family = 'Iosevka'
c.fonts.default_size = '14pt'
c.fonts.web.family.fixed = 'monospace'
c.fonts.web.family.sans_serif = 'monospace'
c.fonts.web.family.serif = 'monospace'
c.fonts.web.family.standard = 'monospace'

# privacy - adjust these settings based on your preference
# config.set("completion.cmd_history_max_items", 0)
# config.set("content.private_browsing", True)
config.set("content.webgl", False, "*")
config.set("content.canvas_reading", False)
config.set("content.geolocation", False)
config.set("content.webrtc_ip_handling_policy", "default-public-interface-only")
config.set("content.cookies.accept", "all")
config.set("content.cookies.store", True)
config.set("content.javascript.enabled", True) # tsh keybind to toggle

# Adblocking info -->
# For yt ads: place the greasemonkey script yt-ads.js in your greasemonkey folder (~/.config/qutebrowser/greasemonkey).
# The script skips through the entire ad, so all you have to do is click the skip button.
# Yeah it's not ublock origin, but if you want a minimal browser, this is a solution for the tradeoff.
# You can also watch yt vids directly in mpv, see qutebrowser FAQ for how to do that.
# If you want additional blocklists, you can get the python-adblock package, or you can uncomment the ublock lists here.
c.content.blocking.enabled = True
# c.content.blocking.method = 'adblock' # uncomment this if you install python-adblock
# c.content.blocking.adblock.lists = [
#         "https://github.com/ewpratten/youtube_ad_blocklist/blob/master/blocklist.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/legacy.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2020.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2021.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2022.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2023.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2024.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badware.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badlists.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances-cookies.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances-others.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badlists.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/quick-fixes.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt",
#         "https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt"]
