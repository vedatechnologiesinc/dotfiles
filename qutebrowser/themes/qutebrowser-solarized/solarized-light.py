# Solarized
solarized = {
    "base03": "#002b36",
    "base02": "#073642",
    "base01": "#586e75",
    "base00": "#657b83",
    "base0": "#839496",
    "base1": "#93a1a1",
    "base2": "#eee8d5",
    "base3": "#fdf6e3",
    "yellow": "#b58900",
    "orange": "#cb4b16",
    "red": "#dc322f",
    "magenta": "#d33682",
    "violet": "#6c71c4",
    "blue": "#268bd2",
    "cyan": "#2aa198",
    "green": "#859900",
}

# Solarized Light
solarized.update(
    {
        "accent": solarized["blue"],
        "fg3": solarized["base01"],  # optional emphasized content
        "fg1": solarized["base00"],  # body text / default code / primary content
        "fg2": solarized["base1"],  # comments / secondary content
        "bg2": solarized["base2"],  # background highlights
        "bg1": solarized["base3"],  # background
    }
)

## Background color of the completion widget category headers.
## Type: QssColor
c.colors.completion.category.bg = solarized["bg1"]

## Bottom border color of the completion widget category headers.
## Type: QssColor
c.colors.completion.category.border.bottom = solarized["bg1"]

## Top border color of the completion widget category headers.
## Type: QssColor
c.colors.completion.category.border.top = solarized["bg1"]

## Foreground color of completion widget category headers.
## Type: QtColor
c.colors.completion.category.fg = solarized["fg1"]

## Background color of the completion widget for even rows.
## Type: QssColor
c.colors.completion.even.bg = solarized["bg2"]

## Text color of the completion widget. May be a single color to use for
## all columns or a list of three colors, one for each column.
## Type: List of QtColor, or QtColor
c.colors.completion.fg = solarized["fg1"]

## Background color of the selected completion item.
## Type: QssColor
c.colors.completion.item.selected.bg = solarized["accent"]

## Bottom border color of the selected completion item.
## Type: QssColor
c.colors.completion.item.selected.border.bottom = solarized["accent"]

## Top border color of the selected completion item.
## Type: QssColor
c.colors.completion.item.selected.border.top = solarized["accent"]

## Foreground color of the selected completion item.
## Type: QtColor
c.colors.completion.item.selected.fg = solarized["bg1"]

## Foreground color of the matched text in the completion.
## Type: QtColor
c.colors.completion.match.fg = solarized["accent"]

## Background color of the completion widget for odd rows.
## Type: QssColor
c.colors.completion.odd.bg = solarized["bg1"]

## Color of the scrollbar in the completion view.
## Type: QssColor
c.colors.completion.scrollbar.bg = solarized["bg2"]

## Color of the scrollbar handle in the completion view.
## Type: QssColor
c.colors.completion.scrollbar.fg = solarized["fg1"]

## Background color of disabled items in the context menu. If set to
## null, the Qt default is used.
## Type: QssColor
c.colors.contextmenu.disabled.bg = None

## Foreground color of disabled items in the context menu. If set to
## null, the Qt default is used.
## Type: QssColor
c.colors.contextmenu.disabled.fg = None

## Background color of the context menu. If set to null, the Qt default
## is used.
## Type: QssColor
c.colors.contextmenu.menu.bg = None

## Foreground color of the context menu. If set to null, the Qt default
## is used.
## Type: QssColor
c.colors.contextmenu.menu.fg = None

## Background color of the context menu's selected item. If set to null,
## the Qt default is used.
## Type: QssColor
c.colors.contextmenu.selected.bg = None

## Foreground color of the context menu's selected item. If set to null,
## the Qt default is used.
## Type: QssColor
c.colors.contextmenu.selected.fg = None

## Background color for the download bar.
## Type: QssColor
c.colors.downloads.bar.bg = solarized["bg1"]

## Background color for downloads with errors.
## Type: QtColor
c.colors.downloads.error.bg = solarized["red"]

## Foreground color for downloads with errors.
## Type: QtColor
c.colors.downloads.error.fg = solarized["bg1"]

## Color gradient start for download backgrounds.
## Type: QtColor
c.colors.downloads.start.bg = solarized["bg1"]

## Color gradient start for download text.
## Type: QtColor
c.colors.downloads.start.fg = solarized["fg1"]

## Color gradient stop for download backgrounds.
## Type: QtColor
c.colors.downloads.stop.bg = solarized["bg1"]

## Color gradient end for download text.
## Type: QtColor
c.colors.downloads.stop.fg = solarized["fg1"]

## Color gradient interpolation system for download backgrounds.
## Type: ColorSystem
## Valid values:
##   - rgb: Interpolate in the RGB color system.
##   - hsv: Interpolate in the HSV color system.
##   - hsl: Interpolate in the HSL color system.
##   - none: Don't show a gradient.
c.colors.downloads.system.bg = "none"

## Color gradient interpolation system for download text.
## Type: ColorSystem
## Valid values:
##   - rgb: Interpolate in the RGB color system.
##   - hsv: Interpolate in the HSV color system.
##   - hsl: Interpolate in the HSL color system.
##   - none: Don't show a gradient.
c.colors.downloads.system.fg = "none"

## Background color for hints. Note that you can use a `rgba(...)` value
## for transparency.
## Type: QssColor
c.colors.hints.bg = solarized["accent"]

## Font color for hints.
## Type: QssColor
c.colors.hints.fg = solarized["bg1"]

## Font color for the matched part of hints.
## Type: QtColor
c.colors.hints.match.fg = solarized["bg2"]

## CSS border value for hints.
## Type: String
c.hints.border = "1px solid " + solarized["accent"]

## Background color of the keyhint widget.
## Type: QssColor
# c.colors.keyhint.bg = 'rgba(0, 0, 0, 80%)'

## Text color for the keyhint widget.
## Type: QssColor
c.colors.keyhint.fg = solarized["fg1"]

## Highlight color for keys to complete the current keychain.
## Type: QssColor
c.colors.keyhint.suffix.fg = solarized["yellow"]

## Background color of an error message.
## Type: QssColor
c.colors.messages.error.bg = solarized["bg2"]

## Border color of an error message.
## Type: QssColor
c.colors.messages.error.border = solarized["red"]

## Foreground color of an error message.
## Type: QssColor
c.colors.messages.error.fg = solarized["red"]

## Background color of an info message.
## Type: QssColor
c.colors.messages.info.bg = solarized["bg2"]

## Border color of an info message.
## Type: QssColor
c.colors.messages.info.border = solarized["fg1"]

## Foreground color of an info message.
## Type: QssColor
c.colors.messages.info.fg = solarized["fg1"]

## Background color of a warning message.
## Type: QssColor
c.colors.messages.warning.bg = solarized["bg2"]

## Border color of a warning message.
## Type: QssColor
c.colors.messages.warning.border = solarized["orange"]

## Foreground color of a warning message.
## Type: QssColor
c.colors.messages.warning.fg = solarized["orange"]

## Background color for prompts.
## Type: QssColor
c.colors.prompts.bg = solarized["bg1"]

## Border used around UI elements in prompts.
## Type: String
c.colors.prompts.border = "1px solid " + solarized["bg2"]

## Foreground color for prompts.
## Type: QssColor
c.colors.prompts.fg = solarized["fg1"]

## Background color for the selected item in filename prompts.
## Type: QssColor
c.colors.prompts.selected.bg = solarized["accent"]

## Foreground color for the selected item in filename prompts.
## Type: QssColor
c.colors.prompts.selected.fg = solarized["bg1"]

## Background color of the statusbar in caret mode.
## Type: QssColor
c.colors.statusbar.caret.bg = solarized["magenta"]

## Foreground color of the statusbar in caret mode.
## Type: QssColor
c.colors.statusbar.caret.fg = solarized["bg1"]

## Background color of the statusbar in caret mode with a selection.
## Type: QssColor
c.colors.statusbar.caret.selection.bg = solarized["violet"]

## Foreground color of the statusbar in caret mode with a selection.
## Type: QssColor
c.colors.statusbar.caret.selection.fg = solarized["bg1"]

## Background color of the statusbar in command mode.
## Type: QssColor
c.colors.statusbar.command.bg = solarized["fg1"]

## Foreground color of the statusbar in command mode.
## Type: QssColor
c.colors.statusbar.command.fg = solarized["bg1"]

## Background color of the statusbar in private browsing + command mode.
## Type: QssColor
c.colors.statusbar.command.private.bg = solarized["fg1"]

## Foreground color of the statusbar in private browsing + command mode.
## Type: QssColor
c.colors.statusbar.command.private.fg = solarized["bg1"]

## Background color of the statusbar in insert mode.
## Type: QssColor
c.colors.statusbar.insert.bg = solarized["green"]

## Foreground color of the statusbar in insert mode.
## Type: QssColor
c.colors.statusbar.insert.fg = solarized["bg2"]

## Background color of the statusbar.
## Type: QssColor
c.colors.statusbar.normal.bg = solarized["bg2"]

## Foreground color of the statusbar.
## Type: QssColor
c.colors.statusbar.normal.fg = solarized["fg1"]

## Background color of the statusbar in passthrough mode.
## Type: QssColor
c.colors.statusbar.passthrough.bg = solarized["cyan"]

## Foreground color of the statusbar in passthrough mode.
## Type: QssColor
c.colors.statusbar.passthrough.fg = solarized["bg1"]

## Background color of the statusbar in private browsing mode.
## Type: QssColor
c.colors.statusbar.private.bg = solarized["bg2"]

## Foreground color of the statusbar in private browsing mode.
## Type: QssColor
c.colors.statusbar.private.fg = solarized["fg1"]

## Background color of the progress bar.
## Type: QssColor
c.colors.statusbar.progress.bg = solarized["fg1"]

## Foreground color of the URL in the statusbar on error.
## Type: QssColor
c.colors.statusbar.url.error.fg = solarized["red"]

## Default foreground color of the URL in the statusbar.
## Type: QssColor
c.colors.statusbar.url.fg = solarized["bg1"]

## Foreground color of the URL in the statusbar for hovered links.
## Type: QssColor
c.colors.statusbar.url.hover.fg = solarized["accent"]

## Foreground color of the URL in the statusbar on successful load
## (http).
## Type: QssColor
c.colors.statusbar.url.success.http.fg = solarized["fg1"]

## Foreground color of the URL in the statusbar on successful load
## (https).
## Type: QssColor
c.colors.statusbar.url.success.https.fg = solarized["green"]

## Foreground color of the URL in the statusbar when there's a warning.
## Type: QssColor
c.colors.statusbar.url.warn.fg = solarized["orange"]

## Background color of the tab bar.
## Type: QssColor
c.colors.tabs.bar.bg = solarized["bg2"]

## Background color of unselected even tabs.
## Type: QtColor
c.colors.tabs.even.bg = solarized["bg2"]

## Foreground color of unselected even tabs.
## Type: QtColor
c.colors.tabs.even.fg = solarized["fg1"]

## Color for the tab indicator on errors.
## Type: QtColor
c.colors.tabs.indicator.error = solarized["red"]

## Color gradient start for the tab indicator.
## Type: QtColor
c.colors.tabs.indicator.start = solarized["accent"]

## Color gradient end for the tab indicator.
## Type: QtColor
c.colors.tabs.indicator.stop = solarized["accent"]

## Color gradient interpolation system for the tab indicator.
## Type: ColorSystem
## Valid values:
##   - rgb: Interpolate in the RGB color system.
##   - hsv: Interpolate in the HSV color system.
##   - hsl: Interpolate in the HSL color system.
##   - none: Don't show a gradient.
c.colors.tabs.indicator.system = "none"

## Background color of unselected odd tabs.
## Type: QtColor
c.colors.tabs.odd.bg = solarized["bg2"]

## Foreground color of unselected odd tabs.
## Type: QtColor
c.colors.tabs.odd.fg = solarized["fg1"]

## Background color of pinned unselected even tabs.
## Type: QtColor
c.colors.tabs.pinned.even.bg = solarized["bg2"]

## Foreground color of pinned unselected even tabs.
## Type: QtColor
c.colors.tabs.pinned.even.fg = solarized["fg1"]

## Background color of pinned unselected odd tabs.
## Type: QtColor
c.colors.tabs.pinned.odd.bg = solarized["bg2"]

## Foreground color of pinned unselected odd tabs.
## Type: QtColor
c.colors.tabs.pinned.odd.fg = solarized["fg1"]

## Background color of pinned selected even tabs.
## Type: QtColor
c.colors.tabs.pinned.selected.even.bg = solarized["bg1"]

## Foreground color of pinned selected even tabs.
## Type: QtColor
c.colors.tabs.pinned.selected.even.fg = solarized["fg1"]

## Background color of pinned selected odd tabs.
## Type: QtColor
c.colors.tabs.pinned.selected.odd.bg = solarized["bg1"]

## Foreground color of pinned selected odd tabs.
## Type: QtColor
c.colors.tabs.pinned.selected.odd.fg = solarized["fg1"]

## Background color of selected even tabs.
## Type: QtColor
c.colors.tabs.selected.even.bg = solarized["bg1"]

## Foreground color of selected even tabs.
## Type: QtColor
c.colors.tabs.selected.even.fg = solarized["accent"]

## Background color of selected odd tabs.
## Type: QtColor
c.colors.tabs.selected.odd.bg = solarized["bg1"]

## Foreground color of selected odd tabs.
## Type: QtColor
c.colors.tabs.selected.odd.fg = solarized["accent"]

## Background color of tooltips. If set to null, the Qt default is used.
## Type: QssColor
c.colors.tooltip.bg = solarized["bg1"]

## Foreground color of tooltips. If set to null, the Qt default is used.
## Type: QssColor
c.colors.tooltip.fg = solarized["fg1"]

## Background color for webpages if unset (or empty to use the theme's
## color).
## Type: QtColor
c.colors.webpage.bg = solarized["bg1"]

## Which algorithm to use for modifying how colors are rendered with dark
## mode. The `lightness-cielab` value was added with QtWebEngine 5.14 and
## is treated like `lightness-hsl` with older QtWebEngine versions.
## Type: String
## Valid values:
##   - lightness-cielab: Modify colors by converting them to CIELAB color space and inverting the L value. Not available with Qt < 5.14.
##   - lightness-hsl: Modify colors by converting them to the HSL color space and inverting the lightness (i.e. the "L" in HSL).
##   - brightness-rgb: Modify colors by subtracting each of r, g, and b from their maximum value.
c.colors.webpage.darkmode.algorithm = "lightness-cielab"

## Contrast for dark mode. This only has an effect when
## `colors.webpage.darkmode.algorithm` is set to `lightness-hsl` or
## `brightness-rgb`.
## Type: Float
c.colors.webpage.darkmode.contrast = 0.0

## Render all web contents using a dark theme. On QtWebEngine < 6.7, this
## setting requires a restart and does not support URL patterns, only the
## global setting is applied. Example configurations from Chromium's
## `chrome://flags`: - "With simple HSL/CIELAB/RGB-based inversion": Set
## `colors.webpage.darkmode.algorithm` accordingly, and   set
## `colors.webpage.darkmode.policy.images` to `never`.  - "With selective
## image inversion": qutebrowser default settings.
## Type: Bool
c.colors.webpage.darkmode.enabled = False

## Which images to apply dark mode to.
## Type: String
## Valid values:
##   - always: Apply dark mode filter to all images.
##   - never: Never apply dark mode filter to any images.
##   - smart: Apply dark mode based on image content. Not available with Qt 5.15.0.
##   - smart-simple: On QtWebEngine 6.6, use a simpler algorithm for smart mode (based on numbers of colors and transparency), rather than an ML-based model. Same as 'smart' on older QtWebEnigne versions.
c.colors.webpage.darkmode.policy.images = "smart"

## Which pages to apply dark mode to. The underlying Chromium setting has
## been removed in QtWebEngine 5.15.3, thus this setting is ignored
## there. Instead, every element is now classified individually.
## Type: String
## Valid values:
##   - always: Apply dark mode filter to all frames, regardless of content.
##   - smart: Apply dark mode filter to frames based on background color.
c.colors.webpage.darkmode.policy.page = "smart"

## Threshold for inverting background elements with dark mode. Background
## elements with brightness above this threshold will be inverted, and
## below it will be left as in the original, non-dark-mode page. Set to
## 256 to never invert the color or to 0 to always invert it. Note: This
## behavior is the opposite of
## `colors.webpage.darkmode.threshold.foreground`!
## Type: Int
c.colors.webpage.darkmode.threshold.background = 128

## Threshold for inverting text with dark mode. Text colors with
## brightness below this threshold will be inverted, and above it will be
## left as in the original, non-dark-mode page. Set to 256 to always
## invert text color or to 0 to never invert text color.
## Type: Int
c.colors.webpage.darkmode.threshold.foreground = 128

## Value to use for `prefers-color-scheme:` for websites. The "light"
## value is only available with QtWebEngine 5.15.2+. On older versions,
## it is the same as "auto". The "auto" value is broken on QtWebEngine
## 5.15.2 due to a Qt bug. There, it will fall back to "light"
## unconditionally.
## Type: String
## Valid values:
##   - auto: Use the system-wide color scheme setting.
##   - light: Force a light theme.
##   - dark: Force a dark theme.
c.colors.webpage.preferred_color_scheme = "light"
