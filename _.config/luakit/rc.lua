require "lousy"
require "globals"

globals.homepage = "http://reddit.com/"
globals.useragent = ""
globals.editor = "vim"
globals.term = "urxvt"

dofile "/etc/xdg/luakit/rc.lua"

globals.homepage = "about:blank"

require "follow"
local s = follow.label_styles
--follow.label_maker = s.sort(s.reverse(s.charset("asdfqwerzxcv")))

require "window"
require "webview"
require "binds"

require "downloads"
require "downloads_chrome"
downloads.default_dir = os.getenv("HOME")

require "adblock"
require "adblock_chrome"

search_engines = {  aur = "https://aur.archlinux.org/packages.php?O=0&K=%s&do_Search=Go",
            duckduckgo  = "https://duckduckgo.com/?q=%s",
            github      = "https://github.com/search?q=%s",
            google      = "https://google.com/search?q=%s",
            imdb        = "http://www.imdb.com/find?s=all&q=%s",
            wikipedia   = "https://en.wikipedia.org/wiki/Special:Search?search=%s",
}

search_engines.default = search_engines.google

domain_props = { 
    ["all"] = {
        enable_scripts          = true,
        enable_plugins          = true,
        enable_private_browsing = false,
        user_stylesheet_uri     = "",
    },
}

local key = lousy.bind.key
add_binds("normal", {
    key({"Control"}, "p", "Begin private browsing",
        function (w) domain_props.all.enable_private_browsing = true end),
    key({"Mod1"}, "p", "End private browsing",
        function (w) domain_props.all.enable_private_browsing = false end),
    key({}, "d", "Close current tab (or `[count]` tabs).",
        function (w, m) for i=1,m.count do if w.tabs:count() > 1 then w:close_tab() end end end, {count=1}),
    key({}, "J", "Go to previous tab.",
        function (w) w:prev_tab() end),
    key({}, "K", "Go to next tab.",
        function (w) w:next_tab() end),
})
local cmd = lousy.bind.cmd
add_cmds({
    cmd("g[oogle]", "Search using google.",
        function (w, a) w:navigate(w:search_open(a)) end),
    cmd({"tabg[oogle]", "tg"}, "Search in a new tab using google.",
        function (w, a) w:new_tab(w:search_open(a)) end),
})

-- vim: et:sw=4:ts=8:sts=4:tw=80
