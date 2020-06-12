Config { font = "xft:Montserrat:size=12:"
       , additionalFonts = [ "xft:Montserrat:size=12:medium"
                           , "xft:Hack:size=10" ]
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 192
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Date "%Y-%m-%d %a %H:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ <fc=#ee9a00>%date%</fc>"
       }
