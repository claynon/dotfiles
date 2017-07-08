-------------------------------------------------------------------------------
-- xmonad.hs for xmonad-darcs
-- Author: Øyvind 'Mr.Elendig' Heggstad <mrelendig AT har-ikkje DOT net>
-------------------------------------------------------------------------------
-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86
import Data.List

-- actions
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = xmonad =<< statusBar {-cmd-} "xmobar" pp kb conf
  where 
--    uhook = withUrgencyHookC NoUrgencyHook urgentConfig
    cmd = "bash -c \"te97ffbc5eddf1fcdf01530eac0dffb628fe23d1a8e >(xmobar -x0) | xmobar -x1\""
    pp = customPP
    kb = toggleStrutsKey
--    conf = uhook myConfig
    conf = withUrgencyHookC NoUrgencyHook urgentConfig myConfig
--    conf = myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = borderWidth'
                         , normalBorderColor = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , focusFollowsMouse = False
                         , terminal = terminal'
                         , keys = keys'
                         , layoutHook = layoutHook'
                         , manageHook = manageSpawn <+> manageHook' <+> manageHook defaultConfig
                         , handleEventHook = fullscreenEventHook
                         , startupHook = startupHook'
                         }

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen             --> doFullFloat,
                           className =? "Vlc"       --> doFloat,
                           insertPosition Below Newer,
                           transience',
                           manageIdeaCompletionWindow
                         ]

{- IntelliJ popup fix from http://youtrack.jetbrains.com/issue/IDEA-74679#comment=27-417315 -}
{- and http://youtrack.jetbrains.com/issue/IDEA-101072#comment=27-456320 -}
(~=?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~=? x = fmap (isPrefixOf x) q

manageIdeaCompletionWindow = (className ~=? "jetbrains-") <&&> (title ~=? "win") --> doIgnore

-------------------------------------------------------------------------------
-- Startup --
startupHook' = do
  setWMName "LG349fd932D"
  spawn "xscreensaver"
  spawn "feh --bg-fill /home/claynon/Images/t3_5wahq5.jpe"
  spawn "/home/claynon/.screenlayout/trabalho.sh"
--  spawnOn "term" "termite"
--  spawnOn "web" "google-chrome-stable"
--  spawnOn "dev" "emacs"


-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#01c9c9" "" . wrap "<" ">"
                     , ppHidden = xmobarColor "#008080" ""
                     , ppHiddenNoWindows = xmobarColor "#004b4b" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]" 
                     , ppLayout = xmobarColor "#008080" ""
                     , ppTitle =  xmobarColor "#ebebeb" "" . shorten 80
                     , ppSep = xmobarColor "#01c9c9" "" " | "
                     }
-- GridSelect
myGSConfig = defaultGSConfig { gs_cellwidth = 160 }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 2
normalBorderColor'  = "#333333"
focusedBorderColor' = "#AFAF87"

-- tabs
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

-- workspaces
workspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- layouts
layoutHook' = tile ||| mtile ||| tab ||| full
  where
    rt = ResizableTall 1 (2/100) (1/2) []
    tile = renamed [Replace "[]="] $ smartBorders rt
    mtile = renamed [Replace "M[]="] $ smartBorders $ Mirror rt
    tab = renamed [Replace "T"] $ noBorders $ tabbed shrinkText tabTheme1
    full = renamed [Replace "[]"] $ noBorders Full 

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "gnome-terminal"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), safeSpawn (XMonad.terminal conf) []) 
    , ((modMask,               xK_p     ), safeSpawn "dmenu_run" []) 
    , ((modMask .|. shiftMask, xK_p     ), safeSpawn "gmrun" [])
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask              , xK_z     ), spawn "xscreensaver-command --lock")
      
    -- multimedia
{-- Alsa mixer bindings
    , ((0, xF86XK_AudioRaiseVolume      ), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
    , ((0, xF86XK_AudioLowerVolume      ), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
    , ((0, xF86XK_AudioMute             ), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
--}
    , ((0, xF86XK_AudioRaiseVolume      ), safeSpawn "ponymix" ["increase", "10"])
    , ((0, xF86XK_AudioLowerVolume      ), safeSpawn "ponymix" ["decrease", "10"])
    , ((0, xF86XK_AudioMute             ), safeSpawn "ponymix" ["toggle"])
    , ((0, xF86XK_AudioPlay             ), safeSpawn "playerctl" ["play-pause"])
--    , ((0, xF86XK_AudioPlay             ), safeSpawn "dbus" ["--print-reply", "--dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"])
    , ((0, xF86XK_AudioNext             ), safeSpawn "playerctl" ["next"])
    , ((0, xF86XK_AudioPrev             ), safeSpawn "playerctl" ["previous"])

    -- brightness
    , ((0, xF86XK_MonBrightnessUp       ), safeSpawn "xbacklight" ["-inc", "10"])
    , ((0, xF86XK_MonBrightnessDown     ), safeSpawn "xbacklight" ["-dec", "10"])     
      
    -- grid
    , ((modMask,               xK_g     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0,1,2]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------


-- -- Imports.
-- import XMonad
-- import XMonad.Config.Desktop
-- import XMonad.Hooks.EwmhDesktops
-- import XMonad.Hooks.DynamicLog
-- import XMonad.Util.EZConfig(additionalKeys)
-- import Graphics.X11.ExtraTypes.XF86
-- import System.IO

-- -- The main function.
-- main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- -- Command to launch the bar.
-- myBar = "xmobar"

-- -- Custom PP, configure it as you like. It determines what is being written to the bar.
-- myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- -- Key binding to toggle the gap for the bar.
-- toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- -- Main configuration, override the defaults to your liking.
-- myConfig = defaultConfig
--   { terminal        = myTerminal
--   , modMask         = myModMask
--   , borderWidth     = myBorderWidth
--   , handleEventHook = myHandleEventHook
--   }

-- myTerminal        = "terminator"
-- myModMask         = mod4Mask -- Win key or Super_L
-- myBorderWidth     = 0
-- myHandleEventHook = fullscreenEventHook

-- myAdditionalKeys =   [ ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
--                      , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
--                      , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
--                      ]
