These are my Emacs settings, designed to be ergonomic when Hyper is
remapped to the F key, F to Caps, and Alt is freed from Meta.

The rationale is that Emacs modifier keys are used so often that,
assuming you are right-handed, they should be positioned directly
under the strongest fingers on your left hand when they are at rest.
Left-Alt (corresponding to the thumb) and (F to the index finger) seem
like a logical choice.  Having the thumb strike Left-Alt works best on
laptops and certain ergonomic keyboards where this key is not
positioned too far along to the left.

These two modifier keys can be used to provide alternatives to the
standard Emacs bindings.  Unfortunately Emacs and its various modes
already make extensive use of Control and Meta modifiers.  Rebinding
these would mean trampling over some potentially useful existing
bindings.  Hyper and Alt can still be used, however.  With the help of
Xmodmap (Linux) or AutoHotKey (Windows), Hyper can be mapped to F and
Meta can be moved off the left-hand Alt to the left-hand Windows key.

If the keys directly above or to the left of these modifier keys are
reserved for prefix keys, and the keys on the right hand side of the
keyboard are used as suffix keys, then any of the combinations
```
MOD-SUFFIX
MOD-PREFIX MOD-SUFFIX
MOD-PREFIX SUFFIX
```
can be executed rapidly.

For example, to move the cursor, hold down F and navigate with I, J, K
and L.  For larger movements, hold down Alt as well.  To begin
selecting text, hold down F, optionally hold down Alt, and tap D with
the middle finger.  Keep holding down F and press Period, Comma or M
to cut, copy or paste.

F can be remapped to Caps Lock.  This feels strange at first, but it
is not too difficult for muscle memory to adjust to the new keymap and
back again.

Below are sample AutoHotKey and Xmodmap scripts that I use for my own
laptop.  A warning: Xmodmap is deprecated and buggy.  I found that for
stability I needed to (a) regroup the modifiers in a certain order and
(b) set the keys by keycode rather than keysym.  This means the
directives are machine-dependent.  You will want to experiment with
the help of xev.

```
;; This is an AutoHotkey script.

;; Map the F key to the unused Menu key.  This will in turn be
;; remapped to Hyper in Emacs.  The Caps key will be the new F key.
f::AppsKey
CapsLock::f
AppsKey::CapsLock

;; cancel this script with Ctrl-Alt-0
^!0::ExitApp
```

```
! This is an Xmodmap script.  When stored as ~/.Xmodmap, the keys
! will be set upon every login.  This is worth keeping in mind if
! things go wrong.  Use setxkbmap to reset the keys.

! begin by clearing the CapsLock modifier group 
clear Lock

! Modifier group Mod4 usually contains Super and/or Hyper; we shall
! remove Hyper and put it in group Mod3.
remove Mod4 = Hyper_L
add Mod3 = Hyper_L

! Mod1 usually contains Alt and/or Meta; we shall remove Meta and put 
! it in group Mod4.
remove Mod1 = Meta_L
add Mod4 = Meta_L

! Xmodmap sometimes moves Alt and Meta together for no good reason.
! Reset Alt to Mod1.
remove Mod4 = Alt_L
add Mod1 = Alt_L

! We won't need the Super modifiers, but removing them completely
! causes weird things to happen.  Put them with AltGr.
remove Mod4 = Super_L Super_R
add Mod5 = Super_L Super_R

! Now map the F key to Hyper, the Alt key to Alt, the Caps key to F
! and the Windows key to Meta
keycode 41 = Hyper_L
keycode 64 = Alt_L
keycode 66 = f
keycode 133 = Meta_L
```

