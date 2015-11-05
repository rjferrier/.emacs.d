These are my Emacs settings, designed to be ergonomic when Alt is
freed from Meta and added to the F key, and F is remapped to Caps.

The rationale is that Emacs modifier keys are used so often that,
assuming you are right-handed, they should be positioned directly
under the strongest fingers on your left hand when they are at rest.
Left-Alt (corresponding to the thumb) seems like a logical choice on
laptops and certain ergonomic keyboards where this key is not
positioned too far along to the left.  However, it is possibly not
good for the thumb joint if it is used all the time.  One solution
might be to also make the left index finger (the F key) into a
modifier.

These modifier keys can be used to provide alternatives to the
standard Emacs bindings.  Unfortunately Emacs and its various modes
already make extensive use of Control and Meta modifiers.  Rebinding
these would mean trampling over some potentially useful existing
bindings.  Alt can still be used, however.  With the help of Xmodmap
(Linux) or AutoHotKey (Windows), Alt can be mapped to F and Meta can
be moved to the left-hand Windows key.

If the keys directly above or to the left of modifier keys are
reserved for prefix keys, and the keys on the right hand side of the
keyboard are used as suffix keys, then any of the combinations
```
MOD-SUFFIX
MOD-PREFIX MOD-SUFFIX
MOD-PREFIX SUFFIX
```
can be executed rapidly.  (One can support more commands with
`MOD-PREFIX ANY`, but these will be slower if `ANY` is on the left
hand side.)

For example, to move the cursor, hold down F and navigate with I, J, K
and L.  For larger movements, prefix with F-D.  To select text, use
the shift key with these operations.  Continue holding down F and
press Period, Comma or M to cut, copy or paste.

F needs to be remapped to Caps Lock.  This feels strange at first, but
it is not too difficult for muscle memory to adjust to the new keymap
and back again.

Below is a sample Xmodmap script that I use for my own laptop.

```
! This is an Xmodmap script.  When stored as ~/.Xmodmap, the keys
! will be set upon every login.  This is worth keeping in mind if
! things go wrong.  Use setxkbmap to reset the keys.

! Keys are remapped as follows:
! F, Alt  -> Alt
! Caps    -> F
! Alt Gr  -> Caps
! Windows -> Meta

clear Lock
clear Mod1
clear Mod3
clear Mod4

keycode 41 = Alt_L
keycode 66 = f
keycode 64 = Alt_L
keycode 108 = Caps_Lock
keycode 133 = Meta_L

add Lock = Caps_Lock
add Mod1 = Alt_L
add Mod3 = Meta_L
add Mod4 = Super_L Hyper_L
```

