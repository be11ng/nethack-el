;; cmd.c is where the command-key mappings are done in the nh src

(and (not nethack-mode-keymap)
     (defvar nethack-mode-keymap
       (make-sparse-keymap 'nethack-mode-keymap))

     ;;^       Show the type of a trap
     (define-key nethack-mode-keymap "^" 'nethack-command-identify-trap)

     ;;^[ Cancel command (NOTE: cant use "C-[" in Emacs, this generates an ESC)
     (define-key nethack-mode-keymap "\M-[" 'nethack-command-cancel)

     ;;^A      Redo the previous command
     (define-key nethack-mode-keymap "\C-a" 'nethack-command-redo-previous)

     ;;^C      Quit the game  (NOTE: dont want to override C-c prefix)
     (define-key nethack-mode-keymap "\C-c\C-c" 'nethack-command-quit-game)

     ;;^D      Kick something (usually a door, chest, or box)
     (define-key nethack-mode-keymap "\C-d" 'nethack-command-kick)

     ;;^E      Search a room (available in debug mode only)
     (define-key nethack-mode-keymap "\C-e" 'nethack-command-wizard-search)

     ;;^F      Map the level (available in debug mode only)
     (define-key nethack-mode-keymap "\C-f" 'nethack-command-wizard-map)

     ;;^G      Create a monster (available in debug mode only)
     (define-key nethack-mode-keymap "\C-g" 'nethack-command-create-monster)

     ;;^I      Identify all items (available in debug mode only)
     (define-key nethack-mode-keymap "\C-i" 'nethack-command-identify-all-items)

     ;;^O      Show location of special levels (available in debug mode only)
     (define-key nethack-mode-keymap "\C-o" 'nethack-command-wizard-show-location)

     ;;^P      Toggle through previously displayed game messages
     (define-key nethack-mode-keymap "\C-p" 'nethack-command-previous-message)

     ;;^R      Redraw screen
     (define-key nethack-mode-keymap "\C-r" 'nethack-command-redraw-screen)

     ;;^T      Teleport around level
     (define-key nethack-mode-keymap "\C-t" 'nethack-command-teleport-around-level)

     ;;^V      Teleport between levels (available in debug mode only)
     (define-key nethack-mode-keymap "\C-v" 'nethack-command-wizard-teleport-between-levels)

     ;;^W      Wish (available in debug mode only)
     (define-key nethack-mode-keymap "\C-w" 'nethack-command-wizard-wish)

     ;;^X      Show your intrinsic attributes (in debug or explore mode only)
     ;;      NOTE: dont want to override C-x prefix
     ;;(define-key nethack-mode-keymap "\C-x" 'nethack-command-wizard-show-instrinsic-attributes)

     ;;^Z      Suspend game (only if defined) (NOTE: this is useless in emacs i think)
     ;;(define-key nethack-mode-keymap "\C-z" 'nethack-command-suspend-game)

     ;;a       Apply (use) a tool
     (define-key nethack-mode-keymap "a" 'nethack-command-apply)

     ;;A       Remove all armor
     (define-key nethack-mode-keymap "A" 'nethack-command-remove-all-armor)

     ;;b       Go southwest 1 space
     (define-key nethack-mode-keymap "b" 'nethack-command-southwest-one-space)

     ;;B       Go southwest until you are on top of something
     (define-key nethack-mode-keymap "B" 'nethack-command-southwest-until-ontop)

     ;;^B      Go southwest until you are near something
     (define-key nethack-mode-keymap "\C-B" 'nethack-command-southwest-until-near)

     ;;c       Close a door
     (define-key nethack-mode-keymap "c" 'nethack-command-close-door)

     ;;C       Call (name) a particular monster
     (define-key nethack-mode-keymap "C" 'nethack-command-call-monster)

     ;;d       Drop an item
     (define-key nethack-mode-keymap "d" 'nethack-command-drop)

     ;;D       Drop specific item types
     (define-key nethack-mode-keymap "D" 'nethack-command-drop-specific-item)

     ;;e       Eat something
     (define-key nethack-mode-keymap "e" 'nethack-command-eat)

     ;;E       Engrave writing on the floor
     (define-key nethack-mode-keymap "E" 'nethack-command-engrave)

     ;;f       Fire ammunition from quiver
     (define-key nethack-mode-keymap "f" 'nethack-command-fire)

     ;;F       Followed by direction, fight a monster (even if you don't sense it)
     (define-key nethack-mode-keymap "F" 'nethack-command-force-fight)

     ;;g       Followed by direction, move until you are near something
     (define-key nethack-mode-keymap "g" 'nethack-command-move-until-near)

     ;;G       Followed by direction, same as control-direction
     (define-key nethack-mode-keymap "G" 'nethack-command-move)

     ;;h       Go west 1 space
     (define-key nethack-mode-keymap "h" 'nethack-command-west-one-space)

     ;;H       Go west until you are on top of something
     (define-key nethack-mode-keymap "H" 'nethack-command-west-until-ontop)

     ;;^H      Go west until you are near something (NOTE: what to do with this key?)
     (define-key nethack-mode-keymap "\C-h" 'nethack-command-west-until-near)

     ;;i       Show your inventory
     (define-key nethack-mode-keymap "i" 'nethack-command-inventory)

     ;;I       Inventory specific item types
     (define-key nethack-mode-keymap "I" 'nethack-command-type-inventory)

     ;;j       Go south 1 space (or if number_pad is on, jump to another location)
     (define-key nethack-mode-keymap "j" 'nethack-command-south)

     ;;J       Go south until you are on top of something
     (define-key nethack-mode-keymap "J" 'nethack-command-south-until-ontop)

     ;;^J      Go south until you are near something
     (define-key nethack-mode-keymap "\C-J" 'nethack-command-south-until-near)

     ;;k       Go north 1 space (or if number_pad is on, kick something)
     (define-key nethack-mode-keymap "k" 'nethack-command-north)

     ;;K       Go north until you are on top of something
     (define-key nethack-mode-keymap "K" 'nethack-command-north-until-ontop)

     ;;^K      Go north until you are near something
     (define-key nethack-mode-keymap "\C-K" 'nethack-command-north-until-near)

     ;;l       Go east 1 space (or if number_pad is on, loot a box on the floor)
     (define-key nethack-mode-keymap "l" 'nethack-command-east)

     ;;L       Go east until you are on top of something
     (define-key nethack-mode-keymap "L" 'nethack-command-east-until-ontop)

     ;;^L      Go east until you are near something
     (define-key nethack-mode-keymap "\C-L" 'nethack-command-east-until-near)

     ;;m       Followed by direction, move without picking anything up or fighting
     (define-key nethack-mode-keymap "m" 'nethack-command-move-no-pickup-or-fight)

     ;;M       Followed by direction, move a distance without picking anything up
     (define-key nethack-mode-keymap "M" 'nethack-command-move-distance-no-pickup)

     ;;n       Go southeast 1 space
     (define-key nethack-mode-keymap "n" 'nethack-command-southeast)

     ;;N       Go southeast until you are on something (if number_pad, name an object)
     (define-key nethack-mode-keymap "N" 'nethack-command-southeast-until-ontop)

     ;;^N      Go southeast until you are near something
     (define-key nethack-mode-keymap "\C-N" 'nethack-command-southeast-until-near)

     ;;o       Open a door
     (define-key nethack-mode-keymap "o" 'nethack-command-open)

     ;;O       Show option settings, possibly change them
     (define-key nethack-mode-keymap "O" 'nethack-command-settings)

     ;;p       Pay your shopping bill
     (define-key nethack-mode-keymap "p" 'nethack-command-pay)

     ;;P       Put on an accessory (ring, amulet, etc)
     (define-key nethack-mode-keymap "P" 'nethack-command-put-on)

     ;;q       Quaff (drink) something
     (define-key nethack-mode-keymap "q" 'nethack-command-quaff)

     ;;Q       Select ammunition for quiver
     (define-key nethack-mode-keymap "Q" 'nethack-command-select-ammo-for-quiver)

     ;;r       Read a scroll or spellbook
     (define-key nethack-mode-keymap "r" 'nethack-command-read)

     ;;R       Remove an accessory (ring, amulet, etc)
     (define-key nethack-mode-keymap "R" 'nethack-command-remove-accessory)

     ;;s       Search for traps and secret doors
     (define-key nethack-mode-keymap "s" 'nethack-command-search)

     ;;S       Save the game
     (define-key nethack-mode-keymap "S" 'nethack-command-save-game)

     ;;t       Throw something
     (define-key nethack-mode-keymap "t" 'nethack-command-throw)

     ;;T       Take off one piece of armor
     (define-key nethack-mode-keymap "T" 'nethack-command-remove-single-armor)

     ;;u       Go northeast 1 space (or if number_pad is on, untrap something)
     (define-key nethack-mode-keymap "u" 'nethack-command-northeast-one-space)

     ;;U       Go northeast until you are on top of something
     (define-key nethack-mode-keymap "U" 'nethack-command-northeast-until-ontop)

     ;;^U      Go northeast until you are near something
     (define-key nethack-mode-keymap "\C-U" 'nethack-command-northeast-until-near)

     ;;v       Show version
     (define-key nethack-mode-keymap "v" 'nethack-command-version)

     ;;V       Show long version and game history
     (define-key nethack-mode-keymap "V" 'nethack-command-version-and-history)

     ;;w       Wield (put in use) a weapon
     (define-key nethack-mode-keymap "w" 'nethack-command-wield)

     ;;W       Wear a piece of armor
     (define-key nethack-mode-keymap "W" 'nethack-command-wear-armor)

     ;;x       Swap wielded and secondary weapons
     (define-key nethack-mode-keymap "x" 'nethack-command-swap-weapons)

     ;;X       Enter explore (discovery) mode (only if defined)
     (define-key nethack-mode-keymap "X" 'nethack-command-explore-mode)

     ;;y       Go northwest 1 space
     (define-key nethack-mode-keymap "y" 'nethack-command-northwest-one-space)

     ;;Y       Go northwest until you are on top of something
     (define-key nethack-mode-keymap "Y" 'nethack-command-northwest-until-ontop)

     ;;^Y      Go northwest until you are near something
     (define-key nethack-mode-keymap "" 'nethack-command-northwest-until-near)

     ;;z       Zap a wand
     (define-key nethack-mode-keymap "z" 'nethack-command-zap-wand)

     ;;Z       Zap (cast) a spell
     (define-key nethack-mode-keymap "Z" 'nethack-command-cast-spell)

     ;;<       Go up a staircase
     (define-key nethack-mode-keymap "<" 'nethack-command-up)

     ;;>       Go down a staircase
     (define-key nethack-mode-keymap ">" 'nethack-command-down)

     ;;/       Show what type of thing a symbol corresponds to
     (define-key nethack-mode-keymap "/" 'nethack-command-what-is-symbol)

     ;;?       Give a help message
     (define-key nethack-mode-keymap "?" 'nethack-command-help)

     ;;&       Tell what a command does
     (define-key nethack-mode-keymap "&" 'nethack-command-command-help)

     ;;!       Do a shell escape (only if defined)
     (define-key nethack-mode-keymap "!" 'nethack-command-shell-escape)

     ;;\       Show what object types have been discovered
     (define-key nethack-mode-keymap "\\" 'nethack-command-show-discoveries)

     ;;.       Rest one move while doing nothing
     (define-key nethack-mode-keymap "." 'nethack-command-rest-one-move)

     ;;        Rest one move while doing nothing (if rest_on_space option is on)
     (define-key nethack-mode-keymap " " 'nethack-command-rest-one-move)

     ;; :       Look at what is on the floor
     (define-key nethack-mode-keymap ":" 'nethack-command-look-here)

     ;; ;       Show what type of thing a map symbol on the level corresponds to
     (define-key nethack-mode-keymap ";" 'nethack-command-what-is-map-piece)

     ;;,       Pick up things at the current location
     (define-key nethack-mode-keymap "," 'nethack-command-pick-up)

     ;;@       Toggle the pickup option on/off
     (define-key nethack-mode-keymap "@" 'nethack-command-toggle-pickup)

     ;;)       Show the weapon currently wielded
     (define-key nethack-mode-keymap ")" 'nethack-command-show-wielded-weapon)

     ;;[       Show the armor currently worn
     (define-key nethack-mode-keymap "[" 'nethack-command-show-worn-armor)

     ;;=       Show the ring(s) currently worn
     (define-key nethack-mode-keymap "=" 'nethack-command-show-worn-rings)

     ;;"       Show the amulet currently worn
     (define-key nethack-mode-keymap "\"" 'nethack-command-show-worn-amulet)

     ;;(       Show the tools currently in use
     (define-key nethack-mode-keymap "(" 'nethack-command-show-tool-in-use)

     ;;*       Show all equipment in use (combination of the ),[,=,",( commands)
     (define-key nethack-mode-keymap "*" 'nethack-command-show-all-equipment-in-use)

     ;;$       Count your gold
     (define-key nethack-mode-keymap "$" 'nethack-command-count-gold)

     ;;+       List known spells
     (define-key nethack-mode-keymap "+" 'nethack-command-list-known-spells)

     ;;#       Perform an extended command
     ;; (define-key nethack-mode-keymap "#" 'nethack-command-extended-comm)

     ;;M-a     Adjust inventory letters
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-c     Talk to someone
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-d     Dip an object into something
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-e     Advance or check weapons skills
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-f     Force a lock
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-i     Invoke an object's special powers
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-j     Jump to another location
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-l     Loot a box on the floor
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-m     Use a monster's special ability
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-n     Name an item or type of object
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-o     Offer a sacrifice to the gods
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-p     Pray to the gods for help
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-q     Quit
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-r     Rub a lamp
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-s     Sit down
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-t     Turn undead
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-u     Untrap something (trap, door, or chest)
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-v     Print compile time options for this version of NetHack
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)

     ;;M-w     Wipe off your face
     ;;(define-key nethack-mode-keymap "" 'nethack-command-)
     )
