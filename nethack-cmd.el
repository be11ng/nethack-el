;;; FIXME: add copyright
;;; cmd.c is the cheat sheet for this file

(defmacro defun-nethack-command (fun docstr cmdstr)
  "Define an interactive nethack command."
  `(defun ,(intern (concat "nethack-command-" (symbol-name fun))) (&optional count)
     ,docstr
     (interactive "p")
     (nethack-handle-command ,cmdstr count)))

(defun-nethack-command identify-trap "Show the type of a trap" "idtrap") ;^
(defun-nethack-command apply "Apply (use) a tool" "apply") ;a
(defun-nethack-command remove-all-armor "Remove all armor" "remarm") ;A
(defun-nethack-command close-door "Close a door" "close") ;c
(defun-nethack-command drop "Drop an item" "drop") ;d
(defun-nethack-command drop-specific-item "Drop specific item types" "ddrop") ;D
(defun-nethack-command kick "Kick" "kick") ;^D
(defun-nethack-command eat "Eat something" "eat") ;e
(defun-nethack-command engrave "Engrave writing on the floor" "engrave") ;E
(defun-nethack-command fire "Fire ammunition from quiver" "fire") ;f
(defun-nethack-command west "Go west 1 space" "gowest") ;h
(defun-nethack-command west-until-ontop "Go west until you are on top of something" "gowestontop") ;H
(defun-nethack-command west-until-near "Go west until you are near something" "gowestnear") ;^H
(defun-nethack-command inventory "Show your inventory" "inv") ;i
(defun-nethack-command type-inventory "Inventory specific item types" "typeinv") ;I
(defun-nethack-command south "Go south 1 space (or if number_pad is on, jump to another location)" "gosouth") ;j
(defun-nethack-command south-until-ontop "Go south until you are on top of something" "gosouthontop") ;J
(defun-nethack-command south-until-near "Go south until you are near something" "gosouthnear") ;^J
(defun-nethack-command north "Go north 1 space (or if number_pad is on, kick something)" "gonorth") ;k
(defun-nethack-command north-until-ontop "Go north until you are on top of something" "gonorthontop") ;K
(defun-nethack-command north-until-near "Go north until you are near something" "gonorthnear") ;^K
(defun-nethack-command east "Go east 1 space (or if number_pad is on, loot a box on the floor)" "goeast") ;l
(defun-nethack-command east-until-ontop "Go east until you are on top of something" "goeastontop") ;L
(defun-nethack-command east-until-near "Go east until you are near something" "goeastnear") ;^L
(defun-nethack-command southwest "Go southeast 1 space" "gosouthwest") ;b
(defun-nethack-command southeast "Go southeast 1 space" "gosoutheast") ;n
(defun-nethack-command southeast-until-ontop "Go southeast until you are on something (if number_pad, name an object)" "gosoutheastontop") ;N
(defun-nethack-command southeast-until-near "Go southeast until you are near something" "gosoutheastnear") ;^N
(defun-nethack-command open "Open a door" "open") ;o
(defun-nethack-command settings "Show option settings, possibly change them" "set") ;O
(defun-nethack-command pay "Pay your shopping bill" "pay") ;p
(defun-nethack-command put-on "Put on an accessory (ring, amulet, etc)" "puton") ;P
(defun-nethack-command quaff "Quaff (drink) something" "drink") ;q
(defun-nethack-command select-ammo-for-quiver "Select ammunition for quiver" "wieldquiver") ;Q
(defun-nethack-command read "Read a scroll or spellbook" "read") ;r
(defun-nethack-command remove-accessory "Remove an accessory (ring, amulet, etc)" "remring") ;R
(defun-nethack-command search "Search for traps and secret doors" "search") ;s
(defun-nethack-command save-game "Save the game" "save") ;S
(defun-nethack-command throw "Throw something" "throw") ;t
(defun-nethack-command remove-single-armor "Take off one piece of armor" "takeoff") ;T
(defun-nethack-command northeast "Go northeast 1 space (or if number_pad is on, untrap something)" "gonortheast") ;u
(defun-nethack-command northeast-until-ontop "Go northeast until you are on top of something" "gonortheastontop") ;U
(defun-nethack-command northeast-until-near "Go northeast until you are near something" "gonortheastnear") ;^U
(defun-nethack-command version "Show version" "version") ;v
(defun-nethack-command version-and-history "Show long version and game history" "history") ;V
(defun-nethack-command wield "Wield (put in use) a weapon" "wield") ;w
(defun-nethack-command wear-armor "Wear a piece of armor" "wear") ;W
(defun-nethack-command swap-weapons "Swap wielded and secondary weapons" "swapweapon") ;x
(defun-nethack-command explore-mode "Enter explore (discovery) mode (only if defined)" "enter_explore_mode") ;X
(defun-nethack-command northwest "Go northwest 1 space" "gonorthwest") ;y
(defun-nethack-command northwest-until-ontop "Go northwest until you are on top of something" "gonorthwestontop") ;Y
(defun-nethack-command northwest-until-near "Go northwest until you are near something" "gonorthwestnear") ;^Y
(defun-nethack-command zap-wand "Zap a wand" "zap") ;z
(defun-nethack-command cast-spell "Zap (cast) a spell" "cast") ;Z
(defun-nethack-command up "Go up a staircase" "up") ;<
(defun-nethack-command down "Go down a staircase" "down") ;>
(defun-nethack-command what-is-symbol "Show what type of thing a symbol corresponds to" "whatis") ;/
(defun-nethack-command help "Give a help message" "help") ;?
(defun-nethack-command command-help "Tell what a command does" "whatdoes") ;&
(defun-nethack-command shell-escape "Do a shell escape (only if defined)" "sh") ;!
(defun-nethack-command show-discoveries "Show what object types have been discovered" "discovered") ;\
(defun-nethack-command rest-one-move "Rest one move while doing nothing" "null") ;.
(defun-nethack-command look-here "Look at what is on the floor" "look") ;:
(defun-nethack-command what-is-map-piece "Show what type of thing a map symbol on the level corresponds to" "quickwhatis") ;;
(defun-nethack-command pick-up "Pick up things at the current location" "pickup") ;,
(defun-nethack-command toggle-pickup "Toggle the pickup option on/off" "togglepickup") ;@
(defun-nethack-command show-all-equipment-in-use "Show all equipment in use (combination of the ),[,=,\",( commands)" "prinuse") ;*
(defun-nethack-command count-gold "Count your gold" "countgold") ;$
(defun-nethack-command list-known-spells "List known spells" "listspells") ;+
;;; wizard (debug) mode only commands:
(defun-nethack-command wizard-search "Search a room (available in debug mode only)" "wizardsearch") ;^E
(defun-nethack-command wizard-map "Map the level (available in debug mode only)" "wizardmap") ;^F
(defun-nethack-command wizard-show-location "Show location of special levels (available in debug mode only)" "wizardshowlocation") ;^O
(defun-nethack-command wizard-teleport-between-levels "Teleport between levels (available in debug mode only)" "wizardteleportbetweenlevels") ;^V
(defun-nethack-command wizard-wish "Wish (available in debug mode only)" "wizardwish") ;^W
;; Extended commands
(defun-nethack-command adjust "adjust inventory letters." "adjust")
(defun-nethack-command chat "talk to someone." "chat")
(defun-nethack-command conduct "list which challenges you have adhered to." "conduct")
(defun-nethack-command dip "dip an object into something." "dip")
(defun-nethack-command enhance "advance or check weapons skills." "enhance")
(defun-nethack-command force "force a lock." "force")
(defun-nethack-command invoke "invoke an object's powers." "invoke")
(defun-nethack-command jump "jump to a location." "jump")
(defun-nethack-command loot "loot a box on the floor." "loot")
(defun-nethack-command monster "use a monster's special ability." "monster")
(defun-nethack-command name "name an item or type of object." "name")
(defun-nethack-command offer "offer a sacrifice to the gods." "offer")
(defun-nethack-command pray "pray to the gods for help." "pray")
(defun-nethack-command quit "exit without saving current game." "quit")
(defun-nethack-command ride "ride (or stop riding) a monster." "ride")
(defun-nethack-command rub "rub a lamp." "rub")
(defun-nethack-command sit "sit down." "sit")
(defun-nethack-command turn "turn undead." "turn")
(defun-nethack-command twoweapon "toggle two-weapon combat." "twoweapon")
(defun-nethack-command untrap "untrap something." "untrap")
(defun-nethack-command version "list compile time options for this version of NetHack." "version")
(defun-nethack-command wipe "wipe off your face." "wipe")

;;; FIXME: defun these:
;; (defun-nethack-command redo-previous "Redo the previous command" ???) ;^A
;; (defun-nethack-command create-monster "Create a monster (available in debug mode only)" ???) ;^G
;; (defun-nethack-command identify-all-items "Identify all items (available in debug mode only)" ???) ;^I
;; (defun-nethack-command previous-message "Toggle through previously displayed game messages" ???) ;^P
;; (defun-nethack-command redraw-screen "Redraw screen" ???) ;^R
;; (defun-nethack-command teleport-around-level "Teleport around level" ???) ;^T
;; (defun-nethack-command suspend-game "Suspend game (only if defined)" ???) ;^Z
;; (defun-nethack-command cancel "Cancel command" ???) ;^[
;; (defun-nethack-command southwest-one-space "Go southwest 1 space" ???) ;b
;; (defun-nethack-command southwest-until-ontop "Go southwest until you are on top of something" ???) ;B
;; (defun-nethack-command southwest-until-near "Go southwest until you are near something" ???) ;^B
;; (defun-nethack-command call-monster "Call (name) a particular monster" ???) ;C
;; (defun-nethack-command force-fight "Followed by direction, fight a monster (even if you don't sense it)" ???) ;F
;; (defun-nethack-command move-until-near "Followed by direction, move until you are near something" ???) ;g
;; (defun-nethack-command move "Followed by direction, same as control-direction" ???) ;G
;; (defun-nethack-command move-no-pickup-or-fight "Followed by direction, move without picking anything up or fighting" ???) ;m
;; (defun-nethack-command move-distance-no-pickup "Followed by direction, move a distance without picking anything up" ???) ;M
;; (defun-nethack-command show-wielded-weapon "Show the weapon currently wielded" ???) ;)
;; (defun-nethack-command show-worn-armor "Show the armor currently worn" ???) ;[
;; (defun-nethack-command show-worn-rings "Show the ring(s) currently worn" ???) ;=
;; (defun-nethack-command show-worn-amulet "Show the amulet currently worn" ???) ;"
;; (defun-nethack-command show-tool-in-use "Show the tools currently in use" ???) ;(

(provide 'nethack-cmd)
