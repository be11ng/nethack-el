;; cmd.c is the cheat sheet for this file

;;(defun nethack-command-redo-previous ()
;;   "^A      Redo the previous command"
;;   )

;; (defun nethack-command-kick ()
;;   "^D      Kick something (usually a door, chest, or box)"
;;   )

;; (defun nethack-command-create-monster ()
;;   "^G      Create a monster (available in debug mode only)"
;;   )

;; (defun nethack-command-identify-all-items ()
;;   "^I      Identify all items (available in debug mode only)"
;;   )

;; (defun nethack-command-previous-message ()
;;   "^P      Toggle through previously displayed game messages"
;;   )

;; (defun nethack-command-redraw-screen ()
;;   "^R      Redraw screen"
;;   )

;; (defun nethack-command-teleport-around-level ()
;;   "^T      Teleport around level"
;;   )

;; (defun nethack-command-suspend-game ()
;;   "^Z      Suspend game (only if defined)"
;;   )

(defun nethack-command-identify-trap ()
  "^       Show the type of a trap"
  (interactive)
  (nethack-handle-command "idtrap"))


(defun nethack-command-apply ()
  "a       Apply (use) a tool"
  (interactive)
  (nethack-handle-command "apply"))

(defun nethack-command-remove-all-armor ()
  "A       Remove all armor"
  (interactive)
  (nethack-handle-command "ddoremarm"))

;; (defun nethack-command-cancel ()
;;   "^[      Cancel command"
;;   )

;; (defun nethack-command-southwest-one-space ()
;;   "b       Go southwest 1 space"
;;   )

;; (defun nethack-command-southwest-until-ontop ()
;;   "B       Go southwest until you are on top of something"
;;   )

;; (defun nethack-command-southwest-until-near ()
;;   "^B      Go southwest until you are near something"
;;   )

(defun nethack-command-close-door ()
  "c       Close a door"
  (interactive)
  (nethack-handle-command "close"))

;; (defun nethack-command-call-monster ()
;;   "C       Call (name) a particular monster"

;;   )

(defun nethack-command-drop ()
  "d       Drop an item"
  (interactive)
  (nethack-handle-command "drop"))

(defun nethack-command-drop-specific-item ()
  "D       Drop specific item types"
  (interactive)
  (nethack-handle-command "ddrop"))

(defun nethack-command-eat ()
  "e       Eat something"
  (interactive)
  (nethack-handle-command "eat"))

(defun nethack-command-engrave ()
  "E       Engrave writing on the floor"
  (interactive)
  (nethack-handle-command "engrave"))

(defun nethack-command-fire ()
  "f       Fire ammunition from quiver"
  (interactive)
  (nethack-handle-command "fire"))

;; (defun nethack-command-force-fight ()
;;   "F       Followed by direction, fight a monster (even if you don't sense it)"
;;   )

;; (defun nethack-command-move-until-near ()
;;   "g       Followed by direction, move until you are near something"
;;   )

;; (defun nethack-command-move ()
;;   "G       Followed by direction, same as control-direction"
;;   )

(defun nethack-command-west ()
  "h       Go west 1 space"
  (interactive)
  (nethack-handle-command "gowest"))

(defun nethack-command-west-until-ontop ()
  "H       Go west until you are on top of something"
  (interactive)
  (nethack-handle-command "gowestontop"))

(defun nethack-command-west-until-near ()
  "^H      Go west until you are near something"
  (interactive)
  (nethack-handle-command "gowestnear"))

(defun nethack-command-inventory ()
  "i       Show your inventory"
  (interactive)
  (nethack-handle-command "inv"))

(defun nethack-command-type-inventory ()
  "I       Inventory specific item types"
  (interactive)
  (nethack-handle-command "typeinv"))

(defun nethack-command-south ()
  "j       Go south 1 space (or if number_pad is on, jump to another location)"
  (interactive)
  (nethack-handle-command "gosouth"))

(defun nethack-command-south-until-ontop ()
  "J       Go south until you are on top of something"
  (interactive)
  (nethack-handle-command "gosouthontop"))

(defun nethack-command-south-until-near ()
  "^J      Go south until you are near something"
  (interactive)
  (nethack-handle-command "gosouthnear"))

(defun nethack-command-north ()
   "k       Go north 1 space (or if number_pad is on, kick something)"
   (interactive)
   (nethack-handle-command "gonorth"))

(defun nethack-command-north-until-ontop ()
  "K       Go north until you are on top of something"
  (interactive)
  (nethack-handle-command "gonorthontop"))

(defun nethack-command-north-until-near ()
  "^K      Go north until you are near something"
  (interactive)
  (nethack-handle-command "gonorthnear"))

(defun nethack-command-east ()
   "l       Go east 1 space (or if number_pad is on, loot a box on the floor)"
   (interactive)
   (nethack-handle-command "goeast"))


(defun nethack-command-east-until-ontop ()
  "L       Go east until you are on top of something"
  (interactive)
  (nethack-handle-command "goeastontop"))

(defun nethack-command-east-until-near ()
  "^L      Go east until you are near something"
  (interactive)
  (nethack-handle-command "goeastnear"))

;; (defun nethack-command-move-no-pickup-or-fight ()
;;   "m       Followed by direction, move without picking anything up or fighting"
;;   )

;; (defun nethack-command-move-distance-no-pickup ()
;;   "M       Followed by direction, move a distance without picking anything up"
;;   )

(defun nethack-command-southeast ()
  "n       Go southeast 1 space"
  (interactive)
  (nethack-handle-command "gosoutheast"))

(defun nethack-command-southeast-until-ontop ()
  "N       Go southeast until you are on something (if number_pad, name an object)"
  (interactive)
  (nethack-handle-command "gosoutheastontop"))

(defun nethack-command-southeast-until-near ()
  "^N      Go southeast until you are near something"
  (interactive)
  (nethack-handle-command "gosoutheastnear"))

(defun nethack-command-open ()
  "o       Open a door"
  (interactive)
  (nethack-handle-command "open"))

(defun nethack-command-settings ()
  "O       Show option settings, possibly change them"
  (interactive)
  (nethack-handle-command "set"))

(defun nethack-command-pay ()
  "p       Pay your shopping bill"
  (interactive)
  (nethack-handle-command "pay"))

(defun nethack-command-put-on ()
  "P       Put on an accessory (ring, amulet, etc)"
  (interactive)
  (nethack-handle-command "puton"))

(defun nethack-command-quaff ()
  "q       Quaff (drink) something"
  (interactive)
  (nethack-handle-command "drink"))

(defun nethack-command-select-ammo-for-quiver ()
  "Q       Select ammunition for quiver"
  (interactive)
  (nethack-handle-command "wieldquiver"))

(defun nethack-command-read ()
  "r       Read a scroll or spellbook"
  (interactive)
  (nethack-handle-command "read"))

(defun nethack-command-remove-accessory ()
  "R       Remove an accessory (ring, amulet, etc)"
  (interactive)
  (nethack-handle-command "remring"))

(defun nethack-command-search ()
  "s       Search for traps and secret doors"
  (interactive)
  (nethack-handle-command "search"))

(defun nethack-command-save-game ()
  "S       Save the game"
  (interactive)
  (nethack-handle-command "save"))

(defun nethack-command-throw ()
  "t       Throw something"
  (interactive)
  (nethack-handle-command "throw"))

(defun nethack-command-remove-single-armor ()
  "T       Take off one piece of armor"
  (interactive)
  (nethack-handle-command "takeoff"))

(defun nethack-command-northeast ()
  "u       Go northeast 1 space (or if number_pad is on, untrap something)"
  (interactive)
  (nethack-handle-command "gonortheast"))

(defun nethack-command-northeast-until-ontop ()
  "U       Go northeast until you are on top of something"
  (interactive)
  (nethack-handle-command "gonortheastontop"))

(defun nethack-command-northeast-until-near ()
  "^U      Go northeast until you are near something"
  (interactive)
  (nethack-handle-command "gonortheastnear"))

(defun nethack-command-version ()
  "v       Show version"
  (interactive)
  (nethack-handle-command "version"))

(defun nethack-command-version-and-history ()
  "V       Show long version and game history"
  (interactive)
  (nethack-handle-command "history"))

(defun nethack-command-wield ()
  "w       Wield (put in use) a weapon"
  (interactive)
  (nethack-handle-command "wield"))

(defun nethack-command-wear-armor ()
  "W       Wear a piece of armor"
  (interactive)
  (nethack-handle-command "wear"))

(defun nethack-command-swap-weapons ()
  "x       Swap wielded and secondary weapons"
  (interactive)
  (nethack-handle-command "swapweapon"))

(defun nethack-command-explore-mode ()
  "X       Enter explore (discovery) mode (only if defined)"
  (interactive)
  (nethack-handle-command "enter_explore_mode"))

(defun nethack-command-northwest ()
  "y       Go northwest 1 space"
  (interactive)
  (nethack-handle-command "gonorthwest"))

(defun nethack-command-northwest-until-ontop ()
  "Y       Go northwest until you are on top of something"
  (interactive)
  (nethack-handle-command "gonorthwestontop"))

(defun nethack-command-northwest-until-near ()
  "^Y      Go northwest until you are near something"
  (interactive)
  (nethack-handle-command "gonorthwestnear"))

(defun nethack-command-zap-wand ()
  "z       Zap a wand"
  (interactive)
  (nethack-handle-command "zap"))

(defun nethack-command-cast-spell ()
  "Z       Zap (cast) a spell"
  (interactive)
  (nethack-handle-command "cast"))

(defun nethack-command-up ()
  "<       Go up a staircase"
  (interactive)
  (nethack-handle-command "up"))

(defun nethack-command-down ()
  ">       Go down a staircase"
  (interactive)
  (nethack-handle-command "down"))

(defun nethack-command-what-is-symbol ()
  "/       Show what type of thing a symbol corresponds to"
  (interactive)
  (nethack-handle-command "whatis"))

(defun nethack-command-help ()
  "?       Give a help message"
  (interactive)
  (nethack-handle-command "help"))

(defun nethack-command-command-help ()
  "&       Tell what a command does"
  (interactive)
  (nethack-handle-command "whatdoes"))

(defun nethack-command-shell-escape ()
  "!       Do a shell escape (only if defined)"
  (interactive)
  (nethack-handle-command "sh"))

(defun nethack-command-show-discoveries ()
  "\       Show what object types have been discovered"
  (interactive)
  (nethack-handle-command "discovered"))

(defun nethack-command-rest-one-move ()
  ".       Rest one move while doing nothing"
  (interactive)
  (nethack-handle-command "null"))

(defun nethack-command-rest-one-move ()
  "<space>     Rest one move while doing nothing (if rest_on_space option is on)"
  (interactive)
  (nethack-handle-command "null"))

(defun nethack-command-look-here ()
  ":       Look at what is on the floor"
  (interactive)
  (nethack-handle-command "look"))

(defun nethack-command-what-is-map-piece ()
  "; Show what type of thing a map symbol on the level corresponds to"
  (interactive)
  (nethack-handle-command "quickwhatis"))

(defun nethack-command-pick-up ()
  ",       Pick up things at the current location"
  (interactive)
  (nethack-handle-command "pickup"))

(defun nethack-command-toggle-pickup ()
  "@       Toggle the pickup option on/off"
  (interactive)
  (nethack-handle-command "togglepickup"))

;; (defun nethack-command-show-wielded-weapon ()
;;   ")       Show the weapon currently wielded"
;;   )

;; (defun nethack-command-show-worn-armor ()
;;   "[       Show the armor currently worn"
;;   )

;; (defun nethack-command-show-worn-rings ()
;;   "=       Show the ring(s) currently worn"
;;   )

;; (defun nethack-command-show-worn-amulet ()
;;   "\"       Show the amulet currently worn"
;;   )

;; (defun nethack-command-show-tool-in-use ()
;;   "(       Show the tools currently in use"
;;   )

(defun nethack-command-show-all-equipment-in-use ()
  "*       Show all equipment in use (combination of the ),[,=,\",( commands)"
  (interactive)
  (nethack-handle-command "prinuse"))

;; (defun nethack-command-count-gold ()
;;   "$       Count your gold"
;;   )

;; (defun nethack-command-list-known-spells ()
;;   "+       List known spells"
;;   )

;;; wizard (debug) mode only commands:
;; (defun nethack-command-wizard-search ()
;;   "^E      Search a room (available in debug mode only)"
;;   )

;; (defun nethack-command-wizard-map ()
;;   "^F      Map the level (available in debug mode only)"
;;   )

;; (defun nethack-command-wizard-show-location ()
;;   "^O      Show location of special levels (available in debug mode only)"
;;   )

;; (defun nethack-command-wizard-teleport-between-levels ()
;;   "^V      Teleport between levels (available in debug mode only)"
;;   )

;; (defun nethack-command-wizard-wish ()
;;   "^W      Wish (available in debug mode only)"
;;   )


;; Extended commands
(defun nethack-command-adjust ()
"adjust inventory letters."
  (interactive)
  (nethack-handle-command "adjust"))

(defun nethack-command-chat ()
  "talk to someone."
  (interactive)
  (nethack-handle-command "chat"))

(defun nethack-command-conduct ()
  "list which challenges you have adhered to."
  (interactive)
  (nethack-handle-command "conduct"))

(defun nethack-command-dip ()
  "dip an object into something."
  (interactive)
  (nethack-handle-command "dip"))

(defun nethack-command-enhance ()
  "advance or check weapons skills."
  (interactive)
  (nethack-handle-command "enhance"))

(defun nethack-command-force ()
  "force a lock."
  (interactive)
  (nethack-handle-command "force"))

(defun nethack-command-invoke ()
  "invoke an object's powers."
  (interactive)
  (nethack-handle-command "invoke"))

(defun nethack-command-jump ()
  "jump to a location."
  (interactive)
  (nethack-handle-command "jump"))

(defun nethack-command-loot ()
  "loot a box on the floor."
  (interactive)
  (nethack-handle-command "loot"))

(defun nethack-command-monster ()
  "use a monster's special ability."
  (interactive)
  (nethack-handle-command "monster"))

(defun nethack-command-name ()
  "name an item or type of object."
  (interactive)
  (nethack-handle-command "name"))

(defun nethack-command-offer ()
  "offer a sacrifice to the gods."
  (interactive)
  (nethack-handle-command "offer"))

(defun nethack-command-pray ()
  "pray to the gods for help."
  (interactive)
  (nethack-handle-command "pray"))

(defun nethack-command-quit ()
  "exit without saving current game."
  (interactive)
  (nethack-handle-command "quit"))

(defun nethack-command-ride ()
  "ride (or stop riding) a monster."
  (interactive)
  (nethack-handle-command "ride"))

(defun nethack-command-rub ()
  "rub a lamp."
  (interactive)
  (nethack-handle-command "rub"))

(defun nethack-command-sit ()
  "sit down."
  (interactive)
  (nethack-handle-command "sit"))

(defun nethack-command-turn ()
  "turn undead."
  (interactive)
  (nethack-handle-command "turn"))

(defun nethack-command-twoweapon ()
  "toggle two-weapon combat."
  (interactive)
  (nethack-handle-command "twoweapon"))

(defun nethack-command-untrap ()
  "untrap something."
  (interactive)
  (nethack-handle-command "untrap"))

(defun nethack-command-version ()
  "list compile time options for this version of NetHack."
  (interactive)
  (nethack-handle-command "version"))

(defun nethack-command-wipe ()
  "wipe off your face."
  (interactive)
  (nethack-handle-command "wipe"))


(provide 'nethack-cmd)
