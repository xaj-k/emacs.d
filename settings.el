(tooltip-mode 1)			; Enable tooltips

;; Fromat the appearance of the mode line
(setq-default mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   " %3l :%3c  "
   mode-line-modes
   " "
   (vc-mode vc-mode)
   (global-mode-string global-mode-string)
   mode-line-end-spaces))

(load-theme 'nispio-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
	("a7851f858e2263f3589f272a637c86395f9e308ea5ab416a718019e4137af8f0" default "eafda598b275a9d68cc1fbe1689925f503cab719ee16be23b10a9f2cc5872069" "1dbbefd94fa8be48309bc1d855b5abdcef24e7a3ea47e07cd1a209ace9ed0ab4" "c6dd63315e98dabed99d373b0c19a9d5e84144e352e745254c5ed2c041ad1fd8" "868f73b5cf78e72ca2402e1d48675e49cc9a9619c5544af7bf216515d22b58e7" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "5e918bd7208208ea1e5856253a16ec2f11a3341d54671612c45578d6ff31ffed" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "c35c0effa648fd320300f3d45696c640a92bdc7cf0429d002a96bda2b42ce966" "8db3d26ab1cf70d1569ecd7256eafb7c3b16ccd691431e562aca65e02f59db78" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "34e7163479ef3669943b3b9b1fabe639d6e0a0453e0de79cea2c52cb520d3bc4" "dba244449b15bdc6a3236f45cec7c2cb03de0f5cf5709a01158a278da86cb69b" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "b85fc9f122202c71b9884c5aff428eb81b99d25d619ee6fde7f3016e08515f07" "b6f42c69cf96795c75b1e79e5cd8ca62f9f9a0cb07bf11d1e0b49f97785358f1" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "f7b950b4995da990c4a868e52543a6ec0557ae2614d1e5b6b9dcaaa5309c3eed" "235560c3139375bae9d1daea52c1ea80513f1c64863a9a2c7170b0fa53771f76" "3a69621a68c2d3550a4c777ffc000e1ea66f5bc2f61112814c591e1bda3f5704" "a5ce06f368dd82a9dade9261bccf5c30e2c7415a582bbd0a9337ea9f6af9e265" "62408b3adcd05f887b6357e5bd9221652984a389e9b015f87bbc596aba62ba48" "8a465f1fd92d6faa513d6d3f57bb354d9a4efabb2a23438d53dfa76fd63a533f" "3cdbd70fef0be339eb41772ec780d0535cf764c2db4491c8df17128e510d2ec6" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "cd03a600a5f470994ba01dd3d1ff52d5809b59b4a37357fa94ca50a6f7f07473" "3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" "a91401f958bcf263096baa22365042ffc2737107863695f0010bac16ed611aa8" "77c65d672b375c1e07383a9a22c9f9fc1dec34c8774fe8e5b21e76dca06d3b09" "0155b3b94f6d5bce0275a15bc662be4f6f9f3284f9e469ca4ab1bd67ff9b5222" "c223d51885a5234a050c65af9748d0a5c251a6b3e19aff945723abf87467b9b7" "5029bd2b49ff369be3541486eed38c3b31d01f8286c8ebde37de212034162726" "7f5837a7dbf54c2b7c41d94f5eb1373cf63274847d1971037faa24d7f2231eea" "4f05713474ea9ba94d8e99700934d355602392d02dfdfbf00f1f70d2a2cd1546" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "c044b28c1e8a47cc7c8ff01d683bd8e7c075d2f758c4f88e5bc67c31d8b9a6e7" "e8e05cd7cd539c8017b1761a8384a2c3b09a6ec04bb46dfd54cf465f31a3f4da" "fa94f0c2ddd30df2bca56ddee6854c5926a8a67125d0c28326fd504e377563a9" "ce585b387d84fc4bbb02b8766bfe82607f891e25602ec3550db858e09c10eb7d" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "889a93331bc657c0f05a04b8665b78b3c94a12ca76771342cee27d6605abcd0e" "8453c6ba2504874309bdfcda0a69236814cefb860a528eb978b5489422cb1791" "2bed8550c6f0a5ce635373176d5f0e079fb4fb5919005bfa743c71b5eed29d81" "3a9249d4c34f75776e130efd7e02c4a0a7c90ad7723b50acc5806112394ec2dd" "9ddc19e0a871253e229bbe73b1a3eafe65e20486d3163cae7f152e165d42be95" "6c0a087a4f49c04d4002393ffd149672f70e4ab38d69bbe8b39059b61682b61c" "121a6df66920d6d111aaeaa07fc45dd53c45502be26e72a85e3e2aa677f88b78" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "00d07f95ca8ddebe0c10becbe96a41601ea1c41eb8295cd39eeeb803bc1fe5dd" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "f946b245b030f1d247c41c14833e3ed441565c600d606c9f336b77f5600acdc7" "c3ac059c19c91351b823de13a2851dd8e334d496e19f2a672702d041f41082bc" "349674c8ce5f663f9f2e5a22700da0b993c7f16cf5ad4cd37e9671669ab3bb6e" "55ed02951e7b458e4cd18837eefa1956884c9afd22bb514f697fd1d2d1abb3d3" "c5395ec2a37ae1eae1e0baada2dbdc7e576cf7eba8a3f2131593644fad2bba2b" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "49ad7c8d458074db7392f8b8a49235496e9228eb2fa6d3ca3a7aa9d23454efc6" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "3038a172e5b633d0b1ee284e6520a73035d0cb52f28b1708e22b394577ad2df1" "ffd9bd2d3b884f3610ae8b6e4fbe61e7459af48a99f1c8aa149ec5b008599954" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "4f2ede02b3324c2f788f4e0bad77f7ebc1874eff7971d2a2c9b9724a50fb3f65" "408e5a19d3a626b76b01b50e105695b03274aafcaeabb403fd74ee7f701897d9" "5cd81f0bbf2cb3326a31f6259b9b866071d9e3729f982d667b65b7b3b28c41e1" "1db337dd3ced609415ca342cb12fedfb14705213d0231fa84e85a97f52f97bd1" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "bdaab014ec6785f64b72efbea80808b762d8971247aacf2ffc6b76a39b9ed97c" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "30070a8208661dc69ae5d65cc2aa549962c572a5d785d913b388585b09f54bde" "21998518340e5952ed1014e751aa68cdf5c9ce5d4b3d7f1fd06ba271aad74a8e" "9f1f7d8ecaa9d379716373983d6ffbac672e2e61968513073a90dad03e4b90b8" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "b8a14cdec19d3307cc5450e9ddda3929f5e909ccb2ee7c2ff07875b27d02bb75" "357d5abe6f693f2875bb3113f5c031b7031f21717e8078f90d9d9bc3a14bcbd8" "4184b3869c5dc105e947db4a695dc268c19061184e2cbd4876f3e96a4b53f933" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "2d16f85f22f1841390dfc1234bd5acfcce202d9bb1512aa8eabd0068051ac8c3" "3cfdcec0375a49aa95c445035d389ed3198459dcfcf7dc9978e6164ae135f1ae" "81a4b3d3751940b01617381397f31168420252e50cc9600cc0fc168ff4819ced" "950a16f0f15ed78a1084049d38ca25475e705d93beb0d8918ab26d54f9dcc896" "8d5de3e855e1bda03999ad417a706e0cc9b529591905e73049013b798a4cd52f" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "d0c21e2be19cee61111a3032e047489768f7ac65fd38f76755e4b083b2003f89" "36dfc36bb73b25b0d42d15bacd4f1b1ccae3f7fdd9959438eb9d01d71a957e15" "5ef9f38db2a1fb04aa9f3957f67559039669d520e069e91c9b1fd31f36950cdc" "e900d58d6df6c85d1e1328bb619c858fe65acffe77a9b73f22017fc2d108ebd4" "2059e232c82b4d7337c4f5ca011df75a07a7c5e733da7c599f1d3a525400aaf5" "7997e0765add4bfcdecb5ac3ee7f64bbb03018fb1ac5597c64ccca8c88b1262f" "2dd32048690787844d8cba601ed3dd8b2f419e9bd985898d0c3792671a05b96b" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "7471cfb244d0973c3b7e7012e01dae0e3e3be7ce821da6af430d65ed021d0e42" "0ccb43b6653b5d377b2e06b685fb13ac5c390b958d7f2a563f2a6f3a71ec8cb6" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "170ca520c97f6e47156871d0f42e4bc8d73847fab1414a0afdb05b69af2749ca" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "bfbe39eae84983ca5c3ad6c9ccccd23d6e324e124b825721c20e3062ebc663bd" "985835e542f7cf6c7201e5149ec2791de0aeb43295a98f425c4d8303b6e3e509" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "de8fa309eed1effea412533ca5d68ed33770bdf570dcaa458ec21eab219821fd" "7db66dafe7a65a8a6a403014edb5e53deca2da82279cb8f3f55e4bc336bf48af" "efe9aa1a078bf00a43342a1fc8b16505d444f8068285f5f83c6275cadcc44b7d" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "12722541c8998f056b761bf63a92216aaf4610e4eb1afe7991842a31fa28b6d8" "90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2")))
 '(diredp-hide-details-initially-flag nil)
 '(fci-rule-color "#14151E" t)
 '(helm-ag-base-command "ag --nocolor --nogroup")
 '(helm-el-package-initial-filter (quote all))
 '(org-agenda-files "~/.org/agendas.ini")
 '(org-agenda-ndays 10)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-deadline-if-done nil)
 '(org-agenda-skip-scheduled-if-done nil)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-todo-ignore-scheduled t)
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-capture-templates
   (quote
	(("t" "Todo" entry
	  (file+headline "~/.org/tasks.org" "Unfiled Tasks")
	  "* TODO %i%?
  - State \"TODO\"       from \"\"           %U")
	 ("T" "Todo (from file)" entry
	  (file+headline "~/.org/tasks.org" "Unfiled Tasks")
	  (function nispio/linked-todo)
	  :immediate-finish t)
	 ("d" "Distraction" entry
	  (file+headline "~/.org/distractions.org" "Distractions")
	  "* %?
  - Added: %U")
	 ("D" "Distraction (as TODO)" entry
	  (file+headline "~/.org/distractions.org" "Tasks")
	  "* TODO %?
  - Added: %U")
	 ("n" "Notes" entry
	  (file+headline "~/.org/notes.org" "Notes")
	  "* %i%?
  - Added: %U")
	 ("N" "Notes (from file)" entry
	  (file+headline "~/.org/notes.org" "Notes")
	  (function nispio/linked-note))
	 ("p" "Pomodoros" entry
	  (file+datetree "~/.org/pomodoros.org")
	  "* TODO %i%?
  - State \"TODO\"       from \"\"           %U" :jump-to-captured t)
	 ("j" "Journal" entry
	  (file+datetree "~/.org/journal.org")
	  "* %?")
	 ("J" "Journal (free writing)" entry
	  (file+datetree "~/.org/freejourn.org")
	  "* %?"))) t)
 '(org-completion-use-ido t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-with-LaTeX-fragments t t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-hide-leading-stars t)
 '(org-link-search-must-match-exact-headline nil)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets (quote ((org-agenda-files :level . 1))))
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-startup-folded t)
 '(org-tag-alist
   (quote
	(("CODE" . 99)
	 ("PLAN" . 112)
	 ("EXPERIMENT" . 101)
	 ("LEARN" . 108)
	 ("DOCUMENT" . 100)
	 ("TEACH" . 116))))
 '(org-tags-column -77)
 '(org-todo-keywords
   (quote
	((sequence "TODO(t!)" "STARTED(s!)" "WAIT(w@/!)" "ASK(a)" "|" "ANSWERED(A@)" "CANCELLED(x@)" "DONE(d)" "COMPLETE(c!)"))))
 '(safe-local-variable-values
   (quote
	((eval add-hook
		   (quote after-save-hook)
		   (lambda nil
			 (setq kill-ring nil))))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#d54e53")
	 (40 . "goldenrod")
	 (60 . "#e7c547")
	 (80 . "DarkOliveGreen3")
	 (100 . "#70c0b1")
	 (120 . "DeepSkyBlue1")
	 (140 . "#c397d8")
	 (160 . "#d54e53")
	 (180 . "goldenrod")
	 (200 . "#e7c547")
	 (220 . "DarkOliveGreen3")
	 (240 . "#70c0b1")
	 (260 . "DeepSkyBlue1")
	 (280 . "#c397d8")
	 (300 . "#d54e53")
	 (320 . "goldenrod")
	 (340 . "#e7c547")
	 (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-background-face ((t (:background "Gray20"))))
 '(elscreen-tab-current-screen-face ((t (:background "gray50" :foreground "white"))))
 '(elscreen-tab-other-screen-face ((t (:background "Gray30" :foreground "Gray50")))))
