
(eval-after-load 'semantic
  (progn
	(semantic-add-system-include (substitute-in-file-name "$XMDISK/xm/inc"))
	(semantic-add-system-include (substitute-in-file-name "$XMDISK/xm/include/midas"))
	nil))
