tell application "Terminal"
	activate
	if not (exists window 1) then reopen
	do script "source ~/.zshrc && emacs" in window 1
	activate
end tell
