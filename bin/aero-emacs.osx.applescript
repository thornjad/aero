tell application "Terminal"
	if not (exists window 1) then reopen
	do script "source ~/.bashrc && emacs" in window 1
	activate
end tell
