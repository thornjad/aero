-- To install, run make install-aero-macos
-- NOTE: Requires running in Zsh

tell application "Terminal"
	do script "cd ~ && source ~/.zshrc && emacs -mm &| exit"
	activate
end tell
