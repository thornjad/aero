-- To install, run make install-aero-macos
-- NOTE: Requires running in Zsh

tell application "Terminal"
  -- TEMP emacs 29 has a packaging bug in macOS, but for some reason the versioned executable works
  -- fine. See https://github.com/d12frosted/homebrew-emacs-plus/issues/392
	do script "cd ~ && source ~/.zshrc && emacs-29.0.50 -mm &| exit"
	-- do script "cd ~ && source ~/.zshrc && emacs -mm &| exit"
	activate
end tell
