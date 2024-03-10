# assist - Aero AI Assistant client

_Author:_ Jade Michael Thornton<br>
_Version:_ 0.9.0<br>

Assist is an Emacs Lisp package that acts as an AI client for Aero Emacs. It leverages various AI
models, including GPT-4, Claude and DALL-E to facilitate a broad range of assistant tasks.

## API Key

GPT and DALL-E usage requires setting `assist-openai-api-key` to your own API key.

Claude usage requires setting `assist-anthropic-api-key` to your own API key.

## Usage

Simply call the `assist-chat` function to start or switch to an Assist session:

    (assist-chat)

If a region is active, `assist` will prefill the input buffer with the content of the
region.

## Using Assist for Git Commit Messages in Magit [Experimental]

The `assist-commit-message` function can add an Aero Assist- generated commit message.
This function requires [Magit](https://github.com/magit/magit).

Whenever you commit using Magit, calling `assist-commit-message` will automatically
generate a commit message based on the staged git changes

CAUTION: GPT isn't actually very good at writing commit messages, so consider this feature
experimental, probably forever

## Using Assist to generate Gherkin comments from spec files [Experimental]

Use the `assist-diff-qa-steps`.

Enable request debugging

To see the output from API requests, toggle on debugging mode with `assist-toggle-debug`.


---
Converted from `assist.el` by [_el2md_](https://gitlab.com/thornjad/el2md).
