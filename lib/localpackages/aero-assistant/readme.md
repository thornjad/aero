# aero-assistant - Aero AI Assistant client

_Author:_ Jade Michael Thornton<br>
_Version:_ 0.1.0<br>

Aero Assistant is an Emacs Lisp package that acts as an AI client for Aero Emacs. It leverages
various AI models, including GPT-4, GPT-3.5-turbo, and Davinci, to facilitate a broad range of
natural language processing tasks right within your Emacs.

## OpenAI API Key

GPT requires setting `aero/assistant-openai-api-key` to your own API key

## Usage

Simply call the `aero/assistant` function to start or switch to an Aero Assistant session:

    (aero/assistant)

If a region is active, `aero/assistant` will prefill the input buffer with the content of the
region.

## Using Aero Assistant for Git Commit Messages in Magit [Experimental]

The `aero/assistant-commit-message` function can add an Aero Assistant- generated commit message.
This function requires [Magit](https://github.com/magit/magit).

Whenever you commit using Magit, calling `aero/assistant-commit-message` will automatically
generate a commit message based on the staged git changes

CAUTION: GPT isn't actually very good at writing commit messages, so consider this feature
experimental, probably forever


---
Converted from `aero-assistant.el` by [_el2md_](https://gitlab.com/thornjad/el2md).