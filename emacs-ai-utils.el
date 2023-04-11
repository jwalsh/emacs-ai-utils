;;; emacs-ai-utils.el --- Utilities and setup for AI

;; Copyright (C) 2023 Jason Walsh 
;; Authors: Jason Walsh <j@wal.sh>

;;; Commentary:
;; This package provides utilities and setup for AI interaction with Emacs.

; Included are functions for data manipulation, machine learning, and natural language processing.

;;; Code:

(require 'cl-lib)

(defun insert-ai ()
  "Insert an empty AI block into the current document."
  (interactive)
  (insert "\n#+begin_ai\n\n#+end_ai\n"))


(defun insert-ai-image ()
  "Insert an empty AI block into the current document."
  (interactive)
  (insert "\n#+begin_ai :image :size 256x256\n\n#+end_ai\n"))

;; Primary Python model and simulator
(defun insert-prompt-python-simulator ()
  "Insert prompt for creating a simulation using Click in Python."
  (interactive)
  (insert "Create a basic command-line interface in Python using Click, typing, docstrings that models the following and runs simulation when executed from the command line: \n\n"))

;; Program development and testing
(defun insert-prompt-calculator-cli ()
  "Insert prompt for creating a command-line calculator using Click in Python."
  (interactive)
  (insert "Create a basic command-line interface that accepts user input and simulates the behavior of a calculator using the click library. Use type hints to specify the input and output types of your functions."))

(defun insert-prompt-typing-test ()
  "Insert prompt for creating a typing test program."
  (interactive)
  (insert "Write a program that simulates a typing test. Prompt the user to type a given sentence or paragraph, then calculate their typing speed and accuracy based on their input. Include a docstring with a brief description of your program and how to run it."))

(defun insert-prompt-game-simulation ()
  "Insert prompt for building a simulation of a game using Click in Python."
  (interactive)
  (insert "Build a simulation of a game like tic-tac-toe or rock-paper-scissors. Use click to accept user input and print the results of each round. Write a docstring to describe the game, and include examples of how to run it."))

(defun insert-prompt-social-media-post-sartre ()
  "Insert prompt for writing a social media post in a conversational first-person tone like Sartre."
  (interactive)
  (insert "Write a 400-word social media post in a conversational first-person tone like Sartre about the following topic: "))

(defun insert-prompt-social-media-post-mishima ()
  "Insert prompt for writing a social media post in the style of Yukio Mishima."
  (interactive)
  (insert "Write a 400-word social media post in the poetic and intense style of Yukio Mishima about the following topic: "))

(defun insert-prompt-social-media-post-socrates ()
  "Insert prompt for writing a social media post in the questioning style of Socrates."
  (interactive)
  (insert "Write a 400-word social media post in the questioning and philosophical style of Socrates about the following topic: "))

;; Additional functions for other authors with distinctive styles
(defun insert-prompt-social-media-post-hemingway ()
  "Insert prompt for writing a social media post in the concise and direct style of Ernest Hemingway."
  (interactive)
  (insert "Write a 400-word social media post in the concise and direct style of Ernest Hemingway about the following topic: "))

(defun insert-prompt-social-media-post-orwell ()
  "Insert prompt for writing a social media post in the clear and critical style of George Orwell."
  (interactive)
  (insert "Write a 400-word social media post in the clear and critical style of George Orwell about the following topic: "))

(defun insert-prompt-social-media-post-austen ()
  "Insert prompt for writing a social media post in the witty and satirical style of Jane Austen."
  (interactive)
  (insert "Write a 400-word social media post in the witty and satirical style of Jane Austen about the following topic: "))

(defun insert-prompt-social-media-post-poe ()
  "Insert prompt for writing a social media post in the dark and mysterious style of Edgar Allan Poe."
  (interactive)
  (insert "Write a 400-word social media post in the dark and mysterious style of Edgar Allan Poe about the following topic: "))

(defun insert-prompt-social-media-post-dickens ()
  "Insert prompt for writing a social media post in the vivid and descriptive style of Charles Dickens."
  (interactive)
  (insert "Write a 400-word social media post in the vivid and descriptive style of Charles Dickens about the following topic: "))

(defun insert-prompt-social-media-post-wilde ()
  "Insert prompt for writing a social media post in the clever and humorous style of Oscar Wilde."
  (interactive)
  (insert "Write a 400-word social media post in the clever and humorous style of Oscar Wilde about the following topic: "))

(defun insert-prompt-social-media-post-shakespeare ()
  "Insert prompt for writing a social media post in the poetic and dramatic style of William Shakespeare."
  (interactive)
  (insert "Write a 400-word social media post in the poetic and dramatic style of William Shakespeare about the following topic: "))

(defun insert-prompt-social-media-post-joyce ()
  "Insert prompt for writing a social media post in the experimental and stream-of-consciousness style of James Joyce."
  (interactive)
  (insert "Write a 400-word social media post in the experimental and stream-of-consciousness style of James Joyce about the following topic: "))

(provide 'emacs-ai-utils)

;;; emacs-ai-utils.el ends here
