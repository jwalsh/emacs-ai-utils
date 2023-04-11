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

(defun insert-prompt-quiz-simulation ()
  "Insert prompt for building a program that simulates a quiz or test using Click in Python."
  (interactive)
  (insert "Create a program that simulates a quiz or test. Prompt the user with a series of questions and then calculate their score. Use click to accept user input and write a docstring with a clear explanation of how to use your program."))

(defun insert-prompt-restaurant-order ()
  "Insert prompt for simulating a restaurant order system using Click in Python."
  (interactive)
  (insert "Develop a program that simulates the behavior of a restaurant order system. Accept user input for menu items and generate a receipt based on their selections. Include type hints and a docstring to describe your program."))

(defun insert-prompt-chatbot-interface ()
  "Insert prompt for building a chatbot interface using Click in Python."
  (interactive)
  (insert "Write a program that simulates a basic chatbot interface. Accept user input and return an appropriate response based on your pre-defined rules. Use click to accept the user's input and write a docstring with examples of how to use your chatbot."))

(defun insert-prompt-advanced-calculator ()
  "Insert prompt for building a more advanced calculator using Click in Python."
  (interactive)
  (insert "Build a program that simulates a simple calculator with advanced functions like square root or exponentiation. Use click to accept user input and write a docstring with examples of how to run different calculations."))

(defun insert-prompt-game-engine ()
  "Insert prompt for building a game engine simulation using Click in Python."
  (interactive)
  (insert "Create a program that simulates the behavior of a simple game engine. Prompt the user to make choices based on their in-game situation, and generate an outcome based on their selections. Use click to handle user input and write a docstring with clear examples of gameplay."))

(defun insert-prompt-traffic-intersection ()
  "Insert prompt for simulating a traffic intersection using Click in Python."
  (interactive)
  (insert "Build a simulation of a traffic intersection. Use click to accept user input to control traffic flow and generate a visualization of the intersection based on their choices. Include explanations in your docstring about how to use your program and what it simulates."))

(defun insert-prompt-search-engine ()
  "Insert prompt for building a basic search engine using Click in Python."
  (interactive)
  (insert "Write a program that simulates the behavior of a basic search engine. Use click to accept user queries and generate search results based on pre-defined criteria. Write a docstring that includes examples of how to use your search engine and what it simulates."))

(defun insert-prompt-url-shortener ()
  (interactive)
  (insert "Build a program that shortens URLs on the command-line using Click in Python. The program should accept a long URL as input and return a shorter URL that redirects to the original page. Use click to handle user input and write a docstring that includes examples of how to use your URL shortener."))

(defun insert-prompt-weather-app ()
  (interactive)
  (insert "Create a command-line weather app using Click in Python. The program should accept a city or zip code as input and return the current weather conditions of that location. Use a weather API to retrieve the information and include examples and instructions in your docstring."))

(defun insert-prompt-math-quiz ()
  (interactive)
  (insert "Create a math quiz game using Click in Python. The program should prompt the user with math problems and check their answers. Use type-hints and click to accept user input and generate a score for the user. Include instructions and examples in your docstring."))

(defun insert-prompt-speech-synthesis ()
  (interactive)
  (insert "Write a program that synthesizes speech on the command-line using Click in Python. The program should accept text as input and return an audio file of the text spoken out loud. Use a speech synthesis package to perform the conversion and include instructions and examples in your docstring."))

(defun insert-prompt-tic-tac-toe-ai ()
  (interactive)
  (insert "Create a command-line version of Tic-Tac-Toe game using Click in Python. The program should be implemented with an AI for the opponent. Include instructions, examples and required AI documentation in your docstring."))

(defun insert-prompt-sorting-algorithms ()
  (interactive)
  (insert "Write a program that visualizes sorting algorithms on the command-line using Click in Python. The program should accept a list of integers and display how the sorting algorithm progresses through each stage of the sort. Include instructions and examples in your docstring."))

(defun insert-prompt-calorie-tracker ()
  (interactive)
  (insert "Develop a program that tracks the user's daily calorie intake and expenditure on the command-line using Click in Python. The program should use a calorie database to provide nutrition information and display the user's progress towards their goals. Include type-hints and a docstring that describes the features and usage of the program."))

(defun insert-prompt-nlp-tasks ()
  (interactive)
  (insert "Create a program that performs NLP tasks on the command-line using Click in Python. The program should accept text as input and perform tasks such as sentiment analysis or named entity recognition. Use a pre-trained NLP model to power the analysis and include instructions and examples in your docstring."))

(defun insert-prompt-password-generator ()
  (interactive)
  (insert "Write a program that generates secure passwords on the command-line using Click in Python. The program should enable users to specify the length and complexity of the password and be able to copy the generated password to the clipboard. Add a docstring with usage and example."))

(defun insert-prompt-shape-area-calculator ()
  (interactive)
  (insert "Create a program that calculates the area of different shapes on the command-line using Click in Python. The program should accept user input for the shape and its dimensions and return the area. Use type-hints and write a docstring with instructions on how to use your program."))

(defun insert-prompt-ascii-art-generator ()
  (interactive)
  (insert "Write a program that generates ASCII art on the command-line using Click in Python. The program should accept an image as input and return an ASCII representation of the image. Use an ASCII art generator library to perform the conversion and include instructions and examples in your docstring."))

(defun insert-prompt-github-api ()
  "Insert prompt for a program using the GitHub API."
  (interactive)
  (insert "Write a program that interacts with the GitHub API on the command-line using Click in Python. The program should allow users to perform actions such as listing repositories, creating new repositories, and managing issues. Use type-hints and write a docstring with instructions on how to use your program."))

;; Entity
(defun insert-prompt-entity-simulation-1 ()
  "Insert prompt for an entity simulation."
  (interactive)
  (insert "As an AI chatbot, simulate a conversation with a user who is seeking advice on time management. Engage the user with questions, offer tips, and provide resources to help them manage their time more effectively."))

(defun insert-prompt-entity-simulation-2 ()
  "Insert prompt for an entity simulation."
  (interactive)
  (insert "As an AI chatbot, simulate a conversation with a user who is seeking advice on how to improve their health. Engage the user with questions, offer tips, and provide resources to help them improve their health."))

(defun insert-prompt-entity-simulation-3 ()
  "Insert prompt for an entity simulation."
  (interactive)
  (insert "As an AI chatbot, simulate a conversation with a user who is seeking advice on how to improve their diet. Engage the user with questions, offer tips, and provide resources to help them improve their diet."))

;; Story 
(defun insert-prompt-story-structure ()
  "Insert prompt for writing a story with a specific structure."
  (interactive)
  (insert "Following this story structure — 1. Capture the heart, 2. Set up a tension, 3. Resolve the tension, 4. Conclude by offering value — write a 1,000-word story at a grade-five reading level in the first person using the following information:"))

;; Authors
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
