;;; emacs-ai-utils.el --- Utilities and setup for AI

;; Copyright (C) 2023 Jason Walsh 
;; Authors: Jason Walsh <j@wal.sh>

;;; Commentary:
;; This package provides utilities and setup for AI interaction with Emacs.

; Included are functions for data manipulation, machine learning, and natural language processing.

;;; Code:


(require 'cl-lib)

(defgroup ai-utils nil
  "Customizations for the Emacs AI utilities."
  :prefix "ai-utils-"
  :group 'applications)

(defcustom ai-utils-max-prompt-size 4096
  "Maximum number of characters allowed in a ChatGPT prompt."
  :type 'integer
  :group 'ai-utils)

(defun ai-utils-limit-buffer-size ()
  "Limit buffer size to avoid exceeding ChatGPT maximum submission size."
  (interactive)
  (let* ((max-size ai-utils-max-prompt-size)
         (prompt (buffer-substring-no-properties (point-min) (line-end-position)))
         (content (buffer-substring-no-properties (line-end-position) (point-max)))
         (total-length (+ (length prompt) (length content))))
    (if (> total-length max-size)
        (progn
          (message "Buffer size exceeds ChatGPT maximum submission size. Truncating buffer...")
          (let ((excess-length (- total-length max-size)))
            (delete-region (- (length prompt) excess-length) (point-max))))
      (message "Buffer size is within ChatGPT maximum submission size."))))

(defun ai-utils-copy-to-kill-ring ()
  "Copy the first `ai-utils-max-prompt-size' characters of the buffer to the kill ring."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (min (point-max) ai-utils-max-prompt-size))))
    (kill-new content)
    (message "Copied up to %d characters into the kill ring." ai-utils-max-prompt-size)))

(defun ai-utils-summarize (url)
  "Insert a summarization block for an article from URL into the current org document."
  (interactive "sURL: ")
  (insert "\n#+begin_src chatgpt-shell :version \"gpt-4\"
Summarize the article at `" url "` in org-mode format using the title of the article at level 3 (also include a reference to the article URL, publish date, author, etc. as available) then all other points as level 3 headlines (*** <header>). If suitable, suggest code, model, or simulation examples in Clojure. Ensure headers and code blocks are formatted correctly for org-mode (e.g., #+begin_src blocks).  
#+end_src\n"))


(defun ai-utils-explain (url)
  "Insert an explanation block for an article from URL into the current org document."
  (interactive "sURL: ")
  (insert "\n#+begin_src chatgpt-shell :version \"gpt-4\"
In the style of Hannah Arendt, provide a longform explanation of the article at ~" url "~:

- Summarize the key points  
- Explain specific evidence
- Evaluate results and emphasize limitations, caveats, practicality and consequences for human destiny
- Discuss anything surprising or unexpected and be specific
- Generate 3 questions the author should be asked or I as a reader should be able to answer

Format as org-mode headings at level 3. If appropriate, suggest code examples in Clojure using #+begin_src blocks with ~ for code literals. Suggest a filename to save this summary.

The response structure should be (without the initial spaces):

 *** Summary
 *** Impact 
 *** Code
 *** Questions

Do this without apologizing for being an AI or mentioning the complexity of the request. 

#+end_src
"))

(defun ai-utils-bootstrap (topic)
  "Insert a learning bootstrap block for TOPIC into the current org document."
  (interactive "sTopic: ")
  (insert "\n#+begin_src chatgpt-shell :version \"gpt-4\"
Bootstrap the education of a software developer with Python experience on `" topic "`:

- Provide a learning roadmap in org-mode format starting at heading level 3 (*** <header>)
- Include key concepts, technologies, and skills to learn
- Suggest courses, books, tutorials, and projects with links 
- Prioritize and sequence the material logically

#+end_src
"))
  
(defun ai-utils-cyberpunk-url (url)
  "Insert AI image generation blocks for cyberpunk themes.

Prompt GPT to generate prompts based on the story at URL."
  (interactive "sURL: ")
  (insert "\n#+begin_src chatgpt-shell :version \"gpt-4\"

Read the following web site and generate five cyberpunk-themed image prompts that relate to the content. Be explicit about style, location, and feel of the generated image. Each of the topics should be surrounded by #+begin_ai blocks like the following:

 #+begin_ai :image :size 256x256
 <topic and prompt>
 #+end_ai

- " url "

#+end_src"))

(defun ai-utils-cyberpunk ()
  "Insert an empty summarization block into the current document."
  (interactive)
  (insert "\n#+begin_src chatgpt-shell :version \"gpt-4\"\nRead the following and given the topics noted generate five image prompts of the topic and a cyberpunk theme:\n\n- \n\nBe explicit about style, location, and feel of the generated image.\n#+end_src"))

(defun ai-utils-insert-ai ()
  "Insert an empty AI block into the current document."
  (interactive)
  (insert "\n#+begin_ai\n\n#+end_ai\n"))

(defun ai-utils-insert-ai-image ()
  "Insert an empty AI block into the current document."
  (interactive)
  (insert "\n#+begin_ai :image :size 256x256\n\n#+end_ai\n"))

(defun ai-utils-insert-ai-image-1080x1080 ()
  "Insert an empty AI block into the current document."
  (interactive)
  (insert "\n#+begin_ai :image :size 1080x1080\n\n#+end_ai\n"))

(defun ai-utils-insert-ai-image-720x720 ()
  "Insert an empty AI block into the current document."
  (interactive)
  (insert "\n#+begin_ai :image :size 720x720\n\n#+end_ai\n"))

(defun ai-utils-insert-ai-image-480x480 ()
  "Insert an empty AI block into the current document."
  (interactive)
  (insert "\n#+begin_ai :image :size 480x480\n\n#+end_ai\n"))

(defun ai-utils-insert-ai-image-320x320 ()
  "Insert an empty AI block into the current document."
  (interactive)
  (insert "\n#+begin_ai :image :size 320x320\n\n#+end_ai\n"))

(defun ai-utils-insert-prompt-shell-script ()
  "Insert prompt for creating a shell script with a specified naming convention."
  (interactive)
  (insert "Please provide a shell script that performs the following tasks [describe tasks here]. When naming the script file, please use dashes to delimit words in the filename, following the convention commonly used in bash, sh, and zsh scripts.\n\n"))

(defun ai-utils-fix-program ()
  "Insert prompt for fixing a script according to best practices."
  (interactive)
  (insert "Update the following program to adhere to best practices for all programming languages. Implement appropriate documentation practices, creating a test harness, and incorporating command line access where possible. Use appropriate design patterns, such as Enums and Classes, to decompose the program. The program follows: \n\n"))

(defun ai-utils-repository ()
  "Insert prompt for fixing a script according to best practices."
  (interactive)
  (insert "Create a shell script that build out a repository for a 1 week course covering the topic noted below. Use the canonical programming language for that topic and name the repoistory, directories, and files correctly for the the language. Add build tooling and include a Makefile in addition to version managing tools. Include dependency managers as appropriate. For code use best practices like typing, docstrings, REPL, and command line access. Topic: \n\n"))

(defun ai-utils-insert-prompt-python-simulator ()
  "Insert prompt for creating a simulation using Click in Python with typing, docstrings, and standard nomenclature."
  (interactive)
  (insert "Create a basic command-line interface in Python using Click, typing, and docstrings that models the following and runs simulation when executed from the command line. Please adhere to standard Python nomenclature and include type annotations as well as descriptive docstrings for functions and classes. Describe the tasks here: \n\n"))

(defun ai-utils-insert-prompt-social-media-post-sartre ()
  "Insert prompt for writing a social media post in a conversational first-person tone like Sartre."
  (interactive)
  (insert "Write a 400-word social media post in a conversational first-person tone like Sartre about the following topic: "))

(defun ai-utils-insert-prompt-social-media-post-joyce ()
  "Insert prompt for writing a social media post in the experimental and stream-of-consciousness style of James Joyce."
  (interactive)
  (insert "Write a 400-word social media post in the experimental and stream-of-consciousness style of James Joyce about the following topic: "))

(provide 'emacs-ai-utils)

;;; emacs-ai-utils.el ends here
