;;; gptel-textanalysis.el --- GPTel text analysis functions -*- lexical-binding: t; -*-

;; Author: E.M. From
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: gptel, text analysis
;; URL:https://github.com/emfrom/gptel-textanalysis


;;; Commentary:

;; This file contains functions for performing text analysis using GPTel
;; as the LLM intermediary.
;; 
;; Proof-of-concept version

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'threads)

(defvar gptel-textanalysis-buffer-name "*gptel text analysis*")

(defvar gptel-textanalysis-questions
  '("Rhetorical devices"
    "Argument structure"
    "Tone and mood"
    "Implicit assumptions"
    "Narrative perspective"
    "Lexical choice"
    "Coherence and cohesion"
    "Logical consistency"
    "Persuasive strategies"
    "Stylistic register"
    "Bias or partiality"
    "Emotional appeal"
    "Clarity and ambiguity"
    "Intended audience"
    "Use of figurative language"
    "Thematic content"
    "Cultural references"
    "Sentence complexity"
    "Formality level"
    "Use of evidence"
    "Formal and informal fallacies"
    "Match between questions formulated and conclusions given"
    "Human rights"
    "Goals, values and editorial standards expressed by Reuters"))


(defvar gptel-textanalysis-summary-prompt
  (concat "Summarise the following down to 200 words\n"
	  "Prioritize very strictly in the following order\n"
	  "1. Violations or encouragment of violations of human right or law\n"
	  "2. Clear departures from Reuters editorial standards and values\n"
	  "3. Clear departures from evidentiary standards and lacking or incomplete sources\n"
	  "4. Departures in language, clarity and assumptions from good writing aimed at objectivity\n"
	  "5. Tone, emotional and cultural aspects.\n"
	  "6. General improvements not covered by previous points.\n"
	  "Prioritize means: dont analyse lower priorities if there are things to say about higher ones\n"
	  "TEXT TO ANALYSE FOLLOWS THIS LINE\n\n"))


(defun gptel-textanalysis--summarise (response-list)
  "Create a buffer containing partial text analyses from RESPONSE-LIST."
  (let ((buf (generate-new-buffer gptel-textanalysis-buffer-name)))
    (with-current-buffer buf
      (markdown-mode)
      (dolist (item response-list)
	(insert "## " (car item) "\n\n" (cdr item) "\n\n")
	(gptel-request
	    (concat gptel-textanalysis-summary-prompt (buffer-string))
	  :callback (lambda (response _event)
                      (with-current-buffer buf
			(goto-char (point-min))
			(insert "# Text analysis\n\n## Summary\n\n" response "\n\n")
			(message "LLM Textanalysis complete"))))))))

;;
;; Use a proper mutex for sync, much faster
;; 
(defun gptel-textanalysis--process-list (text questions-list)
  "Pass TEXT and all QUESTIONS-LIST to GPT."
  (let ((lock (make-mutex "gptel-textanalysis--lock"))
        (response-list nil))
    (dolist (question questions-list)
      (gptel-request (concat
                      "Analyse the following text with respect to: "
                      question
                      "\n"
                      "Be as terse and to the point as possible\n"
		      "Stay below 150 words\n"
		      "Return the result in **Markdown format**\n"
		      "TEXT TO ANALYSE FOLLOWS THIS LINE\n\n"
                      text)
        :callback (lambda (response _event)
                    (mutex-lock lock)
                    (push (cons question response) response-list)
                    (if (= (length response-list) (length questions-list))
                        ;; All callbacks are done
                        (gptel-textanalysis--summarise response-list))
		    (mutex-unlock lock))))))


(defun gptel-textanalysis-buffer ()
  "Perform a two round text analysis of current buffer."
  (interactive)
  (gptel-textanalysis--process-list
   (buffer-string)
   gptel-textanalysis-questions))


(provide 'gptel-textanalysis)
;;; gptel-textanalysis.el ends here
