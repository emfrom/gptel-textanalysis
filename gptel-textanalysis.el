;;; gptel-textanalysis.el --- GPTel text analysis functions -*- lexical-binding: t; -*-

;; Author: E.M. From
;; Version: 0.1
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

(defvar gptel-textanalysis-temp-buffer-name "*gptel text analysis temp*")

(defvar gptel-textanalysis-questions
  '("Analyse this with respect to rhetorical devices."
    "Analyse this with respect to argument structure."
    "Analyse this with respect to tone and mood."
    "Analyse this with respect to implicit assumptions."
    "Analyse this with respect to narrative perspective."
    "Analyse this with respect to lexical choice."
    "Analyse this with respect to coherence and cohesion."
    "Analyse this with respect to logical consistency."
    "Analyse this with respect to persuasive strategies."
    "Analyse this with respect to stylistic register."
    "Analyse this with respect to bias or partiality."
    "Analyse this with respect to emotional appeal."
    "Analyse this with respect to clarity and ambiguity."
    "Analyse this with respect to intended audience."
    "Analyse this with respect to use of figurative language."
    "Analyse this with respect to thematic content."
    "Analyse this with respect to cultural references."
    "Analyse this with respect to sentence complexity."
    "Analyse this with respect to formality level."
    "Analyse this with respect to use of evidence."
    "Analyse this with respect to formal and informal fallacies."
    "Analyse this with respect to match between questions formulated and conclusions given."
    "Analyse this with respect to human rights."
    "Analyse this with respect to goals, values and editorial standards expressed by Reuters."))


(defvar gptel-textanalysis-summary-prompt
  (concat "Summarise the following down to 500 words\n"
	  "Prioritize very strictly in the following order\n"
	  "1. Violations or encouragment of violations of human right or law\n"
	  "2. Clear departures from Reuters editorial standards and values\n"
	  "3. Clear departures from evidentiary standards and lacking or incomplete sources\n"
	  "4. Depatures in language, clarity and assumptions from good writing aimed at objectivity\n"
	  "5. Tone, emotional and ecultural aspects.\n"
	  "6. General improvements not covered by previous points.\n"
	  "Prioritize means: dont analyse lower priorities if there are things to say about higher ones\n"
	  "TEXT FOLLOWS THIS LINE\n"))


;;
;; So this way is very slow, but works
;; 
(defun gptel-textanalysis--process-list (text questions-list summarise-fn)
  "Pass TEXT and all QUESTIONS-LIST to gpt, then call SUMMARISE-FN at the end."
  (let ((buf (get-buffer-create gptel-textanalysis-temp-buffer-name)))
    (cl-labels ((step (xs)
                  (gptel-request
			(concat (car xs) "\n\n" text)
                      :callback (lambda (response _event)
				  (with-current-buffer buf
				    (goto-char (point-max))
				    (insert "\n## " (car xs) "\n\n" response))
				  (if (null (cdr xs))
				      (funcall summarise-fn buf)
				    (step (cdr xs)))))))
      (step questions-list))))

(defun gptel-textanalysis--summarise-callback (result buf)
  "Prepends RESULT to output buffer BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (insert "# Text analysis\n\n## Summary\n\n" result "\n")
    (rename-buffer "*gptel text analysis*" t)
    (markdown-mode)
    (message "LLM Textanalysis complete")))

(defun gptel-textanalysis--summarise (buf)
  "Create a summary of buffer BUF containing partial text analyses."
  (gptel-request
      (concat gptel-textanalysis-summary-prompt (buffer-string buf))
    :callback (lambda (response _event)
		(gptel-textanalysis--summarise-callback response buf))))

(defun gptel-textanalysis-buffer ()
  "Perform a two round text analysis of current buffer."
  (interactive)
  (gptel-textanalysis--process-list
   (buffer-string)
   gptel-textanalysis-questions
   #'gptel-textanalysis--summarise))


;; ;;
;; ;; I dont like the messiness of the following way, very ugly
;; ;; 

;; (defun gptel-textanalysis--append-to-analysis-buffer (prompt response)
;;   "Append RESPONSE to the analysis buffer, indicating what PROMPT was asked."
;;   (let ((buf (get-buffer-create gptel-textanalysis-temp-buffer-name)))
;;     (with-current-buffer buf
;;       (goto-char (point-max))
;;       (insert (concat "---   " prompt "\n\n") response))
;;     ))

;; (defun gptel-textanalysis--make-callback (prompt)
;;   "Create a gptel callback for PROMPT."
;;   (lambda (response _event)
;;     (gptel-textanalysis--append-to-analysis-buffer prompt response)
;;     (message "GPT response: %s" response)))


;; (defun gptel-textanalysis-send-string-and-buffer (prompt buffer)
;;   "Send PROMPT and contents of BUFFER to GPTel."
;;   (let ((content (with-current-buffer buffer (buffer-string))))
;;     (gptel-request
;;      (concat prompt "\n\n" content)
;;      :callback (gptel-textanalysis--make-callback prompt))))


;; (defun gptel-textanalysis--clear-or-create-answer-buffer ()
;;   "Clear the answer buffer or create it if it doesn't exist."
;;   (let ((buf (get-buffer-create gptel-textanalysis-temp-buffer-name)))
;;     (with-current-buffer buf
;;       (erase-buffer))))

;; (defun gptel-textanalysis-send-questions-from-buffer ()
;;   "Send each question from the list using the contents of the current buffer."
;;   (interactive)
;;   (gptel-textanalysis--clear-or-create-answer-buffer)
;;   (dolist (question gptel-textanalysis-questions)
;;     (gptel-textanalysis-send-string-and-buffer
;;      question
;;      (current-buffer))))

;; (defun gptel-textanalysis-send-summary-from-buffer ()
;;   "Send the summary of the temporary buffer using the current buffer."
;;   (interactive)
;;     (gptel-textanalysis-send-string-and-buffer
;;      gptel-textanalysis-summary-prompt
;;      gptel-textanalysis-temp-buffer-name))
    

;; (defun gptel-textanalysis--rename-and-display-buffer ()
;;   "Rename the temporary buffer, make it visible, and scroll to the last answer."
;;   (let ((temp-buf (get-buffer gptel-textanalysis-temp-buffer-name)))
;;     (when temp-buf
;;       ;; Rename the temporary buffer
;;       (rename-buffer "*gptel text analysis*" t)
;;       (with-current-buffer "*gptel text analysis*"
;;         ;; Display the buffer if it's not already visible
;;         (display-buffer (current-buffer))
;;         ;; Search for the "====+====" marker and move to the next line
;;         (goto-char (point-min))
;;         (if (re-search-forward "====+====" nil t)
;;             (forward-line 1)
;;           (goto-char (point-max))) ; If marker not found, go to the end
;;         ;; Recenter to ensure the next line is visible
;;         (recenter -1)))))

(provide 'gptel-textanalysis)
;;; gptel-textanalysis.el ends here
