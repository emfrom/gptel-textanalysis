

(defvar gptel-textanalysis-temp-buffer-name "*gptel text analysis temp*")

(defun gptel-textanalysis--append-to-analysis-buffer (response)
  "Append RESPONSE to the analysis buffer."
  (let ((buf (get-buffer-create gptel-textanalysis-analysis-temp-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\n\n---\n\n" response))))

(defun gptel-textanalysis-send-string-and-buffer (prompt buffer)
  "Send PROMPT and contents of BUFFER to GPTel."
  (let ((content (with-current-buffer buffer (buffer-string))))
    (gptel-textanalysis-send
     (concat prompt "\n\n" content)
     :callback #'gptel-textanalysis--append-to-analysis-buffer)))

(defun gptel-textanalysis-send-string-and-file (prompt file)
  "Send PROMPT and contents of FILE to GPTel."
  (let ((content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
    (gptel-textanalysis-send
     (concat prompt "\n\n" content)
     :callback #'gptel-textanalysis--append-to-analysis-buffer)))


(defvar gptel-textanalysis-temp-buffer-name "*gptel text analysis temp*")

(defvar gptel-textanalysis-questions
  '(("Analyse this with respect to rhetorical devices." . "rhetorical devices")
    ("Analyse this with respect to argument structure." . "argument structure")
    ("Analyse this with respect to tone and mood." . "tone and mood")
    ("Analyse this with respect to implicit assumptions." . "implicit assumptions")
    ("Analyse this with respect to narrative perspective." . "narrative perspective")
    ("Analyse this with respect to lexical choice." . "lexical choice")
    ("Analyse this with respect to coherence and cohesion." . "coherence and cohesion")
    ("Analyse this with respect to logical consistency." . "logical consistency")
    ("Analyse this with respect to persuasive strategies." . "persuasive strategies")
    ("Analyse this with respect to stylistic register." . "stylistic register")
    ("Analyse this with respect to bias or partiality." . "bias or partiality")
    ("Analyse this with respect to emotional appeal." . "emotional appeal")
    ("Analyse this with respect to clarity and ambiguity." . "clarity and ambiguity")
    ("Analyse this with respect to intended audience." . "intended audience")
    ("Analyse this with respect to use of figurative language." . "figurative language")
    ("Analyse this with respect to thematic content." . "thematic content")
    ("Analyse this with respect to cultural references." . "cultural references")
    ("Analyse this with respect to sentence complexity." . "sentence complexity")
    ("Analyse this with respect to formality level." . "formality level")
    ("Analyse this with respect to use of evidence." . "use of evidence")
    ("Analyse this with respect to formal and informal fallacies." . "formal and informal fallacies")
    ("Analyse this with respect to match between questions formulated and conclusions given." . "match between questions and conclusions")
    ("Analyse this with respect to human rights." . "human rights")
    ("Analyse this with respect to goals, values and editorial standards expressed by Reuters." . "Reuters editorial standards")))

(defun gptel-textanalysis--clear-or-create-answer-buffer ()
  "Clear the answer buffer or create it if it doesn't exist."
  (let ((buf (get-buffer-create gptel-textanalysis-temp-buffer-name)))
    (with-current-buffer buf
      (erase-buffer))))

(defun gptel-textanalysis-send-questions-from-buffer ()
  "Send each question from the alist using the contents of the current buffer."
  (gptel-textanalysis--clear-or-create-answer-buffer)
  (let ((content (buffer-string)))
    (dolist (question gptel-textanalysis-questions)
      (gptel-textanalysis-send-string-and-buffer
       (car question)    ; Prompt: the question
       (current-buffer))))) ; Buffer: the current buffer content

(defun gptel-textanalysis-send-questions-from-file (file)
  "Send each question from the alist using the contents of FILE."
  (gptel-textanalysis--clear-or-create-answer-buffer)
  (let ((content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
    (dolist (question gptel-textanalysis-questions)
      (gptel-textanalysis-send-string-and-file
       (car question)
       file))))

(defvar gptel-textanalysis-summary_prompt "Summarise the analysis of the following.")

(defun gptel-textanalysis-send-summary-from-buffer ()
  "Send the summary of the temporary buffer using the current buffer."
  (interactive)
  (gptel-textanalysis--clear-or-create-answer-buffer)
  (let ((content (with-current-buffer gptel-textanalysis-temp-buffer-name
                   (concat gptel-textanalysis-summary_prompt "\n\n"
                           (buffer-string) "\n\n====+===="))))
    (gptel-textanalysis-send-string-and-buffer
     ""  ; Empty string as the string argument
     gptel-textanalysis-temp-buffer-name)
    (gptel-textanalysis--rename-and-display-buffer)))

(defun gptel-textanalysis-send-summary-from-file (file)
  "Send the summary of the temporary buffer using the contents of FILE."
  (interactive "fFile: ")
  (gptel-textanalysis--clear-or-create-answer-buffer)
  (let ((content (with-temp-buffer
                   (insert-file-contents file)
                   (concat gptel-textanalysis-summary_prompt "\n\n"
                           (buffer-string) "\n\n====+===="))))
    (gptel-textanalysis-send-string-and-file
     ""  ; Empty string as the string argument
     file)
    (gptel-textanalysis--rename-and-display-buffer)))

(defun gptel-textanalysis--rename-and-display-buffer ()
  "Rename the temporary buffer, make it visible, and scroll to the last answer."
  (let ((temp-buf (get-buffer gptel-textanalysis-temp-buffer-name)))
    (when temp-buf
      ;; Rename the temporary buffer
      (rename-buffer "*gptel text analysis*" t)
      (with-current-buffer "*gptel text analysis*"
        ;; Display the buffer if it's not already visible
        (display-buffer (current-buffer))
        ;; Search for the "====+====" marker and move to the next line
        (goto-char (point-min))
        (if (re-search-forward "====+====" nil t)
            (forward-line 1)
          (goto-char (point-max))) ; If marker not found, go to the end
        ;; Recenter to ensure the next line is visible
        (recenter -1)))))

