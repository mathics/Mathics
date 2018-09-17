I am able to collect output data from a process, but I don't understand the
coordination of `point` and `process-mark`. In particular, the visible cursor in
a non-selected window visiting the buffer does not advance, even though `point`
is reported at correct locations when I access the variable. Selecting the
window moves `point` to an incorrect location.

(**EDIT**: in an earlier version of this question, I had some basic confusion about
windows, frames, selection, and focus. I consulted the [documentation here][2]
and edited this question to be coherent with emacs definitions.)

I'd be grateful for some explanations and corrections to the following MVE
(minimal viable example). First, I start a process:

    (start-process-shell-command
     "foo-process"
     "foo-buffer"
     "python")

I open another window to watch what's going on: <kbd>C-x</kbd><kbd>3</kbd>,
visit foo-buffer in that window <kbd>C-x</kbd><kbd>b</kbd>`foo-buffer`, and
select my programming window window again <kbd>C-x</kbd><kbd>o</kbd>. The window
visiting foo-buffer is not selected at this time, but a shadowy cursor is
visible in the window.

I set up a couple of global variables for debugging and for collecting output
and a function to report on the state:

    (defvar my-python-buffer-data '())
    (defvar my-python-latest-output "")

    (defun report ()
      (with-current-buffer "foo-buffer"
        `((:point-min     ,(point-min))
          (:point         ,(point))
          (:point-max     ,(point-max))
          (:process-mark  ,(process-mark (get-process "foo-process")))
          (:buffer-data   ,my-python-buffer-data)
          (:latest-output ,my-python-latest-output))))

Before sending data to the buffer, I check the state by calling `report`:

    (report)

    ((:point-min 1)
     (:point 149)
     (:point-max 149)
     (:process-mark #<marker at 149 in foo-buffer>)
     (:buffer-data nil)
     (:latest-output ""))

Things seems consistent: the point and process marker are at the end of the
buffer. I can also see that the shadowy cursor in the other window (which is not
selected) is at the end of the buffer.

I now set up my custom "insertion filter," following the documentation in
section 38.9.2 of the elisp manual at [this link address][1]. I am using the
documented option of unconditionally setting `point` via `goto-char` at the end
of the filter function. I want point to be always at the end of the new output.

I collect data about point and the process mark as I go along, so I need a
function to do that:

    (defun collect-python-buffer-data (sigil)
      (setq my-python-buffer-data
            (cons
             `(:sigil ,sigil :marker ,(process-mark proc) :point ,(point))
             my-python-buffer-data)))

and here is my filter function, copy-pasted and modded from the doc:

    (defun extracting-insertion-filter (proc string)
      (setq my-python-buffer-data '())
      (setq my-python-latest-output "")
      (let ((pb (process-buffer proc)))
       (when (buffer-live-p pb)
         (with-current-buffer pb
           (save-excursion
             (collect-python-buffer-data 1)
             (goto-char (process-mark proc))
             (collect-python-buffer-data 2)
             (insert string) ; advances point
             (collect-python-buffer-data 3)
             (set-marker (process-mark proc) (point))
             (collect-python-buffer-data 4))
           (goto-char (process-mark proc))
           (collect-python-buffer-data 5)))
       ;; for reporting:
       (setq my-python-buffer-data (reverse my-python-buffer-data))
       (setq my-python-latest-output string)))

I now set the process filter for the process to my new function:

    (set-process-filter
     (get-process "foo-process")
     #'extracting-insertion-filter)

and send some data to my process:

    (process-send-string "foo-process" "420 / 3\n")

Now report:

    (report)

    ((:point-min 1)
     (:point 157)
     (:point-max 157)
     (:process-mark #<marker at 157 in foo-buffer>)
     (:buffer-data ((:sigil 1 :marker #<marker at 157 in foo-buffer> :point 149)
                    (:sigil 2 :marker #<marker at 157 in foo-buffer> :point 149)
                    (:sigil 3 :marker #<marker at 157 in foo-buffer> :point 157)
                    (:sigil 4 :marker #<marker at 157 in foo-buffer> :point 157)
                    (:sigil 5 :marker #<marker at 157 in foo-buffer> :point 157)))
     (:latest-output "140
    >>> "))

Things look OK, here. I get my output, and `point` and `process-mark` are at the
end, where I want them.

However, the little shadowy cursor is at the wrong place in the non-selected
window that shows my buffer. Before selecting that window (and spoiling the
state), let me make sure that I can send more data over:

    (process-send-string "foo-process" "42 / 3\n")

    (report)

    ((:point-min 1)
     (:point 164)
     (:point-max 164)
     (:process-mark #<marker at 164 in foo-buffer>)
     (:buffer-data ((:sigil 1 :marker #<marker at 164 in foo-buffer> :point 149)
                    (:sigil 2 :marker #<marker at 164 in foo-buffer> :point 157)
                    (:sigil 3 :marker #<marker at 164 in foo-buffer> :point 164)
                    (:sigil 4 :marker #<marker at 164 in foo-buffer> :point 164)
                    (:sigil 5 :marker #<marker at 164 in foo-buffer> :point 164)))
     (:latest-output "14
    >>> "))

Everything is OK in the program, but the cursor in the non-selected window has
not moved. If I select that that window and un-select it, the party is spoiled,
i.e., `point` moved from 164 (where I want it) to 149, where it should
not have been after the initialization of the process buffer.
<kbd>C-x</kbd><kbd>o</kbd><kbd>C-x</kbd><kbd>o</kbd>

    (with-current-buffer "foo-buffer" (point))

    149

Again, I'd be grateful for guidance on how to fix this. I tried the `moving`
option [in the documentation][1], but the effect is the same.

[1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html#Filter-Functions
[2]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Windows.html#Basic-Windows
